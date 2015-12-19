library(ggplot2)
library(plyr)

fit_to_inductive_only = F
experiment_filename = "aggregate_sorites_relative_data.csv"
# experiment_filename = "aggregate_sorites_data.csv"

files <- dir("model_output/", pattern="*.csv")

d = do.call(rbind, lapply(files, function(file) {
  return(read.csv(paste("model_output/", file, sep=""),
           colClasses=c("character",
                        "numeric", "numeric", "numeric",
                        "numeric", "numeric", "numeric"),
           col.names=c("item", "cost", "lambda", "value", "theta", "score", "probability")))
}))

# ### WARNING: IS THIS A PROBABILITY DISTRIBUTION?
# ### rounding error? or bug?
# ddply(d, .(item, cost, lambda), summarise, sum_prob = sum(probability))

marginalized = ddply(d, .(item, cost, lambda), function(df) {
  marginalized_values = ddply(df, .(value), summarise, sum_prob = sum(probability))
  marginalized_thetas = ddply(df, .(theta), summarise, sum_prob = sum(probability))
  newdf = data.frame(
    variable = c(rep("value", nrow(marginalized_values)),
                 rep("theta", nrow(marginalized_thetas))),
    dollar_amount = c(marginalized_values$value, marginalized_thetas$theta),
    probability = c(marginalized_values$sum_prob, marginalized_thetas$sum_prob),
    item = df$item[[1]],
    cost = df$cost[[1]],
    lambda = df$lambda[[1]]
  )
})

# ### WARNING: IS THIS A PROBABILITY DISTRIBUTION?
# ### rounding error? or bug?
# ddply(marginalized, .(item, cost, lambda, variable), summarise, sum_prob = sum(probability))

priors = read.csv("mean_priors.csv", as.is=T)
priors$variable = "value"

draw_priors = do.call(rbind, lapply(1:6, function(i) {
  df = priors
  df$cost = NA
  df$lambda = i
  return(df)
}))

ggplot(marginalized, aes(x=dollar_amount, y=probability,
                         linetype=variable, colour=cost,
                         group=paste(variable, cost, item, lambda))) +
  geom_line() +
  geom_line(data=draw_priors, aes(x=lowers, y=normed_responses), linetype=1, colour="gray") +
  facet_grid(lambda ~ item, scale="free_x")

aggregate_sorites_relative_data = read.csv(experiment_filename)
expt = aggregate_sorites_relative_data[,c("item", "qtype", "dollar_amount", "response")]
expt$model_or_expt = "expt"

items = unique(as.character(expt$item))
dollar_amounts = lapply(items, function(obj) {
  return(unique(expt$dollar_amount[expt$item == obj]))
})
names(dollar_amounts) = items

model = ddply(d, .(item, cost, lambda), function(df) {
  obj = df$item[[1]]
  epsilons = unique(expt$dollar_amount[expt$item == obj & expt$qtype == "inductive"])
  values = unique(expt$dollar_amount[expt$item == obj & expt$qtype == "concrete"])
  new_df = data.frame(
    item = obj,
    cost = df$cost[[1]],
    lambda = df$lambda[[1]],
    qtype = c( rep("concrete", length(epsilons)),
               rep("inductive", length(values)) ),
    dollar_amount = c( values, epsilons ),
    response = c( sapply(values,
                         function(dollar_amount) {
                           ## concrete
                           return(sum(df$probability[dollar_amount > df$theta ])) }),
                  sapply(epsilons,
                         function(dollar_amount) {
                           ## indutive
                           return(sum(df$probability[df$value - dollar_amount > df$theta]))  }) )
  )
})

fits = ddply(model, .(cost, lambda), function(df) {
  if (fit_to_inductive_only) {
    subdf = subset(df, qtype == "inductive")
    subexpt = subset(expt, qtype == "inductive")
    return(cor(subdf$response, subexpt$response))
  } else {
    return(cor(df$response, expt$response))
  }
})

best_parameters = fits[fits$V1 == max(fits$V1),]
best_cost = best_parameters$cost
best_lambda = best_parameters$lambda

best_model = model[model$lambda == best_lambda & model$cost == best_cost, c("item", "qtype", "dollar_amount", "response")]
best_model$model_or_expt = "model"
model_and_expt = rbind(best_model, expt)

model_and_expt_same_scale = model_and_expt
model_and_expt_same_scale$response[model_and_expt_same_scale$model_or_expt == "expt"] = (model_and_expt_same_scale$response[model_and_expt_same_scale$model_or_expt == "expt"] + 4 )/8
ggplot(model_and_expt_same_scale, aes(x=dollar_amount, y=response, colour=item, linetype=model_or_expt)) +
  geom_point(size=2) +
  geom_line() +
  ylim(0, 1) +
  facet_grid(qtype ~ item, scale="free_x")

model_vs_expt = reshape(model_and_expt, direction="wide", v.names="response", timevar="model_or_expt", idvar=c("item", "qtype", "dollar_amount"))
ggplot(model_vs_expt, aes(x=response.model, y=response.expt, colour=item)) +
  geom_point(size=3) +
  facet_wrap(~ qtype)

with(subset(model_vs_expt, qtype == "inductive"), cor.test(response.model, response.expt))

ggplot(subset(marginalized, lambda == best_lambda & cost == best_cost),
       aes(x=dollar_amount, y=probability,
           linetype=variable, colour=item)) +
  geom_line() +
  geom_line(data=draw_priors, aes(x=lowers, y=normed_responses), linetype=1, colour="gray") +
  facet_grid(. ~ item, scale="free_x")