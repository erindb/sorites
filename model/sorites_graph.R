library(plyr)
library(rjson)
library(ggplot2)
source("~/opt/r_helper_scripts/bootsSummary.r")

preferred_alpha = 1
preferred_cost = 1

objects = c("laptop", "watch", "coffee maker", "sweater", "headphones")
alphas = c(1, 3, 5)
utterance_costs = c(1, 3, 5)
prior_versions = c("v4_actual")

var_and_eps = read.table("vars_and_eps.csv", sep=",", header=T)

d = read.table("model_output/model_objectcoffee maker_versionv4_actual_alpha1_cost1.csv",
               sep=",", header=T)

for (alpha in alphas) {
  for (utterance_cost in utterance_costs) {
    for (prior_version in prior_versions) {
      for (object in objects) {
        if (alpha != 1 || utterance_cost != 1 || prior_version != "v4_actual" ||
              object != "coffee maker") {
          d = rbind(d, read.table(paste(
            "model_output/model_object", object, "_version", prior_version, "_alpha",
              alpha, "_cost", utterance_cost, ".csv", sep=""), sep=",", header=T))
        }
      }
    }
  }
}
names(d)[names(d) == "object"] = "item"

## convert to long form (for marginal distributions)
d_marginal_price = ddply(.data=d, .(price, alpha, utterance_cost, item, prior_version), summarize,
                         probability = sum(probability))
names(d_marginal_price)[names(d_marginal_price) == "price"] = "value"
d_marginal_price$QUD = "value"
d_marginal_theta = ddply(.data=d, .(theta, alpha, utterance_cost, item, prior_version), summarize, probability = sum(probability))
names(d_marginal_theta)[names(d_marginal_theta) == "theta"] = "value"
d_marginal_theta$QUD = "theta"
d_marginal = rbind(d_marginal_price, d_marginal_theta)
d_marginal$alpha_cost = paste("alpha=", d_marginal$alpha, ",cost=", d_marginal$utterance_cost, sep="")
  
## plot adjective posterior

p = ggplot(data=subset(d_marginal, alpha==preferred_alpha & utterance_cost == preferred_cost),
           aes(x=value, y=probability, colour=QUD)) +
  geom_line() +
  facet_grid(. ~ item, scales="free") +
  ggtitle("Adjectives Model") +
  theme_bw(9) +
  theme(panel.grid=element_blank()) +
  scale_colour_brewer(type='qual', palette='Set1') +
  scale_fill_brewer(type='qual', palette='Set1')
print(p)
ggsave("adjective.pdf", width=8.5, height=3)

d_marginal$alpha_cost = as.factor(d_marginal$alpha_cost)
p = ggplot(data=d_marginal,
           aes(x=value, y=probability, colour=QUD)) +
  geom_line() +
  facet_grid(alpha_cost ~ item, scales="free") +
  ggtitle("Adjectives Model") +
  theme_bw(9) +
  theme(panel.grid=element_blank()) +
  scale_colour_brewer(type='qual', palette='Set1') +
  scale_fill_brewer(type='qual', palette='Set1')
print(p)
ggsave("adjective_parameter_search.pdf", width=8.5, height=6)

## plot sorites model probabilities

model_var_and_eps = ddply(.data=var_and_eps, .(dollarAmt, qType, item), .fun=function(subd) {
  object = subd$item[[1]]
  qType = subd$qType[[1]]
  dollarAmt = subd$dollarAmt[[1]]
  dist = subset(d, alpha==preferred_alpha & utterance_cost == preferred_cost & item == object)
  if (qType == "eps") {
    subd$probability = sum(dist$probability[dist$price - dollarAmt > dist$theta])
  } else {
    subd$probability = sum(dist$probability[dollarAmt > dist$theta])
  }
  return(subd)
})

model_sorites = model_var_and_eps
model_var_and_eps$qType = factor(model_var_and_eps$qType, levels=c("val", "eps"),
                                 labels=c("an X that's $V is expensive",
                                          "an X that's $E less is also expensive"))

p = ggplot(data=model_var_and_eps, aes(x=dollarAmt, y=probability, colour=item, linetype=qType)) +
  geom_line() +
  facet_grid(. ~ item, scale="free") +
  ggtitle("Sorites Model") +
  theme_bw(9) +
  theme(panel.grid=element_blank()) +
  scale_colour_brewer(type='qual', palette='Set1') +
  scale_fill_brewer(type='qual', palette='Set1')
print(p)
ggsave("sorites_model.pdf", width=8.5, height=3)

write.table(model_sorites, file="model_v4.csv", sep=",", row.names=F)