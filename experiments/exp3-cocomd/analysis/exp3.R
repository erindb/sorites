library(plyr)
library(rjson)
library(ggplot2)
source("~/opt/r_helper_scripts/bootsSummary.r")
source("~/opt/r_helper_scripts/json_to_r.R")

alpha=1
cost=1

d = json_to_r()
d$response = as.numeric(d$response)

# p = ggplot(data=d, aes(x=dollar_amount, y=response, colour=qtype)) +
#   geom_point(size=3) +
#   geom_line() +
#   facet_grid(workerid ~ object, scale="free") +
#   theme_bw(12) +
#   theme(panel.grid=element_blank()) +
#   scale_colour_brewer(type='qual', palette='Set1') +
#   scale_fill_brewer(type='qual', palette='Set1')
# print(p)
# ggsave("sorites_by_worker.pdf", width=8.5, height=20)

d_summary = bootsSummary(data=d, measurevar="response",
                         groupvars=c("object", "qtype", "dollar_amount", "level"))
                         
p = ggplot(data=d_summary, aes(x=dollar_amount, y=response, colour=qtype)) +
  geom_point(size=3) +
  geom_line() +
  geom_errorbar(aes(x=dollar_amount, ymin=bootsci_low, ymax=bootsci_high), width=0) +
  facet_grid(. ~ object, scale="free") +
  theme_bw(12) +
  theme(panel.grid=element_blank()) +
  scale_colour_brewer(type='qual', palette='Set1') +
  scale_fill_brewer(type='qual', palette='Set1')
print(p)
ggsave("sorites.pdf", width=8.5, height=3)

### load model

eps = read.table("eps.csv", sep=",", header=T)
vals = read.table("vals.csv", sep=",", header=T)
colnames(eps)[colnames(eps) == "coffee.maker"] = "coffee maker"
colnames(vals)[colnames(vals) == "coffee.maker"] = "coffee maker"

objects = c("laptop", "watch", "coffee maker", "sweater", "headphones")

m = read.table("all_model_output.csv", sep=",",
               col.names=c("prior_version", "item", "price", "theta", "probability",
                           "cost", "alpha"))
m = subset(m, prior_version == "exp1" & alpha==alpha & cost==cost & item != "object")
m$item = factor(m$item)
m$probability = as.numeric(as.character(m$probability))
m$theta = as.numeric(as.character(m$theta))
m$price = as.numeric(as.character(m$price))

model = data.frame(
  qtype = c(rep("inductive", 25), rep("concrete", 25)),
  object = rep(objects, 10),
  dollar_amount = c( c(t(eps)), c(t(vals)) )
)
model$response = sapply(1:nrow(model), function(i) {
#   subm = subset(m, item == model$object[[i]])
  #   if (model$qtype[[i]] == "inductive") {
  #     return(sum(subm$probability[ subm$price - model$dollar_amount[[i]] > subm$theta ]))
  #   } else {
  #     return(sum(subm$probability[ model$dollar_amount[[i]] > subm$theta ]))
  #   }
  if (model$qtype[[i]] == "inductive") {
    return(sum(m$probability[ m$price - model$dollar_amount[[i]] > m$theta &
                                m$item == model$object[[i]] ]))
  } else {
    return(sum(m$probability[ model$dollar_amount[[i]] > m$theta &
                                m$item == model$object[[i]] ]))
  }
})

p = ggplot(data=model, aes(x=dollar_amount, y=response, colour=qtype)) +
  geom_point(size=3) +
  geom_line() +
  facet_grid(. ~ object, scale="free") +
  theme_bw(12) +
  theme(panel.grid=element_blank()) +
  scale_colour_brewer(type='qual', palette='Set1') +
  scale_fill_brewer(type='qual', palette='Set1')
print(p)
ggsave(paste("model_alpha", alpha, "_cost", cost, ".pdf", sep=""), width=8.5, height=3)

### model versus data

modvd = d_summary
names(modvd)[names(modvd) == "response"] = "people_response"
modvd$N = NULL
modvd$model_probability = sapply(1:nrow(modvd), function(i) {
  return( model$response[ model$qtype == modvd[i, "qtype"] &
                             model$object == modvd[i,"object"] &
                             model$dollar_amount == modvd[i, "dollar_amount"] ][[1]] )
})
modvd$object = factor(modvd$object, levels=objects)
p = ggplot(data=modvd, aes(x=model_probability, y=people_response, colour=object, shape=qtype)) +
  geom_point(size=4) +
  geom_errorbar(aes(x=model_probability, ymin=bootsci_low, ymax=bootsci_high)) +
  theme_bw(12) +
  theme(panel.grid=element_blank()) +
  scale_colour_brewer(type='qual', palette='Set1') +
  scale_fill_brewer(type='qual', palette='Set1')
print(p)
ggsave(paste("sorites_modelvdata_alpha", alpha, "_cost", cost, ".pdf", sep=""), width=8.5, height=5)
cor.test(modvd$people_response, modvd$model_probability)
## correlaton = 0.72

inductive_modvd = subset(modvd, qtype == "inductive")
p = ggplot(data=inductive_modvd, aes(x=model_probability, y=people_response, colour=object)) +
  geom_point(size=4) +
  geom_errorbar(aes(x=model_probability, ymin=bootsci_low, ymax=bootsci_high)) +
  theme_bw(12) +
  theme(panel.grid=element_blank()) +
  scale_colour_brewer(type='qual', palette='Set1') +
  scale_fill_brewer(type='qual', palette='Set1')
print(p)
ggsave(paste("sorites_inductive_modelvdata_alpha", alpha, "_cost", cost, ".pdf", sep="")
       , width=8.5, height=5)
cor.test(inductive_modvd$people_response, inductive_modvd$model_probability)
## correlation = 0.92