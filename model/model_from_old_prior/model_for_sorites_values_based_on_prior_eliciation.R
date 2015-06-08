library(plyr)
library(rjson)
library(ggplot2)
source("~/opt/r_helper_scripts/bootsSummary.r")

eps = read.table("eps.csv", sep=",", header=T)
vals = read.table("vals.csv", sep=",", header=T)

objects = c("laptop", "watch", "coffee.maker", "sweater", "headphones")

d = read.table("all_model_output.csv", sep=",",
               col.names=c("prior_version", "item", "price", "theta", "probability",
                           "cost", "alpha"))

model = list(
  inductive = lapply(objects, function(obj) {
    return(lapply(eps[, obj], function(dollar_amount) {
      total_probability = sum(d$probability[(d$price - rep(dollar_amount, length(d$theta))) > d$theta])
      return(total_probability)
    }))
  }),
  concrete = lapply(objects, function(obj) {
    object_probabilities = lapply(vals[obj], function(dollar_amount) {
      total_probability = sum(d$probability[d$theta < rep(dollar_amount, length(d$theta))])
      return(total_probability)
    })
    return(object_probabilities)
  })
)
save(model)