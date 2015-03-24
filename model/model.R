library(plyr)
library(rjson)
library(ggplot2)
source("~/opt/r_helper_scripts/bootsSummary.r")

var_and_eps = read.table("vars_and_eps.csv", sep=",", header=T)
d = read.table("all_model_output.csv", sep=",",
               col.names=c("prior_version", "item", "price", "theta", "probability",
                           "cost", "alpha"))
d$probability = as.numeric(as.character(d$probability))
model = rbind(var_and_eps, var_and_eps)
model$prior_version = c(rep("v4", nrow(var_and_eps)), rep("ebay", nrow(var_and_eps)))
model = ddply(.data=model, .(item, dollarAmt, qType, prior_version), .fun=function(subd) {
  item = subd$item[[1]]
  dollarAmt = subd$dollarAmt[[1]]
  qType = subd$qType[[1]]
  prior_version = subd$prior_version[[1]]
  if (qType == "eps") {
    subd$model = sum(d$probability[d$price - dollarAmt > d$theta &
                                     d$prior_version == prior_version &
                                     d$item == item])
  } else {
    subd$model = sum(d$probability[dollarAmt > d$theta &
                                     d$prior_version == prior_version &
                                     d$item == item])
  }
  return(subd)
})

model$item = factor(model$item,
                    levels=c("laptop", "watch", "coffee maker", "sweater", "headphones"))

model = subset(model, prior_version == "v4")
p = ggplot(data=model, aes(x=dollarAmt, y=model, colour=item, linetype=qType)) +
  geom_line() +
#   facet_grid(prior_version ~ item, scales="free") +
  facet_grid(. ~ item, scales="free") +
  ggtitle("Sorites Model") +
  ylab("probability") +
  theme_bw(9) +
  theme(panel.grid=element_blank()) +
  scale_colour_brewer(type='qual', palette='Set1') +
  scale_fill_brewer(type='qual', palette='Set1')
print(p)
ggsave("model.pdf", width=8.5, height=3)

write.table(model, "model.csv", sep=",", row.names=F)