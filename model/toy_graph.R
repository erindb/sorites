library(plyr)
library(rjson)
library(ggplot2)
source("~/opt/r_helper_scripts/bootsSummary.r")

d = read.table("toy_parameter_search/toy_simulation_mean10_sd1_alpha1_cost1.csv", sep=",", header=T)

for (alpha in paste("alpha", 1:9, sep="")) {
  for (utterance_cost in paste("cost", 1:9, sep="")) {
    if (alpha != 1 || utterance_cost != 1) {
      d = rbind(d, read.table(paste("toy_parameter_search/toy_simulation_mean10_sd1_", alpha, "_", utterance_cost, ".csv", sep=""), sep=",", header=T))
    }
  }
}

## convert to long form (for marginal distributions)
d_marginal_price = ddply(.data=d, .(price, alpha, utterance_cost), summarize,
                         probability = sum(probability))
names(d_marginal_price)[names(d_marginal_price) == "price"] = "value"
d_marginal_price$QUD = "value"
d_marginal_theta = ddply(.data=d, .(theta, alpha, utterance_cost), summarize, probability = sum(probability))
names(d_marginal_theta)[names(d_marginal_theta) == "theta"] = "value"
d_marginal_theta$QUD = "theta"
d_marginal = rbind(d_marginal_price, d_marginal_theta)
d_marginal$alpha = paste("alpha", d_marginal$alpha, sep="=")
d_marginal$utterance_cost = paste("cost", d_marginal$utterance_cost, sep="=")
  
p = ggplot(data=d_marginal, aes(x=value, y=probability, colour=QUD)) +
  geom_line() +
  facet_grid(alpha ~ utterance_cost) +
  ggtitle("Adjectives Model: Effects of Parameters") +
  theme_bw(9) +
  theme(panel.grid=element_blank()) +
  scale_colour_brewer(type='qual', palette='Set1') +
  scale_fill_brewer(type='qual', palette='Set1')
print(p)
ggsave("toy_parameter_search.pdf", width=8.5, height=5)

### now with alternatives!!! :)

d = read.table("toy_parameter_search/toy_simulation_mean10_sd1_alpha1_cost1alternative.csv", sep=",", header=T)

for (alpha in paste("alpha", 1:9, sep="")) {
  for (utterance_cost in paste("cost", 1:9, sep="")) {
    if (alpha != 1 || utterance_cost != 1) {
      d = rbind(d, read.table(paste("toy_parameter_search/toy_simulation_mean10_sd1_", alpha, "_", utterance_cost, "alternative.csv", sep=""), sep=",", header=T))
    }
  }
}

## convert to long form (for marginal distributions)
d_marginal_price = ddply(.data=d, .(price, alpha, utterance_cost), summarize,
                         probability = sum(probability))
names(d_marginal_price)[names(d_marginal_price) == "price"] = "value"
d_marginal_price$QUD = "value"
d_marginal_theta = ddply(.data=d, .(theta, alpha, utterance_cost), summarize, probability = sum(probability))
names(d_marginal_theta)[names(d_marginal_theta) == "theta"] = "value"
d_marginal_theta$QUD = "theta"
d_marginal = rbind(d_marginal_price, d_marginal_theta)
d_marginal$alpha = paste("alpha", d_marginal$alpha, sep="=")
d_marginal$utterance_cost = paste("cost", d_marginal$utterance_cost, sep="=")

p = ggplot(data=d_marginal, aes(x=value, y=probability, colour=QUD)) +
  geom_line() +
  facet_grid(alpha ~ utterance_cost) +
  ggtitle("Adjectives Model: Effects of {arameters with Alternatives") +
  theme_bw(9) +
  theme(panel.grid=element_blank()) +
  scale_colour_brewer(type='qual', palette='Set1') +
  scale_fill_brewer(type='qual', palette='Set1')
print(p)
ggsave("toy_parameter_search_alternatives.pdf", width=8.5, height=5)