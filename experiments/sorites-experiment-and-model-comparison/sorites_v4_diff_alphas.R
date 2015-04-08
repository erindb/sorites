library(plyr)
library(rjson)
library(ggplot2)
source("~/opt/r_helper_scripts/bootsSummary.r")

preferred_alpha = 1
preferred_cost = 1

chop = function(s) {
  a = strsplit(s, "")[[1]]
  return(paste(a[2:length(a)], collapse=""))
}

model = read.table(paste("model_v4_alpha", preferred_alpha, "_cost",
                         preferred_cost, ".csv", sep=""), sep=",", header=T)
d = read.table("sorites-jan31.csv", sep=",", header=T)

# d = d[d$Answer.version == "“jan31”",]
d$questions = as.character(d$Answer.questions)
d = d[,c("workerid", "questions")]

d_long = ddply(d, .(workerid), .fun=function(subd) {
  json_data = fromJSON(subd$questions[[1]])
  new_subd = reshape(data = data.frame(json_data), direction="long", varying=1:385,
              v.names=c("dollarAmt", "qNumber", "qType", "sigs", "item", "response", "rt"),
              timevar="qNumber")
  new_subd$workerid = subd$workerid[[1]]
  return(new_subd)
})
names(d_long) = c("item", "qNumber", "qType", "dollarAmt", "response",
                  "rt", "sigs", "id", "workerid")
d_long$dollarAmt = sapply(d_long$dollarAmt, function(da) {
  return(as.numeric(chop(as.character(da))))
})
d_long$response = as.numeric(as.character(d_long$response))
d_long$item = factor(d_long$item,
                levels=c("laptop", "watch", "coffee maker", "sweater", "headphones"),
                labels=c("laptop", "watch", "coffee maker", "sweater", "headphones"))

d_summary = bootsSummary(data=d_long, measurevar="response", groupvars=c("dollarAmt", "item", "qType"))

model$qType = factor(model$qType, levels=c("val", "eps"),
                         labels=c("an X that's $V is expensive",
                                  "an X that's $E less is also expensive"))
d_summary$qType = factor(d_summary$qType, levels=c("val", "eps"),
                         labels=c("an X that's $V is expensive",
                                  "an X that's $E less is also expensive"))

d_summary = ddply(d_summary, .(item, qType, dollarAmt), .fun=function(subd) {
  iii = as.character(subd$item)[[1]]
  ddd = subd$dollarAmt[[1]]
  qqq = as.character(subd$qType)[[1]]
  subbbd = subset(model,
                  dollarAmt == ddd &
                    item == iii &
                    qType == qqq)
  subd$model = subbbd$probability[[1]]
  return(subd)
})

p = ggplot(data=d_summary, aes(x=model, y=response, colour=item, shape=qType)) +
  geom_point() +
  geom_errorbar(aes(x=model, ymin=bootsci_low, ymax=bootsci_high)) +
  ylab("people's endorsement of premise") +
  xlab("model's probability of premise") +
  ggtitle("Sorites: Model vs Experiment") +
  theme_bw(18) +
  theme(panel.grid=element_blank()) +
  scale_colour_brewer(type='qual', palette='Set1') +
  scale_fill_brewer(type='qual', palette='Set1')
print(p)
ggsave(paste("sorites-model-vs-people_all_alpha", preferred_alpha,
             "_cost", preferred_cost, ".pdf", sep=""), width=8.5, height=4)
with(d_summary, print(cor.test(model, response)))

p = ggplot(data=subset(d_summary, qType == "an X that's $E less is also expensive"),
           aes(x=model, y=response, colour=item)) +
  geom_point() +
  geom_errorbar(aes(x=model, ymin=bootsci_low, ymax=bootsci_high)) +
  ylab("people's endorsement of inductive premise") +
  xlab("model's probability of inductive premise") +
  ggtitle("Sorites: Model vs Experiment") +
  theme_bw(18) +
  theme(panel.grid=element_blank()) +
  scale_colour_brewer(type='qual', palette='Set1') +
  scale_fill_brewer(type='qual', palette='Set1')
print(p)
ggsave(paste("sorites-model-vs-people_alpha", preferred_alpha,
             "_cost", preferred_cost, ".pdf", sep=""), width=8.5, height=6)
with(subset(d_summary, qType == "an X that's $E less is also expensive"),
     print(cor.test(model, response)))

p = ggplot(data=d_summary, aes(x=dollarAmt, y=response, colour=item, linetype=qType)) +
  geom_line() +
  geom_smooth(aes(x=dollarAmt, ymin=bootsci_low, ymax=bootsci_high, fill=item),
              alpha=0.2, stat='identity') +
  facet_grid(. ~ item, scale="free") +
  ggtitle("Sorites Experiment") +
  theme_bw(9) +
  theme(panel.grid=element_blank()) +
  scale_colour_brewer(type='qual', palette='Set1') +
  scale_fill_brewer(type='qual', palette='Set1')
print(p)
ggsave("sorites-experiment.pdf", width=8.5, height=3)