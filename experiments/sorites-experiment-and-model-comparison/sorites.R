library(plyr)
library(rjson)
library(ggplot2)
source("~/opt/r_helper_scripts/bootsSummary.r")

chop = function(s) {
  a = strsplit(s, "")[[1]]
  return(paste(a[2:length(a)], collapse=""))
}

clean_data = function(d) {
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
  return(d_summary)
}

model = read.table("model.csv", sep=",", header=T)
all_data = read.table("sorites-jan31.csv", sep=",", header=T)
d_relative = clean_data(all_data[1:70,])
d_relative$phrasing = "relative"
d_conditional = clean_data(all_data[71:120,])
d_conditional$phrasing = "conditional"
d_all = rbind(d_relative, d_conditional)
d_all_wide = reshape(d_all, direction="wide", timevar="phrasing",
        idvar=c("item", "qType", "dollarAmt"), drop="N")
p = ggplot(data=d_all_wide, aes(x=response.relative, y=response.conditional, colour=item)) +
  geom_point() +
  geom_errorbar(aes(x=response.relative, ymin=bootsci_low.conditional,
                    ymax=bootsci_high.conditional)) +
  geom_errorbarh(aes(xmin=bootsci_low.relative, xmax=bootsci_high.relative,
                     y=response.conditional)) +
  ggtitle("Different Phrasings for Sorites Experiment") +
  xlab("Version 1: relative clause") +
  ylab("Version 2: conditional statement") +
  theme_bw(18) +
  theme(panel.grid=element_blank()) +
  scale_colour_brewer(type='qual', palette='Set1') +
  scale_fill_brewer(type='qual', palette='Set1')
print(p)
ggsave("phrasings.pdf", width=8.5, height=6)
# with(d_all_wide, print(cor.test(response.relative, response.conditional)))

graph_everything = function(d, label) {
  
  model = subset(model, prior_version == "v4")
  
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
    subd$model = subbbd$model[[1]]
    return(subd)
  })
  
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
  ggsave(paste("sorites-model-vs-people",label,".pdf", sep=""), width=8.5, height=6)
  with(subset(d_summary, qType == "an X that's $E less is also expensive"), cor.test(model, response))
  
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
  ggsave(paste("sorites-experiment",label,".pdf", sep=""), width=8.5, height=3)
}

graph_everything(all_data, "")