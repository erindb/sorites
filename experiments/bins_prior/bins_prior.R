library(plyr)
library(rjson)
library(ggplot2)
source("~/opt/r_helper_scripts/bootsSummary.r")

v3 = read.table("prior-dec4-jan9.csv", sep=",", header=T)[1:9,] ## the 10th subject is doing something strange. maybe i mis-logged something?
v4 = read.table("prior-dec4-jan9.csv", sep=",", header=T)[11:20,] ## this is the data I'm mainly using for the model.
v6 = subset(read.table("prior-feb12.csv", sep=",", header=T),
            Answer.condition == '"prior"' &
              Answer.domain == '"price"')

objects = c("laptop", "watch", "coffee maker", "sweater", "headphones")

### clean up v3 --- I DO NOT use this for the model
### but it's useful for double checking that we're getting representative responses
v3$buyer_genders = v3$Answer.cond
v3 = v3[,c("workerid", paste("Answer", 0:4, sep="."), "buyer_genders")]
v3 = reshape(data=v3, varying=2:6, timevar="trial_num", direction="long")
v3$Answer = as.character(v3$Answer)
v3 = v3[,c("workerid", "trial_num", "Answer", "buyer_genders")]
v3_long = ddply(.data=v3, .variables = .(workerid, trial_num), .fun=function(subd){
  subd_json_data = fromJSON(subd$Answer[[1]])
  uppers = unlist(subd_json_data$uppers)
  uppers[uppers == "infty"] = NA
  uppers = as.numeric(uppers)
  lowers = subd_json_data$lowers
  responses = subd_json_data$responses
  buyer = subd_json_data$buyer
  item = subd_json_data$item
  workerid = subd$workerid
  trial_num = subd$trial_num
  buyer_genders = subd$buyer_genders
  n = length(responses)
  subd_long = data.frame(
    workerid = rep(workerid, n),
    trial_num = rep(trial_num, n),
    buyer = rep(buyer, n),
    buyer_genders = rep(buyer_genders, n),
    item = rep(item, n),
    lowers = lowers,
    uppers = uppers,
    responses = responses,
    probability = responses / sum(responses))
  return(subd_long)
})
v3_long$item = factor(v3_long$item, levels = objects)

### clean up v4 --- I use this for the model
v4$buyer_genders = v4$Answer.cond
v4 = v4[,c("workerid", paste("Answer", 0:4, sep="."), "buyer_genders")]
v4 = reshape(data=v4, varying=2:6, timevar="trial_num", direction="long")
v4$Answer = as.character(v4$Answer)
v4 = v4[,c("workerid", "trial_num", "Answer", "buyer_genders")]
v4_long = ddply(.data=v4, .variables = .(workerid, trial_num), .fun=function(subd){
  subd_json_data = fromJSON(subd$Answer[[1]])
  uppers = unlist(subd_json_data$uppers)
  uppers[uppers == "infty"] = NA
  uppers = as.numeric(uppers)
  lowers = subd_json_data$lowers
  responses = subd_json_data$responses
  buyer = subd_json_data$buyer
  item = subd_json_data$item
  workerid = subd$workerid
  trial_num = subd$trial_num
  buyer_genders = subd$buyer_genders
  n = length(responses)
  subd_long = data.frame(
    workerid = rep(workerid, n),
    trial_num = rep(trial_num, n),
    buyer = rep(buyer, n),
    buyer_genders = rep(buyer_genders, n),
    item = rep(item, n),
    lowers = lowers,
    uppers = uppers,
    responses = responses,
    probability = responses / sum(responses))
  return(subd_long)
})
v4_long$item = factor(v4_long$item, levels = objects)

### clean up v6 --- I DO NOT use this for the model,
### but it's useful for double checking that we're getting representative responses
v6$buyer_genders = v6$Answer.gender
v6 = v6[,c("workerid", paste("Answer", 0:2, sep="."), "buyer_genders")]
v6 = reshape(data=v6, varying=2:4, timevar="trial_num", direction="long")
v6$Answer = as.character(v6$Answer)
v6 = v6[,c("workerid", "trial_num", "Answer", "buyer_genders")]
v6_long = ddply(.data=v6, .variables = .(workerid, trial_num), .fun=function(subd){
  subd_json_data = fromJSON(subd$Answer[[1]])
  uppers = unlist(subd_json_data$uppers)
  uppers[uppers == "infty"] = NA
  uppers = as.numeric(uppers)
  lowers = subd_json_data$lowers
  responses = subd_json_data$responses
  buyer = subd_json_data$buyer
  item = subd_json_data$item
  workerid = subd$workerid
  trial_num = subd$trial_num
  buyer_genders = subd$buyer_genders
  n = length(responses)
  subd_long = data.frame(
    workerid = rep(workerid, n),
    trial_num = rep(trial_num, n),
    buyer = rep(buyer, n),
    buyer_genders = rep(buyer_genders, n),
    item = rep(item, n),
    lowers = lowers,
    uppers = uppers,
    responses = responses,
    probability = responses / sum(responses))
  return(subd_long)
})
v6_long$item = factor(v6_long$item, levels = objects)

## some graphs that show consistency across versions

v3_long$prior_version = rep("Version A", nrow(v3_long))
v4_long$prior_version = rep("Version B", nrow(v4_long))
v6_long$prior_version = rep("Version C", nrow(v6_long))
all_long = rbind(v3_long, v4_long, v6_long)
all_summary = bootsSummary(data=all_long, measurevar="probability", groupvars=c("prior_version", "lowers", "item"))
all_summary$cutoff = sapply(all_summary$item, function(object) {
  return(max(subset(v3_long, item == object)$lowers))
})
ggplot(data=all_summary, aes(x=lowers, y=probability, colour=item, fill=item)) +
  #geom_smooth(method=loess) +
  geom_line() +
#   geom_errorbar(aes(x=lowers, ymin=bootsci_low, ymax=bootsci_high), alpha=1/2) +
  facet_grid(prior_version ~ item, scale="free") +
  ggtitle("Prior Elicitation Experiments") +
  ylab("probability (normalized slider response)") +
  xlab("lower bound of interval") +
  theme_bw(9) +
  theme(panel.grid=element_blank()) +
  scale_colour_brewer(type='qual', palette='Set1') +
  scale_fill_brewer(type='qual', palette='Set1')
ggsave("all_bins_priors.pdf", width=8.5, height=4)

## aggregate data that I care about for modeling

d = ddply(.data=v4_long, .(item, lowers), summarize, probability = mean(probability))
d$prior_version = "v4_actual"

## bootstrapping on v4 data

bootstrapping_n = 100

for (i in 1:bootstrapping_n) {
  resampled_workers = sample(unique(v4_long$workerid), replace=T)
  resampled_data = v4_long[v4_long$workerid == resampled_workers,]
  resampled_mean = ddply(.data=resampled_data, .(item, lowers), summarize, probability = mean(probability))
  resampled_mean$prior_version = paste("v4", "resampled", i, sep="_")
  d = rbind(d, resampled_mean)
}

d$item = factor(d$item,
                levels=c("laptop", "watch", "coffee maker", "sweater", "headphones"),
                labels=c("laptop", "watch", "coffee maker", "sweater", "headphones"))

p = ggplot(data=d, aes(x=lowers, y=probability, colour=item, group=prior_version)) +
  geom_line(alpha=1/20) +
  facet_grid(. ~ item, scales="free") +
  xlab("price") +
  ggtitle("bootstrapped mean bin responses for model input") +
  theme_bw(9) +
  theme(panel.grid=element_blank()) +
  scale_colour_brewer(type='qual', palette='Set1') +
  scale_fill_brewer(type='qual', palette='Set1')
print(p)
ggsave("bootstrapped_bins_priors.pdf", width=8.5, height=3)

write.table(d, "bootstrapped_bins_priors.csv", sep=",", row.names=F)