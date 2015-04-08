library(plyr)
library(rjson)
library(ggplot2)
source("~/opt/r_helper_scripts/bootsSummary.r")

var_and_eps = read.table("vars_and_eps.csv", sep=",", header=T)
resolution = 10
eps_multiplier = 5
objects = c("laptop", "watch", "coffee maker", "sweater", "headphones")

v3 = read.table("prior-dec4-jan9.csv", sep=",", header=T)[1:9,] ## the 10th subject is doing something strange. maybe i mis-logged something?
v4 = read.table("prior-dec4-jan9.csv", sep=",", header=T)[11:20,] ## this is the data I'm mainly using for the model.
v6 = subset(read.table("prior-feb12.csv", sep=",", header=T),
            Answer.condition == '"prior"' &
              Answer.domain == '"price"')

# ### clean up v3
# v3$buyer_genders = v3$Answer.cond
# v3 = v3[,c("workerid", paste("Answer", 0:4, sep="."), "buyer_genders")]
# v3 = reshape(data=v3, varying=2:6, timevar="trial_num", direction="long")
# v3$Answer = as.character(v3$Answer)
# v3 = v3[,c("workerid", "trial_num", "Answer", "buyer_genders")]
# v3_long = ddply(.data=v3, .variables = .(workerid, trial_num), .fun=function(subd){
#   subd_json_data = fromJSON(subd$Answer[[1]])
#   uppers = unlist(subd_json_data$uppers)
#   uppers[uppers == "infty"] = NA
#   uppers = as.numeric(uppers)
#   lowers = subd_json_data$lowers
#   responses = subd_json_data$responses
#   buyer = subd_json_data$buyer
#   item = subd_json_data$item
#   workerid = subd$workerid
#   trial_num = subd$trial_num
#   buyer_genders = subd$buyer_genders
#   n = length(responses)
#   subd_long = data.frame(
#     workerid = rep(workerid, n),
#     trial_num = rep(trial_num, n),
#     buyer = rep(buyer, n),
#     buyer_genders = rep(buyer_genders, n),
#     item = rep(item, n),
#     lowers = lowers,
#     uppers = uppers,
#     responses = responses,
#     probability = responses / sum(responses))
#   return(subd_long)
# })
# v3_long$item = factor(v3_long$item, levels = objects)
# 
# # ggplot(data=v3_long, aes(x=lowers, y=probability, colour=factor(workerid), group=workerid)) +
# #   geom_line() +
# #   facet_grid(. ~ item, scale="free") +
# #   ggtitle("Prior elicitation version 3 (N=9)") +
# #   ylab("probability (normalized slider response)") +
# #   xlab("lower bound of interval") +
# #   theme_bw(9) +
# #   theme(panel.grid=element_blank())
# # ggsave("v3_priors_by_worker.pdf", width=8.5, height=4)
# # 
# # ggplot(data=v3_long, aes(x=lowers, y=probability, colour=item, fill=item)) +
# #   geom_smooth(method=loess) +
# #   facet_grid(. ~ item, scale="free") +
# #   geom_line(aes(x=lowers, y=0), colour="black") +
# #   ggtitle("Prior elicitation version 3 (N=9)") +
# #   ylab("probability (normalized slider response)") +
# #   xlab("lower bound of interval") +
# #   theme_bw(9) +
# #   theme(panel.grid=element_blank()) +
# #   scale_colour_brewer(type='qual', palette='Set1') +
# #   scale_fill_brewer(type='qual', palette='Set1')
# # ggsave("v3_priors.pdf", width=8.5, height=3)
# 
# v3_smoothed = lapply(objects, function(object) {
#   subd = subset(v3_long, item == object)
#   y.loess = loess(subd$probability ~ subd$lowers, normalize=F)
# #   x.values = seq(from=min(subd$lowers), to=max(subd$lowers), length.out=resolution)
# x.values = seq(from=min(subd$lowers), to=max(subd$lowers),
#                by=(min(var_and_eps$dollarAmt[var_and_eps$qType == "eps" &
#                                                var_and_eps$item == object])*eps_multiplier))
#   y.predict = predict(y.loess, x.values)
#   y.predict[y.predict < 0] = 0
#   data.frame(y = y.predict, x = x.values)
# })
# names(v3_smoothed) = objects

### clean up v4
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

# ggplot(data=v4_long, aes(x=lowers, y=probability, colour=factor(workerid), group=workerid)) +
#   geom_line() +
#   facet_grid(. ~ item, scale="free") +
#   ggtitle("Prior elicitation version 4 (N=10)") +
#   ylab("probability (normalized slider response)") +
#   xlab("lower bound of interval") +
#   theme_bw(9) +
#   theme(panel.grid=element_blank())
# ggsave("v4_priors_by_worker.pdf", width=8.5, height=4)

v4_long$cutoff = sapply(v4_long$item, function(object) {
  return(max(subset(v3_long, item == object)$lowers))
})
# # ggplot(data=v4_long, aes(x=lowers, y=probability, colour=item, fill=item)) +
# #   geom_smooth(method=loess) +
# #   facet_grid(. ~ item, scale="free") +
# #   geom_line(aes(x=lowers, y=0), colour="black") +
# #   geom_line(aes(x=cutoff)) +
# #   ggtitle("Prior elicitation version 4 (N=10)") +
# #   ylab("probability (normalized slider response)") +
# #   xlab("lower bound of interval") +
# #   theme_bw(9) +
# #   theme(panel.grid=element_blank()) +
# #   scale_colour_brewer(type='qual', palette='Set1') +
# #   scale_fill_brewer(type='qual', palette='Set1')
# # ggsave("v4_priors.pdf", width=8.5, height=3)
# ggplot(data=v4_long, aes(x=lowers, y=probability, colour=item, fill=item)) +
#   geom_smooth(method=loess) +
#   facet_grid(. ~ item, scale="free") +
#   geom_line(aes(x=lowers, y=0), colour="black") +
#   ggtitle("Prior elicitation (N=10)") +
#   ylab("probability (normalized slider response)") +
#   xlab("lower bound of interval") +
#   theme_bw(9) +
#   theme(panel.grid=element_blank()) +
#   scale_colour_brewer(type='qual', palette='Set1') +
#   scale_fill_brewer(type='qual', palette='Set1')
# ggsave("bins_prior_smoothed.pdf", width=8.5, height=3)

v4_summary = bootsSummary(data=v4_long, measurevar="probability",
                          groupvars=c("lowers", "item"))
ggplot(data=v4_summary, aes(x=lowers, y=probability, colour=item, fill=item)) +
  geom_line() +
  facet_grid(. ~ item, scale="free") +
  geom_errorbar(aes(x=lowers, ymin=bootsci_low, ymax=bootsci_high, fill=item, alpha=1/2)) +
  ggtitle("Prior elicitation (N=10)") +
  ylab("probability (normalized slider response)") +
  xlab("lower bound of interval") +
  theme_bw(9) +
  theme(panel.grid=element_blank()) +
  scale_colour_brewer(type='qual', palette='Set1') +
  scale_fill_brewer(type='qual', palette='Set1')
ggsave("bins_prior.pdf", width=8.5, height=3)

ggplot(data=v4_summary, aes(x=lowers, y=probability, colour=item, fill=item)) +
  geom_line() +
  geom_errorbar(aes(x=lowers, ymin=bootsci_low, ymax=bootsci_high, fill=item, alpha=1/2)) +
  ggtitle("Prior elicitation (N=10)") +
  ylab("probability (normalized slider response)") +
  xlab("lower bound of interval") +
  theme_bw(9) +
  theme(panel.grid=element_blank()) +
  scale_colour_brewer(type='qual', palette='Set1') +
  scale_fill_brewer(type='qual', palette='Set1')

v4_smoothed = lapply(objects, function(object) {
  subd = subset(v4_long, item == object)
  y.loess = loess(subd$probability ~ subd$lowers, normalize=F)
#   x.values = seq(from=min(subd$lowers), to=max(subd$lowers), length.out=resolution)
  x.values = seq(from=min(subd$lowers), to=max(subd$lowers),
                 by=(min(var_and_eps$dollarAmt[var_and_eps$qType == "eps" &
                                                 var_and_eps$item == object])*eps_multiplier))
  y.predict = predict(y.loess, x.values)
  y.predict[y.predict < 0] = 0
  data.frame(y = y.predict, x = x.values)
})
names(v4_smoothed) = objects

### clean up v6
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

# ggplot(data=v6_long, aes(x=lowers, y=probability, colour=workerid, group=workerid)) +
#   geom_line(alpha=1/2) +
#   facet_grid(. ~ item, scale="free") +
#   ggtitle("Prior elicitation version 6 (N=36)") +
#   ylab("probability (normalized slider response)") +
#   xlab("lower bound of interval") +
#   theme_bw(9) +
#   theme(panel.grid=element_blank())
# ggsave("v6_priors_by_worker.pdf", width=8.5, height=3)

v6_long$cutoff = sapply(v6_long$item, function(object) {
  return(max(subset(v3_long, item == object)$lowers))
})
# ggplot(data=v6_long, aes(x=lowers, y=probability, colour=item, fill=item)) +
#   geom_smooth(method=loess) +
#   facet_grid(. ~ item, scale="free") +
#   geom_line(aes(x=lowers, y=0), colour="black") +
#   geom_line(aes(x=cutoff)) +
#   ggtitle("Prior elicitation version 6 (N=36)") +
#   ylab("probability (normalized slider response)") +
#   xlab("lower bound of interval") +
#   theme_bw(9) +
#   theme(panel.grid=element_blank()) +
#   scale_colour_brewer(type='qual', palette='Set1') +
#   scale_fill_brewer(type='qual', palette='Set1')
# ggsave("v6_priors.pdf", width=8.5, height=3)

v6_smoothed = lapply(objects[1:3], function(object) {
  subd = subset(v6_long, item == object)
  y.loess = loess(subd$probability ~ subd$lowers, normalize=F)
  #x.values = seq(from=min(subd$lowers), to=max(subd$lowers), length.out=resolution)
  x.values = seq(from=min(subd$lowers), to=max(subd$lowers),
                 by=(min(var_and_eps$dollarAmt[var_and_eps$qType == "eps" &
                                                var_and_eps$item == object])*eps_multiplier))
  y.predict = predict(y.loess, x.values)
  y.predict[y.predict < 0] = 0
  data.frame(y = y.predict, x = x.values)
})
names(v6_smoothed) = objects[1:3]

v3_long$cutoff = sapply(v3_long$item, function(object) {
  return(max(subset(v3_long, item == object)$lowers))
})
v3_long$version = rep("Version A", nrow(v3_long))
v4_long$version = rep("Version B", nrow(v4_long))
v6_long$version = rep("Version C", nrow(v6_long))
all_long = rbind(v3_long, v4_long, v6_long)
# ggplot(data=all_long, aes(x=lowers, y=probability, colour=item, fill=item)) +
#   geom_smooth(method=loess) +
#   facet_grid(version ~ item, scale="free") +
#   geom_line(aes(x=lowers, y=0), colour="black") +
#   geom_line(aes(x=cutoff)) +
#   ggtitle("Prior Elicitation Experiments") +
#   ylab("probability (normalized slider response)") +
#   xlab("lower bound of interval") +
#   theme_bw(9) +
#   theme(panel.grid=element_blank()) +
#   scale_colour_brewer(type='qual', palette='Set1') +
#   scale_fill_brewer(type='qual', palette='Set1')
# ggsave("all_bins_priors.pdf", width=8.5, height=4)

v4_unsmoothed = lapply(objects, function(object) {
  subd = subset(v4_summary, item == object)
  data.frame(y=subd$lowers, x=subd$probability)
})

all_bins_data = list(
  v3_loess=v3_smoothed,
  v4_loess=v4_smoothed,
  v6_loess=v6_smoothed,
  v4_actual_mean = v4_unsmoothed
)

# bootstrapping_n = 10
# 
# for (i in 1:bootstrapping_n) {
#   resampled_data = ddply(.data=v4_long[sample.int(nrow(v4_long), replace=T),], .(item, lowers), summarize, probability = mean(probability))
#   all_bins_data[paste("v4", "resampled", "mean", i, sep="_")] = lapply(objects, function(object) {
#     subd = subset(resampled_data, item == object)
#     data.frame(y=subd$lowers, x=subd$probability)
#   })
# }
# 
# sample_means_data = rbind(
#   all_bins_data$v4_resampled_mean_1,
#   all_bins_data$v4_resampled_mean_2,
#   all_bins_data$v4_resampled_mean_3,
#   all_bins_data$v4_resampled_mean_4,
#   all_bins_data$v4_resampled_mean_5,
#   all_bins_data$v4_resampled_mean_6,
#   all_bins_data$v4_resampled_mean_7,
#   all_bins_data$v4_resampled_mean_8,
#   all_bins_data$v4_resampled_mean_9,
#   all_bins_data$v4_resampled_mean_10
# )
# nr = nrow(all_bins_data$v4_resampled_mean_1)
# sample_means_data$sample_number = c(
#   rep(1, nr), rep(2, nr), rep(3, nr),
#   rep(4, nr), rep(5, nr), rep(6, nr),
#   rep(7, nr), rep(8, nr), rep(9, nr),
#   rep(10, nr))
# 
# p = ggplot(data=sample_means_data, aes(x=x, y=y, colour=factor(sample_number))) +
#   geom_point(alpha=1/2)
# print(p)

## write priors.json
writeLines(
  toJSON(
    all_bins_data
  ),
  "bins_priors.json"
)