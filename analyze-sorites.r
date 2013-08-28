library(rjson)

################# reading and cleaning data for all pieces
setwd("~/Code/cocolab/analyzing_experiments/sorites-analysis/")
rd <- read.table("sorites.results", sep="\t", quote='"', header=TRUE)

subjects <- rd$workerid
nsubj <- length(subjects)

questions <- lapply(as.character(rd$Answer.questions), fromJSON)

getProp <- function(id, qs) {
  return(c(sapply(qs, function(q) {
    return(q[[id]])
  })))
}

q1 <- questions[[1]]
data <- data.frame(subj=rep(subjects[[1]], 40), #worker id
                   qnum=as.numeric(getProp("qNumber", q1)), #order of presentation
                   qtype=as.character(getProp("qType", q1)),
                                     #val for "an X that costs value is expensive"
                                     #or eps for "an X that costs epsilon less 
                                     #than and expensive X is expensive"
                   dollars=as.character(getProp("dollarAmt", q1)), #eps or val in dollars
                   sigs=as.numeric(getProp("sigs", q1)), #eps or val in standard deviations
                   item=as.factor(getProp("item", q1)), #headphones, coffee maker, watch, sweater, laptop
                   response=as.numeric(getProp("response", q1)) #1 (disagree) to 9 (agree)

data.frames <- lapply(2:length(subjects), function(s) {
  subj <- subjects[[s]]
  qs <- questions[[s]]
  n.qs <- length(qs)
  subjs <- rep(subj, n.qs)
  if (!(n.qs == 40 || n.qs == 44)) {print("error1")}
  qnum <- as.numeric(getProp("qNumber", qs))
  qtype <- as.character(getProp("qType", qs))
  dollars <- as.character(getProp("dollarAmt", qs))
  sigs <- getProp("sigs", qs)
  item <- getProp("item", qs)
  response <- as.numeric(getProp("response", qs))
  
  data <- rbind(data, data.frame(subj=subjs,
                                 qnum=qnum,
                                 qtype=qtype,
                                 dollars=dollars, 
                                 sigs=as.numeric(sigs),
                                 item=item,
                                 response=as.numeric(response)))
})

# avg.data <- aggregate(response ~ sigs + qtype, data=data, FUN=mean)
# nsubj <- length(unique(data$subj))
# conf.data <- aggregate(response ~ sigs + qtype, data=data, FUN=function(v) {
#   sample.means <- replicate(100, mean(sample(v, nsubj, replace=TRUE)))
#   return(quantile(sample.means, c(0.025, 0.975)))
# })
# 
# epsilons <- avg.data$sigs[avg.data$qtype=="eps"]
# eps.y <- avg.data$response[avg.data$qtype=="eps"]
# eps.low <- conf.data$response[avg.data$qtype=="eps",1]
# eps.high <- conf.data$response[avg.data$qtype=="eps",2]
# values <- avg.data$sigs[avg.data$qtype=="val"]
# val.y <- avg.data$response[avg.data$qtype=="val"]
# val.low <- conf.data$response[avg.data$qtype=="val",1]
# val.high <- conf.data$response[avg.data$qtype=="val",2]
# 
# inductive <- plot(epsilons, eps.y, ylim=c(1,9), type="l", ylab="goodness of inductive premise",
#                   xlab="epsilons (in standard deviations)",
#                   main="Sorites Expt - Inductive Premise Falloff")
# arrows(epsilons, eps.high, epsilons, eps.low, angle=90, code=3, length=0.1)
# concrete <- plot(values, val.y, ylim=c(1,9), type="l", ylab="goodness of concrete premise",
#                  xlab="values (in standard deviations above the mean)",
#                  main="Sorites Expt - Concrete Premise Prior")
# arrows(values, val.high, values, val.low, angle=90, code=3, length=0.1)
# 
# # 
# # # # ################ z scores
# # # # z <- c()
# # # # for (s in subjects) {
# # # #   subj.data <- subset(good.data, good.data$subj==s)
# # # #   subj.z <- scale(subj.data$mp)
# # # #   z <- c(z, subj.z)
# # # # }
# # # # z.data <- data.frame(subj=good.data$subj, dist=good.data$dist, mod=good.data$mod, mp=z)
# # # # #z.means <- aggregate(z ~ dist + mod, data = z.data, FUN = mean)
# # # # 
# # # ################ anova
# # # adj.anova <- aov(mp ~ dist*mod, data=good.data)
# # # print(summary(adj.anova))
# # # # 
# # # ################ graph
# # # mygraph <- function(mydata, range, mytitle) {
# # #   avg.data <- aggregate(mp ~ dist + mod, data = mydata, FUN = mean)
# # #   
# # #   down.none <- 0#avg.data$mp[avg.data$dist == "down" & avg.data$mod == "none"]
# # #   down.adj <- 0#avg.data$mp[avg.data$dist == "down" & avg.data$mod == "adj"]
# # #   down.very <- 0#avg.data$mp[avg.data$dist == "down" & avg.data$mod == "very"]
# # #   mid.none <- avg.data$mp[avg.data$dist == "mid" & avg.data$mod == "none"]
# # #   mid.adj <- avg.data$mp[avg.data$dist == "mid" & avg.data$mod == "adj"]
# # #   mid.very <- avg.data$mp[avg.data$dist == "mid" & avg.data$mod == "very"]
# # #   unif.none <- avg.data$mp[avg.data$dist == "unif" & avg.data$mod == "none"]
# # #   unif.adj <- avg.data$mp[avg.data$dist == "unif" & avg.data$mod == "adj"]
# # #   unif.very <- avg.data$mp[avg.data$dist == "unif" & avg.data$mod == "very"]
# # #   
# # #   mp <- c(down.none, down.adj, down.very,
# # #           mid.none, mid.adj, mid.very,
# # #           unif.none, unif.adj, unif.very)
# # #   
# # #   graph.data <- (matrix(data=mp, nrow=3, ncol=3,
# # #                        dimnames=list(c("none", "adj", "very"),
# # #                                      c("peakedDown", "peakedMid", "uniform"))))
# # #   novel.adj.bar <- barplot(as.matrix(graph.data), main=mytitle,
# # #                            ylab="feppiness", beside=TRUE, col=rainbow(3), ylim=range)
# # #   legend("topleft", c("wug", "feppy wug", "very feppy wug"), cex=0.6, bty="n", fill=rainbow(3));
# # #   
# # # #   ### confidence intervals
# # # #   conf.int <- aggregate(mp ~ dist + mod, data=mydata, FUN = function(df) {
# # # #                 sample.means <- replicate(100, mean(sample(df, 15, replace=TRUE)))
# # # #                 return(quantile(sample.means, c(0.025, 0.975)))
# # # #               })
# # # #   lower <- t(matrix(data=conf.int[[3]][,1], nrow=3, ncol=3))
# # # #   higher <- t(matrix(data=conf.int[[3]][,2], nrow=3, ncol=3))
# # # #   error.bar(novel.adj.bar, as.matrix(graph.data), higher, lower)
# # # }
# # # 
# # # mygraph(good.data, c(0,1), "Novel Adj Scale")
# # # # mygraph(z.data, c(-2,2), "z-scored novel adj scale")
# # # # 
# # # # lapply(good.subjects, function(s) {
# # # #   png(paste(c("subjects/", as.character(s)), collapse=""), 600, 400)
# # # #   a <- good.data$mp[good.data$subj == s]
# # # #   gd <- matrix(a, nrow=3, ncol=3,
# # # #                dimnames=list(c("none", "adj", "very"),
# # # #                              c("peakedDown", "peakedMid", "peakedUp")))
# # # #   g <- barplot(gd, main=as.character(s),
# # # #                ylab="feppiness", beside=TRUE, col=rainbow(3), ylim=c(0,1))
# # # #   legend("topleft", c("wug", "feppy wug", "very feppy wug"), cex=0.6, bty="n", fill=rainbow(3));
# # # #   dev.off()
# # # # })
# # # # 
# # # # #################model output
# # # # model.data <- lapply(0:9, function(i) {
# # # #   filename <- paste(c('model-data/data_gtr', i), collapse='')
# # # #   return(read.table(filename))
# # # # })
# # # # 
# # # # mean.model <- matrix(rep(0,9), nrow=3, ncol=3,
# # # #                dimnames=list(c("none", "adj", "very"),
# # # #                              c("peakedDown", "peakedMid", "peakedUp")))
# # # # lower.model <- matrix(rep(0,9), nrow=3, ncol=3,
# # # #                dimnames=list(c("none", "adj", "very"),
# # # #                              c("peakedDown", "peakedMid", "peakedUp")))
# # # # higher.model <- matrix(rep(0,9), nrow=3, ncol=3,
# # # #                 dimnames=list(c("none", "adj", "very"),
# # # #                               c("peakedDown", "peakedMid", "peakedUp")))
# # # # for (row in 1:3) {
# # # #   for (col in 1:3) {
# # # #     model.runs <- rep(0,10)
# # # #     for (s in 1:10) {
# # # #       model.runs[s] <- model.data[[s]][row,col]
# # # #     }
# # # #     mean.model[row,col] <- mean(model.runs)
# # # #     sample.means <- replicate(100, mean(sample(model.runs, 10, replace=TRUE)))
# # # #     conf <- quantile(sample.means, c(0.025, 0.975))
# # # #     lower.model[row,col] <- conf[[1]]
# # # #     higher.model[row,col] <- conf[[2]]
# # # #   }
# # # # }
# # # # 
# # # # png("model", 600, 400)
# # # # model.bar <- barplot((mean.model), main="Novel Adj Model (very > adj)",
# # # #                          ylab="feppiness", beside=TRUE, col=rainbow(3), ylim=c(0,1))
# # # # legend("topleft", c("wug", "feppy wug", "very feppy wug"), cex=0.6, bty="n", fill=rainbow(3));
# # # # 
# # # # ### confidence intervals
# # # # error.bar(model.bar, as.matrix(mean.model), higher.model, lower.model)
# # # # dev.off()
# # # # 
# # # # getTime <- function(elem) {
# # # #   timey.wimey <- strsplit(as.character(elem), " ")[[1]][4]
# # # #   minutes <- as.numeric(strsplit(timey.wimey, ":")[[1]][2])
# # # # }
# # # # accept <- sapply(rd$assignmentaccepttime, getTime)
# # # # submit <- sapply(rd$assignmentsubmittime, getTime)
# # # # print(mean(submit - accept))
