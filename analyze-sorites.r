library(rjson)

################# reading and cleaning data for all pieces
setwd("~/sorites-analysis/")
rd <- read.table("sorites.results", sep="\t", quote='"', header=TRUE)

subjects <- rd$workerid
nsubj <- length(subjects)

questions <- lapply(as.character(rd$Answer.questions), fromJSON)
n.qs <- length(questions[[1]])

getProp <- function(id) {
  unlist(sapply(questions, function(question.set){
    return(c(sapply(question.set, function(q) {
      return(q[[id]])
    })))
  }))
}

subjs <- unlist(sapply(1:length(subjects), function(s){
  subj <- subjects[[s]]
  n.qs <- length(questions[[s]])
  if (n.qs != 40 && n.qs != 44) {print("error1")}
  return(rep(subj,n.qs))
}))
qnum <- getProp("qNumber")
qtype <- as.character(getProp("qType"))
dollars <- getProp("dollarAmt")
sigs <- getProp("sigs")
item <- getProp("item")
response <- getProp("response")

data <- data.frame(subj=subjs, #worker id
                   qnum=qnum, #order of presentation
                   qtype=qtype, #val for "an X that costs value is expensive"
                                #or eps for "an X that costs epsilon less than
                                #and expensive X is expensive"
                   dollars=dollars, #eps or val in dollars
                   sigs=as.numeric(sigs), #eps or val in standard deviations
                   item=item, #headphones, coffee maker, watch, sweater, laptop
                   response=as.numeric(response)) #1 (disagree) to 9 (agree)
data$dollars = as.numeric(substr(as.character(data$dollars), 2, length(as.character(data$dollars))))
concrete.data = aggregate(response ~ dollars + item, data=data[data$qtype == "val",], FUN=mean)

#################cumulants: 1-exp(alpha*x)


#################cumulants: linear
# # unscaled.examples <- list()
# # unscaled.examples[["watch"]] = read.table("~/CoCoLab/price-priors/ebay/watch-in-watches.txt")$V1
# # unscaled.examples[["laptop"]] = read.table("~/CoCoLab/price-priors/ebay/laptop.txt")$V1
# # unscaled.examples[["headphones"]] = read.table("~/CoCoLab/price-priors/ebay/headphones.txt")$V1
# # unscaled.examples[["sweater"]] = read.table("~/CoCoLab/price-priors/ebay/sweater.txt")$V1
# # unscaled.examples[["coffee maker"]] = read.table("~/CoCoLab/price-priors/ebay/coffee-maker.txt")$V1
# # maxes = lapply(unscaled.examples, max)
# 
# # #i picked these by looking at where the interval graph reached very close to zero
# # #(sometimes it picks back up, but this method wouldn't be able to capture that)
# maxes = list(watch=3000, headphones=1000, laptop=5000, sweater=400)
# maxes[["coffee maker"]] = 400
# #nbins = 50
# maxbinwidth = 10000
# #maxresponse=8.4
# get.prior.bins = function(item) {
#   item.data = concrete.data[concrete.data$item == item,]
#   vals = sort(item.data$dollars)
#   #extra.bins = nbins-length(vals)
#   #vals = c(vals, maxes[[item]])
#   #vals = c(vals, vals[length(vals)]+maxbinwidth)
#   #vals = c(vals, seq(vals[length(vals)], maxes[[item]], length.out=extra.bins)[2:extra.bins])
#   responses = sort(item.data$response)
#   #responses = c(responses, 9)
#   #responses = c(responses, maxresponse)
# #   plot(vals, responses, main=item)
#   #responses = c(responses, seq(responses[length(responses)], 9, length.out=extra.bins)[2:extra.bins]) #highest expensive rating is 9
#   if (item == "watch") {
#     plot(vals, responses)
#   }
#   get.lower = function(i, lst) {
#     if (i == 1) {
#       return(0)
#     } else {
#       return(lst[i-1])
#     }
#   }
#   bin.widths = sapply(1:length(vals), function(i) {
#     lower.x = get.lower(i, vals)
#     upper.x = vals[i]
#     return(upper.x - lower.x)
#   })
#   bin.heights = sapply(1:length(responses), function(i) {
#     lower.y = get.lower(i, responses)
#     upper.y = responses[i]
#     return(upper.y - lower.y)
#   })
#   bin.mids = sapply(1:length(vals), function(i) {
#     lower.x = get.lower(i, vals)
#     upper.x = vals[i]
#     return((upper.x - lower.x)/2 + lower.x)
#   })
#   bin.props = sapply(1:length(bin.mids), function(i) {
#     #return(bin.widths[i] * bin.heights[i])
#     return(bin.heights[i])
#   })
#   #plot(bin.mids, bin.props, main=item)
# #   if (item == "watch") {
# #     plot(bin.mids, bin.props, main=item)
# #   }
#   write(item, "hi", append=T)
#   write(bin.mids, "hi", append=T, ncol=length(bin.mids))
#   write(bin.props, "hi", append=T, ncol=length(bin.props))
#   return(list(mids=bin.mids, heights=bin.props))
# }
# 
# items = unique(as.character(data$item))
# 
# prior.bins = lapply(items, get.prior.bins)
# names(prior.bins) = items


#############dunno
# 
# get.smoothed.prior = function(item) {
#   nbins = 50
#   
#   item.data = concrete.data[concrete.data$item == item,]
#   vals = sort(item.data$dollars)
#   
#   approx.prior = list()
#   approx.prior$x = c(sapply(1:(length(vals)+1), function(i) {
#     if (i == 1) {
#       lower = 0
#       upper = vals[i]
#     } else if (i == (length(vals)+1)) {
#       lower = vals[i-1]
#       upper = maxes[[item]]
#     } else {
#       lower = vals[i-1]
#       upper = vals[i]
#     }
#     return(mean(c(lower, upper)))
#   }))
#   approx.prior$y = c(sapply(1:(length(vals)+1), function(i) {
#     if (i == 1) {
#       lower = 0
#       upper = item.data$response[item.data$dollars == vals[i]]
#       range = vals[i] - 0
#     } else if (i == length(vals)+1) {
#       lower = item.data$response[item.data$dollars == vals[i-1]]
#       upper = 0
#       range = maxes[[item]] - vals[i-1]
#     } else {
#       lower = item.data$response[item.data$dollars == vals[i-1]]
#       upper = item.data$response[item.data$dollars == vals[i]]
#       range = vals[i] - vals[i-1]
#     }
#     return((upper-lower)/range)
#   }))
#   approx.prior$y[approx.prior$y < 0] = 0
#   plot(approx.prior, main=item)
#   print(item)
#   print(approx.prior)
#   return(approx.prior)
# }
# 
# items = unique(as.character(data$item))
# 
# smoothed.priors = lapply(items, get.smoothed.prior)
# 

expt.pdf = lapply(c("laptop", "watch"), function(item) {
    
  })
names(expt.pdf) = c("laptop", "watch")

est.kernel <- function(dist, bw) {
  k = list()
  k$x = grid
  k$y = predict(expt.pdf[[dist]], grid)$y/((sum(predict(expt.pdf[[dist]])$y, grid)*(grid[2]-grid[1])))
  return(k)
}





graph.df <- function(df, label, range, save=F, file.names=c("",""), item.label="",
                     ind.xlab="epsilons (in standard deviations)",
                     ind.ylab="goodness of inductive premise",
                     conc.ylab="goodness of concrete premise",
                     conc.xlab="values (in standard deviations above the mean)") {
  
  avg.data <- aggregate(response ~ sigs + qtype, data=df, FUN=mean)
  nsubj <- length(unique(data$subj))
  conf.data <- aggregate(response ~ sigs + qtype, data=df, FUN=function(v) {
    sample.means <- replicate(100, mean(sample(v, nsubj, replace=TRUE)))
    return(quantile(sample.means, c(0.025, 0.975)))
  })
  
  epsilons <- avg.data$sigs[avg.data$qtype=="eps"]
  eps.y <- avg.data$response[avg.data$qtype=="eps"]
  eps.low <- conf.data$response[avg.data$qtype=="eps",1]
  eps.high <- conf.data$response[avg.data$qtype=="eps",2]
  values <- avg.data$sigs[avg.data$qtype=="val"]
  val.y <- avg.data$response[avg.data$qtype=="val"]
  val.low <- conf.data$response[avg.data$qtype=="val",1]
  val.high <- conf.data$response[avg.data$qtype=="val",2]
  if (is.na(label)) {
    inductive.label <- item.label#paste("I", item.label)
    concrete.label <- ""#paste("C", item.label)
  } else {
    inductive.label <- paste(c("Sorites Expt - Inductive Premise Falloff", label), collapse="")
    concrete.label <- paste(c("Sorites Expt - Concrete Premise Prior", label), collapse="")
  }
  lwd<-2
  inductive <- plot(epsilons, eps.y, ylim=range, type="l", ylab=ind.ylab,
                    xlab=ind.xlab, font.main=32,
                    main=inductive.label, lwd=lwd)
  arrows(epsilons, eps.high, epsilons, eps.low, angle=90, code=3, length=0.1, lwd=lwd)
  concrete <- plot(values, val.y, ylim=range, type="l", xlab=conc.xlab, font.main=32,
                   main=concrete.label, lwd=lwd, ylab=conc.ylab)
  arrows(values, val.high, values, val.low, angle=90, code=3, length=0.1, lwd=lwd)
}

################ z scores
z.subj <- unlist(sapply(1:length(subjects), function(s) {
  subj <- subjects[[s]]
  n.qs <- length(questions[[s]])
  if (n.qs != 40 && n.qs != 44) {print("error2")}
  return(rep(subj, n.qs))
}))
z.response <- unlist(sapply(1:length(subjects), function(s) {
  subj <- subjects[[s]]
  return(scale(data$response[data$subj==subj])[,1])
}))
z.qnum <- unlist(sapply(1:length(subjects), function(s) {
  subj <- subjects[[s]]
  data$qnum[data$subj==subj]
}))
z.qtype <- unlist(sapply(1:length(subjects), function(s) {
  subj <- subjects[[s]]
  data$qtype[data$subj==subj]
}))
z.dollars <- unlist(sapply(1:length(subjects), function(s) {
  subj <- subjects[[s]]
  data$dollars[data$subj==subj]
}))
z.sigs <- unlist(sapply(1:length(subjects), function(s) {
  subj <- subjects[[s]]
  data$sigs[data$subj==subj]
}))
z.item <- unlist(sapply(1:length(subjects), function(s) {
  subj <- subjects[[s]]
  data$item[data$subj==subj]
}))
z.data <- data.frame(subj=z.subj, qnum=z.qnum, qtype=z.qtype, dollars=z.dollars,
                     sigs=z.sigs, item=z.item, response=z.response)
# # 
# # 
# graph.df(data, "", c(1,9))
# graph.df(z.data, " (z-scored)", c(-1.5,1.5))
# # par(mfcol=c(2,5))
# # sapply(unique(as.character(data$item)), function(lalala) {
# #   graph.df(subset(z.data, z.data$item==lalala), label=NA,
# #            range=c(-1.5,1.5), item.label=lalala)
# # })
png("sorites-items.png", 2000, 800, pointsize=28)
par(mfcol=c(2,5))
sapply(unique(as.character(data$item)), function(kind.of.item) {
  if (kind.of.item == "laptop") {
    ind.ylab <- "goodness of inductive premise"
    ind.xlab <- "epsilons (standard deviations)"
    conc.xlab <- "values (standard deviations above the mean)"
    conc.ylab <- "goodness of concrete premise"
  } else {
    ind.ylab <- ""
    ind.xlab <- ""
    conc.xlab <- ""
    conc.ylab <- ""
  }
  graph.df(subset(data, data$item==kind.of.item), label=NA,
           range=c(1,9), item.label=kind.of.item, ind.xlab=ind.xlab,
           ind.ylab=ind.ylab, conc.xlab=conc.xlab, conc.ylab=conc.ylab)
})
dev.off()


png("sorites-z-items.png", 2000, 800, pointsize=28)
par(mfcol=c(2,5))
sapply(unique(as.character(z.data$item)), function(kind.of.item) {
  if (kind.of.item == "laptop") {
    ind.ylab <- "goodness of inductive premise"
    ind.xlab <- "epsilons (standard deviations)"
    conc.xlab <- "values (standard deviations above the mean)"
    conc.ylab <- "goodness of concrete premise"
  } else {
    ind.ylab <- ""
    ind.xlab <- ""
    conc.xlab <- ""
    conc.ylab <- ""
  }
  graph.df(subset(z.data, data$item==kind.of.item), label=NA,
           range=c(-2,1.5), item.label=kind.of.item, ind.xlab=ind.xlab,
           conc.xlab=conc.xlab, ind.ylab=ind.ylab, conc.ylab=conc.ylab)
})
dev.off()
