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
