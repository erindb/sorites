library(rjson)

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


items <- fromJSON(readLines("human-priors.JSON")[[1]])
item.names <- c("laptop", "sweater", "coffee maker", "watch", "headphones")


avg.data <- aggregate(response ~ sigs + qtype + item, data=data, FUN=mean)
nsubj <- length(unique(data$subj))
conf.data <- aggregate(response ~ sigs + qtype + item, data=data, FUN=function(v) {
  sample.means <- replicate(100, mean(sample(v, nsubj, replace=TRUE)))
  return(quantile(sample.means, c(0.025, 0.975)))
})



# png("sorites-priors-epsbound-lines.png", 2000, 400, pointsize=28)
# par(mfrow=c(1,5))
# sapply(1:length(item.names), function(i) {
#   item <- item.names[[i]]
#   samples <- items[[item]]
#   mean <- mean(samples)
#   f <- density(samples, kernel="gaussian", bw="sj")
#   plot(f$x, f$y, type="l", ylab="", xlab="price in dollars",
#        main=item, font.main=32, lwd=2)
#   abline(v=mean, lwd=5, col="blue")
#   abline(v=3*sd(samples), lwd=3, col="red")
# })
# dev.off()

png("sorites-priors.png", 2500, 400, pointsize=28)
par(mfrow=c(1,5))
sapply(1:length(item.names), function(i) {
  item <- item.names[[i]]
  samples <- items[[item]]
  par(mai=c(1,1.8,1,1.5))
#   f <- density(samples, kernel="gaussian", bw="sj")
  ms <- max(samples)
  f <- list()
  f$x <- seq(0,ms,length.out=512)
  f$y <- dlogspline(f$x, logspline(samples, lbound=0))#,ubound=1))
#   f$x <- ms*f$x 
#   f$y <- ms*f$y
  plot(f$x, f$y, type="l", ylab="", xlab="price in dollars",
       main=item, font.main=32, lwd=2, xlim=c(0,max(f$x)), yaxt='n')
  axis(4)
  par(new=T)
  indices <- avg.data$qtype=="val" & avg.data$item==item
  values <- avg.data$sigs[indices]*sd(items[[item]])+mean(items[[item]])
  val.y <- avg.data$response[indices]
  val.low <- conf.data$response[indices,1]
  val.high <- conf.data$response[indices,2]
  ylab <- if(item=="laptop"){"goodness of concrete premise (1-9)"} else {""}
  concrete <- plot(values, val.y, type="l", xlab="",#conc.xlab,
                   font.main=32,
                   ylim=c(1,9),
                   #main=concrete.label,
                   lwd=3,
                   ylab = ylab,
                   xlim=c(0,max(f$x)))#conc.ylab
  arrows(values, val.high, values, val.low, angle=90, code=3, length=0.1, lwd=3)
})
dev.off()