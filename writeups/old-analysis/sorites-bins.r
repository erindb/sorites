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

items = c("watch", "laptop", "coffee maker", "headphones", "sweater")

get.bins = function(item) {
  #################cumulants: A + (9-A)*(1-exp(-lambda*x))
  ##reset "lower" as the new "zero" and still approach the same asymptote (9)
  #item = "watch"
  item.data = concrete.data[concrete.data$item == item,]
  vals = sort(item.data$dollars)
  responses = sort(item.data$response)
  get.lower = function(i, lst) {
    if (i == 1) {
      return(0)
    } else {
      return(lst[i-1])
    }
  }
  seps = sapply(1:length(vals), function(i) {
    return(vals[i] - get.lower(i, vals))
  })
  sep = min(seps)/30 ###can change from 2 to some larger number for more bins
  
  ##plot original points:
  xlim = c(0, 2*max(vals))
  ylim = c(0, 9)
  
  png(paste(c(item, "-priors.png"), collapse=""))
  plot(vals, responses, main=item, ylim=ylim, xlim=xlim)
  
  ##fit exponential cdf:
  params = sapply(1:length(vals), function(i) {
    lower.x = get.lower(i, vals)
    upper.x = vals[i]
    lower.y = get.lower(i, responses)
    upper.y = responses[i]
    a = lower.x
    b = upper.x
    A = lower.y
    B = upper.y
    lambda = (log(9-B) - log(9-A)) / (a-b)
    
    ##plot piecewise exponential:
    par(new=T)
    if (i == length(vals)) {
      x = seq(a, xlim[2], length.out=60)
    } else {
      x = seq(a, b, length.out=30)
    }
    y = A + (9-A)*(1-exp(-lambda*(x-a)))
    plot(x,y,ylim=ylim,xlim=xlim,xlab="",ylab="",type="l")
    par(new=F)
    
    return(c(lambda, a, A)) #in equation: y = A + (9-A)*(1-exp(-lambda*(x-a)))
  })
  
  #figure out which indices in params to use for a given x value
  get.ind = function(x) {
    for (i in 1:length(vals)) {
      if (x < vals[i]) {
        return(i)
      }
    }
    return(length(vals)) #after the highest val, just use the last parameters
  }
  get.max.x = function() {
    p = ncol(params)
    lambda = params[1,p]
    a = params[2,p]
    A = params[3,p]
    return( log(1 - (8.9-A)/(9-A)) / (-lambda) + a )
  }
  max.x = get.max.x()
  bin.bounds = seq(sep, max.x, sep)
  bin.mids = sapply(1:length(bin.bounds), function(i) {
    lower.x = get.lower(i, bin.bounds)
    upper.x = bin.bounds[i]
    return((upper.x + lower.x) / 2)
  })
  bin.props = sapply(1:length(bin.mids), function(i) {
    lower.x = get.lower(i, bin.bounds)
    upper.x = bin.bounds[i]
    mid.x = bin.mids[i]
    p = get.ind(mid.x)
    lambda = params[1,p]
    a = params[2,p]
    A = params[3,p]
    lower.y = A + (9-A)*(1-exp(-lambda*(lower.x-a)))
    upper.y = A + (9-A)*(1-exp(-lambda*(upper.x-a)))
    return(upper.y - lower.y) #difference between cdf evaluated at endpoints of bin = probability on that bin
  })
  
  #plot pdf:
  par(new=T)
  plot(bin.mids, bin.props, ylim=c(0,max(bin.props)), main=item, #type="l",
       col="blue",#
       ylab="", xlab="",
       yaxt="n", xlim=xlim#
       )
  par(new=F)
  dev.off()
  
  #return value:
  bin = list(mids=bin.mids, props=bin.props)
  return(bin)
}

bins = lapply(items, get.bins)
names(bins) = items

print("watch")
print(bins[["watch"]]$mids)
print(bins[["watch"]]$props)
print("laptop")
print(bins[["laptop"]]$mids)
print(bins[["laptop"]]$props)
print("coffee maker")
print(bins[["coffee maker"]]$mids)
print(bins[["coffee maker"]]$props)
print("headphones")
print(bins[["headphones"]]$mids)
print(bins[["headphones"]]$props)
print("sweater")
print(bins[["sweater"]]$mids)
print(bins[["sweater"]]$props)


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