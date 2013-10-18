# setwd("~/CoCoLab/prior-elicitation/")
# source('prior-elicitation.R')
# examples = getExamples()

setwd("~/sorites-analysis/")  ###change this to actual location of repo

library(stats)
library(rjson)
library(logspline)

#for speaker1 discretization:
grid.steps = 64
grid = seq(0,1,length.out=grid.steps)
cache.index = function(v) {
  return(1+round(v*(grid.steps-1)))
}

# #rough data from amazon
# #http://www.amazon.com/s/ref=sr_ex_p_36_0?rh=n%3A377110011&bbn=377110011&ie=UTF8&qid=1382134494
# sizes = c(173430, 60977, 52537, 36543, 10227, 5600, 4918, 2212, 901, 53, 3)
# mins = c(0, 50, 100, 200, 500, 1000, 2000, 5000, 10000, 50000, 100000)
# maxes = c(50, 100, 200, 500, 1000, 2000, 5000, 10000, 50000, 100000, 300000)
# w = unlist(sapply(1:length(sizes), function(i) {
#   return(runif(round(sizes[i]/100), mins[i], maxes[i]))
# }))
# hist(w, breaks=300)
# unscaled.examples[["watch"]] = w
unscaled.examples = list()
unscaled.examples[["watch"]] = read.table("watch.txt")$V1
unscaled.examples[["coffee maker"]] = read.table("coffee-maker.txt")$V1
unscaled.examples[["laptop"]] = read.table("laptop.txt")$V1
unscaled.examples[["headphones"]] = as.numeric(read.table("headphones.txt")$V1)
unscaled.examples[["sweater"]] = as.numeric(read.table("sweater.txt")$V1)
# # normsample = rnorm(100,1,1)
# # lnormsample = rlnorm(100)
# # hist(normsample, main="normal", ylab="", xlab="")
# # hist(lnormsample, main="lognormal", ylab="", xlab="")
# # examples <- list("lognormal"=lnormsample, "normal"=normsample[normsample>0])
# #scale to max 1:
examples.scale <- lapply(unscaled.examples, max)
examples <- lapply(unscaled.examples, function(exs){
  return(exs/max(exs))
})



get.human.parameters = function(examples.scale) {
  #load human priors:
  human.priors <- fromJSON(readLines("human-priors.JSON")[[1]])
  unscaled.sds = lapply(names(examples.scale), function(cat) {
    return(sd(human.priors[[cat]])/examples.scale[[cat]])
  })
  unscaled.means = lapply(names(examples.scale), function(cat) {
    return(mean(human.priors[[cat]])/examples.scale[[cat]])
  })
  names(unscaled.sds) = names(examples.scale)
  names(unscaled.means) = names(examples.scale)
  return(list(sds=unscaled.sds, means=unscaled.means))
}
human.params = get.human.parameters(examples.scale)
scaled.expt.sds = human.params[["sds"]]
scaled.expt.means = human.params[["means"]]

possible.utterances = c('no-utt', 'pos') 
utterance.lengths = c(0,1)
utterance.polarities = c(0,+1)

#using r function density to find kernal density, so it's not actually continuous
# kernel.granularity <- grid.steps #2^12 #how many points are calculated for the kernel density estimate
# est.kernel <- function(dist, bw) {
#   return(density(examples[[dist]], from=0, to=1, n=kernel.granularity,
#                  kernel="gaussian", bw=bw, adjust=1))
# }
est.kernel <- function(dist,bw) {
  e <- examples[[dist]]
  es <- examples.scale[[dist]]
#   k <- list()
#   k$y <- dlogspline(grid*es, logspline(es*e, lbound=0))#,ubound=1)) #do smoothing in original space
#   k$x <- grid
  k = as.list(density(es*e, bw=bw, kernel="epanechnikov"))
  k$x = grid
  return(k)
}

#norms the kernel density
#takes in all the points where kernel density is estimated
make.pdf.cache <- function(kernel.est) {
  k = kernel.est$y + 0.00001
  area <- sum(k) 
  normed.dens <- k/area
  return(normed.dens)
}

#creates fn that approximates percentage of area before x
#takes in all the points where kernel density is estimated
make.cdf.cache <- function(kernel.est) {
  cumulants <- cumsum(make.pdf.cache(kernel.est))
  return(cumulants)
}


##caching. (R has strange purity on global assignments, so must use <<- to set cache)
L0.cache <- array(NA,dim = c(grid.steps,grid.steps,length(possible.utterances)))
S1.cache <- array(NA,dim = c(grid.steps,grid.steps,length(possible.utterances)))
cache.misses=0 #to track whether caching is working right.

clear.cache = function(){
  cache.misses<<-0
  L0.cache <<- array(NA,dim = c(grid.steps,grid.steps,length(possible.utterances)))
  S1.cache <<- array(NA,dim = c(grid.steps,grid.steps,length(possible.utterances)))
}


listener0 = function(utterance.idx, thetas.idx, degree.idx, pdf, cdf) {
  
  if(is.na(L0.cache[degree.idx,thetas.idx[1],utterance.idx])) {
    cache.misses <<- cache.misses + 1
    if (utterance.idx == 1) { #assume the null utterance
      L0.cache[degree.idx,thetas.idx[1],utterance.idx] <<- pdf[degree.idx]
    }  else if(utterance.polarities[utterance.idx] == +1) {
      theta.idx = thetas.idx[utterance.idx-1]
      utt.true = grid[degree.idx] >= grid[theta.idx]  
      true.norm = if(theta.idx==1){1} else {1-cdf[theta.idx-1]}
      L0.cache[degree.idx,thetas.idx[1],utterance.idx] <<- utt.true * pdf[degree.idx] / true.norm
    } else {
      theta.idx = thetas.idx[utterance.idx-1]
      utt.true = grid[degree.idx] <= grid[theta.idx] 
      true.norm = cdf[theta.idx]
      L0.cache[degree.idx,thetas.idx[1],utterance.idx] <<- utt.true * pdf[degree.idx] / true.norm
    }
  }
  return(L0.cache[degree.idx,thetas.idx[1],utterance.idx])
}

speaker1 = function(thetas.idx, degree.idx, utterance.idx, alpha, utt.cost, pdf, cdf, thetaGtr) {
  
  if(is.na(S1.cache[degree.idx,thetas.idx[1],utterance.idx])) {
    cache.misses <<- cache.misses + 1
    utt.probs = array(0,dim=c(length(possible.utterances)))
    for(i in 1:length(possible.utterances)) {
      l0 = listener0(i, thetas.idx, degree.idx, pdf, cdf)
      utt.probs[i] <- (l0^alpha) * exp(-alpha * utt.cost *  utterance.lengths[i])
    }
    S1.cache[degree.idx,thetas.idx[1],] <<- utt.probs/sum(utt.probs)
  }
  
  return(S1.cache[degree.idx,thetas.idx[1],utterance.idx])
}

listener1 = function(utterance, alpha, utt.cost, n.samples, step.size,
                     dist, band.width, thetaGtr) {
  
  utt.idx = which(possible.utterances == utterance)
  
  kernel.est <- est.kernel(dist, band.width)
  pdf <- make.pdf.cache(kernel.est)
  cdf <- make.cdf.cache(kernel.est)
  
  dim1 <- paste('samp', 1:n.samples, sep='')
  dim2 <- c('degree', paste('theta.', possible.utterances[-1], sep=''))
  dimnames <- list(dim1, dim2)
  samples = matrix(NA, nrow=n.samples, ncol=length(possible.utterances), dimnames=dimnames)
  
  
  #scoring function, to compute (unormalized) probability of state. (should be in log domain?)
  prob.unnormed = function(state) {
    #check bounds:
    if (any(state < 0) || any(state > 1)) {return(0)}
    degree.idx = cache.index(state[1])
    thetas.idx = c(cache.index(state[2]))#sapply(thetas,cache.index)
    #prior for degree (thetas have unif prior):
    prior = pdf[degree.idx]
    #probbaility speaker would have said this (given state):
    likelihood = speaker1(thetas.idx, degree.idx, utt.idx, alpha, utt.cost, pdf, cdf, thetaGtr)
    return(prior*likelihood)
  }
  
  #initialize chain by rejection:
  print("initializing")
  state.prob=0
  state = runif(length(possible.utterances), 0, 1) #a degree val, and a theta for all but "no-utt"
  while(state.prob==0) {
    state = runif(length(possible.utterances), 0, 1) #a degree val, and a theta for all but "no-utt"
    state.prob = prob.unnormed(state)
    print(state.prob)
  }
  samples[1,] = state
  
  #make an MH proposal, spherical gaussian on degree and thetas. 
  make.proposal = function(v) {
    perturbations = rnorm(length(v), mean = 0, sd = step.size)
    return(v + perturbations)
  }
  
  #run mcmc chain:
  print("running mcmc")
  n.proposals.accepted = 0
  for (i in 2:n.samples) {
    proposal = make.proposal(state)
    proposal.prob = prob.unnormed(proposal)
    #MH acceptance, assumes proposal is symmetric:
    if(runif(1,0,1) <= min(1, proposal.prob/state.prob)) {
      n.proposals.accepted = n.proposals.accepted + 1
      state = proposal
      state.prob = proposal.prob
    }
    samples[i,] = state
  }
  
  print("acceptance rate:")
  print(n.proposals.accepted/(n.samples-1))
  #print("misses since last cache clear:")
  #print(cache.misses)
  
  return(list(samples=samples, prop.accepted=n.proposals.accepted/(n.samples-1)))
}

model.sorites <- function(cat) {
  n.true.samples <- 30000 #number of samples to keep
  lag <- 5 #number of samples to skip over
  burn.in <- 10
  n.samples <- n.true.samples * lag + burn.in
  step.size <- 0.03 #note this may not be appropriate for all conditions.
  utt.cost <- 1
  alpha<-5
  
  epsilons <- seq(0,3*scaled.expt.sds[[cat]],length.out=100)
  epsilons.instdevs <-seq(0,3,length.out=100)
  
  clear.cache()
  print(cat)
  samples = listener1('pos', alpha=alpha, utt.cost=utt.cost, n.samples=n.samples,
                      step.size=step.size, dist=cat, band.width="SJ", thetaGtr=F)
  
  #want to check what fraction of thetas are below degree - epsilon
  inductive.prem <- sapply(epsilons, function(eps) {
    return(sum(samples$samples[,2]<=(samples$samples[,1]-eps))/length(samples$samples[,1]))
  })
  
  values = seq(0,4*scaled.expt.sds[[cat]],length.out=100)
  values.instdevs = seq(0,4,length.out=100)
  #want to check what fraction of thetas are below mean + value
  concrete.prem <- sapply(values, function(val) {
    return(sum(samples$samples[,2]<=(scaled.expt.means[[cat]]+val))/length(samples$samples[,1]))
  })
  
  ret <- list(epsilons, inductive.prem, epsilons.instdevs,
              values, concrete.prem, values.instdevs)
  names(ret) <- c("x","y","x.instdevs", "val", "c", "val.instdevs")
  
  #plot(epsilons,ind.prem)
  
  return(ret)
}

# allcat <- lapply(names(examples), model.sorites)
# names(allcat) <- names(examples)
# par(mfrow=c(2,3))
# sapply(names(allcat), function(cat){
#   plot(allcat[[cat]]$x,allcat[[cat]]$y,type="l",main=cat,ylim=c(0,1),xlim=c(0,0.5))})

item.names <- c("laptop", "sweater", "coffee maker", "watch", "headphones")
allcat <- lapply(names(examples), model.sorites)
names(allcat) <- names(examples)

#par(mfrow=c(2,3))
#sapply(names(allcat), function(cat){
#  plot(allcat[[cat]]$x,allcat[[cat]]$y,type="l",main=cat,ylim=c(0,1),xlim=c(0,0.5))})

# png("sorites-lognorm-vs-norm.png", 880, 450, pointsize=24)
# par(mfrow=c(1,2))
# dists = c("lognormal", "normal")
# sapply(dists, function(cat){
#   if (cat == "laptop") {
#     xlab = "epsilon (in standard deviations)"
#     ylab = "probability inductive premise is true"
#   } else {
#     xlab = ""
#     ylab = ""
#   }
#   plot(allcat[[cat]]$x.instdevs, #allcat[[cat]]$x, #seq(0,3,length.out=100),
#        allcat[[cat]]$y,
#        type="l",
#        main=cat,
#        ylim=c(0,1),
#        xlim=c(0,3),
#        xlab=xlab,
#        ylab=ylab,
#        lwd=3)
# })
# dev.off()

png("sorites-model.png", 2200, 450, pointsize=32)
print("png open")
par(mfrow=c(1,5))
sapply(item.names, function(cat){
  print("in item loop")
  if (cat == "laptop") {
    xlab = "epsilon (in standard deviations)"
    ylab = "probability inductive premise is true"
  } else {
    xlab = ""
    ylab = ""
  }
  print('about to plot')
  plot(allcat[[cat]]$x.instdevs, #allcat[[cat]]$x, #seq(0,3,length.out=100),
       allcat[[cat]]$y,
       type="l",
       main=cat,
       ylim=c(0,1),
       xlim=c(0,3),
       xlab=xlab,
       ylab=ylab,
       lwd=3)
  print("plotted")
})
dev.off()

png("sorites-concrete.png", 2200, 450, pointsize=32)
par(mfrow=c(1,5))
sapply(item.names, function(cat){
  if (cat == "laptop") {
    xlab = "values (in standard deviations)"
    ylab = "probability concrete premise is true"
  } else {
    xlab = ""
    ylab = ""
  }
  print('about to plot')
  plot(allcat[[cat]]$val.instdevs, #allcat[[cat]]$x, #seq(0,3,length.out=100),
       allcat[[cat]]$c,
       type="l",
       main=cat,
       ylim=c(0,1),
       xlim=c(0,4),
       xlab=xlab,
       ylab=ylab,
       lwd=3)
  print("plotted")
})
dev.off()

##############inductive scatterplot
eps <- c(0.01, 0.1, 0.5, 1, 2, 3)
model.judgements <- sapply(item.names, function(cat) {
  model.y <- allcat[[cat]]$y
  return(sapply(eps, function(e) {
    eps.scaled <- e*scaled.expt.sds[[cat]]
    return(model.y[[round(eps.scaled/3*(length(model.y)-1))+1]])
    #return(model.y[[round((e/3)*(length(model.y)-1))+1]])
  }))
})
cat.to.colors <- function(cat) {
  col <- if(cat=="laptop") {
    "red"
  } else if(cat=="sweater") {
    "green"
  } else if(cat=="watch") {
    "blue"
  } else if(cat=="headphones") {
    "yellow"
  } else if(cat=="coffee maker") {
    "cyan"
  }
  return(col)
}
model.cols <- sapply(item.names, function(cat) {
  return(sapply(eps, function(e) {
     return(cat.to.colors(cat))
  }))
})
source("analyze-sorites.r")
people.judgements <- sapply(item.names, function(cat) {
  df <- subset(data, data$item==cat & qtype=="eps")
  return(aggregate(response ~ sigs + qtype, data=df, FUN=mean)$response)
})
x <- c(people.judgements)
y <- c(model.judgements)
cols <- c(model.cols)
png("scatterplot.png", 1000, 800, pointsize=32)
plot(x,y,xlim=c(1,9), ylim=c(0,1), ylab="model", xlab="experiment",type="p",pch=20,col=cols)
legend("topleft", legend=item.names, fill=sapply(item.names,cat.to.colors))
dev.off()

print(paste("inductive correlation:", cor(x,y)))

###########concrete scatterplot
vals = c(0, 1, 2, 3, 4)
model.concrete <- sapply(item.names, function(cat) {
  model.c <- allcat[[cat]]$c
  return(sapply(vals, function(v) {
    val.scaled <- v*scaled.expt.sds[[cat]] + scaled.expt.means[[cat]]
    #return(model.y[[round(eps.scaled/3*(length(model.y)-1))+1]])
    return(model.c[[round((val.scaled/4)*(length(model.c)-1))+1]])
  }))
})
people.concrete <- sapply(item.names, function(cat) {
  df <- subset(data, data$item==cat & qtype=="val")
  return(aggregate(response ~ sigs + qtype, data=df, FUN=mean)$response)
})
x <- c(people.concrete)
y <- c(model.concrete)
png("scatterplot-concrete.png", 1000, 800, pointsize=32)
plot(x,y,xlim=c(1,9), ylim=c(0,1), ylab="model", xlab="experiment",type="p",pch=20,col=cols)
legend("topleft", legend=item.names, fill=sapply(item.names,cat.to.colors))
dev.off()

print(paste("concrete correlation:", cor(x,y)))

print(paste("watch:", cor(model.judgements[,"watch"], people.judgements[,"watch"])))
print(paste("laptop:", cor(model.judgements[,"laptop"], people.judgements[,"laptop"])))
print(paste("sweater:", cor(model.judgements[,"sweater"], people.judgements[,"sweater"])))
print(paste("headphones:", cor(model.judgements[,"headphones"], people.judgements[,"headphones"])))
print(paste("coffee maker:", cor(model.judgements[,"coffee maker"], people.judgements[,"coffee maker"])))

cor(c(model.judgements[,c("laptop", "sweater", "headphones", "coffee maker")]), c(people.judgements[, c("laptop", "sweater", "headphones", "coffee maker")]))