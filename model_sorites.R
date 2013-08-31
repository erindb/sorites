setwd("~/sorites-analysis/")  ###change this to actual location of repo

library(stats)
library(rjson)

#for speaker1 discretization:
grid.steps = 64
grid = seq(0,1,length.out=grid.steps)
cache.index = function(v) {
  return(1+round(v*(grid.steps-1)))
}

#load human priors:
examples <- fromJSON(readLines("human-priors.JSON")[[1]])
#scale to max 1:
examples <- lapply(examples, function(exs){
  return(exs/max(exs))
})

# down.examples <- c(0.04583, 0.003231, 0.07391, 0.01884, 0.00003024, 0.04158,
#                    0.09081, 0.06746, 0.01949, 0.1007, 0.1633, 0.1441, 0.1655,
#                    0.2697, 0.2161)
# mid.examples <- c(0.31404, 0.30456, 0.39520, 0.56064, 0.49728, 0.53187, 0.55993,
#                   0.47519, 0.54332, 0.48362, 0.51678, 0.44763, 0.68272, 0.61375,
#                   0.69832)
# unif.examples <- c(0.9730805, 0.0589135, 0.1332413, 0.5568001, 0.6201130, 0.4243146,
#                    0.4176713, 0.2215742, 0.6778150, 0.6834636, 0.8716204, 0.5641932,
#                    0.3503760, 0.9606276, 0.0048311)
# examples <- list(down.examples, mid.examples, unif.examples)
# names(examples) <- c("down", "mid", "unif")

possible.utterances = c('no-utt', 'pos') 
utterance.lengths = c(0,1)
utterance.polarities = c(0,+1)

#using r function density to find kernal density, so it's not actually continuous
kernel.granularity <- grid.steps #2^12 #how many points are calculated for the kernel density estimate
est.kernel <- function(dist, bw) {
  return(density(examples[[dist]], from=0, to=1, n=kernel.granularity,
                 kernel="gaussian", bw=bw, adjust=1))
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
    }	else if(utterance.polarities[utterance.idx] == +1) {
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
  print("misses since last cache clear:")
  print(cache.misses)
  
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
  
  epsilons <- seq(0,3*sd(examples[[cat]]),length.out=100)
  
  clear.cache()
  samples = listener1('pos', alpha=alpha, utt.cost=utt.cost, n.samples=n.samples,
              step.size=step.size, dist=cat, band.width="SJ", thetaGtr=F)
  
  #want to check what fraction of thetas are below degree - epsilon
  prem <- sapply(epsilons, function(eps) {
    return(sum(samples$samples[,2]<=samples$samples[,1]-eps)/length(samples$samples[,1]))
  })
    
  ret <- list(epsilons,prem)
  names(ret) <- c("x","y")
  
  #plot(epsilons,ind.prem)
  
  return(ret)
}

allcat <- lapply(names(examples), model.sorites)
names(allcat) <- names(examples)
par(mfrow=c(2,3))
sapply(names(allcat), function(cat){
  plot(allcat[[cat]]$x,allcat[[cat]]$y,type="l",main=cat,ylim=c(0,1),xlim=c(0,0.5))})




