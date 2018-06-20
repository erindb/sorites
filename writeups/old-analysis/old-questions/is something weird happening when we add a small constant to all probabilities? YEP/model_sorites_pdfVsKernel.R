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

item.names <- c("laptop", "sweater", "coffee maker", "watch", "headphones")

#priors on prices from ebay:
n=200
unscaled.examples <- list()
w = read.table("~/CoCoLab/price-priors/ebay/watch.txt")$V1
unscaled.examples[["watch"]] = w
unscaled.examples[["laptop"]] = read.table("~/CoCoLab/price-priors/ebay/laptop.txt")$V1
unscaled.examples[["headphones"]] = read.table("~/CoCoLab/price-priors/ebay/headphones.txt")$V1
unscaled.examples[["sweater"]] = read.table("~/CoCoLab/price-priors/ebay/sweater.txt")$V1
unscaled.examples[["coffee maker"]] = read.table("~/CoCoLab/price-priors/ebay/coffee-maker.txt")$V1
#scale to max 1:
examples.scale <- lapply(unscaled.examples, max)
examples <- lapply(item.names, function(item){
  exs = unscaled.examples[[item]]
  return(exs/max(exs))
})
names(examples) = item.names

#the data given to the model might have a different standard deviation from 
human.priors = fromJSON(readLines("~/CoCoLab/price-priors/justine-orig/human-priors.JSON")[[1]])
expt.sds = lapply(item.names, function(item) {
  sd(human.priors[[item]])/examples.scale[[item]]
})
expt.means = lapply(item.names, function(item) {
  mean(human.priors[[item]])/examples.scale[[item]]
})
names(expt.sds) = item.names
names(expt.means) = item.names

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
  k <- list()
  k$y <- dlogspline(grid*es, logspline(es*e, lbound=0))#,ubound=1)) #do smoothing in original space
  k$x <- grid
  return(k)
}

#norms the kernel density
#takes in all the points where kernel density is estimated
make.pdf.cache <- function(kernel.est, eps) {
  k = kernel.est$y + eps#10^(-20)#0.00001
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

# for (item in c("headphones")) { #item.names) {#
#   colors = matrix(c("red", "darkred", "blue", "darkblue",
#                    "green", "darkgreen", "cyan", "darkcyan"), ncol=4, nrow=2)
#   print(colors)
#   epsilons = c(10^(-5), 10^(-6), 10^(-15), 10^(-30))
#   first = T
#   for (i in 1:length(epsilons)) {
#     e = epsilons[i]
#     k = est.kernel(item)
#     if (first) {
#       plot(x=grid, y=make.pdf.cache(k, e), type="l", col=colors[1,i], main=item)
#       first = F
#     } else {
#       lines(x=grid, y=make.pdf.cache(k, e), type="l", col=colors[1,i])
#     }
#     lines(k, col=colors[2,i])
#   }
#   legend("topright", legend=c(sapply(epsilons, function(e) {return(paste(e, c("kernel", "pdf")))})), fill=colors)
# }

for (item in item.names) {#c("headphones")) { #
  colors = c("red", "blue", "green", "cyan")
  epsilons = c(10^(-5), 10^(-6), 10^(-15), 10^(-30))
  first = T
  for (i in 1:length(epsilons)) {
    e = epsilons[i]
    k = est.kernel(item)
    if (first) {
      pdf = make.pdf.cache(k, e)
      kmax = max(pdf)
      graph.mid = grid[which(pdf > (kmax*0.9))[1]]
      png(paste(c("vary-eps-", item), collapse=""), 300, 240)
      if (item == "watch") {
        ylim = c(kmax*0.95,kmax*2)
      } else {
        ylim = c(kmax*0.95,kmax*1.04)
      }
      plot(x=grid, y=pdf, type="l", col=colors[i], main=item, ylim=ylim,
           xlim=c(graph.mid-0.001, graph.mid+0.01))
      first = F
    } else {
      lines(x=grid, y=make.pdf.cache(k, e), type="l", col=colors[i])
    }
    #lines(k, col=colors[2,i])
  }
  legend("topright", legend=epsilons, fill=colors)
  dev.off()
}