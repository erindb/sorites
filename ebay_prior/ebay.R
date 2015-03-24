library(plyr)
library(rjson)
library(ggplot2)
source("~/opt/r_helper_scripts/bootsSummary.r")

ebay = list(
  "coffee maker" = read.table("coffee-maker.txt")$V1,
  "laptop" = read.table("laptop.txt")$V1,
  "headphones" = read.table("headphones.txt")$V1,
  "sweater" = read.table("sweater.txt")$V1,
  "watch" = read.table("watch.txt")$V1
)

d = data.frame(
  item = c(
    rep("coffee maker", length(ebay$`coffee maker`)),
    rep("laptop", length(ebay$laptop)),
    rep("headphones", length(ebay$headphones)),
    rep("sweater", length(ebay$sweater)),
    rep("watch", length(ebay$watch))),
  price = c(
    ebay$`coffee maker`,
    ebay$laptop,
    ebay$headphones,
    ebay$sweater,
    ebay$watch)
)

p = ggplot(data=d, aes(x=price, colour=item)) +
  geom_density() +
  facet_grid(. ~ item, scales="free") +
  ggtitle("Unnormalized Priors Scraped from Ebay") +
  ylab("probability") +
  xlab("price") +
  theme_bw(9) +
  theme(panel.grid=element_blank()) +
  scale_colour_brewer(type='qual', palette='Set1') +
  scale_fill_brewer(type='qual', palette='Set1')
print(p)
ggsave("ebay_priors.pdf", width=8.5, height=3)

get_density_pair = function(item) {
  fit = density(ebay[[item]])
  fit_x = fit$x[fit$x > 0]
  fit_y = fit$y[fit$x > 0]
  return(list(x=fit_x, y=fit_y))
}

objects = c("coffee maker", "headphones", "laptop", "watch", "sweater")
distributions = sapply(as.list(objects), get_density_pair)
names(distributions) = objects

## write priors.json
writeLines(
  toJSON(distributions),
  "ebay_prior.json"
)