library(rwebppl)
library(tidyverse)
library(jsonlite)
library(ggthemes)
library(pander)

project_dir = "../"
data_dir = function(path) {
  return(paste(project_dir, "data/", path, sep = ""))
}
cache_dir = function(path) {
  return(paste(project_dir, "analysis/.cache/", path, sep = ""))
}
model_dir = function(path) {
  return(paste(project_dir, "models/", path, sep = ""))
}

char = as.character
num = function(v) {return(as.numeric(as.character(v)))}

theme.new = theme_set(theme_few(12))

# for bootstrapping 95% confidence intervals
theta <- function(x,xdata) {mean(xdata[x])}
ci.low <- function(x) {
  quantile(bootstrap::bootstrap(1:length(x),1000,theta,x)$thetastar,.025)}
ci.high <- function(x) {
  quantile(bootstrap::bootstrap(1:length(x),1000,theta,x)$thetastar,.975)}

named_vec = function(df, label_vec, value_vec) {
  if (!is.null(df)) {
    label_vec <- df[[deparse(substitute(label_vec))]]
    value_vec <- df[[deparse(substitute(value_vec))]]
  }
  names(value_vec) = label_vec
  return(value_vec)
}
