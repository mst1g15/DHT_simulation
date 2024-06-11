###############################################################################
# Date: 10 July 2024 
# Author: Mia Tackney 
###############################################################################

#Purpose: this script loads libraries and key functions 

library(tidyverse)
library(truncnorm)
library(MASS)

calc_lognorm_param <- function(mu, sigma){
  #Purpose: given a log-normal distribution X with mean mu and standard deviation sigma,
  # provide the location (mu_x) and scale (sigma_x) parameters of X.
  
  mu_x=log((mu^2)/sqrt(mu^2+sigma^2))
  sigma_x=sqrt(log(1+(sigma^2)/(mu^2)))
  
  return(list(mu_x, sigma_x))
}