# clear workspace
rm(list = ls())

# load packages
library(arm)
library(compactr)
library(texreg)
library(R2jags)

# set working directory
setwd("~/Dropbox/Projects/Need")

# load date
d <- read.csv("Data/politics_and_need_rescale.csv")

# create function to set up and estimate JAGS model
mcmc <- function(f) {
  y <- d$oppose_expansion
  X <- model.matrix(f, d)
  n <- length(y)
  K <- ncol(X)
  scale <- 2.5
  jags.data <- list("y", "X", "n", "K", "scale")
  jags.params <- "beta"
  jags.inits <- function() {
    list("beta" = rnorm(ncol(X), 0, 3))
  }
  m1 <- jags(data = jags.data,
             param = jags.params,
             inits = jags.inits,
             DIC = FALSE,
             model = "R_Code/cauchy_logit.bugs",
             n.chains = 3,
             n.iter = 10000)
  plot(m1)
  mcmc.sims <- m1$BUGSoutput$sims.matrix
  return(mcmc.sims)
}

res <- NULL
# obama_share
f <- oppose_expansion ~ gop_governor + obama_share + gop_leg + percent_uninsured + 
  income + percent_nonwhite + percent_metro
m <- mcmc(f)
prh <- sum(m[, "beta[3]"] < 0)/nrow(m)
est <- median(m[, "beta[3]"])
prh; est


# obama_win
f <- oppose_expansion ~ gop_governor + obama_win + gop_leg + percent_uninsured + 
  income + percent_nonwhite + percent_metro
m <- mcmc(f)
prh <- sum(m[, "beta[3]"] < 0)/nrow(m)
est <- median(m[, "beta[3]"])
prh; est


# ideology
f <- oppose_expansion ~ gop_governor + ideology + gop_leg + percent_uninsured + 
  income + percent_nonwhite + percent_metro
m <- mcmc(f)
prh <- sum(m[, "beta[3]"] > 0)/nrow(m)
est <- median(m[, "beta[3]"])
prh; est

# gop_house
f <- oppose_expansion ~ gop_governor + percent_favorable_aca + gop_house + percent_uninsured + 
  income + percent_nonwhite + percent_metro
m <- mcmc(f)
prh <- sum(m[, "beta[4]"] > 0)/nrow(m)
est <- median(m[, "beta[4]"])
prh; est

# gop_sen
f <- oppose_expansion ~ gop_governor + percent_favorable_aca + gop_sen + percent_uninsured + 
  income + percent_nonwhite + percent_metro
m <- mcmc(f)
prh <- sum(m[, "beta[4]"] > 0)/nrow(m)
est <- median(m[, "beta[4]"])
prh; est

# gop_house + gop_sen
f <- oppose_expansion ~ gop_governor + percent_favorable_aca + gop_house + gop_sen + percent_uninsured + 
  income + percent_nonwhite + percent_metro
m <- mcmc(f)
prh <- sum(m[, "beta[4]"] > 0)/nrow(m)
prh2 <- sum(m[, "beta[5]"] > 0)/nrow(m)
est <- median(m[, "beta[4]"])
est2 <- median(m[, "beta[5]"])
prh; est; prh2; est2

# dsh 
f <- oppose_expansion ~ gop_governor + percent_favorable_aca + gop_leg + dsh + 
  income + percent_nonwhite + percent_metro
m <- mcmc(f)
prh <- sum(m[, "beta[5]"] < 0)/nrow(m)
est <- median(m[, "beta[5]"])
res <- rbind(res, c(est, prh))
prh; est

# compare the coefficients (negate coefs with negative expectations)
sum(m[, "beta[2]"] - -m[, "beta[5]"] > 0)/nrow(m) # gop_governor - dsh
sum(-m[, "beta[3]"] - -m[, "beta[5]"] > 0)/nrow(m) # percent_favorable_aca - dsh
sum(m[, "beta[4]"] - -m[, "beta[5]"] > 0)/nrow(m) # gop_leg - dsh
