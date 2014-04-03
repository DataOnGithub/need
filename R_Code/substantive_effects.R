rm(list = ls())

library(arm)
library(compactr)
library(brglm)
library(texreg)

setwd("~/Dropbox/ACA")
d <- read.csv("Data/politics_and_need_rescale.csv")

bs.form <- oppose_expansion ~ gop_governor + obama_win + gop_leg + percent_uninsured + 
  income + percent_nonwhite + percent_metro

bs.size <- 10000
bs.res <- NULL
for (i in 1:bs.size) {
  bs.samp <- sample(1:nrow(d), nrow(d), replace = TRUE)
  bs.data <- d[bs.samp, ]
  bs.est <- bayesglm(bs.form, 
                     prior.df = 1, prior.scale = 2.5,
                     family = binomial, data = bs.data)$coef
  bs.res <- rbind(bs.res, bs.est)
}

# Effect of gop_governor in a Romney state
x.lo <- c(1, min(d$gop_governor), min(d$obama_win), median(d$gop_leg), quantile(d$percent_uninsured, .50), median(d$income), median(d$percent_nonwhite), median(d$percent_metro))
x.hi <- c(1, max(d$gop_governor), min(d$obama_win), median(d$gop_leg), quantile(d$percent_uninsured, .50), median(d$income), median(d$percent_nonwhite), median(d$percent_metro))
pr.lo.sims <- plogis(bs.res%*%x.lo)
pr.hi.sims <- plogis(bs.res%*%x.hi)
fd.sims <- pr.hi.sims - pr.lo.sims
quantile(fd.sims, c(.05, .5, .95))
gov.fd.sims <- fd.sims

# Effect of gop_governor in an Obama state
x.lo <- c(1, min(d$gop_governor), max(d$obama_win), median(d$gop_leg), quantile(d$percent_uninsured, .50), median(d$income), median(d$percent_nonwhite), median(d$percent_metro))
x.hi <- c(1, max(d$gop_governor), max(d$obama_win), median(d$gop_leg), quantile(d$percent_uninsured, .50), median(d$income), median(d$percent_nonwhite), median(d$percent_metro))
pr.lo.sims <- plogis(bs.res%*%x.lo)
pr.hi.sims <- plogis(bs.res%*%x.hi)
fd.sims <- pr.hi.sims - pr.lo.sims
quantile(fd.sims, c(.05, .5, .95))



# Effect of obama_win in a dem_controlled state
x.lo <- c(1, min(d$gop_governor), min(d$obama_win), median(d$gop_leg), quantile(d$percent_uninsured, .5), median(d$income), median(d$percent_nonwhite), median(d$percent_metro))
x.hi <- c(1, min(d$gop_governor), max(d$obama_win), median(d$gop_leg), quantile(d$percent_uninsured, .5), median(d$income), median(d$percent_nonwhite), median(d$percent_metro))
pr.lo.sims <- plogis(bs.res%*%x.lo)
pr.hi.sims <- plogis(bs.res%*%x.hi)
fd.sims <- pr.hi.sims - pr.lo.sims
quantile(fd.sims, c(.05, .5, .95))

# Effect of obama_win in a gop_controlled state
x.lo <- c(1, max(d$gop_governor), min(d$obama_win), median(d$gop_leg), quantile(d$percent_uninsured, .5), median(d$income), median(d$percent_nonwhite), median(d$percent_metro))
x.hi <- c(1, max(d$gop_governor), max(d$obama_win), median(d$gop_leg), quantile(d$percent_uninsured, .5), median(d$income), median(d$percent_nonwhite), median(d$percent_metro))
pr.lo.sims <- plogis(bs.res%*%x.lo)
pr.hi.sims <- plogis(bs.res%*%x.hi)
fd.sims <- pr.hi.sims - pr.lo.sims
quantile(fd.sims, c(.05, .5, .95))

# Effect of a gop_leg in gop_gov/romney state
x.lo <- c(1, max(d$gop_governor), min(d$obama_win), min(d$gop_leg), quantile(d$percent_uninsured, .5), median(d$income), median(d$percent_nonwhite), median(d$percent_metro))
x.hi <- c(1, max(d$gop_governor), min(d$obama_win), max(d$gop_leg), quantile(d$percent_uninsured, .5), median(d$income), median(d$percent_nonwhite), median(d$percent_metro))
pr.lo.sims <- plogis(bs.res%*%x.lo)
pr.hi.sims <- plogis(bs.res%*%x.hi)
fd.sims <- pr.hi.sims - pr.lo.sims
quantile(fd.sims, c(.05, .5, .95))

# Effect of a need in dem_gov/obama state
x.lo <- c(1, min(d$gop_governor), max(d$obama_win), min(d$gop_leg), quantile(d$percent_uninsured, .25), median(d$income), median(d$percent_nonwhite), median(d$percent_metro))
x.hi <- c(1, min(d$gop_governor), max(d$obama_win), min(d$gop_leg), quantile(d$percent_uninsured, .75), median(d$income), median(d$percent_nonwhite), median(d$percent_metro))
pr.lo.sims <- plogis(bs.res%*%x.lo)
pr.hi.sims <- plogis(bs.res%*%x.hi)
fd.sims <- pr.hi.sims - pr.lo.sims
quantile(d$percent_uninsured_original_scale, c(.25, .75))
quantile(pr.lo.sims, c(.05, .5, .95))
quantile(pr.hi.sims, c(.05, .5, .95))
quantile(fd.sims, c(.05, .5, .95))


# Effect of a need in rep_gov/romey state/with a gop_legislature
x.lo <- c(1, max(d$gop_governor), min(d$obama_win), max(d$gop_leg), quantile(d$percent_uninsured, .25), median(d$income), median(d$percent_nonwhite), median(d$percent_metro))
x.hi <- c(1, max(d$gop_governor), min(d$obama_win), max(d$gop_leg), quantile(d$percent_uninsured, .75), median(d$income), median(d$percent_nonwhite), median(d$percent_metro))
pr.lo.sims <- plogis(bs.res%*%x.lo)
pr.hi.sims <- plogis(bs.res%*%x.hi)
fd.sims <- pr.hi.sims - pr.lo.sims
quantile(pr.lo.sims, c(.05, .5, .95))
quantile(pr.hi.sims, c(.05, .5, .95))
quantile(fd.sims, c(.05, .5, .95))


#### comparing the effect of gubernatorial partisanship to uninsured

# compare the coefficients (negate coefs with negative expectations)
sum(bs.res[, "gop_governor"] - -bs.res[, "percent_uninsured"] < 0)/bs.size
sum(-bs.res[, "obama_win"] - -bs.res[, "percent_uninsured"] < 0)/bs.size
sum(bs.res[, "gop_leg"] - -bs.res[, "percent_uninsured"] < 0)/bs.size

x.lo <- c(1, min(d$gop_governor), min(d$obama_win), max(d$gop_leg), quantile(d$percent_uninsured, .5), median(d$income), median(d$percent_nonwhite), median(d$percent_metro))
x.hi <- c(1, max(d$gop_governor), min(d$obama_win), max(d$gop_leg), quantile(d$percent_uninsured, .5), median(d$income), median(d$percent_nonwhite), median(d$percent_metro))
pr.lo.sims <- plogis(bs.res%*%x.lo)
pr.hi.sims <- plogis(bs.res%*%x.hi)
gov.fd.sims <- pr.hi.sims - pr.lo.sims

x.lo <- c(1, max(d$gop_governor), min(d$obama_win), max(d$gop_leg), quantile(d$percent_uninsured, .25), median(d$income), median(d$percent_nonwhite), median(d$percent_metro))
x.hi <- c(1, max(d$gop_governor), min(d$obama_win), max(d$gop_leg), quantile(d$percent_uninsured, .75), median(d$income), median(d$percent_nonwhite), median(d$percent_metro))
pr.lo.sims <- plogis(bs.res%*%x.lo)
pr.hi.sims <- plogis(bs.res%*%x.hi)
need.fd.sims <- pr.hi.sims - pr.lo.sims
quantile(gov.fd.sims - -need.fd.sims, c(.05, .5, .95))
