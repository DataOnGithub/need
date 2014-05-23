# clear workspace
rm(list = ls())

# set working directory
setwd("~/Dropbox/Projects/Need")

# load packages
library(arm)
library(compactr)
library(R2jags)
library(devEMF)

d <- read.csv("Data/politics_and_need_rescale.csv")

################################################################################
## Estimate the Models
################################################################################

f <- oppose_expansion ~ gop_governor + percent_favorable_aca + gop_leg + percent_uninsured + 
  bal2012 + multiplier + percent_nonwhite + percent_metro

# cauchy prior + mcmc
#y <- rep(NA, 50)
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

set.seed(831075023)
m1 <- jags(data = jags.data,
                      param = jags.params,
                      inits = jags.inits,
                      DIC = FALSE,
                      model = "R_Code/cauchy_logit.bugs",
                      n.chains = 3,
                      n.iter = 100000)
plot(m1)
mcmc.sims <- m1$BUGSoutput$sims.matrix
#rm(y, X, n, K, jags.data, jags.params, jags.inits)

################################################################################
## Coefficient Plot
################################################################################

var.names <- c('GOP Governor', 'Percent Favorable to ACA', 'GOP Controlled Legislature', 'Percent Uninsured',
               'Fiscal Health', 'Medicaid Multiplier', 
               'Percent Nonwhite', 'Percent Metropolitan', 'Constant')
lwr <- min(apply(mcmc.sims, 2, quantile, .05))
upr <- max(apply(mcmc.sims, 2, quantile, .95))
var.order <- c(2:length(var.names), 1)

emf("Figures/main_coef.emf", height = 4, width = 3, family = "serif")
par(mfrow = c(1,1), mar = c(3,1,1,1), oma = c(0,0,0,0), family = "serif")
eplot(xlim = c(lwr, upr), ylim = c(ncol(mcmc.sims), .5),
      anny = FALSE,
      xlab = "Logit Coefficient")
abline(v = 0, lty = 3, col = "grey50")
for (i in 1:ncol(mcmc.sims)) {
  q <- quantile(mcmc.sims[, var.order[i]], c(0.05, .5, .95))
  print(q)
  col <- "black"
  if (i > 4) col <- "grey50"
  #print(q)
  points(q[2], i, pch = 19, col = col)
  lines(c(q[1], q[3]), c(i, i), col = col, lwd = 2)
  text(q[2], i, var.names[i], pos = 3, cex = .7, col = col)
}
dev.off()

################################################################################
## Scaling Table
################################################################################

Fn <- ecdf(d$percent_favorable_aca[order(d$percent_favorable_aca)])
stable_po <- data.frame(d$state_abbr[order(d$percent_favorable_aca)], 
                        1*(d$gop_governor[order(d$percent_favorable_aca)] > 0),
                        Fn(d$percent_favorable_aca[order(d$percent_favorable_aca)]),
                 round(d$percent_favorable_aca[order(d$percent_favorable_aca)], 2),
                 round(d$percent_favorable_aca_original_scale[order(d$percent_favorable_aca)], 2))
names(stable_po) <- c("state", "gop_gov", "percentile", "scaled", "unscaled")
stable_po         

Fn <- ecdf(d$percent_uninsured[order(d$percent_uninsured)])
stable_po <- data.frame(d$state_abbr[order(d$percent_uninsured)], 
                        1*(d$gop_governor[order(d$percent_uninsured)] > 0),
                        Fn(d$percent_uninsured[order(d$percent_uninsured)]),
                        round(d$percent_uninsured[order(d$percent_uninsured)], 2),
                        round(d$percent_uninsured_original_scale[order(d$percent_uninsured)], 2))
names(stable_po) <- c("state", "gop_gov", "percentile", "scaled", "unscaled")
stable_po    

################################################################################
## Evidence for the Hypotheses
################################################################################

# compare the coefficients (negate coefs with negative expectations)
sum(mcmc.sims[, "beta[2]"] > 0)/nrow(mcmc.sims) # gop_governor
sum(mcmc.sims[, "beta[3]"] < 0)/nrow(mcmc.sims) # percent_favorable_aca
sum(mcmc.sims[, "beta[4]"] > 0)/nrow(mcmc.sims) # gop_leg
sum(mcmc.sims[, "beta[5]"] < 0)/nrow(mcmc.sims) # percent_uninsured

################################################################################
## Calculate the Substantive Effect Sizes
################################################################################

# Effect of gop_governor in otherwise GOP
x.lo <- c(1, min(d$gop_governor), quantile(d$percent_favorable_aca, 0.25), max(d$gop_leg), quantile(d$percent_uninsured, .50), median(d$bal2012), median(d$multiplier), median(d$percent_nonwhite), median(d$percent_metro))
x.hi <- c(1, max(d$gop_governor), quantile(d$percent_favorable_aca, 0.25), max(d$gop_leg), quantile(d$percent_uninsured, .50), median(d$bal2012), median(d$multiplier), median(d$percent_nonwhite), median(d$percent_metro))
pr.lo.sims <- plogis(mcmc.sims%*%x.lo)
pr.hi.sims <- plogis(mcmc.sims%*%x.hi)
fd.sims <- pr.hi.sims - pr.lo.sims
round(quantile(fd.sims, c(.05, .5, .95)) , 2)
gov.fd.sims <- fd.sims

# Effect of gop_governor in a Dem state
x.lo <- c(1, min(d$gop_governor), quantile(d$percent_favorable_aca, 0.75), min(d$gop_leg), quantile(d$percent_uninsured, .50), median(d$bal2012), median(d$multiplier), median(d$percent_nonwhite), median(d$percent_metro))
x.hi <- c(1, max(d$gop_governor), quantile(d$percent_favorable_aca, 0.75), min(d$gop_leg), quantile(d$percent_uninsured, .50), median(d$bal2012), median(d$multiplier), median(d$percent_nonwhite), median(d$percent_metro))
pr.lo.sims <- plogis(mcmc.sims%*%x.lo)
pr.hi.sims <- plogis(mcmc.sims%*%x.hi)
fd.sims <- pr.hi.sims - pr.lo.sims
round(round(quantile(fd.sims, c(.05, .5, .95)) , 2) , 2)

# Effect of percent_favorable_aca in a dem_controlled state
x.lo <- c(1, min(d$gop_governor), quantile(d$percent_favorable_aca, 0.25), min(d$gop_leg), quantile(d$percent_uninsured, .5), median(d$bal2012), median(d$multiplier), median(d$percent_nonwhite), median(d$percent_metro))
x.hi <- c(1, min(d$gop_governor), quantile(d$percent_favorable_aca, 0.75), min(d$gop_leg), quantile(d$percent_uninsured, .5), median(d$bal2012), median(d$multiplier), median(d$percent_nonwhite), median(d$percent_metro))
pr.lo.sims <- plogis(mcmc.sims%*%x.lo)
pr.hi.sims <- plogis(mcmc.sims%*%x.hi)
fd.sims <- pr.hi.sims - pr.lo.sims
round(quantile(fd.sims, c(.05, .5, .95)) , 2)

# Effect of percent_favorable_aca in a gop_controlled state
x.lo <- c(1, max(d$gop_governor), quantile(d$percent_favorable_aca, 0.25), max(d$gop_leg), quantile(d$percent_uninsured, .5), median(d$bal2012), median(d$multiplier), median(d$percent_nonwhite), median(d$percent_metro))
x.hi <- c(1, max(d$gop_governor), quantile(d$percent_favorable_aca, 0.75), max(d$gop_leg), quantile(d$percent_uninsured, .5), median(d$bal2012), median(d$multiplier), median(d$percent_nonwhite), median(d$percent_metro))
pr.lo.sims <- plogis(mcmc.sims%*%x.lo)
pr.hi.sims <- plogis(mcmc.sims%*%x.hi)
fd.sims <- pr.hi.sims - pr.lo.sims
round(quantile(fd.sims, c(.05, .5, .95)) , 2)

# Effect of a gop_leg in gop_gov/unfavorable
x.lo <- c(1, max(d$gop_governor), quantile(d$percent_favorable_aca, 0.25), min(d$gop_leg), quantile(d$percent_uninsured, .5), median(d$bal2012), median(d$multiplier), median(d$percent_nonwhite), median(d$percent_metro))
x.hi <- c(1, max(d$gop_governor), quantile(d$percent_favorable_aca, 0.25), max(d$gop_leg), quantile(d$percent_uninsured, .5), median(d$bal2012), median(d$multiplier), median(d$percent_nonwhite), median(d$percent_metro))
pr.lo.sims <- plogis(mcmc.sims%*%x.lo)
pr.hi.sims <- plogis(mcmc.sims%*%x.hi)
fd.sims <- pr.hi.sims - pr.lo.sims
round(quantile(fd.sims, c(.05, .5, .95)) , 2)

# Effect of a need in dem_gov/favorable state
x.lo <- c(1, min(d$gop_governor), quantile(d$percent_favorable_aca, 0.75), min(d$gop_leg), quantile(d$percent_uninsured, .25), median(d$bal2012), median(d$multiplier), median(d$percent_nonwhite), median(d$percent_metro))
x.hi <- c(1, min(d$gop_governor), quantile(d$percent_favorable_aca, 0.75), min(d$gop_leg), quantile(d$percent_uninsured, .75), median(d$bal2012), median(d$multiplier), median(d$percent_nonwhite), median(d$percent_metro))
pr.lo.sims <- plogis(mcmc.sims%*%x.lo)
pr.hi.sims <- plogis(mcmc.sims%*%x.hi)
fd.sims <- pr.hi.sims - pr.lo.sims
quantile(d$percent_uninsured_original_scale, c(.25, .75))
quantile(pr.lo.sims, c(.05, .5, .95))
quantile(pr.hi.sims, c(.05, .5, .95))
round(quantile(fd.sims, c(.05, .5, .95)) , 2)


# Effect of a need in rep_gov/unfavorable/with a gop_legislature
x.lo <- c(1, max(d$gop_governor), quantile(d$percent_favorable_aca, 0.25), max(d$gop_leg), quantile(d$percent_uninsured, .25), median(d$bal2012), median(d$multiplier), median(d$percent_nonwhite), median(d$percent_metro))
x.hi <- c(1, max(d$gop_governor), quantile(d$percent_favorable_aca, 0.25), max(d$gop_leg), quantile(d$percent_uninsured, .75), median(d$bal2012), median(d$multiplier), median(d$percent_nonwhite), median(d$percent_metro))
pr.lo.sims <- plogis(mcmc.sims%*%x.lo)
pr.hi.sims <- plogis(mcmc.sims%*%x.hi)
fd.sims <- pr.hi.sims - pr.lo.sims
quantile(pr.lo.sims, c(.05, .5, .95))
quantile(pr.hi.sims, c(.05, .5, .95))
round(quantile(fd.sims, c(.05, .5, .95)) , 2)


#### comparing the effect of polical variables to uninsured

# compare the coefficients (negate coefs with negative expectations)
sum(mcmc.sims[, "beta[2]"] - -mcmc.sims[, "beta[5]"] > 0)/nrow(mcmc.sims) # gop_governor - percent_uninsured
sum(-mcmc.sims[, "beta[3]"] - -mcmc.sims[, "beta[5]"] > 0)/nrow(mcmc.sims) # percent_favorable_aca - percent_uninsured
sum(mcmc.sims[, "beta[4]"] - -mcmc.sims[, "beta[5]"] > 0)/nrow(mcmc.sims) # gop_leg - percent_uninsured

# compare the first differences of gop_governor and percent_uninsured
x.lo <- c(1, min(d$gop_governor), quantile(d$percent_favorable_aca, 0.25), max(d$gop_leg), quantile(d$percent_uninsured, .5), median(d$bal2012), median(d$multiplier), median(d$percent_nonwhite), median(d$percent_metro))
x.hi <- c(1, max(d$gop_governor), quantile(d$percent_favorable_aca, 0.25), max(d$gop_leg), quantile(d$percent_uninsured, .5), median(d$bal2012), median(d$multiplier), median(d$percent_nonwhite), median(d$percent_metro))
pr.lo.sims <- plogis(mcmc.sims%*%x.lo)
pr.hi.sims <- plogis(mcmc.sims%*%x.hi)
gov.fd.sims <- pr.hi.sims - pr.lo.sims

x.lo <- c(1, max(d$gop_governor), quantile(d$percent_favorable_aca, 0.25), max(d$gop_leg), quantile(d$percent_uninsured, .25), median(d$bal2012), median(d$multiplier), median(d$percent_nonwhite), median(d$percent_metro))
x.hi <- c(1, max(d$gop_governor), quantile(d$percent_favorable_aca, 0.25), max(d$gop_leg), quantile(d$percent_uninsured, .75), median(d$bal2012), median(d$multiplier), median(d$percent_nonwhite), median(d$percent_metro))
pr.lo.sims <- plogis(mcmc.sims%*%x.lo)
pr.hi.sims <- plogis(mcmc.sims%*%x.hi)
need.fd.sims <- pr.hi.sims - pr.lo.sims
quantile(gov.fd.sims - -need.fd.sims, c(.05, .5, .95))



