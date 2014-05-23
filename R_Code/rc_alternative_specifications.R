# clear workspace
rm(list = ls())

# load packages
library(arm)
library(compactr)
library(R2jags)
library(xtable)

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
             n.iter = 100000)
  plot(m1)
  mcmc.sims <- m1$BUGSoutput$sims.matrix
  return(mcmc.sims)
}

################################################################################
## Alternative Measures of Public Opinion
################################################################################

# obama_share
res <- data.frame(Variable = NA, Expectation = NA, Estimate = NA, PrH = NA)
f <- oppose_expansion ~ gop_governor + obama_share + gop_leg + percent_uninsured + 
  bal2012 + multiplier + percent_nonwhite + percent_metro
m <- mcmc(f)
prh <- round(sum(m[, "beta[3]"] < 0)/nrow(m), 2)
est <- round(median(m[, "beta[3]"]), 2)
prh; est
res$Variable <- "Obama's 2012 Vote Share"
res$Expectation <- "-"
res$Estimate <- est
res$PrH <- prh

# obama_win
res0 <- data.frame(Variable = NA, Expectation = NA, Estimate = NA, PrH = NA)
f <- oppose_expansion ~ gop_governor + obama_win + gop_leg + percent_uninsured + 
  bal2012 + multiplier + percent_nonwhite + percent_metro
m <- mcmc(f)
prh <- round(sum(m[, "beta[3]"] < 0)/nrow(m), 2)
est <- round(median(m[, "beta[3]"]), 2)
prh; est
res0$Variable <- "Obama Victory in 2012"
res0$Expectation <- "-"
res0$Estimate <- est
res0$PrH <- prh
res <- rbind(res, res0)

# ideology
res0 <- data.frame(Variable = NA, Expectation = NA, Estimate = NA, PrH = NA)
f <- oppose_expansion ~ gop_governor + ideology + gop_leg + percent_uninsured + 
  bal2012 + multiplier + percent_nonwhite + percent_metro
m <- mcmc(f)
prh <- round(sum(m[, "beta[3]"] > 0)/nrow(m), 2)
est <- round(median(m[, "beta[3]"]), 2)
prh; est
res0$Variable <- "State Ideology"
res0$Expectation <- "+"
res0$Estimate <- est
res0$PrH <- prh
res <- rbind(res, res0)

# support for medicaid expansion
res0 <- data.frame(Variable = NA, Expectation = NA, Estimate = NA, PrH = NA)
f <- oppose_expansion ~ gop_governor + percent_supporting_expansion + gop_leg + percent_uninsured + 
  bal2012 + multiplier + percent_nonwhite + percent_metro
m <- mcmc(f)
prh <- round(sum(m[, "beta[3]"] < 0)/nrow(m), 2)
est <- round(median(m[, "beta[3]"]), 2)
prh; est
res0$Variable <- "Percent Supporting Medicaid Expansion"
res0$Expectation <- "-"
res0$Estimate <- est
res0$PrH <- prh
res <- rbind(res, res0)

# support for tea party
res0 <- data.frame(Variable = NA, Expectation = NA, Estimate = NA, PrH = NA)
f <- oppose_expansion ~ gop_governor + percent_supporting_tea_party + gop_leg + percent_uninsured + 
  bal2012 + multiplier + percent_nonwhite + percent_metro
m <- mcmc(f)
prh <- round(sum(m[, "beta[3]"] > 0)/nrow(m), 2)
est <- round(median(m[, "beta[3]"]), 2)
prh; est
res0$Variable <- "Percent Supporting Tea Party"
res0$Expectation <- "+"
res0$Estimate <- est
res0$PrH <- prh
res <- rbind(res, res0)
res

################################################################################
## Alternative Measures of Legislative Composition
################################################################################

# gop_house
res0 <- data.frame(Variable = NA, Expectation = NA, Estimate = NA, PrH = NA)
f <- oppose_expansion ~ gop_governor + percent_favorable_aca + gop_house + percent_uninsured + 
  bal2012 + multiplier + percent_nonwhite + percent_metro
m <- mcmc(f)
prh <- round(sum(m[, "beta[4]"] > 0)/nrow(m), 2)
est <- round(median(m[, "beta[4]"]), 2)
prh; est
res0$Variable <- "GOP House"
res0$Expectation <- "+"
res0$Estimate <- est
res0$PrH <- prh
res <- rbind(res, res0)
res

# gop_sen
res0 <- data.frame(Variable = NA, Expectation = NA, Estimate = NA, PrH = NA)
f <- oppose_expansion ~ gop_governor + percent_favorable_aca + gop_sen + percent_uninsured + 
  bal2012 + multiplier + percent_nonwhite + percent_metro
m <- mcmc(f)
prh <- round(sum(m[, "beta[4]"] > 0)/nrow(m), 2)
est <- round(median(m[, "beta[4]"]), 2)
prh; est
res0$Variable <- "GOP Senate"
res0$Expectation <- "+"
res0$Estimate <- est
res0$PrH <- prh
res <- rbind(res, res0)
res


# gop_house + gop_sen
f <- oppose_expansion ~ gop_governor + percent_favorable_aca + gop_house + gop_sen + percent_uninsured + 
  bal2012 + multiplier + percent_nonwhite + percent_metro
m <- mcmc(f)
prh <- sum(m[, "beta[4]"] > 0)/nrow(m)
prh2 <- sum(m[, "beta[5]"] > 0)/nrow(m)
est <- median(m[, "beta[4]"])
est2 <- median(m[, "beta[5]"])
prh; prh2
est; est2


################################################################################
## Alternative Measures of Need
################################################################################

res2 <- NULL
# dsh 
res0 <- data.frame(Variable = NA, Expectation = NA, Estimate = NA, PrH = NA)
f <- oppose_expansion ~ gop_governor + percent_favorable_aca + gop_leg + dsh + 
  bal2012 + multiplier + percent_nonwhite + percent_metro
m <- mcmc(f)
prh <- round(sum(m[, "beta[5]"] < 0)/nrow(m), 2)
est <- round(median(m[, "beta[5]"]), 2)
prh; est
res0$Variable <- "DSH Payments per Capita"
res0$Expectation <- "-"
res0$Estimate <- est
res0$PrH <- prh
res <- rbind(res, res0)
res
# compare the coefficients (negate coefs with negative expectations)
p1 <- round(sum(m[, "beta[2]"] - -m[, "beta[5]"] > 0)/nrow(m), 2) # gop_governor - dsh
p2 <- round(sum(-m[, "beta[3]"] - -m[, "beta[5]"] > 0)/nrow(m), 2) # percent_favorable_aca - dsh
p3 <- round(sum(m[, "beta[4]"] - -m[, "beta[5]"] > 0)/nrow(m), 2) # gop_leg - dsh
res2 <- rbind(res2, c(p1, p2, p3))

# percent below 138% poverty
res0 <- data.frame(Variable = NA, Expectation = NA, Estimate = NA, PrH = NA)
f <- oppose_expansion ~ gop_governor + percent_favorable_aca + gop_leg + percent_poverty138 + 
  bal2012 + multiplier + percent_nonwhite + percent_metro
m <- mcmc(f)
prh <- round(sum(m[, "beta[5]"] < 0)/nrow(m), 2)
est <- round(median(m[, "beta[5]"]), 2)
prh; est
res0$Variable <- "Percent Below 138% Poverty"
res0$Expectation <- "-"
res0$Estimate <- est
res0$PrH <- prh
res <- rbind(res, res0)
res
# compare the coefficients (negate coefs with negative expectations)
p1 <- round(sum(m[, "beta[2]"] - -m[, "beta[5]"] > 0)/nrow(m), 2) # gop_governor - poverty138
p2 <- round(sum(-m[, "beta[3]"] - -m[, "beta[5]"] > 0)/nrow(m), 2) # percent_favorable_aca - poverty138
p3 <- round(sum(m[, "beta[4]"] - -m[, "beta[5]"] > 0)/nrow(m), 2) # gop_leg - poverty138
res2 <- rbind(res2, c(p1, p2, p3))

# low birthweight
res0 <- data.frame(Variable = NA, Expectation = NA, Estimate = NA, PrH = NA)
f <- oppose_expansion ~ gop_governor + percent_favorable_aca + gop_leg + low_birthweight + 
  bal2012 + multiplier + percent_nonwhite + percent_metro
m <- mcmc(f)
prh <- round(sum(m[, "beta[5]"] < 0)/nrow(m), 2)
est <- round(median(m[, "beta[5]"]), 2)
prh; est
res0$Variable <- "Low Birthweight"
res0$Expectation <- "-"
res0$Estimate <- est
res0$PrH <- prh
res <- rbind(res, res0)
res
# compare the coefficients (negate coefs with negative expectations)
p1 <- round(sum(m[, "beta[2]"] - -m[, "beta[5]"] > 0)/nrow(m), 2) # gop_governor - lb
p2 <- round(sum(-m[, "beta[3]"] - -m[, "beta[5]"] > 0)/nrow(m), 2) # percent_favorable_aca - lb
p3 <- round(sum(m[, "beta[4]"] - -m[, "beta[5]"] > 0)/nrow(m), 2) # gop_leg - lb
res2 <- rbind(res2, c(p1, p2, p3))

# heart disease
res0 <- data.frame(Variable = NA, Expectation = NA, Estimate = NA, PrH = NA)
f <- oppose_expansion ~ gop_governor + percent_favorable_aca + gop_leg + heart_disease_death_rate + 
  bal2012 + multiplier + percent_nonwhite + percent_metro
m <- mcmc(f)
prh <- round(sum(m[, "beta[5]"] < 0)/nrow(m), 2)
est <- round(median(m[, "beta[5]"]), 2)
prh; est
res0$Variable <- "Heart Disease Death Rate"
res0$Expectation <- "-"
res0$Estimate <- est
res0$PrH <- prh
res <- rbind(res, res0)
res
# compare the coefficients (negate coefs with negative expectations)
p1 <- round(sum(m[, "beta[2]"] - -m[, "beta[5]"] > 0)/nrow(m), 2) # gop_governor - hd
p2 <- round(sum(-m[, "beta[3]"] - -m[, "beta[5]"] > 0)/nrow(m), 2) # percent_favorable_aca - hd
p3 <- round(sum(m[, "beta[4]"] - -m[, "beta[5]"] > 0)/nrow(m), 2) # gop_leg - hd
res2 <- rbind(res2, c(p1, p2, p3))

# life expectacy
res0 <- data.frame(Variable = NA, Expectation = NA, Estimate = NA, PrH = NA)
f <- oppose_expansion ~ gop_governor + percent_favorable_aca + gop_leg + life_expectancy + 
  bal2012 + multiplier + percent_nonwhite + percent_metro
m <- mcmc(f)
prh <- round(sum(m[, "beta[5]"] > 0)/nrow(m), 2)
est <- round(median(m[, "beta[5]"]), 2)
prh; est
res0$Variable <- "Life Expectancy"
res0$Expectation <- "+"
res0$Estimate <- est
res0$PrH <- prh
res <- rbind(res, res0)
res
# compare the coefficients (negate coefs with negative expectations)
p1 <- round(sum(m[, "beta[2]"] - -m[, "beta[5]"] > 0)/nrow(m), 2) # gop_governor - le
p2 <- round(sum(-m[, "beta[3]"] - -m[, "beta[5]"] > 0)/nrow(m), 2) # percent_favorable_aca - le
p3 <- round(sum(m[, "beta[4]"] - -m[, "beta[5]"] > 0)/nrow(m), 2) # gop_leg - le
res2 <- rbind(res2, c(p1, p2, p3))
            
colnames(res2) <- c("Gov", "Opinion", "Leg")
rownames(res2) <- c("DSH", "Poverty138", "Birthweight", "Heart Disease", "Life Expectancy")
            
tab <- xtable(res)
print.xtable(tab, type = "html", file = "Figures/alt_spec.html")

tab2 <- xtable(res2)
print.xtable(tab2, type = "html", file = "Figures/alt_spec2.html")
