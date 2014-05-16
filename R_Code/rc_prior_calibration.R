library(arm)
library(compactr)
library(scoring)

setwd("~/Dropbox/Projects/Need")
d <- read.csv("Data/politics_and_need_rescale.csv")

n <- nrow(d)
state.id <- 1:50

# create a function to cross-validate a given model
# note that this function uses data and objects from
# the environment, but prior.scale and prior.df are 
# arguments. This is because only the arguments change
# throughout the analysis. Also note that the prior.scale
# and prior.df arguments here are scalars, not vectors.
cv <- function(prior.scale, prior.df) {
  pred <- numeric(n)
  for (i in 1:n) {
    d.train <- d[state.id != i, ]
    d.test <- d[state.id == i, ]
    m <- bayesglm(oppose_expansion ~ gop_governor + percent_favorable_aca + gop_leg + percent_uninsured + 
                    income + percent_nonwhite + percent_metro, 
                  family = binomial,
                  control = glm.control(maxit = 100000),
                  prior.df = prior.df,
                  prior.scale = prior.scale,
                  data = d.train)
    pred[state.id == i] <- predict(m, type = "response", newdata = d.test)
  }
  return(pred)
}

# function to loop over the vectors prior.scale
# and prior.df
loop.it <- function(cv, prior.scale, prior.df) {
  ### initialize progress bar
  iter <- 0
  total.iter <- length(prior.scale)*length(prior.df)
  pb <- txtProgressBar(min = 0, max = total.iter, style = 3)
  ### setup matrix to hold prediction scores 
  brier <- log <- matrix(NA, nrow = length(prior.scale), 
                         ncol = length(prior.df))
  ### loop over the prior scale and df
  for (i in 1:length(prior.scale)) {
    for (j in 1:length(prior.df)) {
      ### obtain the leave-one-out out-of-sample prediction
      pred <- cv(prior.scale[i], prior.df[j])
      ### calculate the brier score
      brier[i, j] <- mean(brierscore(d$oppose_expansion ~ pred))
      ### calculte the log score
      log[i, j] <- mean(logscore(d$oppose_expansion ~ pred))
      ### update the progress bar
      iter <- iter + 1
      setTxtProgressBar(pb, iter)
    }
  }

  ### store the scores in a list and return
  scores <- list(brier = brier,
                 log = log)
  return(scores)
}

# run the estimations
prior.df <- c(1, 10, Inf)
prior.scale <- exp(seq(log(.1), log(10), length.out = 100))
scores <- loop.it(cv, prior.scale, prior.df)
# pull out the brier and log scores
brier <-  scores$brier
log <- scores$log

tiff("Figures/rc_brier_score.tiff", height = 3, width = 5, units = "in", res = 300, family = "serif")
# plot of brier scores
par(mfrow = c(1,1), mar = c(3,4,1,1), oma = c(0,0,0,0),
    family = "serif")
xlim <- mm(prior.scale)
eplot(xlim = xlim, ylim = mm(brier),
      xlab = "Prior Scale",
      ylab = "Brier Scores",
      ylabpos = 2.5)
for (j in 1:length(prior.df)) {
  lines(prior.scale, brier[, j], lty = j, col = j, lwd = 3)
  #text(prior.scale[length(prior.scale)], 
       #brier[length(prior.scale), j],
       #paste("df = ", prior.df[j], sep = ""), 
       #pos = 4, cex = .7)
}
legend(legend = c("Cauchy", "t(df = 10)", "Normal"), lty = c(1,2,3), col = c(1,2,3), lwd = 3,
       x = .5, y = .22, bty = "n")
dev.off()

# emf("Figures/rc_log_score.emf", height = 3, width = 5, family = "serif")
# # plot of brier scores
# par(mfrow = c(1,1), mar = c(3,4,1,1), oma = c(0,0,0,0),
#     family = "serif")
# xlim <- mm(prior.scale)
# eplot(xlim = xlim, ylim = mm(log),
#       xlab = "Prior Scale",
#       ylab = "Log Scores",
#       ylabpos = 2.5)
# for (j in 1:length(prior.df)) {
#   lines(prior.scale, log[, j], lty = j, col = j, lwd = 3)
#   #text(prior.scale[length(prior.scale)], 
#   #brier[length(prior.scale), j],
#   #paste("df = ", prior.df[j], sep = ""), 
#   #pos = 4, cex = .7)
# }
# legend(legend = c("Cauchy", "t(df = 10)", "Normal"), lty = c(1,2,3), col = c(1,2,3), lwd = 3,
#        x = .5, y = -.6, bty = "n")
# dev.off()