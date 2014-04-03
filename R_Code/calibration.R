library(arm)
library(compactr)

setwd("~/Dropbox/ACA")
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
    m <- bayesglm(oppose_expansion ~ gop_governor + obama_win + gop_leg + percent_uninsured + 
                    income + percent_nonwhite + percent_metro, 
                  family = binomial,
                  control = glm.control(maxit = 3000),
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
      brier[i, j] <- mean((pred - d$oppose_expansion)^2)
      ### calculte the log score
      log[i, j] <- mean(d$oppose_expansion*log(pred) + (1-d$oppose_expansion)*log(1-pred))
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
prior.df <- c(1, 7, Inf)
prior.scale <- c(seq(.1, .4, length.out = 200), seq(.41, 3, length.out = 30))
scores <- loop.it(cv, prior.scale, prior.df)
# pull out the brier and log scores
brier <-  scores$brier
log <- scores$log

svg("Figures/brier.svg", height = 3, width = 5, family = "Georgia")
# plot of brier scores
par(mfrow = c(1,1), mar = c(3,4,1,1), oma = c(0,0,0,0),
    family = "serif")
xlim <- mm(prior.scale)
xlim[2] <- xlim[2] + diff(xlim)/7
eplot(xlim = xlim, ylim = mm(brier),
      xlab = "Prior Scale",
      ylab = "Brier Scores",
      ylabpos = 2.5)
for (j in 1:length(prior.df)) {
  lines(prior.scale, brier[, j], lty = j)
  #text(prior.scale[length(prior.scale)], 
       #brier[length(prior.scale), j],
       #paste("df = ", prior.df[j], sep = ""), 
       #pos = 4, cex = .7)
}

legend(legend = c("Cauchy", "t(df = 7)", "Normal"), lty = c(1,2,3),
       x = 2.4, y = .23, bty = "n")
dev.off()