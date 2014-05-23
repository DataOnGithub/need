# clear workspace
rm(list = ls())

# set working directory
setwd("~/Dropbox/Projects/Need")

# load packages
library(arm)
library(compactr)
library(party)
library(devEMF)


# d <- read.csv("Data/politics_and_need.csv")
# d$oppose_expansion <- as.factor(d$gov_position == "Opposes")
# summary(d)
# d <- na.omit(d)
#  
# f <- oppose_expansion ~ gop_governor + 
#   obama_win + obama_share + percent_favorable_aca + percent_supporting_expansion + percent_supporting_tea_party + 
#   gop_house + gop_sen + 
#   percent_uninsured + percent_poverty138 + dsh + low_birthweight + 
#   heart_disease_death_rate + life_expectancy
# 
# plot(ctree(f, data = d))
#
# m0 <- bayesglm(f, data = d, family = binomial)
# display(m0)
# 
# # using party package
# f <- oppose_expansion ~ gop_governor + 
#   obama_win + obama_share + percent_favorable_aca + percent_supporting_expansion + percent_supporting_tea_party + 
#   gop_house + gop_sen + 
#   percent_uninsured + percent_poverty138 + dsh + low_birthweight + 
#   heart_disease_death_rate + life_expectancy
# 
# need.var <- c(rep(0, 8), rep(1, 9))
# 
# var.names <- c("GOP Governor", "Obama Win in 2012", "Obama Share in 2012", 
#                "Percent Favorable to ACA", "Percent Supporting Expansion", 
#                "Percent Supporting Tea Party", "GOP House", "GOP Senate", 
#                "Percent without Insurance", "Percent Below 138% Poverty", 
#                "DSH Payments per Capita", "Low Birthweight Rate",
#                "Heart Disease Death Rate", "Life Expectancy")
# d1 <- cforest(f, data = d, controls = cforest_unbiased(mtry = 4, ntree = 500))
# imp <- varimp(d1, conditional = TRUE)
# ord <- order(imp)
# par(mfrow = c(1,1), oma = c(0,0,0,0), mar = c(3, 10, 1, 1), family = "serif")
# barplot(imp[ord], col = 1 + need.var[ord], horiz = TRUE, las = 1, xlab = "Variable Importance")

d <- read.csv("Data/politics_and_need_rescale.csv")
d$oppose_expansion <- as.factor(d$oppose_expansion)
summary(d)
d <- na.omit(d)

# bootstrap
n.bs <- 100
library(party)
f <- oppose_expansion ~ gop_governor + 
  obama_share + ideology + percent_favorable_aca + percent_supporting_expansion + percent_supporting_tea_party + 
  gop_house + gop_sen + 
  percent_uninsured + percent_poverty138 + dsh + low_birthweight + 
  heart_disease_death_rate + life_expectancy
need.var <- c(rep(0, 8), rep(1, 6))

var.names <- c("GOP Governor", "Obama Share in 2012", "Ideology",
               "Percent Favorable to ACA", "Percent Supporting Expansion", 
               "Percent Supporting Tea Party", "GOP House", "GOP Senate", 
               "Percent without Insurance", "Percent Below 138% Poverty", 
               "DSH Payments per Capita", "Low Birth Weight Rate",
               "Heart Disease Death Rate", "Life Expectancy")
n.var <- length(var.names)
imp <- matrix(NA, nrow = n.bs, ncol = n.var)
colnames(imp) <- var.names
pb <- txtProgressBar(min = 0, max = n.bs, style = 3)
for (bs.iter in 1:n.bs) {
  bs.data <- d[sample(nrow(d), replace = TRUE), ]
  bs.forest <- cforest(f, data = d, controls = cforest_unbiased(mtry = 3, ntree = 500))
  imp[bs.iter, ] <- varimp(bs.forest, conditional = TRUE)
  setTxtProgressBar(pb, bs.iter)
}

write.csv(imp, "Data/bs_imp.csv")
#imp <- read.csv("Data/bs_imp.csv")[, -1]

mean.imp <- apply(imp, 2, mean)
ord <- order(mean.imp)
pch <- numeric(length(ord))
pch[need.var[ord] == 1] <- 21
pch[need.var[ord] == 0] <- 19
col <- numeric(length(ord))
col[need.var[ord] == 1] <- "red"
col[need.var[ord] == 0] <- "black"

emf("Figures/bs_imp.emf", height = 4, width = 7)
par(mfrow = c(1,1), oma = c(0,0,0,0), mar = c(3, 13, 3, 1), family = "serif")
eplot(xlim = mm(imp), ylim = c(0, n.var + 1),
      yat = 1:n.var, yticklab = var.names[ord],
      xlab = "Bootstrapped Variable Importance",
      main = "Predicting Governor's Decision to Oppose Medicaid Expansion")
for (i in 1:n.var) {
  abline(h = i, lty = 3, col = "grey80")
  q <- quantile(imp[, ord[i]], c(.05, .5, .95))
  lines(q[c(1, 3)], c(i, i), lwd = 3, col = col[i])
  points(q[2], i, pch = pch[i], bg = "white", col = col[i])
  #text(q[3], i, var.names[ord][i], pos = 4)
}
xpos <- par("usr")[2]
ypos <- par("usr")[3]
legend(x = xpos, y = ypos, legend = c("Politics", "Need"), pch = c(19, 21),
       xjust = 1, yjust = 0, lty = 1, pt.bg = "white", lwd = 3, pt.lwd = 1,
       col = c("black", "red"), bg = "white")
dev.off()

