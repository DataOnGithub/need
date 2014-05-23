# clear workspace 
rm(list = ls())

library(compactr)
library(devEMF)

setwd("~/Dropbox/Projects/Need")
d <- read.csv("Data/politics_and_need.csv", stringsAsFactors = FALSE)
emf("Figures/mrp_ests.emf", height = 7, width = 5, family = "serif")
par(mfrow = c(1,1), oma = c(3,1,1,1), mar = c(0,0,0,0), family = "serif")
eplot(xlim = mm(d$percent_favorable_aca), ylim = c(1, 51), anny = FALSE,
      xlab = "Percent Favorable to ACA")
abline(v = .5, col = "grey50")
pos = c(rep(4, 49), 2)
points(d$percent_favorable_aca[order(d$percent_favorable_aca)], 1:50, pch = 19)
text(d$percent_favorable_aca[order(d$percent_favorable_aca)], 1:50, d$state_abbr[order(d$percent_favorable_aca)], cex = .8, pos = pos)
dev.off()