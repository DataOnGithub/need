rm(list = ls())

library(arm)
library(compactr)
library(texreg)

setwd("~/Dropbox/ACA")
d <- read.csv("Data/politics_and_need_rescale.csv")


# Obama Share
bs.form <- oppose_expansion ~ gop_governor + obama_share + gop_leg + percent_uninsured + 
  income + percent_nonwhite + percent_metro
m <- bayesglm(bs.form, 
              prior.df = 1, prior.scale = 2.5,
              family = binomial, data = d)
bs.size <- 1000
bs.res <- NULL
for (i in 1:bs.size) {
  bs.samp <- sample(1:nrow(d), nrow(d), replace = TRUE)
  bs.data <- d[bs.samp, ]
  bs.est <- bayesglm(bs.form, 
                     prior.df = 1, prior.scale = 2.5,
                     family = binomial, data = bs.data)$coef
  bs.res <- rbind(bs.res, bs.est)
}
sum(bs.res[, "obama_share"] > 0)/bs.size

# Ideology
bs.form <- oppose_expansion ~ gop_governor + ideology + gop_leg + percent_uninsured + 
  income + percent_nonwhite + percent_metro
m <- bayesglm(bs.form, 
         prior.df = 1, prior.scale = 2.5,
         family = binomial, data = d)
bs.size <- 1000
bs.res <- NULL
for (i in 1:bs.size) {
  bs.samp <- sample(1:nrow(d), nrow(d), replace = TRUE)
  bs.data <- d[bs.samp, ]
  bs.est <- bayesglm(bs.form, 
                     prior.df = 1, prior.scale = 2.5,
                     family = binomial, data = bs.data)$coef
  bs.res <- rbind(bs.res, bs.est)
}
sum(bs.res[, "ideology"]< 0)/bs.size


# Dem Legislature
bs.form <- oppose_expansion ~ gop_governor + obama_win + dem_leg + percent_uninsured + 
  income + percent_nonwhite + percent_metro
m <- bayesglm(bs.form, 
              prior.df = 1, prior.scale = 2.5,
              family = binomial, data = d)
bs.size <- 1000
bs.res <- NULL
for (i in 1:bs.size) {
  bs.samp <- sample(1:nrow(d), nrow(d), replace = TRUE)
  bs.data <- d[bs.samp, ]
  bs.est <- bayesglm(bs.form, 
                     prior.df = 1, prior.scale = 2.5,
                     family = binomial, data = bs.data)$coef
  bs.res <- rbind(bs.res, bs.est)
}
sum(bs.res[, "dem_leg"] > 0)/bs.size

# Rep Legislature and Divided
bs.form <- oppose_expansion ~ gop_governor + obama_win + gop_leg + divided_leg + percent_uninsured + 
  income + percent_nonwhite + percent_metro
m <- bayesglm(bs.form, 
              prior.df = 1, prior.scale = 2.5,
              family = binomial, data = d)
bs.size <- 1000
bs.res <- NULL
for (i in 1:bs.size) {
  bs.samp <- sample(1:nrow(d), nrow(d), replace = TRUE)
  bs.data <- d[bs.samp, ]
  bs.est <- bayesglm(bs.form, 
                     prior.df = 1, prior.scale = 2.5,
                     family = binomial, data = bs.data)$coef
  bs.res <- rbind(bs.res, bs.est)
}
sum(bs.res[, "gop_leg"] < 0)/bs.size
sum(bs.res[, "divided_leg"] < 0)/bs.size
sum(bs.res[, "gop_leg"] - bs.res[, "divided_leg"] < 0)/bs.size



# GOP House
bs.form <- oppose_expansion ~ gop_governor + obama_win + gop_house + percent_uninsured + 
  income + percent_nonwhite + percent_metro
m <- bayesglm(bs.form, 
              prior.df = 1, prior.scale = 2.5,
              family = binomial, data = d)
bs.size <- 1000
bs.res <- NULL
for (i in 1:bs.size) {
  bs.samp <- sample(1:nrow(d), nrow(d), replace = TRUE)
  bs.data <- d[bs.samp, ]
  bs.est <- bayesglm(bs.form, 
                     prior.df = 1, prior.scale = 2.5,
                     family = binomial, data = bs.data)$coef
  bs.res <- rbind(bs.res, bs.est)
}
sum(bs.res[, "gop_house"] < 0)/bs.size

# GOP Senate
bs.form <- oppose_expansion ~ gop_governor + obama_win + gop_sen + percent_uninsured + 
  income + percent_nonwhite + percent_metro
m <- bayesglm(bs.form, 
              prior.df = 1, prior.scale = 2.5,
              family = binomial, data = d)
bs.size <- 1000
bs.res <- NULL
for (i in 1:bs.size) {
  bs.samp <- sample(1:nrow(d), nrow(d), replace = TRUE)
  bs.data <- d[bs.samp, ]
  bs.est <- bayesglm(bs.form, 
                     prior.df = 1, prior.scale = 2.5,
                     family = binomial, data = bs.data)$coef
  bs.res <- rbind(bs.res, bs.est)
}
sum(bs.res[, "gop_sen"] < 0)/bs.size



# Tables

m <- list()
# main
m[[1]] <- bayesglm(oppose_expansion ~ gop_governor + obama_win + gop_leg + percent_uninsured + 
                     income + percent_nonwhite + percent_metro, 
                   prior.df = 1, prior.scale = 2.5,
                   family = binomial, data = d)
# no controls
m[[2]] <- bayesglm(oppose_expansion ~ gop_governor + obama_win + gop_leg + percent_uninsured, 
                   prior.df = 1, prior.scale = 2.5,
                   family = binomial, data = d)
# ideology
m[[3]] <- bayesglm(oppose_expansion ~ gop_governor + ideology + gop_leg + percent_uninsured + 
                     income + percent_nonwhite + percent_metro, 
                   prior.df = 1, prior.scale = 2.5,
                   family = binomial, data = d)
# obama_share
m[[4]] <- bayesglm(oppose_expansion ~ gop_governor + obama_share + gop_leg + percent_uninsured + 
                     income + percent_nonwhite + percent_metro, 
                   prior.df = 1, prior.scale = 2.5,
                   family = binomial, data = d)
# dem_leg
m[[5]] <- bayesglm(oppose_expansion ~ gop_governor + obama_win + dem_leg + percent_uninsured + 
                     income + percent_nonwhite + percent_metro, 
                   prior.df = 1, prior.scale = 2.5,
                   family = binomial, data = d)
# rep_leg + divided
m[[6]] <- bayesglm(oppose_expansion ~ gop_governor + obama_win + gop_leg + divided_leg + percent_uninsured + 
                     income + percent_nonwhite + percent_metro, 
                   prior.df = 1, prior.scale = 2.5,
                   family = binomial, data = d)
# gop_house
m[[7]] <- bayesglm(oppose_expansion ~ gop_governor + obama_win + gop_house + percent_uninsured + 
                     income + percent_nonwhite + percent_metro, 
                   prior.df = 1, prior.scale = 2.5,
                   family = binomial, data = d)
# gop_sen
m[[8]] <- bayesglm(oppose_expansion ~ gop_governor + obama_win + gop_sen + percent_uninsured + 
                     income + percent_nonwhite + percent_metro, 
                   prior.df = 1, prior.scale = 2.5,
                   family = binomial, data = d)

htmlreg(file = "Tables/alt.doc",
        m, stars = c(0.05, 0.1, 0.2), 
        custom.coef.names = c("Constant",
                              "GOP Governor",
                              "Obama Win in 2012",
                              "GOP Controlled Legislature",
                              "% Uninsured",
                              "Income",
                              "% Non-White",
                              "% Metro",
                              "Ideology",
                              "Obama Share",
                              "Democrat-Controlled Legislature",
                              "Divided Legislature",          
                              "GOP House",
                              "GOP Senate"),
        reorder.coef = c(2, 3, 9, 10, 4, 11, 12, 13, 14, 5, 6, 7, 8, 1),
        inline.css = FALSE, doctype = TRUE, html.tag = TRUE, 
        head.tag = TRUE, body.tag = TRUE
)
