# clear workspace
rm(list = ls())

# set working directory
setwd("~/Dropbox/Projects/Need")

# load packages
library(arm)
library(plyr)

# load data
d <- read.csv("Data/politics_and_need.csv")
mrp <- read.csv("~/Dropbox/Projects/ACA_Opinion/Data/mrp_est.csv")
d <- join(d, mrp)

# Variables for the analysis
state <- d$state
state_abbr <- d$state_abbr
oppose_expansion <- 1*(d$support_expansion == "Opposes")
gop_governor <- rescale(d$gop_governor)
percent_favorable_aca <- rescale(d$percent_favorable_aca)
percent_favorable_aca_original_scale <- d$percent_favorable_aca
obama_share <- rescale(d$obama_share)
obama_win <- rescale(d$obama_share > 50)
ideology <- rescale(d$ideology)
gop_house <- rescale(d$hscontrol)
dem_house <- rescale(1 - d$hscontrol)
gop_sen <- rescale(d$sencontrol)
gop_leg <- rescale(d$legGOP)
dem_leg <- rescale(d$legDem)
divided_leg <- rescale(d$legDem == 0 & d$legGOP == 0)
percent_uninsured <- rescale(d$percent_uninsured)
percent_uninsured_original_scale <- d$percent_uninsured
dsh <- rescale(d$dsh)
percent_nonwhite <- rescale(d$percent_hispanic + d$percent_black)
income <- rescale(d$income)
percent_poverty <- rescale(d$percent_poverty)
percent_metro <- rescale(d$percent_metro)

d.rescale <- data.frame(state, 
                        state_abbr,
                        oppose_expansion,
                        gop_governor,
                        percent_favorable_aca,
                        percent_favorable_aca_original_scale,
                        obama_share,
                        obama_win,
                        ideology,
                        gop_house,
                        gop_sen,
                        gop_leg,
                        dem_leg,
                        divided_leg,
                        percent_uninsured,
                        percent_uninsured_original_scale,
                        dsh,
                        percent_nonwhite,
                        income,
                        percent_poverty,
                        percent_metro)


write.csv(d.rescale, "Data/politics_and_need_rescale.csv", row.names = FALSE)
