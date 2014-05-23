# clear workspace
rm(list = ls())

# set working directory
setwd("~/Dropbox/Projects/Need")

# load packages
library(arm)
library(plyr)
library(RCurl)

# load data
d <- read.csv("Data/politics_and_need.csv")

# Variables for the analysis
state <- d$state
state_abbr <- d$state_abbr
oppose_expansion <- 1*(d$gov_position == "Opposes")
gop_governor <- rescale(d$gov_party == "Republican")
percent_favorable_aca <- rescale(d$percent_favorable_aca)
percent_supporting_expansion <- rescale(d$percent_supporting_expansion)
percent_supporting_tea_party <- rescale(d$percent_supporting_tea_party)
percent_favorable_aca_original_scale <- d$percent_favorable_aca
obama_share <- rescale(d$obama_share_12)
obama_win <- rescale(d$obama_share_12 > 50)
ideology <- rescale(d$ideology)
gop_house <- rescale(d$house_party == "Republican")
gop_sen <- rescale(d$sen_party == "Republican")
gop_leg <- rescale(d$house_party == "Republican" & d$sen_party == "Republican")
percent_uninsured <- rescale(d$percent_uninsured)
percent_uninsured_original_scale <- d$percent_uninsured
dsh <- rescale(d$dsh_payments/d$population)
percent_nonwhite <- rescale(100 - d$percent_white)
bal2012 <- rescale(d$bal2012)
percent_poverty <- rescale(d$percent_poverty)
percent_poverty138 <- rescale(d$percent_poverty138)
percent_metro <- rescale(d$percent_metropolitan)
multiplier <- rescale(d$multiplier)
low_birthweight <- rescale(d$n_low_birthweight/d$population)
infant_mortality_rate <- rescale(d$infant_mortality_rate)
cancer_incidence <- rescale(d$cancer_incidence)
heart_disease_death_rate <- rescale(d$heart_disease_death_rate)
life_expectancy <- rescale(d$life_expectancy)

d.rescale <- data.frame(state, 
                        state_abbr,
                        oppose_expansion,
                        gop_governor,
                        percent_favorable_aca,
                        percent_favorable_aca_original_scale,
                        percent_supporting_expansion,
                        percent_supporting_tea_party,
                        obama_share,
                        obama_win,
                        ideology,
                        gop_house,
                        gop_sen,
                        gop_leg,
                        percent_uninsured,
                        percent_uninsured_original_scale,
                        dsh,
                        percent_nonwhite,
                        bal2012,
                        percent_poverty,
                        percent_poverty138,
                        percent_metro,
                        multiplier,
                        low_birthweight,
                        infant_mortality_rate,
                        cancer_incidence,
                        heart_disease_death_rate,
                        life_expectancy)


write.csv(d.rescale, "Data/politics_and_need_rescale.csv", row.names = FALSE)
