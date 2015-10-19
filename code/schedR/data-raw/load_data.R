#!/usr/bin/Rscript

library(devtools)

# read in the CPS data 
# df.cps.small <- read.csv("./atuscps_0314.dat", nrow = 4)

df.cps <- subset(read.csv("./atuscps_0314.dat"), select = c("TUCASEID", "PEIO1OCD", "PRERNHLY", "PRHRUSL"))
                                        #' hourly earnings are in cents

#' PRHRUSL - "Usual hours worked weekly"
#' PRERNHLT - "Hourly earnings"

hours.labels <- list(
"-1"= "Blank",
"-2"= "Don't Know",
"-3"= "Refused",
"1"= "0-20 hours",
"2"= "21-34 hours",
"3"= "35-39 hours",
"4"= "40 hours",
"5"= "41-49 hours",
"6"= "50 or more hours",
"7"= "Varies - full time",
"8"= "Varies - part time")

df.cps$hours <- with(df.cps, factor(PRHRUSL, levels = names(hours.labels),
                                    labels = unlist(hours.labels, use.names = FALSE)))


SHIFT.OCCUPATIONS <- c("4760")
shift.workers <- subset(df.cps, PEIO1OCD %in% SHIFT.OCCUPATIONS)$TUCASEID
df_cps_shift <- subset(df.cps, PEIO1OCD %in% SHIFT.OCCUPATIONS)
df_cps_shift$year <- as.integer(with(df_cps_shift, substring(TUCASEID, first = 1, last = 4)))

devtools::use_data(df_cps_shift, overwrite = TRUE)

rm(df.cps)

# read in the master activity file, restricted to shift workers  
df.act <- subset(read.csv("atusact_0314.dat"), TUCASEID %in% shift.workers)
# identify work activity codes 
df.act$work <- with(df.act, grepl("50*", TRCODEP))
# get start and end
df.act$t.start <- with(df.act, as.numeric(TUSTARTTIM))
df.act$t.end.temp <- with(df.act, as.numeric(TUSTOPTIME))
df.act$t.end <- with(df.act, ifelse(t.start > t.end.temp, t.end.temp + 1440, t.end.temp))
# write to the package's data file 
df_act_work <- subset(df.act, select = c(TUCASEID, t.start, t.end, work))

df_act_work <- merge(df_act_work, df_cps_shift,
                by.x = "TUCASEID",
                by.y = "TUCASEID", all.x = TRUE,
                all.y = FALSE)

rm(df_cps_shift)

df_act_work$year <- as.integer(with(df_act_work, substring(TUCASEID, first = 1, last = 4)))

devtools::use_data(df_act_work, overwrite = TRUE, pkg = "../schedR")

