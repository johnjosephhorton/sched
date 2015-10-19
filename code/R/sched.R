#!/usr/bin/Rscript --vanilla

WIDTH = 5
HEIGHT = 3

library(devtools)

#devtools::install_github("johnjosephhorton/JJHmisc")
library(JJHmisc)

devtools::install("../schedR", overwrite = TRUE)
library(schedR)

#'---------------------------
#' CPS weekly hours reporting
#'---------------------------

schedR::HoursTable("../../writeup/tables/hours.tex")
g.hours.reporting <- schedR::HoursReportingPlot()
JJHmisc::writeImage(g.hours.reporting, "hours_reporting", width = WIDTH, height = HEIGHT)

#-------------------------------------
# ATUS amount worked on the survey day
#-------------------------------------

df.by.year <- schedR::summaryStats()

g.by.year <- ggplot(data = df.by.year, aes(x = year)) +
    geom_line(aes(y = mean.minutes.worked)) +
    geom_errorbar(aes(ymin = mean.minutes.worked - 2*se.minutes.worked,
                      ymax = mean.minutes.worked + 2*se.minutes.worked)) +
    geom_line(aes(y = mean.work.range), colour = "red") + 
    theme_bw() 


JJHmisc::writeImage(g.by.year, "mean_minutes_worked", width = 8, height = 5)

#'------------------------------------------------------
#' Fraction of workers responding they worked on that day
#'------------------------------------------------------

g.frac.working <- schedR::FracWorkingPlot()
JJHmisc::writeImage(g.frac.working, "frac_working", width = WIDTH, height = HEIGHT)

#m <- lm(high.hours ~ year * log(PRERNHLY), subset(df_cps_shift, PRERNHLY > 1))
#m <- lm(I(hours == "Varies - part time") ~ year * log(PRERNHLY), subset(df_cps_shift, PRERNHLY > 1))

#'--------------------
#' Shift vizualization
#'---------------------

g.shift <- schedR::ShiftVisualization(2014)
JJHmisc::writeImage(g.shift, "scheduling_2014", width = WIDTH, height = HEIGHT)

#--------------------------------------
#' Work window (looking for clopenings)
#' ------------------------------------ 

g.clopening <- schedR::WorkWindowPlot()
JJHmisc::writeImage(g.clopening, "clopening", width = WIDTH, height = HEIGHT)

#---------------------------------------
# Number of shift "spells" in a work day 
#---------------------------------------

outcome = "mean.num.shifts"
ggplot(data = df.by.year, aes(x = year)) +
    geom_line(aes_string(y = outcome)) +
    geom_errorbar(aes(ymin = mean.num.shifts - 2*se.num.shifts,
                      ymax = mean.num.shifts + 2*se.num.shifts)) +
    theme_bw() 

