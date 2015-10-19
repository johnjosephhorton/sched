#' Return a dataframe of summary stats by year, conditional upon working 
#'
#' Return a dataframe of summary stats by year
#'
#' @return data.frame
#' @export

summaryStats <- function(){
    
    data("df_act_work")
    df_act_work <- data.table(subset(df_act_work, work))
    df_act_work$id <- with(df_act_work, as.numeric(factor(TUCASEID)))
    df_act_work[, shift.length := t.end - t.start]
    df_act_work[, day.length := max(t.end) - min(t.start), by = id]
    df.by.worker <- df_act_work[, list(minutes.worked = sum(shift.length),
                                   num.shifts = .N, 
                                   minutes.work.range = max(t.end) - min(t.start),
                                   day.start = min(t.start),
                                   day.end = max(t.end)
                                   ), by = list(id, year)]
    df.by.year <- df.by.worker[, list(mean.minutes.worked = mean(minutes.worked),
                                  mean.day.start = mean(day.start),
                                  se.day.start = sd(day.start)/sqrt(.N),
                                  se.day.end = sd(day.end)/sqrt(.N), 
                                  mean.day.end = mean(day.end), 
                                  se.minutes.worked = sd(minutes.worked)/sqrt(.N), 
                                  mean.work.range = mean(minutes.work.range),
                                  se.work.range = sd(minutes.work.range)/sqrt(.N), 
                                  mean.num.shifts = mean(num.shifts),
                                  se.num.shifts = sd(num.shifts)/sqrt(.N),
                                  num.obs = .N),
                               by = list(year)]
    df.by.year
}
