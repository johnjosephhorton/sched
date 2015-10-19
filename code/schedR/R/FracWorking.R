#' Fraction of retail workers working on the sample day
#'
#' Fraction of retail workers working on the same day
#'
#' @return data frame 
#' @export

FracWorking <- function(){
    data("df_act_work")
    library(data.table)
    df_act_work <- data.table::data.table(df_act_work)

    df.by.worker <- df_act_work[ , list(any.work = any(work)), by = list(year, TUCASEID)]

    df.frac.working <- df.by.worker[, list(frac.working = mean(any.work),
                                          se = sd(any.work)/sqrt(.N)),
                                   by = year]
    df.frac.working
}
