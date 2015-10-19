#' Plot showing fraction of workers selecting various hours-reports 
#'
#'  Plot showing fraction of workers selecting various hours-reports 
#'
#' @return ggplot2 
#' @export

HoursReportingPlot <- function(){
    data("df_cps_shift")
    df.by.hours <- data.table(df_cps_shift)[, list(num.obs = .N), by = list(year, hours)]
    df.by.hours[, frac.obs := num.obs / sum(num.obs), by = list(year)]

    #' Fraction of workers reporting various hours-per-week
    g.hours.reporting <- ggplot(data = df.by.hours, aes(x = year, y = frac.obs)) +
        geom_line() +
        facet_wrap(~hours, ncol = 3) +
        theme_bw()
    g.hours.reporting
}
