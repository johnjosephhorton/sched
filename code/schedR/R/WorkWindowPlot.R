#' Work window over time
#'
#' Worker window over time
#'
#' @return ggplot2
#' @export

WorkWindowPlot <- function(){
    df.by.year <- schedR::summaryStats()

    g <- ggplot(data = df.by.year, aes(x = year)) +
        geom_line(aes(y = mean.day.start), colour = "green") +
        geom_errorbar(aes(ymin = mean.day.start - 2*se.day.start, ymax = mean.day.start + 2*se.day.start), colour = "green") +
        geom_line(aes(y = mean.day.end), colour = "red") +
        geom_errorbar(aes(ymin = mean.day.end - 2*se.day.end, ymax = mean.day.end + 2*se.day.end), colour = "red") + 
        theme_bw() 
    g
}
