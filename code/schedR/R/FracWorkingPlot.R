#' Fraction of retail workers working on the sample day, over time
#'
#' Fraction of retail workers working on the same day, over time 
#'
#' @return data frame 
#' @export

FracWorkingPlot <- function(){
    df.frac.working <- schedR::FracWorking()

    g.frac.working <- ggplot(data = df.frac.working, aes(x = year, y = frac.working)) + geom_line() +
        geom_errorbar(aes(ymin = frac.working - 2*se, ymax = frac.working + 2*se)) +
        theme_bw()
    
    g.frac.working
}
