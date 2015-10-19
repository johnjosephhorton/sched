#' Visualize the shift patterns for a given year
#'
#' Visualize the shift patterns for a given year
#'
#' @param visualization.year
#' @return ggplot2
#' @export

ShiftVisualization <- function(visualization.year){
    data("df_act_work")
    df_act_work <- subset(df_act_work, year == visualization.year & work)    
    df_act_work$id <- with(df_act_work, TUCASEID)
    df_act_work$id.int <- as.numeric(with(df_act_work, factor(id, labels = 1:length(unique(id)))))
    
    g <- ggplot(data = df_act_work) +
        geom_rect(aes(xmin = t.start, xmax = t.end, ymin = id.int, ymax = id.int + 1)) +
        geom_vline(xintercept = (9 * 60), colour = "red") +
        geom_vline(xintercept = (17 * 60), colour = "green") +
        xlab("Minutes of the day") +
        theme_bw()
    g
}
