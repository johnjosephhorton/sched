#' Creates hours table
#'
#' Creates hours table
#'
#' @return none
#' @export


HoursTable <- function(out.file){
    data("df_cps_shift")
    df_cps_shift$high.hours <- with(df_cps_shift, hours %in% c("40 hours", "41-49 hours", "50 or more hours"))

    m.var.pt <- lm(I(hours == "Varies - part time") ~ year, data = df_cps_shift)
    m.var.ft <- lm(I(hours == "Varies - full time") ~ year, data = df_cps_shift)
    m.high.hours <- lm(high.hours ~ year, data = df_cps_shift)

    covariate.labels <- c("Year", "Constant")
    dep.var.labels <- c("Varies - part time", "Varies - full time", "High hours")
    title = "Respondent reported hours category"
    sink(file = "/dev/null")
    s <- stargazer::stargazer(m.var.pt, m.var.ft, m.high.hours,
                          title = title,
                          star.cutoffs = star.cutoffs,
                          star.char = star.char, 
                          covariate.labels = covariate.labels,
                          label = "tab:hired_rate",
                          omit.stat = c("aic", "f", "adj.rsq", "ll", "bic", "ser"),
                          dep.var.labels = dep.var.labels 
                          )
    sink() 
    note <- paste("Here are some notes.")
    JJHmisc::AddTableNote(s, out.file, JJHmisc::NoteWrapper(note))
}
