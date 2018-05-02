################################################################################
#' Plots a generic stacked impact bar chart, where the user can decide how the
#' data is agregated and seperated
#'
#' @param dat A dataframe (or similar) of country data for ...
#' @param params A list (TODO consider using an S3 class) of parameters or the
#'               plot
#' @param compare A string of the column name in dat for the variable that will
#'                be used to determine the x axis
#' @param discriminent A string of the column name in dat for the variable that
#'                     be used to determine the block of the bar chart
#'
#' @return A ggplot object
#' @export
plot_generic_compare <- function(dat, params, compare, disciminent) {
  dat$outcome <- impact_data[, params$metric]
  dat$compare <- impact_data[, compare]
  dat$disc    <- impact_data[, disciminent]
  
  print(table(dat$disc))
  
  dat <- filter_by_params(dat, params)
  
  print(table(dat$disc))
  
  dat <- dat %>%
    group_by(compare, disc) %>%
    summarize(outcome = sum(outcome, na.rm = TRUE)) %>%
    arrange(desc(outcome))
  
  print(table(dat$disc))
  
  ggplot(dat, aes(x = compare,
                  y = outcome,
                  fill = disc)) +
    geom_bar(stat = "identity", color = "black") + 
    xlab("") + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.title = element_blank()) +
    
    ggtitle(params$title)
}