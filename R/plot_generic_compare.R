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
  dat$country_name = sapply(dat$country_name, shorten_name, USE.NAMES = FALSE)

  dat$outcome <- dat[, params$metric]
  dat$compare <- dat[, compare]
  dat$disc    <- dat[, disciminent]

  dat <- filter_by_params(dat, params)

  dat <- dat %>%
    group_by(compare, disc) %>%
    summarize(outcome = sum(outcome, na.rm = TRUE)) %>%
    arrange(desc(outcome))

  units <- graph_num_div(max(dat$outcome), params$metric)
  dat$outcome <- dat$outcome / units$numdiv
  
  # find the top n_plot compares by outcome
  df_top_compares <- dat %>%
    group_by(compare) %>%
    summarize(outcome = sum(outcome, na.rm = TRUE)) %>%
    arrange(desc(outcome))

  top_compares <- df_top_compares$compare
  if (params$n_plot < length(top_compares))
    top_compares <- top_compares[1:params$n_plot]
  
  dat <- dat %>%
    filter(compare %in% top_compares)
  
  dat$compare <-
    factor(dat$compare, levels = top_compares)
  
  my_cols <- generic_palette(unique(dat$disc))
  
  print(unique(dat$disc))
  print(my_cols)

  ggplot(dat, aes(x = compare,
                  y = outcome,
                  fill = factor(disc))) +
    geom_bar(stat = "identity", color = "black") + 
    scale_fill_manual(values = my_cols) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1),
          legend.title = element_blank()) +
    xlab("") + 
    ylab(units$ylabscale) + 
    
    ggtitle(params$title)
}