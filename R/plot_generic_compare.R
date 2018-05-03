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
  no_disc <- is.null(disciminent)
  
  if (no_disc) {
    disciminent <- compare # we set this 
  }

  dat$outcome <- dat[, params$metric]
  dat$compare <- dat[, compare]
  dat$disc    <- dat[, disciminent]
  
  ordered_compare <- is.numeric(dat$compare)

  dat <- filter_by_params(dat, params)

  dat <- dat %>%
    group_by(compare, disc) %>%
    summarize(outcome = sum(outcome, na.rm = TRUE)) %>%
    arrange(desc(outcome))

  # find the top n_plot compares by outcome
  if (ordered_compare) {
    df_top_compares <- dat %>%
      group_by(compare) %>%
      summarize(outcome = sum(outcome, na.rm = TRUE)) %>%
      arrange(compare)
  } else {
    df_top_compares <- dat %>%
      group_by(compare) %>%
      summarize(outcome = sum(outcome, na.rm = TRUE)) %>%
      arrange(desc(outcome))   
  }
               
  units <- graph_num_div(max(df_top_compares$outcome), params$metric)
  dat$outcome <- dat$outcome / units$numdiv
  df_top_compares$outcome <- df_top_compares$outcome / units$numdiv
  top_label_shift <- max(df_top_compares$outcome) / 50

  top_compares <- df_top_compares$compare
  if (!is.null(params$n_plot)) {
    if (params$n_plot < length(top_compares)) {
      top_compares <- top_compares[1:params$n_plot]
    }
  }
  
  df_top_compares <- df_top_compares %>%
    filter(compare %in% top_compares)
  
  dat <- dat %>%
    filter(compare %in% top_compares)
  
  dat$compare <-
    factor(dat$compare, levels = top_compares)
  
  my_cols <- generic_palette(unique(dat$disc))

  dat$compare <- as.factor(dat$compare)
  df_top_compares$compare <- as.factor(df_top_compares$compare)

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
    annotate("text", x = df_top_compares$compare, 
                     y = df_top_compares$outcome + top_label_shift,
                     label = as.character(signif(df_top_compares$outcome, 2)),
                     colour = "black", size = 3) +
    
    ggtitle(params$title)
}