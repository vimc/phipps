################################################################################
#' Plots a generic stacked impact bar chart, comparing two touchstones
#'
#' @param dat A dataframe (or similar) of country data for ...
#' @param params A list (TODO consider using an S3 class) of parameters or the
#'               plot
#' @param compare A variable to compare across               
#' @param print_threshold
#'
#' @return A list containt p = ggplot object, d = the data frame used in the plot
#' @export
graph_diff_touchstones <- function(dat, params, compare = NULL, print_threshold = NA, n_plot = NULL) {
  dat$country_name = sapply(dat$country_name, shorten_name, USE.NAMES = FALSE)
  dat <- prep_diff_touchstone(dat, params, compare)
  units <- graph_num_div(max(abs(dat$tot)), params$outcome)
  
  # normalise the outcome human readable units
  dat <- dat %>%
    mutate(outcome = tot / units$numdiv) %>%
    mutate(numlab = signif(outcome, 2))
  
  # filter so that only the top plot_n remain
  df_top_compares <- dat %>%
    group_by(compare) %>%
    summarize(outcome = abs(sum(outcome, na.rm = TRUE))) %>%
    arrange(desc(outcome))
  
  top_compares <- df_top_compares$compare
  if (!is.null(n_plot)) {
    if (n_plot < length(top_compares)) {
      top_compares <- top_compares[1:n_plot]
    }
  }
  
  df_top_compares <- df_top_compares %>%
    filter(compare %in% top_compares)
  
  dat <- dat %>%
    filter(compare %in% top_compares)
  
  dat$compare <-
    factor(dat$compare, levels = top_compares)
  
  rot_angle <- if (length(unique(dat$compare)) >= 4) {90} else {0}
  
  # set offset for label
  top_label_shift <- max(df_top_compares$outcome) / (20)
  if (is.na(print_threshold)) {
    print_threshold <- signif(top_label_shift, 1)
  }
  
  toplab <- dat %>%
    group_by(outcome < 0, compare) %>%
    summarise(top = sum(outcome)) %>%
    mutate(toplab = signif(top, 2))
  
  # remove labels of small value
  small_top_labels <- which(abs(toplab$toplab) < max(df_top_compares$outcome) / (20))
  toplab$toplab[small_top_labels] <- NA
  small_num_labels <- which(abs(dat$numlab) < 50)
  dat$numlab[small_num_labels] <- NA
  
  my_cols <- brewer.pal(10, "Spectral")
  names(my_cols) <- levels(dat$gap_reason)
  
  if (is.null(params$title)) {
    my_title <- title_diff_touchstones(params)
  } else {
    my_title <- params$title
  }
  
  plot <- ggplot(dat, aes(x = compare, y = outcome, fill = gap_reason)) +
    geom_bar(stat = "identity", position = "stack", width = 0.8) +
    scale_fill_manual(values = my_cols) +
    ylab(units$ylabscale) +
    geom_hline(yintercept = 0, colour = "black", alpha = 1, size = 1) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = if (is.null(compare)) { element_blank() } else { element_text(angle = rot_angle, hjust = 0.5) },
          axis.ticks.x = element_blank(),
          legend.text = element_text(size = 4)) +
    theme(legend.title = element_blank(),
          legend.justification = c(1,1), legend.position = "right") +
    guides(fill = guide_legend(reverse = TRUE)) +
    geom_text(aes(x = dat$compare, label = numlab),
              color = "white", size = 3,
              position = position_stack(vjust = 0.5)) +
    annotate("text", x = toplab$compare, y = toplab$top + top_label_shift * sign(toplab$top),
             label = toplab$toplab, colour = "black", size = 4) +
    ggtitle(my_title)
  
  return(list(p = plot, d = dat))
}

################################################################################
#' processes a data frame of country data to calculate the difference between to
#' touchstones. This should probably only be called from graph_diff_touchstones
#'
#' @param dat A dataframe (or similar) of country data for ...
#' @param params A list (TODO consider using an S3 class) of parameters or the
#'               plot
#' @param compare A variable to compare across      
#' @return The processesd data frame
prep_diff_touchstone <- function(dat_all, params, compare) {
  # assume params had been checked
  
  # we need to get rid some columns so that the tidyr gather->unite->spread
  # paradigm works later
  vars_to_keep <- c("touchstone", "is_focal", "disease", "vaccine",
                    "vaccine_delivery", "activity_type", "support_type",
                    "country", "country_name", "gavi73", "year", "deaths_averted",
                    "deaths_averted_rate", "cases_averted_rate", "cases_averted",
                    "fvps", "year_intro", "coverage")
  
  touchstone_rename <- c(touchstone_1 = params$touchstone_1,
                         touchstone_2 = params$touchstone_2)
  
  dat <- dat_all %>%
    select(vars_to_keep) %>%
    filter(is_focal)
  
  dat <- filter_by_params(dat, params)
  
  # if no compare value has been supplied fill a dummy columnd with 1s
  if (!is.null(compare)) {
    dat$compare <- dat[, compare]
  } else {
    dat$compare <- rep(1, nrow(dat))
  }
  
  ## renaming a couple of variables:
  dat <- dat %>%
    dplyr::rename(outcome = !!params$outcome) %>%
    dplyr::rename(outcome_rate = !!paste0(params$outcome, "_rate")) %>%
    mutate(touchstone = replace(touchstone,
                                touchstone == params$touchstone_1, "t1"),
           touchstone = replace(touchstone,
                                touchstone == params$touchstone_2, "t2")) %>%
    mutate(year_intro = replace(year_intro,
                                is.na(year_intro) &
                                  activity_type == "routine", Inf))
  
  # now remove unneeded columns that will break the gather->unite->spread
  cols_to_delete <- c("deaths_averted", "deaths_averted_rate",
                      "cases_averted", "cases_averted_rate",
                      "dalys_averted", "dalys_averted_rate")
  cols_to_delete  <- cols_to_delete[cols_to_delete %in% names(dat)]
  
  dat  <- dat %>%
    select(-one_of(cols_to_delete))
  
  # gather->unite->spread statement to generate diff columns
  dat_wide <- dat %>%
    gather(variable, value,
           c(coverage, year_intro, outcome, outcome_rate, fvps)) %>%
    unite(var_comb, touchstone, variable) %>%
    spread(var_comb, value) %>%
    mutate(diff_coverage = t2_coverage - t1_coverage) %>%
    mutate(diff_outcome = t2_outcome - t1_outcome) %>%
    mutate(diff_outcome_rate = t2_outcome_rate - t1_outcome_rate) %>%
    mutate(diff_intro = t2_year_intro - t1_year_intro) %>%
    mutate(diff_fvps = t2_fvps - t1_fvps) 
  # assign a reason for the difference
  dat_wide <- gap_attribution(dat_wide)
  
  ## here I'm filtering out some of the sins in gap_reasons
  ## TO DO: still need to debug the sins!
  dat_wide <- dat_wide %>%
    filter(gap_reason %in%
             grep("([+-])", levels(dat_wide$gap_reason), value = TRUE))
  
  dat_wide <- dat_wide %>%
    group_by(gap_reason, compare)%>%
    summarise(tot = sum(diff_outcome, na.rm = TRUE))
  
  return(dat_wide)
}

################################################################################
#' Atributes a label to to each difference calculated in prep_diff_touchstone
#'
#' @param dat A dataframe containing processed touchstone data (from 
#'            prep_diff_touchstone)
#'
#' @return The processesd data frame with an extra column (gap_reason) added
gap_attribution <- function(dat) {
  
  reason_levels <- c("(-) RI coverage lower than forecast",
                     "(-) RI intro later than forecast",
                     "(-) SIA did not occur",
                     "(-) SIA reached smaller population than forecast",
                     "",
                     "(0) No difference",
                     "(+) RI coverage higher than forecast",
                     "(+) RI intro earlier than forecast",
                     "(+) SIA happened which was not forecast",
                     "(+) SIA reached larger population than forecast")
  
  dat <- dat %>%
    mutate(gap_reason = "",
           gap_reason = replace(gap_reason,
                                diff_coverage == 0 | diff_outcome == 0,
                                "(0) No difference"),
           gap_reason = replace(gap_reason, activity_type == "routine" &
                                  t1_coverage == 0 & t2_coverage > 0,
                                "(+) RI intro earlier than forecast"),
           gap_reason = replace(gap_reason, activity_type == "routine" &
                                  t1_coverage > 0 & t2_coverage == 0,
                                "(-) RI intro later than forecast"),
           gap_reason = replace(gap_reason, activity_type == "routine" &
                                  t1_coverage > 0 & t2_coverage > 0 &
                                  diff_coverage < 0 & diff_outcome < 0 ,
                                "(-) RI coverage lower than forecast"),
           gap_reason = replace(gap_reason, activity_type == "routine" &
                                  t1_coverage > 0 & t2_coverage > 0 &
                                  diff_coverage > 0 & diff_outcome > 0,
                                "(+) RI coverage higher than forecast")) %>%
    mutate(gap_reason = replace(gap_reason, activity_type == "campaign" &
                                  t1_fvps == 0 & t2_fvps > 0,
                                "(+) SIA happened which was not forecast"),
           gap_reason = replace(gap_reason, activity_type == "campaign" &
                                  t1_fvps > 0 & t2_fvps == 0,
                                "(-) SIA did not occur"),
           gap_reason = replace(gap_reason,
                                activity_type == "campaign" &
                                  t1_fvps > t2_fvps & t2_fvps > 0 &
                                  diff_outcome < 0,
                                "(-) SIA reached smaller population than forecast"),
           gap_reason = replace(gap_reason,
                                activity_type == "campaign" &
                                  t1_fvps < t2_fvps & t1_fvps > 0 &
                                  diff_outcome > 0,
                                "(+) SIA reached larger population than forecast")) %>%
    mutate(gap_reason = factor(gap_reason, levels = reason_levels))
  
  ## ROUTINE:
  
  ## for routine, the category "" contains cases where there is a
  ## difference in coverage, but the difference in outcome is in the
  ## wrong direction - meaning there must be a negative impact rate.
  
  ## These are clearly weirdnesses in the data - however, there don't
  ## seem to be any negative impact rates recorded. I wonder if that's
  ## to do with weird target populations?
  
  ## for these weird points, fvps seem to be linked closer to outcome
  ## than coverage, but I can't find any particular patterns with
  ## respect to country, year, disease.
  
  ## Just weird. Not sure what's going on in the data.
  
  ## CAMPAIGN:
  
  ## have just added a restriction to keep those instances where
  ## diff_fvps and diff_outcome go into opposite directions in the
  ## category "", similarly to the routine case. 
  
  
  return(dat)
}

################################################################################
#' Creates a title for the touchstone difference plot
#'
#' @param params the list of paramerers used to generate the plot
#'
#' @return The title for the plot as a string
title_diff_touchstones <- function(params) {
  
  ## checking if necessary parameters are supplied:
  if(is.null(params$year_first))
    stop("year_first is not supplied in 'title_diff_touchstones'.\n")
  if(is.null(params$year_last))
    stop("year_last is not supplied in 'title_diff_touchstones'.\n")
  
  my_title <-
    sprintf("Impact difference by vaccine delivery mode, %d-%d",
            params$year_first, params$year_last)
  
  return(my_title)
}