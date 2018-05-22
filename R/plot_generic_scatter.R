plot_generic_scatter <- function(dat, params, compare, disciminent) {
  dat <- filter_by_params(dat, params)
  
  dat$outcome <- dat[, params$metric]
  dat$compare <- dat[, compare]
  dat$disc    <- dat[, disciminent]
  
  if (length(unique(dat[, compare])) != 2) {
    stop("you are trying to compare more that one")
  }
  
  d <- prep_scatter(dat, params, compare, disciminent)
  
  max_scale <- max(max(d[, 2], d[, 3]))

  p <- ggplot(data = d, aes(x=d[, 2], y=d[, 3])) +
    geom_point(aes(colour = factor(disc)), size = 4, alpha = 0.5) + 
    xlab("deaths averted touchstone 1") +
    ylab("deaths averted touchstone 2") +
    theme_bw() + 
    geom_abline(slope = 1, alpha = 0.25) + 
    xlim(0, max_scale) +
    ylim(0, max_scale)
  
  return(list(p = p, d = d))
}

prep_scatter <- function(dat, params, compare, disciminent) {
  # assume params had been checked
  dat <- filter_by_params(dat, params)
  # we need to get rid some columns so that the tidyr gather->unite->spread
  # paradigm works later
  vars_to_keep <- c("touchstone", "is_focal", "disease", "vaccine",
                    "vaccine_delivery", "activity_type", "support_type",
                    "country", "country_name", "gavi73", "year", "deaths_averted",
                    "deaths_averted_rate", "cases_averted_rate", "cases_averted",
                    "fvps", "year_intro", "coverage", "outcome", "disc")
  
  compares <- unique(dat[, compare])
  dat$outcome <- dat[, params$metric]
  dat$disc    <- dat[, disciminent]
  
  compare_rename <- c(compare_1 = compares[1], compare_2 = compares[2])
  
  dat <- dat %>%
    select(vars_to_keep) %>%
    filter(is_focal)
  
  # if no compare value has been supplied fill a dummy columnd with 1s
  if (!is.null(compare)) {
    dat$compare <- dat[, compare]
  } else {
    dat$compare <- rep(1, nrow(dat))
  }
  
  dat <- dat %>%
    group_by(compare, disc) %>%
    summarize(outcome = sum(outcome, na.rm = TRUE)) %>%
    arrange(desc(outcome))
  
  
  dat <- dat %>% gather(variable, value, outcome) %>% 
    unite(var_comb, compare, variable) %>%
    spread(var_comb, value)
  
  #remove '_outcome' from the end of each name
  names(dat) <- sapply(names(dat), function(x) {gsub("_outcome", "", x)})
  
  return(dat)
}