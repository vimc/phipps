################################################################################
#' Filters and does some processing of the data set before plotting
#' EXPORTED FUNCTION
#' Must be called BEFORE graph_impact_top_countries
#'
#' @param dat A dataframe (or similar) of country data for ...
#' @param params A list (TODO consider using an S3 class) of parameters or the
#'               plot
#'
#' @return A list containing filtered data, and information about units
#'         dat, will be a data frame with columns country_name, vaccine_delivery
#'         and outcome
#' @export
prep_impact_top_countries <- function(dat, params) {
  ## setting up defaults for params that are not supplied:
  if (is.null(params$touchstone)) {
    stop("no Touchstone supplied")
  }

  if (is.null(params$year_first)) {
    params$year_first <- -Inf
    warning("year_first parameter was not supplied, using -Inf instead")
    # TODO Do we want to set this -Inf, or should we set it to min(dat$year)?
  }
  if (is.null(params$year_last)) {
    params$year_last <- Inf
    warning("year_last parameter was not supplied, using Inf instead")
    # TODO Do we want to set this -Inf, or should we set it to max(dat$year)?
  }
  if (is.null(params$countries)) {
    if (is.null(params$country_set)) {
      params$countries <- unique(dat$country)
      warning("countries and country_set parameters were not supplied, all counties will be used")
    } else if (params$country_set == "gavi73") {
      params$countries <- unique(dat$country[dat$gavi73])
    } else {
      stop(
        sprintf("country set '%s' not implemented in 'prep_impact_top_countries'",
                params$country_set))
    }
  }
  # TODO should we warn user that countries parameter overrides, country_set parameter?
  # Is this the intended behaviour?

  ## checking that all necessary params without sensible defaults are supplied:
  if (is.null(params$outcome))
    stop("required input 'params$outcome' is missing.\n")

  ## At this point the revelent parameters are present
  ## Shorten the country names
  dat$country_name = sapply(dat$country_name, shorten_name, USE.NAMES=FALSE)
  ## main filtering:
  dat <- filter_by_params(dat, params)

  if (nrow(dat) == 0)
    stop("no data with specified parameters")

  ## selecting the outcome variable
  dat$outcome <- dat[[params$outcome]]

  ## finding the order of countries (descending burden)
  top_countries <- dat %>%
    group_by(country_name) %>%
    summarize(outcome = sum(outcome, na.rm = TRUE)) %>%
    arrange(desc(outcome))
  unit <- get_order_of_magnitude(top_countries$outcome[1])

  country_name_ordered <- top_countries %>%
    select(country_name) %>%
    unlist()

  ## aggregating by country and vaccine_delivery:
  dat <- dat %>%
    group_by(country_name, vaccine_delivery) %>%
    summarize(outcome = sum(outcome, na.rm = TRUE))
  ## converting country to factor in order to guarantee ordering:
  dat$country_name <-
    factor(dat$country_name, levels = country_name_ordered)
  dat$vaccine_delivery <-
    factor(dat$vaccine_delivery)

  ## adding the "total" level to "vaccine_delivery" prior to merging
  ## with top_counties:
  levels(dat$vaccine_delivery) <- c(levels(dat$vaccine_delivery), "total")

  top_countries$country_name <-
    factor(top_countries$country_name, levels = country_name_ordered)
  top_countries$vaccine_delivery <-
    factor("total", levels = levels(dat$vaccine_delivery))

  dat <- dplyr::union(dat, top_countries) %>%
    arrange(country_name, vaccine_delivery)

  ## dat is the long form of the dataset, for plotting.
  ## for the plot itself, it needs to be filtered for vaccine_delivery != "total"
  ## for the labels over the bars, only keep vaccine_delivery == "total".
  return(list(dat = dat, unit = unit$unit, unit_label = unit$unit_label))
}

################################################################################
#' Plots the top country impact graph
#' EXPORTED FUNCTION
#' Must be called AFTER prep_impact_top_countries
#'
#' @param dat A list containing teh filtered data set, and some information
#' about the units. This is the output from prep_impact_top_countries
#' @param params A list (TODO consider using an S3 class) of parameters or the
#'               plot. (TODO does this list have to be the same as the params
#'               for prep_impact_top_countries? If so consider adding params
#'               to dat)
#' @param print_threshold A numerical value that determines how when labels are
#'                        added to the plot. DEFAULT = NA, which sets the
#'                        threshold in the body of the function based on data
#'
#' @return A ggplot2 plot
#' @export
graph_impact_top_countries <- function(dat, params, print_threshold = NA) {

  if (is.null(params$year_first))
    params$year_first <- -Inf
  if (is.null(params$year_last)) params$year_last <- Inf
  if (is.null(params$n_countries)) {
    params$n_countries <- length(unique(dat$dat$country_name))
    warning("n_countries parameter was not supplied, using all countries instead")
  }

  ## setting default precision and location for labels:
  top_label_shift <- max(dat$dat$outcome)/(50 * dat$unit)
  if (is.na(print_threshold)) {
    print_threshold <- signif(top_label_shift, 1)
  }

  dat$dat <- dat$dat %>%
    filter(unclass(country_name) <= params$n_countries) %>%
    mutate(outcome = outcome / dat$unit) %>%
    mutate(numlab = signif(outcome, 2)) ## sprintf("%0.2f", outcome))
  ## precision of the numlab figures could be made into a parameter,
  ## or determined in a cleverer functional form depending on the
  ## largest value.

  ## generating the country-totals to plot above the bars:
  top <- dat$dat %>%
    filter(vaccine_delivery == "total") ## %>%

  ## restricting to the countries to be plotted:
  dat_bars <- dat$dat %>%
    filter(vaccine_delivery != "total")

  dat_bars$numlab[dat_bars$outcome < print_threshold]  <- ""

  my_cols <- get_palette("vaccine_delivery", NULL)
  my_title <- title_impact_top_countries(params)

  ## the plotting:
  ggplot(dat_bars, aes(x = country_name,
                       y = outcome,
                       fill = vaccine_delivery)) +
    geom_bar(stat = "identity", color = "white") +
    scale_fill_manual(values = my_cols) +
    xlab("") +
    ylab(make_tidy_ylab(params$outcome, dat$unit_label)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1),
          legend.title = element_blank()) +
    annotate("text", x = top$country_name, y = top$outcome + top_label_shift,
             label = top$numlab, colour = "black", size = 3) +
    geom_text(aes(x = country_name, label = numlab),
              colour = "white", size = 2,
              position = position_stack(vjust = 0.5)) +
    ggtitle(my_title)

}
################################################################################
#' Creates the title for the plot based on the parameters given
#'
#' @param params A list (TODO consider using an S3 class) of parameters or the
#'               plot.
#'
#' @return The title as an R string
title_impact_top_countries <- function(params) {
  ## setting up defaults for params that are not supplied:
  if (is.null(params$year_first))
    params$year_first <- -Inf
  if (is.null(params$year_last))
    params$year_last <- Inf

  if (is.null(params$n_countries)) {
    n_country_string <- ""
  } else {
    n_country_string <- sprintf(" in top %d countries", params$n_countries)
  }

  title <- paste0("Vaccine impact", n_country_string, ", ",
                  params$year_first, " - ", params$year_last)

  return(title)
}
################################################################################
#' Creates a simplified table mirroring the data from the top country impact
#' graph
#' EXPORTED
#'
#' @param dat A list containing the filtered data set, and some information
#' about the units. This is the output from prep_impact_top_countries
#' @param params A list (TODO consider using an S3 class) of parameters or the
#'               plot. (TODO does this list have to be the same as the params
#'               for prep_impact_top_countries? If so consider adding params
#'               to dat)
#'
#' @return A list of the wide table and a smaller printable table
#' @export
table_impact_top_countries <- function(dat, params) {

  if(is.null(params$n_countries))
    params$n_countries <- length(unique(dat$country))

  if(is.null(params$outcome))
    stop("required input 'params$outcome' is missing.\n")


  ## generating the wide form of the dataset for output:
  ## Would this be better off in a separate function?
  dat_wide <- dat$dat %>%
    mutate(outcome = round(outcome)) %>%
    spread(key = vaccine_delivery, value = outcome)
  dat_wide <- dat_wide %>%
    mutate(percent = 100 * total / sum(dat_wide$total)) %>%
    ungroup() %>%
    mutate(cum_percent = cumsum(percent))

  outcome_name <- gsub("_", " ", params$outcome)
  substr(outcome_name, 1, 1) <- toupper(substr(outcome_name, 1,1))

  dat_print <- dat_wide %>%
    slice(1:params$n_countries) %>%
    select(country_name, total, percent, cum_percent) %>%
    mutate(total = format(total, big.mark = ","),
           percent = sprintf("%0.1f", percent),
           cum_percent = sprintf("%0.1f", cum_percent)) %>%
    rename(Country := country_name,
           !!outcome_name := total,
           Percent := percent,
           'Cumulative percent' := cum_percent)

  return(list(dat_out = dat_wide, dat_print = dat_print))
}
################################################################################
#' Given a number in (0,1e12) calculates the order of magnitude in powers of 1e3
#'
#' @param x A numerical value strictly between 0 and 1e12
#'          TODO do we want to catch values outside this range, they should never
#'          occur and if they do it implies there is an error elsewhere in the code
#'
#' @return A list of the wide table and a smaller printable table
get_order_of_magnitude <- function(x) {
  magnitude <- floor(log10(x)/3) + 1
  unit_label = c("", "thousands", "millions", "billions")
  return(list(unit = 10^(3*(magnitude - 1)),
              unit_label = unit_label[magnitude]))
}





