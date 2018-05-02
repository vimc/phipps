#=====================================================
#
#                PLOT FUNCTION
#               tracking targets
#         (plot with two line graphs)
#
#=====================================================
################################################################################
#' Filters and does some processing of the data set before plotting
#' EXPORTED FUNCTION
#' Must be called BEFORE graph_impact_top_countries
#'
#' @param dat_all A dataframe (or similar) of country data to be plotted
#' @param params A list (TODO consider using an S3 class) of parameters for the
#'               plot
#'
#' @return A list containing the ggplot2 plot object, a data frame contaiing the
#'         data used in the plot and some metadata
#' @export
graph_tracking_targets <- function(dat, params) {
  if (is.null(params$label_blue)) {
    params$label_blue <- params$touchstone_blue
  }

  if (is.null(params$label_red)) {
    params$label_red <- params$touchstone_red
  }

  if (is.null(params$label_red_dotted)) {
    params$label_red_dotted <- params$touchstone_red_dotted
  }

  # filter data based on the parameters
  dat <- filter_by_params(dat, params)
  # set up the y axis labels
  if (params$metric %in% c("deaths_averted", "future_deaths_averted")) {
    y_label  <- "Future Deaths Averted"
  } else if (params$outcome %in% c("cases_averted", "future_cases_averted")) {
    y_label <- "Future Cases Averted"
  }

  # filter the data into touchstones by date
  aggregated_data <- aggregate_data(dat, params, y_label)
  # the aggregated data set
  res <- aggregated_data$res
  # The Y axis label
  y_lab_scale <- aggregated_data$unit_list$ylabscale
  # The y intercept values
  int_lines <- aggregated_data$int_lines

  ## Create a custom color scale and line types
  myColors          <- c("#bd0026","#377eb8", "#dd1c77")
  names(myColors)   <- c(params$label_red, params$label_blue, params$label_red_dotted)
  colScale          <- scale_colour_manual(name = "label", values = myColors)
  v_linetype        <- (c("solid", "solid", "dotted"))
  names(v_linetype) <- c(params$label_red, params$label_blue, params$label_red_dotted)
  country_set_label <- country_set_fun(params$country_set)

  # Picking the title
  plot_title <- set_tracking_title(params)

  #========== The_plot
  the_plot <- ggplot(res, aes(x = year, y = metric_short, colour = label,
                              linetype = label)) +
    geom_path(size = 2, lineend = "round") +
    geom_vline(xintercept = params$year_current, linetype = "longdash", alpha = .2) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    ylab(y_lab_scale) +
    xlab("") +
    scale_x_continuous(limits = c(params$year_first, params$year_last),
                       breaks = c(params$year_first, 2014, params$year_current, 2018, params$year_last),
                       labels = c(params$year_first, "2014",
                                  paste(params$year_current, "\n (Current Data Extent)"),
                                  "2018", params$year_last)) +
    theme(legend.justification = c(1, 0), legend.position = c(.98, .05)) +
    theme(legend.key = element_blank(), axis.text = element_text(size = 10),
          legend.text = element_text(size = 10)) +
    labs(colour = "") +
    colScale +
    scale_linetype_manual(values = v_linetype) +
    geom_hline(yintercept = int_lines$red_bottom, colour = "red1", alpha = .4, size = .5) +
    geom_hline(yintercept = int_lines$blue_bottom, colour = "dodgerblue2", alpha = .4, size = .5) +
    geom_hline(yintercept = int_lines$red_top, colour = "red1", linetype = 'longdash',
               alpha = .4, size = .5) +
    geom_hline(yintercept = int_lines$blue_top, colour = "dodgerblue2",
               linetype = "longdash", alpha = .4, size = .5) +
    theme(legend.title = element_blank())  +
    ggtitle(plot_title)

  ### ========= Graph_data_file
  data_graph <- res[, c("year", "metric", "label")]
  data_graph$metric  <- round(data_graph$metric)
  data_graph  <- tidyr::spread(data_graph, key = label, value = metric)
  ## names(data_graph)<- c("year", metric, "source")
  names(data_graph) <- c("year", "Projection", "Actual", "Projection_update")

  ## ========= Graph_meta_data_file
  meta <- build_meta_data(params)
  meta_t <- (t(meta))
  meta_data <- data.frame(row.names(meta_t), meta_t)
  names(meta_data) <- c("item", "detail")

  return(list(graph = the_plot, graph_data = data_graph, meta_data = meta_data))
}


#=================================================================
#
#                     CAPTION FUNCTION
#
# ================================================================
caption_function_tracking_targets <- function(params, names){
  touchstone_blue       <-  params$touchstone_blue
  touchstone_red        <-  params$touchstone_red
  touchstone_red_dotted <-  params$touchstone_red_dotted
  label_blue            <-  params$label_blue
  label_red             <-  params$label_red
  label_red_dotted      <-  params$label_red_dotted
  year_first            <-  params$year_first
  year_last             <-  params$year_last
  year_current          <-  params$year_current
  metric                <-  params$outcome
  focal_model           <-  params$focal_model
  vaccine_type          <-  params$vaccine_type
  vaccine               <-  params$vaccine
  support_type          <-  params$support_type
  country_set           <-  params$country_set
  continent             <-  params$continent
  country               <-  params$country
  region                <-  params$region
  title                 <-  params$title

  METRIC <- gsub("_", " ", metric)

  touchstone_blue_long       <-
    names$long_name_combined[names$touchstone_name == touchstone_blue]
  touchstone_red_long        <-
    names$long_name_past[names$touchstone_name == touchstone_red]
  touchstone_red_dotted_long <-
    names$long_name_future[names$touchstone_name == touchstone_red_dotted]

  model_type <- ""
  if (focal_model == TRUE)   model_type <- ", based on focal models"
  if (focal_model == FALSE)  model_type <- ", based on non-focal models"

  country_set_label <- country_set_fun(country_set)

  figure_caption <-
    paste0("Comparison of cumulative vaccine impact as ", METRIC, " in ",
           country_set_label, " countries, between ",
           year_first, "-", year_last, model_type,
           ". The blue line uses ", touchstone_blue_long,
           ", the solid red line uses ",  touchstone_red_long,
           ", and the dotted red line uses ", touchstone_red_dotted_long, ".")

  if (!is.null(continent))
    figure_caption <-
      paste0("Comparison of cumulative vaccine impact as ", METRIC,
             " in ", continent, " continent (",
             country_set_label, " countries), between ",
             year_first, "-", year_last, model_type,
             ". The blue line is from ", touchstone_blue_long,
             ", the solid red line is from ",  touchstone_red_long,
             ", and the dotted red line is from ",
             touchstone_red_dotted_long, ".")

  if (!is.null(region))
    figure_caption <-
      paste0("Comparison of cumulative vaccine impact as ", METRIC,
             " in ", region, " region (",
             country_set_label, " countries), between ",
             year_first, "-", year_last, model_type,
             ". The blue line is from ", touchstone_blue_long,
             ", the solid red line is from ",  touchstone_red_long,
             ", and the dotted red line is from ",
             touchstone_red_dotted_long, ".")

  if (!is.null(country))
    figure_caption <-
      paste0("Comparison of cumulative vaccine impact as ", METRIC,
             " in ", country, ", between ",
             year_first, "-", year_last, model_type,
             ". The blue line is from ", touchstone_blue_long,
             ", the solid red line is from ",  touchstone_red_long,
             ", and the dotted red line is from ",
             touchstone_red_dotted_long, ".")
  return(figure_caption)
}

###############################################################################
#' Creates the plot title from the parameter set
#'
#' @param params The parameter set
#'
#' @return The plot tile
set_tracking_title <- function(params) {
  plot_title <- "Default Title" # This will be used if params does not contain
  # a title or a continent or a country
  if (!is.null(params$title))
    plot_title <- paste0("Cumulative Impact of Vaccination, ",
                         params$year_first , "-", params$year_last)
  if (!is.null(params$continent))
    plot_title <- paste0("Cumulative Impact of Vaccination in ",
                         params$continent, " continent ", params$year_first , "-", params$year_last)
  if (!is.null(params$country))
    plot_title <- paste0("Cumulative Impact of Vaccination in ", params$country, ", ",
                         params$year_first , "-", params$year_last)

  return(plot_title)
}


###############################################################################
#' Filters the data set based on the touchstone and date parameters
#'
#' @param dat The data set as a data frame
#' @param params The parameter set
#'
#' @return A list of three data frames for the projection, actual and future data
#'
filter_by_date <- function(dat, params) {
  # filter the data into touchstones by date
  projection <- dat %>%
    filter(touchstone == params$touchstone_blue)
  actual <- dat %>%
    filter(touchstone == params$touchstone_red & year <= params$year_current)
  future <- dat %>%
    filter(touchstone == params$touchstone_red_dotted & year > params$year_current)

  return(list(projection = projection, actual = actual, future = future))
}

###############################################################################
#' Filters the data set based on the touchstone and date parameters
#'
#' @param dat The data set as a data frame
#' @param params The parameter set
#'
#' @return A list of three data frames for the projection, actual and future data
#'
aggregate_data <- function(dat, params, y_label) {
  filtered_data <- filter_by_date(dat, params)
  ## combine actual and future data
  df = rbind(filtered_data$actual, filtered_data$future)
  ## aggregate
  res1 <- aggregate(metric ~ year, filtered_data$projection, sum)
  res2 <- aggregate(metric ~ year, df, sum)

  res1$metric <- cumsum(res1$metric)
  res2$metric <- cumsum(res2$metric)
  res1$label  <- params$label_blue
  res2$label  <- params$label_red

  res <- rbind(res2, res1)

  # Scales
  res$total <- res$metric
  unit_list <- graph_num_div(max(res$total), y_label)

  res$metric_short <- res$metric / unit_list$numdiv

  res.t <- res[res$label == params$label_red & res$year >= params$year_current, ]
  res <- res[!(res$label == params$label_red & res$year >  params$year_current), ]
  res.t$label <- params$label_red_dotted

  # Intercept lines
  red_bottom  <- sum(df$metric[df$year <= params$year_current], na.rm = TRUE) / unit_list$numdiv
  blue_bottom <- sum(filtered_data$projection$metric[filtered_data$projection$year <= params$year_current],
                     na.rm = TRUE) / unit_list$numdiv
  blue_top <- sum(filtered_data$projection$metric, na.rm = TRUE) / unit_list$numdiv
  red_top  <- sum(df$metric, na.rm = TRUE) / unit_list$numdiv
  int_lines <- list(red_bottom = red_bottom, blue_bottom = blue_bottom,
                    blue_top = blue_top, red_top = red_top)

  res <- rbind(res, res.t)
  res$label <- factor(res$label,
                      levels = c(params$label_blue, params$label_red, params$label_red_dotted),
                      labels = c(params$label_blue, params$label_red, params$label_red_dotted))

  return(list(res = res, int_lines = int_lines, unit_list = unit_list))
}
