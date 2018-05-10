################################################################################
#' Creates a human readable label for the y axis of the graph_impact_top_countries
#' plot
#'
#' @param string The name of the outcome column in the full data set, usually
#'               "deaths_averted" or "cases_averted"
#' @param unit_label A string for the order of magnitude e.g. "thousands",
#'                   "millions" or "billions". usualy output from get_order_of_magnitude()
#'
#' @return The y label as a string
make_tidy_ylab <- function(string, unit_label = "") {
  
  string <- gsub("_", " ", string)
  string <- paste0("Future ", string)
  
  if (unit_label != "") string <- paste0(string, " in ", unit_label)
  
  return(string)
}

find.intros <- function(future=future201510gavi) {

  lg <- function(x,l) {c(NA[1:l], x[1:(length(x)-l)])}
  ld <- function(x,l) {c(x[1+l:(length(x)-l)],NA[(length(x)-l+1):length(x)])}


  future$intro<-future$intro_year<-NULL
  ## get lag coverage = if it was zero and current isn't then count it as intro

  future$year=as.numeric(future$year)
  future<-arrange(future,country,vaccine,year)

  future<-ddply(future, ~country+vaccine, here(transform), lag1.coverage  = lg(coverage ,1))


  ## taking full empirical approach, so cannot include 2011.
  ## get intro years for lags for 2012-2020
  future$iy=NA
  future$iy[future$coverage!=0&future$lag1.coverage==0&(!is.null(future$coverage)&!is.null(future$lag1.coverage))]=
    future$year[future$coverage!=0&future$lag1.coverage==0&(!is.null(future$coverage)&!is.null(future$lag1.coverage))]

  tmp<-future[!is.null(future$iy) ,]
  tmp<-tmp[c('country','vaccine','iy')]
  future<-merge(future,tmp,by=c('country','vaccine'),all.x=TRUE)


  future$intro_year[!is.null(future$iy.y)]=future$iy.y[!is.null(future$iy.y)]
  future$iy.x<-future$iy.y<-NULL

  ## mark intro year dummy
  future$intro=0
  future$intro[future$intro_year==future$year]=1

  ## does not apply to SIA
  future$intro_year[future$activity_type=="campaign"]=NA
  future$intro[future$activity_type=="campaign"]=0

  return(future)
}


cum_metric_by_year <- function(i,res,vaccine_vector){

  cum_metric <- cumsum(res[res$vaccine_delivery == vaccine_vector[i], ]$metric)

  return(cum_metric)
}

################################################################################
#' Filter a data frame (or equivalent) based on a set of parameters
#'
#' @param dat A data frame to be filtered
#' @param params A list of parameters for the to filter data against. Can 
#'               include 'country_set', 'vaccine_type', 'vaccine', 'country',
#'               'continent', 'region', 'metric', 'year_lo', 'year_hi',
#'               'support_type', 'is_focal', 'model', 'touchstone'
#'
#' @return The filtered data frame
filter_by_params <- function(dat, params) {
  # split campaign into intro
  if (!is.null(params$split_intro)) {
    if (params$split_intro) {
      intro_date <- which(dat$year_intro >= params$year_current)
      dat$activity_type[intro_date] <- "intro"
    }
  }  
  
  # combine Hib and HepB into PENTA
  if (!is.null(params$comb_penta)) {
    if (params$comb_penta) {
      hep_hib <- which(dat$vaccine == "HepB" | dat$vaccine == "Hib3")
      dat$vaccine[hep_hib] <- "Penta"
    }
  }
  
  # Filter by country set
  pine <- c("PAK", "IND", "NGA", "ETH")
  if (!is.null(params$country_set)) {
    if (params$country_set == "gavi73") {
      dat <- dat %>%
        filter(gavi73)
    } else if (params$country_set == "gavi69") {
      dat <- dat %>%
        filter(gavi73) %>%
        filter(!(country %in% pine))
    } else if (params$ountry_set == "pine") {
      dat <- dat %>%
        filter(params$country %in% pine)
    } else {
      stop(sprintf("country_set %s not implemented.\n", params$country_set))
    }
  }

  # filter by activity type / vaccine type
  if (!is.null(params$vaccine_type)) {
    dat <- dat %>% filter(activity_type %in% params$vaccine_type)
  }

  # filter by vaccine
  if (!is.null(params$vaccine)) {
    dat <- dat %>% filter(vaccine %in% params$vaccine)
  }
  # filter by country, continent or region
  # TODO there should be some logic here, indicating how this parameters
  # interact with one another
  if (!is.null(params$country)) {
    dat <- dat %>% filter(country %in% params$country)
  }
  if (!is.null(params$continent)) {
    dat <- dat %>% filter(continent %in% params$continent)
  }
  if (!is.null(params$region)) {
    dat <- dat %>% filter(region %in% params$region)
    }

  if (!is.null(params$metric)) {
    if (params$metric %in% c("deaths_averted", "future_deaths_averted")) {
      dat$metric <- dat$deaths_averted
    } else if (params$metric %in% c("cases_averted", "future_cases_averted")) {
      dat$metric <- dat$cases_averted
    } else {
      stop(sprintf("metric %s not implemented.\n", params$metric))
    }
  }

  # filter the years
  if (!is.null(params$year_first)) {
    dat <- dat %>% filter(year >= params$year_first)
  }
  if (!is.null(params$year_last)) {
    dat <- dat %>% filter(year <= params$year_last)
  }

  # filter by support type
  if (!is.null(params$support_type)) {
    dat <- dat %>% filter(support_type %in% params$support)
  }

  # filter by model
  if (!is.null(params$is_focal)) {
    dat <- dat %>% filter(params$is_focal)
  } else {
    if (!is.null(params$model)) {
      dat <- dat %>% filter(model %in% params$model)
    }
  }

  # filter by touchstone
  if (!is.null(params$touchstone)) {
    dat <- dat %>% filter(touchstone %in% params$touchstone)
  }

  if (nrow(dat) < 0)
    warning("There is no data for this set of parameters")
  
  return(dat)
}

################################################################################
#' Converts a list of parameters into a data frame of metadata
#'
#' @param params A list of parameters
#'
#' @return A data frame of parameters
build_meta_data <- function(params) {
  # intialise a data frame with 1 row and 0 columns
  meta <- data.frame(t(character(0)))
  for (par_nam in names(params)) {
    if (!is.null(params[[par_nam]])) {
      meta[, par_nam] <- params[[par_nam]]
    } else {
      meta[, par_nam] <- NA
    }
  }
  meta$dummy <- NULL
  return (meta)
}

################################################################################
#' Create the correct axis label based on the maximum value
#'
#' @param max_result The maximum value of the y axis
#' @param y_label The y axis label to be appended to
#'
#' @return A list contianing the numerical scale (1, 1e3 or 1e6), and the units
#' in short form as strings  ("", "K", "M") and y label with the long form of
#' the units appended
graph_num_div <- function(max_result, y_label) {
  # strip out underscores
  y_label_fixed <- gsub("_", " ", y_label)
  # make first character of each word upper case
  y_label_fixed <- gsub("(^|[[:space:]])([[:alpha:]])",
                        "\\1\\U\\2",
                        y_label_fixed, 
                        perl = TRUE)
  if (max_result <= 1e3) {
    num_div = 1
    num_scale = ""
    y_lab_scale = paste(y_label_fixed)}
  if (max_result > 1e3) {
    num_div = 1e3
    num_scale = "K"
    y_lab_scale = paste(y_label_fixed, "(thousands)")}
  if (max_result > 0.6e6) {
    num_div = 1e6
    num_scale = "M"
    y_lab_scale = paste(y_label_fixed, "(millions)")}
  return(list(numdiv = num_div, numscale = num_scale, ylabscale = y_lab_scale))
}

graph_totals <- function(res,num_div,num_scale,print_threshold) {

  if (is.null(print_threshold)) { print_threshold == 0.01 }

  res$metric_short=res$metric/num_div
  res$total=signif(res$total/num_div,3)
  res$numlab=paste0(signif(res$metric_short,2), num_scale)
  num_threshold <- sum(res$metric)*print_threshold
  res$numlab[res$metric < num_threshold]=""

  res$metric_top <- paste0(res$total, num_scale)
  max_res <- max(res$total)
  res$y <- res$total + ((max_res * 0.03)) # where to print the text on the y_scale
  return(res)
}

graph_labels <- function(res,label,y_label,print_threshold){

  res <- res[with(res, order(year)), ]
  year_vector<- unique(res$year)
  totals_cal <- function(i){

    total <- sum(res[res$year == year_vector[i], ]$metric)
    return(rep(total, length(unique(res[res$year == year_vector[i], ]$vaccine_delivery))))
  }

  b<-do.call(c, lapply(1:length(year_vector),totals_cal))

  res$total <- unlist(b)

  res$label <-label

  # Remove zeros
  res<- res[res$metric >0,]

  res$label=factor(res$label,levels=c(label))

  vector_a<-graph_num_div(res,y_label)

  num_div <- vector_a$numdiv
  num_scale <- vector_a$numscale
  y_lab_scale <- vector_a$ylabscale

  res$num_div <- vector_a$numdiv
  res$num_scale <- vector_a$numscale
  res$y_lab_scale <- vector_a$ylabscale

  res <- graph_totals(res,num_div,num_scale,print_threshold)

  res$y_lab_scale <- y_lab_scale

  return(res)
}


make_disease_colors <- function(){

  disease_vector <- c("HepB", "Hib3", "HPV", "JE", "MCV2", "MenA", "PCV3", "Rota", "Rubella", "YF")

  myColors <- brewer.pal(10, "Set3")
  names(myColors) <- paste0("[Rout] ",disease_vector)

  Hib3 <- myColors["[Rout] Hib3"]
  JE <- myColors["[Rout] JE"]

  myColors["[Rout] Hib3"] <- JE

  myColors["[Rout] JE"] <- Hib3

  darker <- col2rgb(myColors)/1.5

  darker_myColors <- rgb(t(darker), maxColorValue = 255)

  names(darker_myColors) <- paste0("[SIA] ",disease_vector)

  names(darker_myColors)[names(darker_myColors) == "[SIA] MCV2"] <- "[SIA] Measles"

  darker_myColors <- c(darker_myColors, darker_myColors["[SIA] Measles"])

  names(darker_myColors)[length(darker_myColors)] <- "[SIA] MR_Measles"

  darker_myColors["[SIA] MR_Measles"] <- rgb(t(col2rgb(darker_myColors["[SIA] MR_Measles"])-5),max=255)

  disease_colors <- c(myColors,darker_myColors)

  return(disease_colors)
}

read_data<- function(touchstone_left, touchstone_right){


  if (touchstone_right == "201510gavi"){
    d <- readRDS("summary.rds")
    data_object_A <- d
  }

  if (touchstone_left == "201510gavi_original"){
    p <- readRDS("201510-201510_summary.rds")
    p2 <- p
    p2$touchstone <- paste0(p$touchstone, "_original")
    data_object_B <- p2[,names(data_object_A)]
  }

  data_object<- rbind(data_object_A, data_object_B)

  return(list(data_object= data_object,
              touchstone_left= touchstone_left,
              touchstone_right= touchstone_right))
}

# changing label for gavi73 to gavi 73 where applicable

country_set_fun <- function(country_set){

  country_set_label <- country_set

  if(country_set == "gavi73"){
    country_set_label <- "73 gavi"
  }

  if(country_set == "gavi69"){
    country_set_label <- "all 73 gavi countries excluding India, Nigeria, Ethiopia, and Pakistan"
  }

  return(country_set_label)
}

################################################################################
#' Simple hacky function that converts longer country names to shorter
#' alternatives to make the plot look nicer
#'
#' @param name The name of the country as a string
#'
#' @return The shorter (where apllicable) name of the country as a string
shorten_name <- function(name) {
  if (is.na(name) || is.null(name))
    return(NA)
  
  if (name == "Congo, the Democratic Republic of the")
    return("DR Congo")
  if (name == "Bolivia, Plurinational State of")
    return("Bolivia")
  if (name == "Micronesia, Federated States of")
    return("Micronesia")
  if (name == "Lao People's Democratic Republic")
    return("Laos")
  if (name == "Korea, Democratic People's Republic of")
    return("DPR Korea")
  if (name == "Tanzania, United Republic of")
    return("Tanzania")
  
  return(name)
}

# selects long label for figure caption for touchstone depending whether it is a
# future projection or comparision with the past

touchstone_long_label_select <- function(names,
                                         year_first,
                                         year_last,
                                         year_current,
                                         touchstone) {

  if(year_current <= year_first) {
    touchstone_long <-
      names$long_name_future[names$touchstone_name == touchstone]
  } else if(year_current <= year_last) {
    touchstone_long <-
      names$long_name_combined[names$touchstone_name == touchstone]
  } else if(year_current > year_last) {
    touchstone_long <-
      names$long_name_past[names$touchstone_name == touchstone]
  }

  return(touchstone_long)
}


# selects short label for figure title for touchstone depending whether it is a
# future projection or comparision with the past

touchstone_short_label_select <- function(year_first,year_last,year_current,touchstone){

  names<- read.csv("csv/names_equivalent.csv")

  if(year_first >= year_current){
    touchstone_short  <- names$title_name_future[names$touchstone_name == touchstone]
  }

  if(year_last < year_current){
    touchstone_short  <- names$title_name_past[names$touchstone_name == touchstone]
  }

  return(touchstone_short)
}

country_name_function <- function(country){
  sd <- read.csv("csv/gavi_73.csv")
  country_name <- sd[sd$country == country,]$country_name
  return(country_name)
}

model_type_function <- function(focal_model){
  model_type <- ""
  if (focal_model == TRUE)   model_type <- " Estimates are based on focal models."
  if (focal_model == FALSE)  model_type <- " Estimates are based on non-focal models."
  return(model_type)
}


