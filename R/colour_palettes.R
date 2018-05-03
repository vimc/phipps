################################################################################
#' Creates a vector of colours for graph_impact_top_countries
#'
#' @return A vector of colours
make_disease_colours <- function() {
  
  ## this is just for routine - need to get that from Montagu.
  disease_vector <- c("HepB",
                      "Hib3",
                      "HPV",
                      "JE",
                      "MCV2",
                      "MenA",
                      "PCV3",
                      "Rota",
                      "Rubella",
                      "YF")
  
  my_colours <- brewer.pal(10, "Set3")
  names(my_colours) <- paste0("[Rout] ", disease_vector)
  
  ## switching some colours:
  Hib3 <- my_colours["[Rout] Hib3"]
  JE   <- my_colours["[Rout] JE"]
  my_colours["[Rout] Hib3"] <- JE
  my_colours["[Rout] JE"]   <- Hib3
  
  
  ## generating the darker colours via hsv colour space:
  colour_matrix <- rgb2hsv(col2rgb(my_colours))
  colour_matrix["s", ] <- colour_matrix["s", ] + 0.5
  colour_matrix["v", ] <- colour_matrix["v", ] - 0.25
  
  colour_matrix[colour_matrix > 1] = 1
  colour_matrix[colour_matrix < 0] = 0
  
  darker_my_colours <- hsv(h = colour_matrix[1, ],
                           s = colour_matrix[2,],
                           v = colour_matrix[3, ])
  
  ## generating the darker colours via rgb colour space:
  ## darker <- col2rgb(my_colours)/1.5
  ## darker_my_colours <- rgb(t(darker), maxColorValue = 255)
  
  names(darker_my_colours) <- paste0("[SIA] ", disease_vector)
  
  names(darker_my_colours)[names(darker_my_colours) == "[SIA] MCV2"] <-
    "[SIA] Measles"
  
  darker_my_colours <- c(darker_my_colours, darker_my_colours["[SIA] Measles"])
  
  names(darker_my_colours)[length(darker_my_colours)] <- "[SIA] MR_Measles"
  
  darker_my_colours["[SIA] MR_Measles"] <-
    rgb(t(col2rgb(darker_my_colours["[SIA] MR_Measles"])-5), max = 255)
  
  disease_colours <- c(my_colours, darker_my_colours)
  
  return(disease_colours)
}

palette_vaccine <- function() {
  c("[SIA] Rubella"    = "#762a83",
    "[SIA] MR_Measles" = "#9970ab",
    "[SIA] MenA"       = "#5aae61",
    "[SIA] Measles"    = "#1b7837",
    "[SIA] JE"         = "#00441b",
    "[SIA] YF"         = "#40004b",
    "[Rout] YF"        = "#67001f",
    "[Rout] Rubella"   = "#d73027",
    "[Rout] Rota"      = "#b2182b",
    "[Rout] PCV3"      = "#d6604d",
    "[Rout] MenA"      = "#f4a582",
    "[Rout] MCV2"      = "#fddbc7",
    "[Rout] JE"        = "#92c5de",
    "[Rout] HPV"       = "#4393c3",
    "[Rout] Hib3"      = "#2166ac",
    "[Rout] HepB"      = "#053061")
}

################################################################################
#' Given a container of values (upto 20) returns a list of perceptually 
#' distinct colours, one for each value. This depends on the set of values
#' passed in so the same value in general will not be assigned the same value 
#' every time. This is intended as fall back when we do not have an aggreed 
#' predefined palette.
#' 
#' If more than 20 values are supplied, we add 'randomly' generated colours
#' 
#' TODO Do we need yo worry about being colour-blind friendly?
#' 
#' @param values A list fo values to be assigned colours
#'
#' @return A list of colours
generic_palette <- function(values) {
  good_cols <- c("#e6194b",        # red
                 "#3cb44b",        # green
                 "#ffe119",        # yellow
                 "#0082c8",        # blue
                 "#f58231",        # orange
                 "#911eb4",        # purple
                 "#46f0f0",        # cyan
                 "#f032e6",        # magneta
                 "#d2f53c",        # lime
                 "#fabebe",        # pink
                 "#008080",        # teal
                 "#e6beff",        # lavender
                 "#aa6e28",        # brown
                 "#fffac8",        # beige
                 "#800000",        # maroon
                 "#aaffc3",        # mint
                 "#808000",        # olive
                 "#ffd8b1",        # coral
                 "#000080",        # navy
                 "#808080"        # gray
  )
  
  # we don't have enough colours so generate some more
  if (length(values) > length(good_cols)) {
    len_diff <- length(values) - length(good_cols)
    good_cols <- c(good_cols, 
                   rgb(runif(len_diff), runif(len_diff), runif(len_diff)))
  }
  # we have too many colours
  if (length(values) < length(good_cols)) { 
    good_cols <- good_cols[1:length(values)]
  }
  
  setNames(good_cols, values)
}
