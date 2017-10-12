##' Apply a palette to a set of levels
##' @title Apply a palette to a set of levels
##' @param x A character or factor vector
##' @param pal A palette to use (named character vector of colours)
##' @param missing_error Logical indicating if missing values should throw errors
##' @param named Logical indicating if the output should share names with \code{x}
##' @export
palette_apply <- function(x, pal, missing_error = TRUE, named = TRUE) {
  nms <- if (named) names(x) else NULL
  x <- as.character(x)
  ret <- pal[x]
  msg <- is.na(ret) & !is.na(x)
  if (any(msg) && missing_error){
    stop("Unknown entries in 'x': ",
         paste(unique(x[msg]), collapse = ", "))
  }
  names(ret) <- if (named) names(x) else NULL
  ret
}

##' @export
##' @rdname palette_apply
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
