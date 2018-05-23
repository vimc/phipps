################################################################################
#' Performs a dplyr-style spread on several variables
#'
#' @param dat A dataframe to be converted from long to wide format.
#' @param key The variable according to which the data should be spread out.
#' @param varlist A character vector of variable names that need to be spread with respect to the key.
#' 
#' @return The dataframe in wide format. For each value of the
#'   variable 'key', there will be one column for each variable given
#'   in varlist. So if there are k distinct 'key' values and n
#'   variables to spread, n+1 columns in the original dataframe will
#'   be replaced with k*n columns.
#' @export
spread_many <- function(dat, key, varlist) {
  ## this seems to be quite slow.
  dat %>%
    gather(variable, value, varlist) %>%
    unite(varcomb, key, variable) %>%
    spread(varcomb, value)
}
