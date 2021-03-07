#' Correlation Matrix of Numeric Variables in a Year
#'
#' This function returns the correlation matrix of all numeric variables in a selected year. Only complete pairs of observations are considered.
#' @param year year selected. Must be between 1950 and 2017 (included).
#' @keywords correlation, matrix, numeric
#' @export
#' @examples
#' corrMatrix(2015)
#'                Age           G          GS          MP         PER         TS.
#' Age    1.000000000  0.12608101  0.02921544  0.08427518  0.08438978  0.11508346
#' ...

corrMatrix <- function(year){

  library(tidyverse, "%>%")

  if(!(dplyr::between(year, 1950, 2017))){
    stop("Year must be between 1950 and 2017 included.", call.=F)
  }

  numeric_df <- nba_stats %>%
    dplyr::filter(Year == year) %>%
    purrr::keep(., is.numeric) %>%
    dplyr::select(-Year) #remove Year as Year is redundant

  M1 <- stats::cor(numeric_df, use="pairwise.complete.obs")

  return(M1)
}
