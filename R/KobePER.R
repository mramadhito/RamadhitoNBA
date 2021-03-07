#' Kobe Bryant's Player Efficiency Rate in Year
#'
#' This function displays the Player Efficiency Rate (PER) of Kobe Bryant in the year selected.
#' @param year year selected. Must be between 1997 and 2016 (included).
#' @keywords PER, Kobe, Bryant
#' @export
#' @examples
#' KobePER(2015)
#' 17.6

KobePER <- function(year){

  library(tidyverse, "%>%")

  if(!(dplyr::between(year, 1997, 2016))){
    stop("Year must be between 1997 and 2016 included.", call.=F)
  }

  dataPoint <- nba_stats %>%
    dplyr::filter(Year == year, Player == "Kobe Bryant") %>%
    dplyr::select(Player, Year, Tm, PER) %>%
    utils::head(10) %>%
    dplyr::arrange(desc(PER)) %>%
    dplyr::pull(PER)

  return(dataPoint)
}
