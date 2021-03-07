#' Oldest Player in a Year
#'
#' This function shows the name of the oldest player in the year selected. In case of multiple players, the first name in alphabetical order will be selected.
#' @param year year selected. Must be between 1950 and 2017 (included).
#' @keywords oldest, player
#' @export
#' @examples
#' oldestPlayer(2015)
#' "Andre Miller"

oldestPlayer <- function(year){

  library(tidyverse, "%>%")

  if(!(dplyr::between(year, 1950, 2017))){
    stop("Year must be between 1950 and 2017 included.", call.=F)
  }
  oldest <- nba_stats %>%
    dplyr::filter(Year == year) %>%
    dplyr::arrange(desc(Age), Player) %>% #selects by alph. order in case of ties
    dplyr::select(Player) %>%
    utils::head(1) %>%
    dplyr::pull(Player)

  return(oldest)
}
