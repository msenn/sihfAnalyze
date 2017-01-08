# purrr version of dataManipulation

load("gamesDetails.RData")

# Basic nested df
#' Make Games Details Nested Data Frame
#'
#' @param gamesDetails.list As returned from SIHF API
#'
#' @return \code{tibble}; gameId, details (list with all details on respective game)
#' @export
#'
#' @examples
makeGamesDetails.df <- function(gamesDetails.list) {
  gamesDetails.list %>% 
    setNames(purrr::map_chr(., "gameId")) %>% 
    tibble::enframe("gameId", "gameDetails")
}




#' Filter Comlpete Games
#'
#' @param gamesDetails \code{list}; as returned by \code{fetchGamesDetails()}
#'
#' @return \code{list}; containing games with status "Ende" only
#' @export
filterGamesComplete <- function(gamesDetails.df)  {
  gamesDetails.df %>% 
    dplyr::filter(
      fullDetails %>% 
        purrr::map_chr(c("status", "name")) == "Ende"
    )
}


# Filter EHCB in Teams
dplyr::filter(
  fullDetails %>% 
    purrr::map
)




#' Get All Teams
#' 
#' @param gamesDetails.df See \code{makeGamesDetails.df}
#'
#' @return \code{tibble}; id, name, and acronym for all teams in \code{gamesDetails.df}
#' @export
allTeams <- function(gamesDetails.df) {
  gamesDetails.df %>% 
    dplyr::mutate(
      homeTeam = gameDetails %>% purrr::map(c("details", "homeTeam")),
      awayTeam = gameDetails %>% purrr::map(c("details", "awayTeam"))) %>% 
    tidyr::gather("key", "value", homeTeam, awayTeam) %>% 
    dplyr::mutate(
      value = value %>% purrr::map(`[`, c("id", "name", "acronym")),
      value = value %>% purrr::map(tibble::as_tibble)) %>% 
    dplyr::select(value) %>% 
    tidyr::unnest() %>% 
    dplyr::distinct()
}
