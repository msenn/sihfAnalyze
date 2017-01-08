# Data manipulation

#' Get Games Summaries (Single Round)
#' 
#' Extract games summaries from round details (see \code{?fetchRoundDetails}) of a single round (e.g., \code{roundDetails[[1]]})
#'
#' @param singleRoundDetails \code{list}; element of return from \code{fetchRoundDetails()}.
#'
#' @return \code{data.frame}
getGamesSummariesSingleRound <- function(singleRoundDetails) {
  games <- lapply(
    singleRoundDetails$data,
    # Over games
    function(x) {
      list(
        gameId = x[[10]]$gameId[[1]],
        date = x[[2]][[1]],
        time = x[[3]][[1]],
        homeTeamId = x[[4]]$id[[1]],
        homeTeamName = x[[4]]$name[[1]],
        awayTeamId = x[[5]]$id[[1]],
        awayTeamName = x[[5]]$name[[1]]
      )
    })
  games <- Reduce(rbind, games)
  games <- data.frame(games, row.names = NULL)
  
  seasonAlias <- singleRoundDetails$filters$selected[singleRoundDetails$filters$alias == "Season"]
  seasonKey <- singleRoundDetails$filters$entries[[1]]
  seasonIdx <- which(seasonKey$alias == seasonAlias)
  games$seasonAlias <- seasonAlias
  games$seasonName <- seasonKey$name[seasonIdx]
  
  leagueAlias <- singleRoundDetails$filters$selected[singleRoundDetails$filters$alias == "League"]
  leagueKey <- singleRoundDetails$filters$entries[[2]] 
  leagueIdx <- which(leagueKey$alias == leagueAlias)
  games$leagueAlias <- leagueAlias
  games$leagueName <- leagueKey$name[leagueIdx]
  
  phaseAlias <- singleRoundDetails$filters$selected[singleRoundDetails$filters$alias == "Phase"]
  phaseKey <- singleRoundDetails$filters$entries[[3]] 
  phaseIdx <- which(phaseKey$alias == phaseAlias)
  games$phaseAlias <- phaseAlias
  games$phaseName <- phaseKey$name[phaseIdx]
  
  games
}

#' Get Games Summaries
#' 
#' Extract games summaries data from round details (as returned by \code{fetchRoundDetails()})
#'
#' @param roundDetails \code{list}; as returned by \code{fetchRoundDetails()}
#'
#' @return \code{data.frame}
#' @export
getGamesSummaries <- function(roundDetails) {
  games <- parallel::mclapply(roundDetails, getGamesSummariesSingleRound, mc.cores = parallel::detectCores() - 1)
  Reduce(rbind, games)
}


#' Filter Comlpete Games
#'
#' @param gamesDetails \code{list}; as returned by \code{fetchGamesDetails()}
#'
#' @return \code{list}; containing games with status "Ende" only
#' @export
filterGamesComplete <- function(gamesDetails) {
  gamesDetails[sapply(gamesDetails, function(x) x$status$name) == "Ende"]
}

#' Filter Games by Teams
#' 
#' @param gamesDetails \code{list}; as returned by \code{fetchGamesDetails()}
#' @param teams \code{numeric} or \code{list}; - if \code{numeric}: return all 
#'   games with participation of the teams with IDs in teams (independent of 
#'   home/away) - if \code{list(home = c(), away = c())}: return all games with
#'   home team in \code{teams$home} and away team in \code{teams$away}. To
#'   filter only on home or away, pass \code{NA} for the other.
#'   Currently, works with team IDs only.
#'   
#' @return \code{list}; gamesDetails.
#' @export
filterGamesTeams <- function(gamesDetails, teams) {
stopifnot(length(teams) < 3 & length(teams) > 0)
  
  # Validate names of teams if list
  if(is.list(teams)) {
    if(is.null(names(teams))) names(teams) <- c("home", "away")
  } else {stopifnot(all(names(teams) %in% c("home", "away")))}
  
  idx <- if(!is.list(teams)) {
    # teams is vector: either home or away teams match
    sapply(gamesDetails, function(x) any(c(x$details$homeTeam$id, x$details$awayTeam$id) %in% teams))
  } else {
    # teams is list: Distinuish home and away
    sapply(gamesDetails, function(x) 
      (all(is.na(teams$home)) | x$details$homeTeam$id %in% teams$home) &
        (all(is.na(teams$away)) | x$details$awayTeam$id %in% teams$away))
  }
  gamesDetails[idx]
}

#' Get Home Team Standings Points for a Single Game
#'
#' @param singleGameDetails \code{list}, element of gamesDeatils (see \code{fetchGamesDetail()})
#'
#' @return \code{numeric}; number of points won by home team
getTeamPoints.Home.SingleGame <- function(singleGameDetails) {
  homeWin <- singleGameDetails$result$homeTeam > singleGameDetails$result$awayTeam
  otWin <- nrow(singleGameDetails$result$scores) > 3
  3 * homeWin - otWin * homeWin + otWin * (1 - homeWin)
}

#' Get Home Team Standings Points from Games Details
#'
#' @param gamesDetails \code{list}; see \code{fetchGamesDetail()}
#'
#' @return \code{numeric}; standings points won by the respective home team
#' @export
getTeamPoints.Home <- function(gamesDetails) {
  sapply(gamesDetails, getTeamPoints.Home.SingleGame)
}

#' Get Standings Points Won by Home or Away Team
#'
#' @param gamesDetails gamesDetails \code{list}; see \code{fetchGamesDetail()}
#' @param homeAway \code{character}; \code{c("home", "away")}. Either \code{length(homeAway) == 1} or \code{length(homeAway) == length(gamesDetails)}.
#'
#' @return \code{numeric}; standings points won by \code{homeAway}
#' @export
getTeamPoints <- function(gamesDetails, homeAway = "home") {
  stopifnot(all(homeAway %in% c("home", "away")))
  homePoints <- getTeamPoints.Home(gamesDetails)
  homePoints * (homeAway == "home") + (3 - homePoints) * (homeAway == "away")
}

#' Is Team Home or Away
#'
#' @param gamesDetails gamesDetails \code{list}; see \code{fetchGamesDetail()}
#' @param team \code{numeric}; team ID 
#'
#' @return \code{character}; \code{c("home", "away")}
#' @export
getHomeAway <- function(gamesDetails, team) {
  isHomeTeam <- team == getTeam(gamesDetails)
  isAwayTeam <- team == getTeam(gamesDetails, "away")
  
  ifelse(isHomeTeam, "home", ifelse(isAwayTeam, "away", NA))
}

#' Get Team from Games Details
#'
#' @param gamesDetails \code{list}; see \code{fetchGamesDetail()}
#' @param homeAway \code{character}; \code{c("home", "away")}
#' @param ret \code{character}; desired return value to identify team: \code{c("id", "name", "acronym")}
#'
#' @return see \code{ret}
#' @export
getTeam <- function(gamesDetails, homeAway = "home", ret = "id") {
  stopifnot(homeAway %in% c("home", "away"))
  stopifnot(ret %in% c("id", "name", "acronym"))
  
  qry <- sprintf("%sTeam", homeAway)
  sapply(gamesDetails, function(x) x$details[[qry]][[ret]])
}

#' Get Game Date
#'
#' @param gamesDetails  \code{list}; see \code{fetchGamesDetail()}
#' @param ret \code{character}; desired return value: currently, only \code{"date"} is implemented
#'
#' @return \code{date}
#' @export
getGameDate <- function(gamesDetails, ret = "date") {
  stopifnot(ret %in% c("date"))
  as.Date(sapply(gamesDetails, function(x) as.Date(x$startDateTime)), origin = "1970-01-01")
}

#' Get Goals for Home/Away Team in a Single Game
#'
#' @param gamesDetails.singleGame \code{list}, element of gamesDeatils (see \code{fetchGamesDetail()})
#' @param homeAway \code{character}; \code{c("home", "away", "both")}
#' @param total \code{logical}; return total or per period
#'
#' @return \code{tibble}; single row, colums either total or per period goals of selected or both teams
#' @import dplyr
getTeamGoals.singleGame <- function(gamesDetails.singleGame, homeAway = "both", total = TRUE) {
  stopifnot(is.logical(total))
  stopifnot(homeAway %in% c("home", "away", "both"))

  gamesDetails.singleGame$result$scores %>% 
    tidyr::gather("team", "goals", homeTeam, awayTeam) %>% 
    mutate(
      team = gsub("Team", "", team),
      key = if(total) team else sprintf("%sP%s", team, indicator),
      goals = as.numeric(goals)
    ) %>% 
    filter(if(homeAway == "both") TRUE else team == homeAway) %>% 
    select(., key, goals) %>% 
    group_by(key) %>% 
    summarise_each(funs(sum)) %>% 
    tidyr::spread(key, goals) %>% 
    setNames(sprintf("%sGoals", names(.)))
}

#' Get Goals for Home/Away Team
#'
#' @param gamesDetails.singleGame \code{list}, element of gamesDeatils (see \code{fetchGamesDetail()})
#' @param homeAway \code{character}; \code{c("home", "away", "both")}
#' @param total \code{logical}; return total or per period
#'
#' @return \code{tibble}; colums are either total or per period goals of selected or both teams
#' @export
getTeamGoals <- function(gamesDetails, homeAway = "both", total = TRUE) {
  stopifnot(is.logical(total))
  stopifnot(homeAway %in% c("home", "away", "both"))
  
  res <- lapply(gamesDetails, getTeamGoals.singleGame, homeAway = homeAway, total = total)
  Reduce(bind_rows, res)
}