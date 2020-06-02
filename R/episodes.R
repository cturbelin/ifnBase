# Episode computation functions
#
# "Episode" is a set of successives surveys who are imputed to describe the same disease episode
# We provide several algorithms to compute this on InfluenzaNet surveys

#' Create a design definition for episodes computation
#'
#' A "design" is a set of information about a process, it's a structure embedding parameters to compute the episodes
#'
#' @param delay_episode int maximum delay to consider two sequential responses are in the same episode
#' @param max_episode_duration int maximum duration for episode (used to find ending date)
#' @param median_episode_duration int median duration of an episode when ending date is not known
#' @param participants list of rules to apply for participants selection will be passed to \code{\link{episode_select_participants}} as `rules` arguments
#' @param method method name to use choices are 'ariza','ecollan' and 'souty'
#' @param onset onset design expression or false, design parameter passed to \code{\link{compute_onset}}
#' @param strategies not implemented yet
#' @return episodes design data structure
#' @family episodes
#' @export
episode_design = function(delay_episode=15, max_episode_duration=11, median_episode_duration=7, participants=list(),  method="ariza", onset=NULL, strategies=NULL ) {

  requireNamespace("dplyr")
  requireNamespace("rlang")

  if(!is.list(participants)) {
    stop("participants should be a list")
  }

  if(is.null(onset)) {
    onset = episode_onset_design()
  }

  match.arg(method, choices=c('ariza', 'ecollan', 'souty'))

  structure(
    list(
      max_episode_duration=check_int(max_episode_duration),
      median_episode_duration=check_int(median_episode_duration),
      delay_episode_max = check_int(delay_episode),
      method = method,
      participants = participants,
      onset=onset,
      strategies = strategies
    ),
    class="episode_design"
  )

}

#' Select participants based on a list of rules
#' @param weekly weekly data.frame weekly data
#' @param intake intake data.frame intake data
#' @param rules list of rule definition (see details)
#' @return data.frame() participants state for each rules, selections count at each step in attributes
#'
#' @details
#'
#' Each rule is a element of the list using 3 forms
#' \itemize{
#'  \item{One character value, the name of the rule (without parameters)}
#'  \item{a named list element, rule name as name, parameters as values (single value or list of named parameters)}
#'  \item{a anonymous element, rule name is given in the `name` entry}
#' }
#'
#' Known rules:
#' \describe{
#' \item{has.intake}{Only participants with at least one intake}
#' \item{has.weekly}{Only participants with at least one weekly}
#' \item{min.survey}{Participants with at least [value] survey during the season, one parameter as single int value}
#' \item{has_before}{Participants with at least one survey before the given date, one parameter a date}
#' \item{has_after}{Participants with at least one survey before the given date, one parameter a date}
#' \item{has_between}{Participants with at least one survey between the given dates, "start" and "end" param are expected}
#' \item{remove_domtom}{Remove french overseas - french specific }
#' }
#'
#' @examples
#'
#' rules = list(
#' "has.intake",
#' "has.weekly",
#' "min.survey"=3,
#' "has_before"="2019-01-01",
#' "has_between"=list(start="2019-01-02",end="2019-03-01")
#' )
#'
#' @family episodes
#' @export
episode_select_participants = function(weekly, intake, rules) {

  sym_person = rlang::sym("person_id")

  participants = data.frame(person_id=unique(c(weekly$person_id, intake$person_id)))

  selections = list()

  participants$keep = TRUE

  selections$all = nrow(participants)
  nn = c()
  for(i in seq_along(rules)) {
    rule_name = names(rules[i])
    rule_param = NULL
    r = rules[[i]]
    if( is.null(rule_name) || rule_name == "" ) {
      if(is.character(r)) {
        rule_name = r
      } else {
        rule_name = r$name
        rule_param = r
      }
    } else {
      rule_param = r
      if(is.list(rule_param) && !is.null(rule_param$name)) {
        rule_name = rule_param$name
      }
    }

    nn = make.unique(c(nn, rule_name))
    rule_id = nn[length(nn)]

    message("Selecting ", rule_id, " (",rule_name,")", if(!is.null(rule_param)) deparse(rule_param), ": ")

    if(rule_name %in% c("has.intake", "has_intake")) {
      participants[[rule_id]] = participants$person_id %in% intake$person_id
    }

    if(rule_name %in% c("has.weekly", "has_weekly")) {
      participants[[rule_id]] = participants$person_id %in% weekly$person_id
    }

    if(rule_name == "min.survey") {
      if(! (is.numeric(rule_param) && rule_param >= 1)) {
        stop(paste0("rule param for min.survey rule should be an positive integer, given: ",rule_param))
      }
      rule_param = as.integer(rule_param)
      p_name = paste0(rule_id,"_param")
      p = weekly %>% dplyr::group_by(!!sym_person) %>% dplyr::summarize(!!p_name:=dplyr::n())
      participants = dplyr::left_join(participants, p, by="person_id")
      participants[[rule_id]] = !is.na(participants[[p_name]]) & participants[[p_name]] >= rule_param
    }

    if(rule_name %in% c("has_before", "has_after")) {
      param = as.Date(rule_param)
      if(is.na(param)) {
        stop(paste0("rule param should be a date for ", rule_id))
      }
      if(rule_name == "has_before") {
        ids = unique(weekly$person_id[ weekly$date <= param ])
      }
      if(rule_name == "has_after") {
        ids = unique(weekly$person_id[ weekly$date >= param ])
      }
      participants[[rule_id]] = participants$person_id %in% ids
    }

    if(rule_name == "remove_domtom") {
      ids = unique(intake$person_id[ !is.na(intake$code_reg) & intake$code_reg %in% c("42", "72", "83", "25", "26", "53", "24", "21", "94", "43", "23", "91", "74", "41", "73", "31", "52", "22", "54", "93", "11", "82") ])
      participants[[rule_id]] = participants$person_id %in% ids
    }

    if(rule_name %in% c("has_between")) {
      if(!is.list(rule_param) | is.null(rule_param$start) | is.null(rule_param$end) ) {
        stop(paste0("Rule param for ", rule_id," should be a list with start and end keys"))
      }

      param = lapply(rule_param, as.Date)

      if(is.na(param$start)) {
        stop(paste0("rule param start should be a date for ", rule_id))
      }

      if(is.na(param$end)) {
        stop(paste0("rule param start should be a date for ", rule_id))
      }

      ids = unique(weekly$person_id[ weekly$date >= param$start & weekly$date <= param$end ])

      participants[[rule_id]] = participants$person_id %in% ids
    }

    participants$keep = participants$keep & participants[[rule_id]]
    selections[[rule_id]] = sum(participants$keep)

    message(paste0(" for this rule", sum(participants[[rule_id]]) , " participants left: ", selections[[rule_id]]))
  }

  attr(participants, "selections") <- selections

  participants
}

#' Prepare data and select participants using given rules
#'
#' `episode_prepare_data` makes some checks in data, and apply selection rules by callng \code{\link{episode_select_participants}}.
#'
#' To compute episode in a simple setting \code{link{episode_build}} would be more convinient to use (it will call this function)
#'
#' @param design episode design data structure (result of \code{\link{episode_design}})
#' @param intake data.frame intake data
#' @param weekly data.frame weekly data
#' @param env environment with intake & weekly data (alternative to provide them directly in existing env)
#' @param keep.onset keep onset column if already exists
#' @return environment environment containing selected data (weekly, intake, participants, selections)
#'
#' @details
#' The environment contains several data.frame()
#' \describe{
#'  \item{participants}{one row for each participants and rule results (TRUE/FALSE) for each participants, column "keep" indicate final state of participant}
#'  \item{selections}{count of selected participants at each step (after applying each rule)}
#'  \item{intake}{the intakes for selected participants}
#'  \item{weekly}{the weekly for selected participants}
#' }
#'
#' @family episodes
#' @export
episode_prepare_data = function(design, intake=NULL, weekly=NULL, env=NULL, keep.onset=FALSE) {

  if( !is.null(env) ) {
    if(!is.null(intake) ) {
      rlang::abort("intake should not be provided if env is provided")
    }
    if(!is.null(weekly) ) {
      rlang::abort("weekly should not be provided if env is provided")
    }

    weekly = env$weekly
    intake = env$intake
  } else {
    env = rlang::new_environment()
  }

  if(is.null(intake) ) {
    rlang::abort("intake should be provided either as argument or in env")
  }
  if(is.null(weekly) ) {
    rlang::abort("weekly should be provided either as argument or in env")
  }

  ### Cleanup data
  if(!isTRUE(attr(weekly, "recode_weekly_date"))) {
    weekly = recode_weekly_date(weekly)
    weekly$date = as.Date(trunc(weekly$timestamp, "day")) # Recompute date to be sure its computed properly
  }

  required.columns = c('id','date','person_id','same.episode')
  n = sapply(required.columns, function(col) hasName(weekly, col))
  if(!all(n)) {
    n = n[!n]
    stop(paste("Some required columns in weekly are not found", paste(names(n), collapse = ",")))
  }

  if(hasName(weekly, "sympt.when.end")) {
    if(!is.factor(weekly$sympt.when.end)) {
      stop("'sympt.when.end' in weekly should be a factor")
    }
  }

  if(hasName(weekly, "sympt.end") && !is(weekly$sympt.end, "Date")) {
    stop("sympt.end must be encoded as a Date")
  }

  if( !(hasName(weekly,"onset") && keep.onset)) {
    weekly = compute_onset(weekly, design$onset)
  }

  sym_person = rlang::sym("person_id")

  # Keep only 1 survey by date
  weekly = dplyr::arrange(weekly, !!sym_person, timestamp)
  weekly = weekly[!duplicated(weekly[, c("person_id","date")], fromLast=TRUE), ] # vecteur de la position des non-doublons

  participants = episode_select_participants(weekly = weekly, intake=intake, rules=design$participants)

  ids = participants$person_id[participants$keep]

  weekly = weekly %>% dplyr::filter(!!sym_person %in% ids)
  intake = intake %>% dplyr::filter(!!sym_person %in% ids)

  selections <- attr(participants, "selections")

  env$weekly = weekly
  env$intake = intake
  env$participants = participants
  env$selections = selections

  env
}

##
# Ariza's Algorithm:
# - Reduction d'un questionnaire par jour pour chaque participant
# - Pour chaque participants, questionnaires triés par date
# Pour chaque questionnaire i
# - On considere la date de début du syndrome (onset)
#
# Avant le 2e questionnaire:
# Si le précédent:[ est le même épisode et correspond au syndrome et date de moins de 15j ]
#
# Après le 2e questionnaire du participant:
# Si: le questionnaire i correspond au syndrome d'interet
# Si:
#   le précédent (i-1):  [correspond au syndrome ET est le même épisode ET date de moins de 15j ]
# ET Celui d'avant (i-2) : [ n'est pas le syndrome OU Est le syndrome mais < 15j OU que sa date de début est différente du précédent ]
# Alors :
#   - On remplace date de début (onset) du questionaire i par celle du précédent (onset[i-1])


#' Compute episodes using M Ariza's algorithm
#' The onset date is used as the episode identifier, the algorithm iteratively replace onset date to the first date for all survey considered
#' to belong to the same disease episode.
#'
#' @param weekly data.frame of weekly data
#' @param syndrome.column name of the syndrome column to use
#' @param params episode_design
#' @param .progress progress_estimated style progress bar, nothing if NULL
#' @family episodes
episode_compute_ariza = function(weekly, syndrome.column, params, .progress=NULL) {

  sym_person = sym("person_id")

  weekly = dplyr::arrange(weekly, !!sym_person, date)

  delay_episode_max = params$delay_episode_max

  use.rank = if(is.null(params$ariza.rank) ) TRUE  else params$ariza.rank

  if(!is.logical(weekly[[syndrome.column]])) {
    stop(paste0("weekly column ", syndrome.column," should be a logical"))
  }

  #' Find episodes, identify episodes sequences for one person_id
  #' Returns a data.frame() with weekly row id (column "id") & episode number
  find_episodes = function(ww, .keys) {
    onset = ww$onset # State vector (keep original onset)
    N = nrow(ww)
    if(N >= 2) {
      sapply(2:nrow(ww), function(i) {
        w = ww[i, ]
        if(w[[syndrome.column]]) {
          p1_delay = as.integer(w$onset - onset[i - 1]) # Recompute delay for previous
          p1_syndrome =  ww[i - 1, syndrome.column] # Previous weekly has syndrome
          same_episode = is.na(w$same.episode) | w$same.episode != NO # Is same episode or unknown

          if(i == 2) {
            if(  same_episode & p1_delay < delay_episode_max & p1_syndrome ) {
              onset[i] <<- onset[i - 1]
            }
          } else { # i > 2
            p2_syndrome = ww[i - 2, syndrome.column] # syndrome ante-previous

            if(
              (!p2_syndrome | ( p2_syndrome & as.integer(w$onset - onset[i - 2]) < delay_episode_max ) | onset[i - 2] != onset[i - 1] )
              & p1_syndrome
              & same_episode
              & p1_delay < delay_episode_max
            ) {
              onset[i] <<- onset[ i - 1 ]
            }
          }
        }
        invisible()
      })
    }
    if( !is.null(.progress) ) {
      .progress$tick()$print()
    }

    if(use.rank) {
      onset[ !ww[[syndrome.column]] ] = NA
      # Create a rank number for episodes instead of onset date
      onset = dplyr::dense_rank(onset)
    }

    data.frame(id=ww$id, episode=onset)
  }

  weekly %>% dplyr::group_by(!!sym_person) %>% dplyr::group_modify(find_episodes)
}

#' Compute Episodes using Marie Ecollan's strategy
#' @param weekly data.frame of weekly data
#' @param syndrome.column name of the syndrome column to use
#' @param params episode_design
#' @param .progress progress_estimated style progress bar, nothing if NULL
#' @family episodes
episode_compute_ecollan <- function(weekly, syndrome.column, params, .progress=NULL) {

  sym_person = sym("person_id")

  weekly = weekly %>%
    dplyr::arrange(!!sym_person, date) %>%
    dplyr::group_by(!!sym_person) %>%
    dplyr::mutate(date_previous=dplyr::lag(date), delay_previous=as.integer(date - date_previous))

  delay_episode_max = params$delay_episode_max

  ## Apply for all weekly of each person_id
  #' ww : all weekly of one person_id ordered by date (!! Only one weekly by date)
  #' .keys = dummy variable needed by dplyr::group_map()
  find_episodes_ecollan <- function (ww, .keys) {
    n.episode = 0L # Index de l'épisode en cours
    episodes = rep(NA, nrow(ww))
    previous_syndrome  = FALSE # Previous weekly is a syndrome if TRUE
    for (j in 1:nrow(ww)) {
      x = ww[j, ]
      if(x[[syndrome.column]]) {
        is.new = TRUE # Create a new episode (default if previous was not a syndrome)

        if (previous_syndrome) {
          # Si le weekly précédent était un syndrome
          if (!is.na(x$same.episode) & x$same.episode != DONTKNOW ) {
            if (x$same.episode == NO) {
              is.new = TRUE
            } else {
              is.new = FALSE
            }
          } else { # Si same.episode == NA ou DNK
            if (x$delay_previous < delay_episode_max) {
              is.new = FALSE
            } else { # > delay max = different episode
              is.new = TRUE
            }
          }
        }

        if(is.new) {
          # Increment episode index
          n.episode = n.episode + 1L
        }
        episodes[j] = n.episode
        previous_syndrome = TRUE # Flag that previous is a syndrome

      } else { # Not a syndrome
        previous_syndrome = FALSE
      }

    } # for

    if( !is.null(.progress) ) {
      .progress$tick()$print()
    }

    data.frame(id=ww$id, episode=episodes)

  } # find ecollan

  weekly %>% dplyr::group_by(!!sym_person) %>% dplyr::group_modify(find_episodes_ecollan)
}

#' Compute Episodes using Cecile Souty's strategy
#' @param weekly data.frame of weekly data
#' @param syndrome.column name of the syndrome column to use
#' @param params episode_design
#' @param .progress progress_estimated style progress bar, nothing if NULL
#' @family episodes
episode_compute_souty <- function(weekly, syndrome.column, params, .progress=NULL) {

  sym_person = sym("person_id")

  weekly = weekly %>%
    dplyr::arrange(!!sym_person, date)

  delay_episode_max = params$delay_episode_max

  ## Apply for all weekly of each person_id
  #' ww : all weekly of one person_id ordered by date (!! Only one weekly by date)
  #' .keys = dummy variable needed by dplyr::group_map()
  find_episodes_souty <- function (ww, .keys) {
    n.episode = 0L # Index de l'épisode en cours
    episodes = rep(NA, nrow(ww))
    episode_onset = NA
    for (j in 1:nrow(ww)) {
      x = ww[j, ]
      same_episode = is.na(x$same.episode) | x$same.episode != NO # Is same episode or unknown
      if(x[[syndrome.column]]) {

        if(is.na(episode_onset)) {
          is.new = TRUE
        } else {
          if((as.integer(x$onset - episode_onset) < delay_episode_max) && same_episode ) {
            is.new = FALSE
          } else {
            is.new = TRUE
          }
        }

        if(is.new) {
          # Increment episode index
          n.episode = n.episode + 1L
          episode_onset = x$onset
        }
        episodes[j] = n.episode
      } else { # Not a syndrome
        episode_onset = NA
      }

    } # for

    if( !is.null(.progress) ) {
      .progress$tick()$print()
    }

    data.frame(id=ww$id, episode=episodes)

  } # find souty

  weekly %>% dplyr::group_by(!!sym_person) %>% dplyr::group_modify(find_episodes_souty)
}


#' Compute episodes in weekly data
#'
#' This function use environment returned by \code{\link{episode_prepare_data}}
#'
#' It is an intermediate function, in a simple setting \code{\link{episode_build}} can be more convenient to use
#'
#' @param .env environment result of \code{\link{episode_prepare_data}}
#' @param syndrome.column name of the column containing episode data
#' @param design study params, structure returned by \code{\link{episode_design}}
#' @param .progress optional progress bar structure, if not NULL must use dplyr::progress_estimated api
#' @param local if TRUE do not modify .env, only returns episodes
#' @export
#' @return data.frame (episodes)
#' @family episodes
episode_compute = function(.env, syndrome.column, design, .progress=TRUE, local=FALSE) {
  if(!is.logical(.env$weekly[[syndrome.column]])) {
    stop(paste0("syndrome column ", syndrome.column, " should be logical"))
  }

  if( isTRUE(.progress) ) {
    .progress = dplyr::progress_estimated(length(unique(.env$weekly$person_id)), min_time = 2)
  }

  if(design$method == "ecollan") {
    ep = episode_compute_ecollan(.env$weekly, syndrome.column, design, .progress = .progress)
  }
  if(design$method == "ariza") {
    ep = episode_compute_ariza(.env$weekly, syndrome.column, design, .progress = .progress)
  }
  if(design$method == "souty") {
    ep = episode_compute_souty(.env$weekly, syndrome.column, design, .progress = .progress)
  }

  if(!local) {
    .env$weekly = dplyr::left_join(.env$weekly, ep[, c('id','episode')], by="id")
  }
  invisible(ep)
}

#' Build Episodes data from survey data
#' @param design a design structure holding computation parameters, \code{\link{episode_design}}
#' @param intake intake data
#' @param weekly weekly data
#' @param syndrome.column name of the boolean column indicating if each weekly fit the syndrome definition or not (TRUE is yes)
#' @param env environment
#' @return environment containing intake, weekly, participants
#'
#' @examples
#' \dontrun{
#'  episode_build(design, weekly, intake)
#' }
#'
#' @export
#' @family episodes
episode_build = function(design, intake=NULL, weekly=NULL, syndrome.column, env=NULL) {
  env = episode_prepare_data(design, intake = intake, weekly=weekly, env=env)
  episode_compute(env, syndrome.column = syndrome.column, design = design)
  if( !is.null(design$strategy) ) {

  }
  env
}

