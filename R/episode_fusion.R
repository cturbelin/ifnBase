
#' Create a list of values marked as "raw"
#'
#' This function can be used in \code{\link{episode_strategy}} to indicate that the list of values are not encoded values but raw values
#' (values as stored in the database)
#' @param ... list of values to mark as raw
#' @export
raw_value = function(...) {
  x <- unlist(rlang::dots_list(...))
  attr(x, "raw_value") <- TRUE
  x
}

#' Create a encoding dictionnary for each variable used in a strategy
#' Lookup for each variable to get raw values (database encoded value) and data recoding available in survey (\code{\link{survey_definition}}).
#' Performs some checks that values should be known and unique.
#' @param data list() describing values for each variable (name as key)
#' @param survey survey name to use to compare variable definition (by default "weekly" survey)
#' @return fusion_strategy_dict object each variables will have recoded (codes) and non encoded (raw) values
strategy_create_coding = function(data, survey="weekly") {
  def = survey_definition(survey)
  vars = names(data)
  dict = lapply(vars, function(name) {
    values = data[[name]]
    if(hasName(def$recodes, name)) {
      # if recoding exists for this variables then create recoded version
      recodes = def$recodes[[name]]
    } else {
      recodes = NULL
    }
    codes = NULL
    raw = NULL

    if( is.null(recodes) ) {
      # No recoding, use all as raw values
      raw = values
    } else {
      if( isTRUE(attr(values, "raw_value")) ) {
        # Raw values provided
        raw = values
        codes = as.character(factor(raw, as.vector(recodes), names(recodes)))
        if(any(is.na(codes))) {
          vv = values[ which(is.na(codes))]
          rlang::abort(paste0("Unexpected raw values for '", name,"' :'", paste(vv, collapse = "','"),"'"))
        }
      } else {
        # Encoded values provided
        codes = as.character(values)
        i = match(codes, names(recodes))
        if(any(is.na(i))) {
          vv = values[ which(is.na(i)) ]
          rlang::abort(paste0("Unexpected encoded values for '", name,"' :'", paste(vv, collapse = "','"),"', did you forget to use raw_value(...)"))
        }
        raw = as.vector(unlist(recodes[i]))
      }
    }
    check_unique = function(x) {
      if(anyDuplicated(x)){
        n = table(x)
        n = n[n > 1]
        n = names(n)
        rlang::abort(paste(substitute(x), " values are not unique for'",name,":'", paste(n, collapse = "','"),"'"), data=x)
      }
    }
    check_unique(raw)
    check_unique(codes)
    list(raw=raw, codes=codes)
  })
  names(dict) <- vars
  structure(dict, class="fusion_strategy_dict")
}


#' Define a strategy to merge data of the same episode of a participant
#' @param type name of the strategy to apply
#' @param ... variable names in weekly (columns) & parameters to apply (see details)
#'
#' @details Parameters:
#'
#' The parameters will depends on the type of strategy
#'
#' For a simple strategy (with no parameter attached to each variable), it will be the name of column to apply the strategy to
#'
#' For a complex strategy it will be the variable name as argument name passed to this function and the parameters for the variables as the argument value
#'
#' @details Type of strategies:
#'
#' Several kind of strategies are useasble:
#' \describe{
#'  \item{min}{Minimal value of the variable for the episode}
#'  \item{max}{Maximal value of the variable for the episode}
#'  \item{any}{Any value TRUE will output TRUE}
#'  \item{mean}{Average value of variable for the episode}
#'  \item{first}{Take the first available value (not NA) in each episode surveys of participant}
#'  \item{last}{Take the last available value (not NA) in each episode surveys of participant}
#'  \item{worst}{Worst Strategy algorithm, see details}
#' }
#'
#' @details General warning:
#'
#' You should be very careful by defining strategy on what data it will be applied. Some strategies will depends on how variables has been
#' recoded or not. Some checks are done but they are not perfects.
#'
#' @details Worst strategy:
#'
#' Worst strategy will expect
#'
#' @examples
#'\dontrun{
#' # Create an "any" strategy for some columns
#' episode_strategy("any", "chills", "cough", "fever")
#'
#' # Define a worst strategy for "take.temp" column in weekly, and give the response order to use
#'
#' episode_strategy("worst", "take.temp"=c('Yes','DontKnow','No'))
#' }
#' @export
episode_strategy = function(type, ...) {
  if(type %in% c('any','min','max','mean','first','last')) {
    klass = "simple_strategy"
    dict = FALSE
  } else {
    klass = paste0(type, "_strategy")
    dict = TRUE
  }

  vars = list(...)

  if(dict) {
    vars = strategy_create_coding(vars)
  } else {
    vars = unlist(vars)
  }

  structure(
    list(
      strategy = type,
      vars = vars
    ),
    class=c('fusion_strategy', klass)
  )
}

#' Print fusion strategy
#' @param x fusion_strategy object
#' @param ... other params (compat print interface)
#' @export
print.fusion_strategy <- function(x, ...) {
  cat("Episode fusion strategy : ", x$strategy,"\n")
  if(is(x, "simple_strategy")) {
    cat("Applied to ", paste(sQuote(x$vars), collapse=','),"\n")
  } else {
    print(x$vars)
  }
}

#' Print fusion strategy
#' @param x fusion_strategy_dict object
#' @param ... other params (compat print interface)
#' @export
print.fusion_strategy_dict <- function(x, ...) {
  Map(function(name, r) {
    cat(" - ", name, ": ")
    m = sapply(seq_along(r$raw), function(i) {
      paste(sQuote(r$raw[i]), if(!is.null(r$codes)) paste0('=',sQuote(r$codes[i])))
    })
    cat(paste(m, collapse=" > "), "\n")
  }, names(x),x)
}

#' Apply function strategy
#'
#' Internal function to apply a fusion strategy of variables of the same episode (of each participant)
#'
#'
#' @param strategy fusion_strategy structure, created by \code{\link{episode_strategy}}
#' @param weekly weekly data with "episode" column
#' @param episode.column character string name of column containing episode
#' @param ... other arguments (not used)
#' @return data.frame() one row by person_id, episode, with selected value for each variable registred in strategy
#' @export
episode_fusion = function(strategy, weekly, episode.column, ...) {
  UseMethod("episode_fusion")
}

group_by_episode = function(.data, episode.column, ...) {
  groups = rlang::syms(c("person_id", episode.column))
  dplyr::group_by(.data, !!!groups, ...)
}


#' Merge episodes using a simple function as merge strategy
#' @param strategy strategy definition structure, created by \code{\link{episode_strategy}}
#' @param weekly weekly data.frame for one episode
#' @param episode.column character string name of column containing episode
#' @param ... other arguments (not used)
#' @seealso \code{\link{episode_fusion}}
#' @return data.frame() one row by person_id, episode, with selected value for each variable registred in strategy
#' @export
episode_fusion.simple_strategy = function(strategy, weekly, episode.column, ...) {

  vars = strategy$vars

  vv = swMisc::select_columns(weekly, vars)

  fusion_fun = switch (strategy$strategy,
     "any" = ~any(., na.rm=TRUE),
     "min" = ~min(., na.rm=TRUE),
     "max" = ~max(., na.rm=TRUE),
     "mean" = ~mean(., na.rm=TRUE),
     "first"= function(y) {
       i = which.min(!is.na(y))
       if(!is.na(i)) {
         return(y[i])
       }
       return(NA)
     },
     'last'=function(y) {
       i = which.max(!is.na(y))
       if(!is.na(i)) {
         return(y[i])
       }
       return(NA)
     },
     match.fun(paste0("strategy_", strategy$strategy))
  )

  fusion_fun = rlang::as_function(fusion_fun)

  apply_strategy = function(y) {
    ifelse(all(is.na(y)), NA, fusion_fun(y))
  }

  weekly %>% group_by_episode(episode.column = episode.column) %>% dplyr::summarize_at(vv, .funs = apply_strategy)

}

#' Apply 'worst' fusion strategy
#'
#' This strategy will consider
#' @param strategy strategy definition structure, created by \code{\link{episode_strategy}}
#' @param weekly weekly data.frame for one episode
#' @param episode.column episode column
#' @param ... other arguments (not used)
#' @seealso \code{\link{episode_fusion}}
#' @return data.frame() one row by person_id, episode, with selected value for each variable registred in strategy
#' @export
episode_fusion.worst_strategy =  function(strategy, weekly, episode.column, ...) {

  # List of available variables in weekly
  vars = swMisc::select_columns(weekly, names(strategy$vars))

  # Create the value set for each variable
  # Try to guess what is the format used in the data
  values = lapply(vars, function(name) {
    ss = strategy$vars[[name]]
    type = NULL
    if( hasName(ss, "codes") ) {
      # Variable with recoding
      if( is.factor(weekly[[name]]) ) {
        vv = as.character(ss$codes)
        type = "recoded"
      } else {
        vv = ss$raw
        type = "raw"
      }
    } else {
      type = "raw"
      vv = ss$raw
    }
    y = weekly[[name]]
    if(type == "raw") {
      if(is.integer(y)) {
        vv = as.integer(vv)
      }
      if(is.logical(y)) {
        if(!is.logical(vv)) {
          rlang::abort(paste0("Strategy values for ",name," should be logical"))
        }
      }
    }
    check = !(is.na(y) | y %in% vv)
    if(any(check)) {
      rlang::abort(paste0("Extra values in weekly$",name," not referenced in strategy"), values=unique(y[check]))
    }
    attr(vv, "type") <- type
    vv
  })
  names(values) <- vars

  apply_strategy = function(ww, .keys) {

    if(nrow(ww) == 1) {
      out = ww[, vars, drop=FALSE]
    } else {

      out = list()

      for(name in vars) {
        y = ww[[name]]
        if(all(is.na(y))) {
          value = NA
        } else {
          y = y[!is.na(y)]
          vv = values[[name]]
          n = min(match(y, vv), na.rm=TRUE)
          value = vv[n]
        }
        out[[name]] = value
      }
      out = as.data.frame(out)
    }
    out
  }

  ww = weekly %>% group_by_episode(episode.column = episode.column) %>% dplyr::group_modify(apply_strategy)
  ww
}


#' Compute episode dataset
#'
#' This function will keep only one weekly by disease "episode" previously computed in the column in \code{episode.column} parameter
#' Variables of all weekly in the same episode will
#'
#' @param weekly weekly data with episode column (typically computed by \code{\link{episode_compute}})
#' @param design study params, structure returned by \code{\link{episode_design}}
#' @param episode.column name of the column containing episode identifier
#' @export
episode_fusion_strategy = function(weekly, design, episode.column="episode") {

  weekly = weekly[ !is.na(weekly[[episode.column]]), ]

  #weekly = dplyr::arrange(weekly, timestamp)

  has_sympt_end = hasName(weekly, "sympt.when.end") & hasName(weekly, "sympt.end")

  median_episode_duration = design$median_episode_duration
  max_episode_duration = design$max_episode_duration

  compute_ending = function(ww, .keys) {
    date_end = NA
    last = nrow(ww)
    onset = ww$onset[1] # Start of episode
    if(has_sympt_end && ww$sympt.when.end[last] == YES && !is.na(ww$sympt.end[last])) {
      date_end = ww$sympt.end[last]
      ok = date_end > onset & date_end <= ww$date[last] & (date_end - onset) <= max_episode_duration
      if(ok & last > 1) {
        ok = ok & date_end >= ww$date[last - 1]
      }
      if(!ok) {
        date_end = NA
      }
    }
    if(is.na(date_end)) {
      date_end = onset + median_episode_duration
    }

    ee = ww[1, c('id', 'timestamp', 'onset')]
    ee$date_end = date_end
    ee
  }

  episodes = weekly %>% group_by_episode(episode.column=episode.column) %>% dplyr::group_modify(compute_ending)


  for(i in seq_along(design$strategies)) {
    strategy = design$strategies[[i]]
    fusion = episode_fusion(strategy, weekly, episode.column=episode.column)
    episodes = merge(episodes, fusion, by=c('person_id','episode'), all=T)
  }

  episodes
}
