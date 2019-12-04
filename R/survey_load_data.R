##
# Load Results & Participation data

#' load a survey results set
#'
#' survey_load_results loads results set for a given survey. It handles several features:
#'  \itemize{
#'    \item mapping of column names (in the DB) to meaningful names (defined in epiwork.tables)
#'    \item merging with geographic tables (geo parameters set geographic level to use)
#'    \item selecting data from a season (automatic table and/or dates bound selection)
#' }
#'
#' Note : "global_id" is not used here we use person_id (sequential index of the user = primary key of survey_surveyuser table) instead because it is really cheaper for memory
#'
#' @param survey : survey name (as defined in epiwork.tables)
#' @param cols column list (you can use aliases)
#' @param geo code for the geo levels to add to the results set (only if the survey contains a geographic column)
#' @param date list(min=,max=) of min and max date to restrict the loaded data (use of the timestamp of the survey)
#' @param db.table override table name (to hanlde backup tables or test table)
#' @param survey.users list of id of SurveyUsers, load data only for these ids
#' @param account boolean if TRUE include "account" id (account user id). Only works if "user" column is defined
#' @param where where condition to apply on the query (caution, no check)
#' @param season if not NULL, season number, will load data for a given season, by default load all data from "pollster_results_[survey]"
#' @param channel if not null get data only for a given channel value
#' @param cols.sup supplementary column or SQL select clauses (caution no check)
#' @param gid if TRUE load gid column
#' @param debug if TRUE show query
#' @param country country name (if survey data has country column)
#' @return data.frame data of the survey
#' @family survey-load
#' @export
survey_load_results = function(survey, cols, geo=NULL, date=NULL, db.table=NULL, survey.users=NULL, debug=F, account=F, where=c(), season=NULL, channel=NULL, cols.sup=c(), gid=F, country=NULL) {
  def = survey_definition(survey)
  if( !is.null(season) ) {
    h = season_definition(season)
    if( is.null(db.table) ) {
      db.table = h[[survey]]
    }
    if( isTRUE(def$single.table) ) {
      # All seasons data are in one single table
      # Use "dates" of the season to load data, or use limits from "date" parameter if defined (and requested dates included in the season's ones)
      # end date could be null (during the current season) & is replaced by the current data & time
      d = lapply(h$dates, function(x) { if(!is.null(x)) { as.POSIXct(x) } else { Sys.time() } })
      if( is.null(date) ) {
        date = list(min=d$start, max=d$end)
      } else {
        # Check that the requested data are included
        date = lapply(date, as.POSIXct)
        if(!is.null(date$min)) {
          if( !( date$min >= d$start && date$min <= d$end )) {
            rlang::abort(paste("Requested minimum date", date$min," is not included in the season period (", d$start,"-", d$end,")"))
          }
        } else {
          date$min = d$start
        }
        if( !is.null(date$max) ) {
          if( !( date$max >= d$start && date$max <= d$end )) {
            rlang::abort(paste("Requested minimum date", date$min," is not included in the season period (", d$start,"-", d$end,")"))
          }
        } else {
          date$max = d$end
        }
      }
    }
  }
  msg = c()
  if( is.null(db.table) ) {
    msg = c(msg, "using default table")
    tb = def$table
  } else {
    msg = c(msg, paste("using table", db.table))
    tb = db.table
  }
  if( length(cols) == 1 && (cols == "*") ) {
    cc = "p.*"
  } else {
    cols = survey_aliases(cols, def)
    cc = paste('p."', cols, '"', sep='')
    cc = c('p.id as id', cc)
  }
  if(account) {
    cc = c("s.user_id as account_id", cc)
  }
  if(gid) {
    cc = c(cc, "p.global_id")
  }
  if( !is.null(geo) ) {
    if( is.null(def$geo.column) ) {
      rlang::abort("geographic column not defined for this column")
    }
    geo.column = def$geo.column
    if( is.logical(geo) ) {
      # get default hierarchy list
      geo = geo_hierarchy()
    }
    gg = sapply(geo, geo_column)
    cc = c(cc, gg)
    if(debug) {
      cat("Joining geo table g, including columns", paste(gg, collapse=', '),"\n")
    }
    join = get_geo_join()
    j = paste0(' left join ', join$table,' g on g."', join$base.column,'"=p."', geo.column, '"')
    if( isTRUE( join$join.country ) ) {
      j = paste(j,' and g."country"=p."country"')
    }
  } else {
    j = ''
  }
  if( !is.null(survey.users) ) {
    where = c(where, paste0('s.id IN(', paste(survey.users, collapse=','), ')'))
  }
  if( !is.null(date) ) {
    msg = c(msg, paste("from", sQuote(format(format="%Y-%m-%d %H:%M:%S", date$min)), "to", sQuote(format(format="%Y-%m-%d %H:%M:%S", date$max))) )
    if( !is.null(date$min) ) {
      where = c(where, paste0(db_quote_var("timestamp")," >= ", db_quote_str(date$min)))
    }
    if( !is.null(date$max) ) {
      m = paste0(as.Date(date$max) + 1,' 00:00:00') # last day + 1
      where = c(where, paste0(db_quote_var("timestamp")," < ", db_quote_str(m)))
    }
  }

  #
  # Disable channel if explicitly set to NA

  if( !is.null(channel) ) {
    if( !is.na(channel) ) {
      where = c(where, paste0(db_quote_var("channel"),'=', db_quote_str(channel)))
      msg = c(msg, paste("channel=",channel))
    }
  } else {
    use_channel = get0("platform.use.channel", ifnotfound = FALSE, envir = .Share)
    if(use_channel) {
      msg = c(msg, "only empty channel")
      where = c(where, paste0(db_quote_var("channel"), "=''"))
    }
  }

  if( !is.null(country) ) {
    if( is.null(def$aliases$country) ) {
      rlang::abort("Country column not set for this table, unable to use 'country' parameter")
    }
    if(length(country) > 0) {
      country = toupper(country) # fix compat with incidence
      if(any(!country %in% .Share$COUNTRY_CODES)) {
        stop(paste("Unknown country codes :", paste(country[!country %in% .Share$COUNTRY_CODES], collapse = ",")))
      }
      country = db_quote_str(country)
      where = c(where, paste("p.", db_quote_var("country")," IN(", paste(country, collapse = ','),")"))
    }
  }

  if(length(msg) > 0) {
    msg = paste("Loading",survey, paste(msg, collapse = ", "))
    cat(msg,"\n")
  }

  cc = c(cc, cols.sup)
  cc = paste(cc, collapse=',')
  if(length(where) > 0) {
    where = paste(' WHERE ', paste(where, collapse=' AND '), sep='')
  } else {
    where = ''
  }
  query = paste0('SELECT s.id as person_id, ',cc,' from ',tb,' p left join ',join_surveyuser('p', 's'), j, where, sep='')
  if(debug) {
    cat(query, "\n")
  }
  r = dbQuery(query)
  if( is.data.frame(r) ) {
    n = names(r)
    n = survey_aliases(n, def, revert=TRUE)
    names(r) <- n
  }
  attr(r,'survey') <- survey
  attr(r, 'db.table') <- db.table
  attr(r, 'season') <- ifelse(!is.null(season), season, NA) # If current season (or not requested) attribute set to NA
  r
}

#' Create join clause to survey_surveyuser table for a survey
#' @param survey_alias table alias name of the survey table
#' @param user_alias table to use for the survey_user table
join_surveyuser = function(survey_alias, user_alias) {
  paste0(' survey_surveyuser ',user_alias,' on ',survey_alias,'.global_id=',user_alias,'.global_id ')
}


#' Get list of participant id (person_id = survey_user_id)
#' List of participants registred in weekly at least once for a given season
#' @param season season number to get, if several seasons are given use min and max of seasons dates
#' @param use.season.dates restrict to season's starting & ending dates. forced if single table model for weekly
#' @param use.min us minimal date of the given season, if not use all before the end of the season (only for single table model)
#' @param country country to restrict participant
#' @family survey-load
#' @export
survey_participant_season = function(season, use.season.dates=FALSE, use.min=TRUE, country=NULL) {
  # For use of dates if single table model
  if(isTRUE(.Share$epiwork.tables$weekly$single.table)) {
    use.season.dates = TRUE
  } else {
    if(use.min) {
      rlang::warn("Use of `use.min` with multiple table cannot work ")
    }
  }
  h = season_definition(season)
  min = NULL
  max = NULL
  if(use.season.dates) {
    dates = get_season_dates(season)
    if(use.min) {
      min = dates$start
    }
    max = dates$end
  }
  w = c()
  time.col = db_quote_var("timestamp")
  if(!is.null(min)) {
    w = c(w, paste0(time.col, " >=", db_quote_str(min) ))
  }
  if(!is.null(max)) {
    w = c(w, paste0(time.col, " <=", db_quote_str(max) ))
  }
  if(can_use_country(country)) {
    w = c(w, paste0("p.", db_quote_var("country"),"=",db_quote_str(country)))
  }

  if(length(w) > 0) {
    w = paste(' where', paste(w, collapse = ' and ') )
  } else {
    w = ''
  }

  query = paste0("SELECT distinct s.id as person_id from ",h$weekly," p left join ",join_surveyuser("p","s"), w)
  p = dbQuery(query)
  p$person_id
}

#' List of participants registred in weekly at least once in previous season (regarding given [season])
#' @param season season year
#' @param ids list of participants to keep
#' @param use.season.dates if TRUE restrict weekly scan to the official date of each season \code{\link{get_historical_tables}}
#' @param country country to use (only on european platform)
#' @param from integer relative index of the oldest season to scan to (index relative to season, e.g. -1 = previous from given [season])
#' @family survey-load
#' @export
survey_participant_previous_season = function(season, ids=NULL, use.season.dates=F, from=NULL, country=NULL) {
  season = parse_season(season)
  if(!is.null(from) && length(from) > 1) {
    rlang::warn("from has more than 1 element, only first will be used")
    from = from[1]
  }
 if(isTRUE(.Share$epiwork.tables$weekly$single.table)) {
    survey_participant_previous_season.single_table(season=season, ids=ids, from=from, country=country)
  } else {
    survey_participant_previous_season.multiple_table(season=season, ids=ids, use.season.dates=use.season.dates, from=from, country=country)
  }
}

#' Get relative season numbers to a reference season number
#' @param season int reference season
#' @param from int vector of relative index to the reference season
#' @keywords internal
#' @export
relative_seasons = function(season, from=NULL) {
  all.seasons = as.integer(get_historical_seasons())
  if( is.null(from) || is.na(from) ) {
    seasons = all.seasons
  } else {
    index = as.integer(from)
    min = -length(all.seasons)
    if(is.na(index) | index < min | index >= 0) {
      rlang::abort(paste0("`from` should be a negative integer value (min ",min,"), given ", from))
    }
    if(index > 0) {
      rlang::abort("from should be a negative")
    }
    seasons = season + index # seasons are relative index to [season]
  }
  seasons = seasons[seasons < season] # exclude given
  seasons = seasons[seasons %in% all.seasons] # keep only valid seasons values
  seasons
}

#' Implementation for multiple table data model
#' we have to check season by season
#' @rdname survey_participant_previous_season
survey_participant_previous_season.multiple_table = function(season, ids=NULL, use.season.dates=F, from=NULL, country=NULL) {
  message("survey_participant_previous_season: multiple table implementation")
  seasons = relative_seasons(season, from=from)
  previous = c()
  for(s in seasons) {
    p = survey_participant_season(s, use.season.dates = use.season.dates)
    previous = unique(c(previous, p))
  }
  if( !is.null(ids) ) {
    previous = previous[ previous %in% ids]
  }
  previous
}

#' Implementation for multiple table data model
#' we have to check season by season
#' @rdname survey_participant_previous_season
survey_participant_previous_season.single_table = function(season, ids=NULL, from=NULL, country=NULL) {
  message("survey_participant_previous_season: single table implementation")
  if(is.null(from)) {
    season = relative_seasons(season - 1)
    if(length(season) == 0) {
      return(ids)
    }
    season = min(season)
    # Only use the season as a maximum
    previous = survey_participant_season(season, use.min = FALSE, use.season.dates = TRUE, country=country)
  } else {
    seasons = relative_seasons(season, from=from)
    previous = survey_participant_season(seasons[1], use.min=TRUE, use.season.dates = TRUE, country=country)
  }
  if( !is.null(ids) ) {
    previous = previous[ previous %in% ids]
  }
  previous
}


#' Load participants data
#' @param active.account logical only active user account if TRUE
#' @param ids list of survey_user ids
#' @family survey-load
#' @export
survey_load_participants = function(active.account=NULL, ids=NULL) {

  where = c()
  join = c()
  select = c()
  if( isTRUE(active.account) ) {

    join = c(join, 'left join auth_user a on a.id=s.user_id')
    where = c(where, 'a.is_active=True')
    select = c(select, 'a.last_login' ,'a.date_joined')
  }

  if( !is.null(ids) ) {
    where = c(where, paste0('s.id in(', paste(ids, collapse = ','),')'))
  }

  where = if(length(where)> 0) paste0(' WHERE ', paste(where, collapse = ' AND ')) else ''
  join = if(length(join) > 0) paste(join, collapse = ' ')
  select = if(length(select) >0) paste0(',', paste(select, collapse = ',')) else ''

  r = dbQuery('select s.id as person_id, s.global_id, s.user_id, deleted',select,' from survey_surveyuser s ', join, where)

  class(r) <- c('gn_participants', class(r))

  r
}

#' Load historical data for a set of users
#' @param ids list of participants (survey_user.id or results from survey_load_participants)
#' @param survey survey shortname
#' @param cols list of columns to load
#' @export
#' @family survey-load
#' @importFrom methods is
survey_load_results_historic = function(ids, survey, cols) {
  if(!requireNamespace("dplyr")) {
    stop("dplyr required to use this function")
  }
  years = sort(names(.Share$historical.tables), decreasing = TRUE)

  if( is(ids, 'gn_participants') ) {
    ids = ids$person_id
  }

  intakes = NULL

  for(year in years) {

    cat(year, "\n")

    if( !is.null(intakes) ) {
      ids = ids[ !ids %in% intakes$person_id ]
    }

    if(length(ids) == 0) {
      break()
    }

    ii = survey_load_results(survey, cols=cols, survey.users = ids, season = year)
    ii = keep_last_survey(ii)

    if(survey == "intake") {
      ii$code_com = as.character(ii$code_com)
      ii$occup.place.com = as.character(ii$occup.place.com)
      ii$vacc.date = as.Date(as.character(ii$vacc.date))
    }
    cat(nrow(ii),"\n")
    ii$season = year
    intakes = dplyr::bind_rows(intakes, ii)
  }
  intakes
}


#' Load last participation date for intake & weekly survey for each participants
#' @param ids list of participants
#' @param years list of season to scan, all if NULL
#' @family survey-load
#' @export
survey_load_participations = function(ids, years=NULL) {

  if(!requireNamespace("dplyr")) {
    stop("dplyr required to use this function")
  }

  if(is.null(years)) {
    years = sort(names(.Share$historical.tables), decreasing = TRUE)
  }

  if( is(ids, 'gn_participants') ) {
    rr = ids
  } else {
    rr = survey_load_participants(ids=ids)
  }
  gids = paste0("'", paste(rr$global_id, collapse="','"),"'")
  participations = NULL

  `%>%` <- dplyr::`%>%`

  for(year in years) {

    h = season.def(year)

    ii = dbQuery("select global_id, timestamp from ", h$intake, " where global_id in (", gids,")")

    ww = dbQuery("select global_id, timestamp from ", h$weekly, " where global_id in (", gids,")")

    ii = ii %>%
      dplyr::group_by(global_id) %>%
      dplyr::summarise(intake=dplyr::n())
    ww = ww %>%
      dplyr::group_by(global_id) %>%
      dplyr::summarise(weekly=dplyr::n())

    ii = merge(ii, ww, by="global_id", all=TRUE)

    if(nrow(ii) > 0) {
      ii$season = year
    }

    participations = dplyr::bind_rows(participations, ii)
  }
  participations
}
