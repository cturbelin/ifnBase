###
# Survey recoding utilities
###

# Export some constants values
# Uppercases are intentional here

#' Constants values
#'
#' Some constants are provided to limit error by using some common values in programs
#' They are defined using UPPERCASE names to be recoginizable as constants
#'
#' Beware to never redefine these objects in your code, or use ifnBase:: prefix to be sure of the value used.
#'
#' \describe{
#' \item{YES}{is 'Yes' level used to recode many variables}
#' \item{NO}{is 'No' level}
#' \item{DONTKNOW}{'DNK' Dont know level}
#' }
#' @name constants

#' @rdname constants
#' @export
YES = 'Yes'

#' @rdname constants
#' @export
NO = 'No'

#' @rdname constants
#' @export
DONTKNOW = 'DNK'

#' @rdname constants
#' @export
YES_NO_DNK = c(YES, NO, DONTKNOW)

#' recode Y/N/DNK where Y=0 to boolean (wow!)
#' Used to recode some InfluenzaNet variables to more error-proof coding
#' So TRUE is YES level, FALSE = NO level and NA missing value.
#' @param x value to recode
#' @export
recode_ynp = function(x) {
  ifelse(is.na(x), NA, ifelse(x == 0, TRUE, ifelse(x == 1, FALSE, NA)))
}

#' Recode Intake variables to more error-proof labels
#' Value to label mapping are defined in the survey \code{\link{platform_define_survey}}
#' @param intake data.frame() raw intake data
#' @param translate translate levels
#' @param remove if TRUE remove date.birth after the age is computed
#' @param mode recoding mode, by default all known variables will be recoded
#' @return intake with recoded variables
#' @export
recode_intake = function(intake, translate=FALSE, remove=TRUE, mode="all") {
  need_recode <- function(name) {
    if(!hasName(intake, name)) {
      return(FALSE)
    }
    if(is.factor(intake[[name]])) {
      warning(paste0("column '", name,"' already in factor, not recoded"))
      return(FALSE)
    }
    TRUE
  }

  recoded = c()

  # recode some variables
  if( hasName(intake, "date.birth") ) {
    if( hasName(intake, "age") ) {
        rlang::warn("age is already in intake, will be overriden")
    }
    intake$age = calc_age(intake$date.birth, intake$timestamp)
    if(remove) {
      intake = subset(intake, select=-date.birth) # remove uneeded variables
    }
    recoded <- c(recoded, "age")
  }

  # if( need_recode("gender") ) {
  #   intake$gender = factor(intake$gender, c(0,1), trans(c('male','female')))
  #   recoded <- c(recoded, "gender")
  # }
  #
  # if( need_recode("vacc.curseason") ) {
  #   # error proof recoding (0 is yes in data)
  #   intake$vacc.curseason = factor(intake$vacc.curseason, 0:2, trans(YES_NO_DNK))
  #   recoded <- c(recoded, "vacc.curseason")
  # }
  #
  # if( need_recode("vacc.lastseason") ) {
  #   # error proof recoding (0 is yes in data)
  #   intake$vacc.lastseason = factor(intake$vacc.lastseason, 0:2, trans(YES_NO_DNK))
  #   recoded <- c(recoded, "vacc.lastseason")
  # }

  #if( need_recode("pregnant") ) {
  #  # error proof recoding (0 is yes in data)
  #  intake$pregnant = factor(intake$pregnant, 0:2, trans(YES_NO_DNK))
  #recoded <- c(recoded, "pregnant")
  #}

  recodes = survey_recodings("intake")
  for(name in names(recodes)) {
    if(need_recode(name)) {
      mapping = recodes[[name]]
      intake[[name]] <- recode_var(intake[[name]], mapping, translate=translate)
      recoded <- c(recoded, name)
    }
  }

  attr(intake, "recode_intake") <- TRUE
  attr(intake, "recode_intake.columns") <- recoded

  intake
}

#' Load health status for each weekly responses
#'
#' load health status from the table (or view) in db. If weekly are store in separated tables by season,
#' the weekly data.frame should have a "season" attribute telling which table to use (historical.tables variable should map table for each seasons)
#' @export
#' @param weekly a data.frame loaded by survey_load_results (weekly survey)
#' @param health.table name of a health status coding view. CAUTION if used, no check is done (is right table used for the right season)
#' @return weekly with extra column 'status'
survey_load_health_status = function(weekly, health.table=NULL) {
  h = .Share$health.status
  id = ifelse(is.null(h), "pollster_results_weekly_id", h$id)
  if( is.null(health.table) ) {
    default.health.table = ifelse(is.null(h), 'pollster_health_status', h$default)
    if( survey_single_table("weekly") ) {
      # In the single table model, there is only one table where health status is stored
      health.table = default.health.table
    } else {
      # Try to detect the table to use by determining the season currently in use
      # One table for each season, then t
      season = attr(weekly, 'season')
      if( is.null(season) ) {
        # we dont have attribute
        rlang::abort("season attribute not found in weekly, cannot determine wich table to use")
      } else {
        if( is.na(season)) {
          # current season
          health.table = default.health.table
        } else {
          def = season.def(season)
          if( is.null(def$health) ) {
            rlang::abort(paste("health table not defined in historic.tables for season", season))
          }
          health.table = def$health
        }
      }
    }
  }
  cat("loading health status using table", health.table,"\n")
  health.status = dbQuery(paste0('SELECT "',id,'" as id, status FROM ', health.table))
  weekly = merge(weekly, health.status, by='id', all.x=TRUE)
  weekly$status = factor(weekly$status)
  weekly
}

#' Apply common recoding for weekly data
#'
#' Variable recoding (from database value to label) are defined in the survey description in the platform
#' definition file using
#'
#' @param weekly data.frame weekly data (return by survey_load_results)
#' @param health.status if TRUE try to get the health status for each row as stored in the db.
#' @param recode.temp recode temperature variable (need to be false for incidence), if TRUE recode temperature variable
#' @param all.variables if TRUE all known variables will be recoded
#' @return weekly data.frame with recoded values & extra column
#' @seealso \code{\link{survey_load_health_status}}
#' @export
recode_weekly <- function(weekly, health.status=TRUE, recode.temp=FALSE, all.variables=FALSE) {

  need_recode <- function(name) {
    if(!hasName(weekly, name)) {
      return(FALSE)
    }
    if(is.factor(weekly[[name]])) {
      rlang::warn(paste0("column '", name,"' already in factor, not recoded"))
      return(FALSE)
    }
    TRUE
  }

  recoded = c()

  if(need_recode("sympt.sudden")) {
    weekly$sympt.sudden = recode_ynp(weekly$sympt.sudden)
    recoded = c(recoded, "sympt.sudden")
  }

  if(need_recode("same.episode")) {
    weekly$same.episode[weekly$same.episode == 3] <- NA

    # Same episode is coded with 0 as Yes level, recoding it to human readable levels
    weekly$same.episode = factor(weekly$same.episode, 0:2, YES_NO_DNK)
    recoded = c(recoded, "same.episode")
  }

  if(need_recode("fever.sudden")) {
    weekly$fever.sudden = recode_ynp(weekly$fever.sudden)
    recoded = c(recoded, "fever.sudden")
  }

  if(need_recode("sympt.when.end")) {
    weekly$sympt.when.end = survey_recode(weekly$sympt.when.end, variable = "sympt.when.end", survey="weekly")
    recoded = c(recoded, "sympt.when.end")
  }

  if(hasName(weekly, "highest.temp") && is.numeric(weekly$highest.temp)) {
    weekly$highest.temp[ weekly$highest.temp == 6] <- NA
    if(!hasName(weekly, "moderate.fever")) {
      weekly$moderate.fever = !is.na(weekly$highest.temp) & weekly$highest.temp == 3 # fever >= 38 & < 39
    }

    if(!hasName(weekly, "high.fever")) {
      weekly$high.fever = !is.na(weekly$highest.temp) & weekly$highest.temp > 3 # fever over 39deg
    }
  }

  if(recode.temp & need_recode("highest.temp")) {
    survey_recode(weekly$highest.temp, variable = "highest.temp", survey="weekly")
    recoded = c(recoded, "highest.temp")
  }

  # Ensure symptomes are encoded as boolean
  ss = function(x) {
    hasName(weekly, x) && is.numeric(weekly[[x]])
  }
  n = Filter(ss, get_symptoms_aliases())
  if(length(n) > 0) {
    weekly[, n] = weekly[, n] > 0
  }

  if(health.status) {
    weekly = survey_load_health_status(weekly)
  }

  if(!hasName(weekly, "date")) {
    weekly$date = as.Date(trunc(weekly$timestamp, "day")) # date
  }

  if(!hasName(weekly, "yw")) {
    weekly$yw = iso_yearweek(weekly$date)
  }

  weekly = recode_weekly_date(weekly)

  if(all.variables) {
    recodes = survey_recodings("weekly")
    nn = names(recodes)
    nn = nn[!nn %in% recoded]
    for(name in nn) {
      if(need_recode(name)) {
        mapping = recodes[[name]]
        weekly[[name]] <- recode_var(weekly[[name]], mapping, translate=FALSE)
        recoded <- c(recoded, name)
      }
    }
  }

  attr(weekly, "recode_weekly") <- TRUE
  attr(weekly, "recoded_columns") <- recoded
  weekly
}

#' Recode all known variables in survey data
#'
#' @param data data.frame with weekly data
#' @param survey survey name (as it is registered in \code{\link{platform_define_survey}})
#' @param warn if TRUE show warning if a variable is already recoded, default is FALSE
#' @return data with recoded variable (to factor)
#' @export
survey_recode_all  <- function(data, survey, warn=FALSE, check_mapping=NULL) {
  need_recode <- function(name) {
    if(!hasName(data, name)) {
      return(FALSE)
    }
    if(is.factor(data[[name]])) {
      if(warn) {
          warning(paste0("column ", sQuote(name)," already in factor, not recoded"))
      }
      return(FALSE)
    }
    TRUE
  }
  recoded = c()
  recodes = survey_recodings(survey)
  for(name in names(recodes)) {
    if(need_recode(name)) {
      mapping = recodes[[name]]
      data[[name]] <- recode_var(data[[name]], mapping)
      recoded <- c(recoded, name)
    }
  }
  r = attr(data, "recoded_columns")
  if(is.null(r)) {
    r = recoded
  } else {
    r = c(r, recoded)
  }
  attr(data, "recoded_columns") <- r
  data

}


#' Recode weekly dates
#' Recode weekly dates
#' @param weekly weekly data
recode_weekly_date = function(weekly) {

  if(!is(weekly$sympt.start,"Date")) {
    weekly$sympt.start = as.Date(as.character(weekly$sympt.start))
  }

  if(!is(weekly$fever.start,"Date")) {
    weekly$fever.start = as.Date(as.character(weekly$fever.start))
  }

  if(hasName(weekly, "sympt.end")) {
    if( !is(weekly$sympt.end, "Date") ) {
      weekly$sympt.end = as.Date(as.character(weekly$sympt.end))
    }

    weekly$sympt.end[ !is.na(weekly$sympt.end) & weekly$sympt.end > weekly$date ] <- NA

  }

  # Set date in the future to NA (not possible cases)
  weekly$sympt.start[ !is.na(weekly$sympt.start) & weekly$sympt.start > weekly$date ] <- NA
  weekly$fever.start[ !is.na(weekly$fever.start) & weekly$fever.start > weekly$date ] <- NA

  attr(weekly, "recode_weekly_date") <- TRUE

  weekly
}

#' Age of participants from a Year-month birth date
#' @param ym year-month vector
#' @param time reference time
#' @export
calc_age = function(ym, time) {
  y = as.numeric(as.character(substr(ym, 1,4)))
  m = as.numeric(as.character(substr(ym, 6, 7)))
  year = as.numeric(format(time, format="%Y"))
  month = as.numeric(format(time, format="%m"))
  cur = year + (month / 12)
  round(cur - (y + (m / 12)), 2)
}

#' Cut age values into categories using breaks and defining pretty levels labels
#' @param age vector of age values
#' @param age.categories list of breaks (@seealso cut)
#' @export
cut_age <- function(age, age.categories) {
  age.cat = cut( age, breaks=age.categories, include.lowest=TRUE, right=FALSE, ordered_result=TRUE)
  # pretty levels for age ranges
  n = length(age.categories)
  lev = rep(NA, n - 1)
  for(i in 1:(n - 1)) {
    if( (i+1) < n ) {
      lev[i] = paste( age.categories[i], (age.categories[i + 1] - 1), sep='-')
    } else {
      lev[i] = paste(">=", age.categories[i])
    }
  }
  levels(age.cat) <- lev
  age.cat
}

