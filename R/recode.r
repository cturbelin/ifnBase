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

#' Recode Intake variables to more error proof labels
#' by default translate labels used to recode variables
#' @param intake data.frame() raw intake data
#' @param translate translate levels
#' @param remove bool unused
#' @export
recode_intake = function(intake, translate=TRUE, remove=TRUE) {

  trans <- function(x) {
    if(translate) {
      i18n(x)
    } else {
      x
    }
  }

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
        warning("")
    }
    intake$age = calc_age(intake$date.birth, intake$timestamp)
    if(remove) {
      intake = subset(intake, select=-date.birth) # remove uneeded variables
    }
    recoded <- c(recoded, "age")
  }

  if( need_recode("gender") ) {
    intake$gender = factor(intake$gender, c(0,1), trans(c('male','female')))
    recoded <- c(recoded, "gender")
  }

  if( need_recode("vacc.curseason") ) {
    # error proof recoding (0 is yes in data)
    intake$vacc.curseason = factor(intake$vacc.curseason, 0:2, trans(YES_NO_DNK))
    recoded <- c(recoded, "vacc.curseason")
  }

  if( need_recode("vacc.lastseason") ) {
    # error proof recoding (0 is yes in data)
    intake$vacc.lastseason = factor(intake$vacc.lastseason, 0:2, trans(YES_NO_DNK))
    recoded <- c(recoded, "vacc.lastseason")
  }

  if( need_recode("pregnant") ) {
    # error proof recoding (0 is yes in data)
    intake$pregnant = factor(intake$pregnant, 0:2, trans(YES_NO_DNK))
    recoded <- c(recoded, "pregnant")
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
        stop("season attribute not found in weekly, cannot determine wich table to use")
      } else {
        if( is.na(season)) {
          # current season
          health.table = default.health.table
        } else {
          def = season.def(season)
          if( is.null(def$health) ) {
            stop(paste("health table not defined in historic.tables for season", season))
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
#' @param weekly data.frame weekly data (return by survey_load_results)
#' @param health.status if TRUE try to get the health status for each row as stored in the db.
#' @seealso survey_load_health_status
#' @export
recode_weekly <- function(weekly, health.status=TRUE) {

  need_recode <- function(name) {
    if(!hasName(weekly, name)) {
      return(FALSE)
    }
    if(is.factor(weekly[[name]])) {
      warning(paste0("column '", name,"' already in factor, not recoded"))
      return(FALSE)
    }
    TRUE
  }

  if(need_recode("sympt.sudden")) {
    weekly$sympt.sudden = recode_ynp(weekly$sympt.sudden)
  }

  if(need_recode("same.episode")) {
    weekly$same.episode[weekly$same.episode == 3] <- NA

    # Same episode is coded with 0 as Yes level, recoding it to human readable levels
    weekly$same.episode = factor(weekly$same.episode, 0:2, YES_NO_DNK)
  }

  if(need_recode("fever.sudden")) {
    weekly$fever.sudden = recode_ynp(weekly$fever.sudden)
  }

  if(need_recode("sympt.when.end")) {
    weekly$sympt.when.end = survey_recode(weekly$sympt.when.end, question = "sympt.when.end", survey="weekly")
  }

  weekly$highest.temp[ weekly$highest.temp == 6] <- NA

  weekly$moderate.fever = !is.na(weekly$highest.temp) & weekly$highest.temp == 3 # fever >= 38 & < 39
  weekly$high.fever = !is.na(weekly$highest.temp) & weekly$highest.temp > 3 # fever over 39deg

  # Ensure symptomes are encoded as boolean
  n = Filter(function(x) !is.logical(weekly[[x]]), get_symptoms_aliases())
  weekly[, n] = weekly[, n] > 0

  if(health.status) {
    weekly = survey_load_health_status(weekly)
  }

  weekly$date = as.Date(trunc(weekly$timestamp, "day")) # date
  weekly$yw = iso_yearweek(weekly$date)

  weekly = recode_weekly_date(weekly)

  attr(weekly, "recode_weekly") <- TRUE

  weekly
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
  cur = (year + (month / 12) )
  round(cur - y + (m / 12), 2)
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

