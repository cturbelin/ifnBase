###
# Survey recoding utilities
###

# Yes, No, DontKnown levels
YES_NO_DNK = c('Yes','No','DNK')

#' recode Y/N/DNK where Y=0 to boolean (wow!)
#' Used to recode some InfluenzaNet variables to more error-proof coding
#' So TRUE is YES level, FALSE = NO level and NA missing value.
#' @param x value to recode
#' @export
recode_ynp = function(x) {
  ifelse(is.na(x), NA, ifelse(x == 0, T, ifelse(x == 1, F, NA)))
}

#' Recode Intake variables to more error proof labels
#' by default translate labels used to recode variables
#' @param intake data.frame() raw intake data
#' @param translate translate levels
#' @param remove bool unused
#' @export
recode_intake = function(intake, translate=T, remove=T) {

  trans <- function(x) {
    if(translate) {
      i18n(x)
    } else {
      x
    }
  }

  # recode some variables
  if( !is.null(intake$date.birth) ) {
    intake$age = calc.age(intake$date.birth, intake$timestamp) # @see share/lib/survey
    if(remove) {
      intake = subset(intake, select=-date.birth) # remove uneeded variables
    }
  }
  if( !is.null(intake$gender) ) {
    intake$gender = factor(intake$gender, c(0,1), trans(c('male','female')))
  }
  if( !is.null(intake$vacc.curseason) ) {
    # error proof recoding (0 is yes in data)
    intake$vacc.curseason = factor(intake$vacc.curseason, 0:2, trans(YES_NO_DNK))
  }
  if( !is.null(intake$vacc.lastseason) ) {
    # error proof recoding (0 is yes in data)
    intake$vacc.lastseason = factor(intake$vacc.lastseason, 0:2, trans(YES_NO_DNK))
  }

  if( !is.null(intake$pregnant) ) {
    # error proof recoding (0 is yes in data)
    intake$pregnant= factor(intake$pregnant, 0:2, trans(YES_NO_DNK))
  }

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
  health.status = dbQuery(paste('SELECT "',id,'" as id, status FROM ', health.table, sep=''))
  weekly = merge(weekly, health.status, by='id', all.x=T)
  weekly$status = factor(weekly$status)
  weekly
}

#' Apply common recoding for weekly data
#' @param weekly data.frame weekly data (return by survey_load_results)
#' @param health.status if TRUE try to get the health status for each row as stored in the db.
#' @seealso survey_load_health_status
#' @export
recode_weekly <- function(weekly, health.status=T) {

  weekly$sympt.sudden = recode_ynp(weekly$sympt.sudden)
  weekly$same.episode[weekly$same.episode == 3] <- NA

  # Same episode is coded with 0 as Yes level, recoding it to human readable levels
  weekly$same.episode = factor(weekly$same.episode, 0:2, YES_NO_DNK)
  weekly$fever.sudden = recode_ynp(weekly$fever.sudden)

  weekly$highest.temp[ weekly$highest.temp == 6] <- NA

  weekly$moderate.fever = !is.na(weekly$highest.temp) & weekly$highest.temp ==3 # fever >= 38 & < 39
  weekly$high.fever = !is.na(weekly$highest.temp) & weekly$highest.temp > 3 # fever over 39deg

  # sympt.aliases @see share/lib/survey.r
  n = survey_labels('weekly','symptoms')
  weekly[, n] = weekly[, n] > 0

  if(health.status) {
    weekly = survey_load_health_status(weekly)
  }
  weekly$date = as.Date(weekly$timestamp) # date
  weekly$yw = iso_yearweek(weekly$date)
  weekly
}
