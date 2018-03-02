###
# Survey recoding utilities
###

# Yes, No, DontKnown levels
YES_NO_DNK = c('Yes','No','DNK')

#' recode Y/N/DNK where Y=0 to boolean (wow!)
#' Used to recode some InfluenzaNet variables to more error-proof coding
#' So TRUE is YES level, FALSE = NO level and NA missing value.
#' @export
recode.ynp = function(x) {
  ifelse(is.na(x), NA, ifelse(x == 0, T, ifelse(x==1, F, NA)))
}

#' Recode Intake variables to more error proof labels
#' by default translate labels used to recode variables
#' @param intake data.frame() raw intake data
#' @param translate translate levels
#' @param remove bool unused
#' @export
recode.intake = function(intake, translate=T, remove=T) {

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

#' Apply common recoding for weekly data
#' @param weekly data.frame weekly data (return by survey_load_results)
#' @param health.status if TRUE try to get the health status for each row as stored in the db.
#' @seealso survey_load_health_status
#' @export
recode.weekly <- function(weekly, health.status=T) {

  weekly$sympt.sudden = recode.ynp(weekly$sympt.sudden)
  weekly$same.episode[weekly$same.episode == 3] <- NA

  # Same episode is coded with 0 as Yes level, recoding it to human readable levels
  weekly$same.episode = factor(weekly$same.episode, 0:2, YES_NO_DNK)
  weekly$fever.sudden = recode.ynp(weekly$fever.sudden)

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
  weekly$yw = ISOYearWeek(weekly$date)
  weekly
}
