#####
# Syndromes definitions and computation
#####

#' List of coded values for fever
#' names are pretty labels, values coded
FEVER.CODES = c('<37'=0,'[37,37.5)'=1,'[37.5,38)'=2,'[38,39)'=3,'[39,40)'=4, '>40'=5)

#' Load health status for each weekly responses
#'
#' load health status from the table (or view) in db. If weekly are store in separated tables by season,
#' the weekly data.frame should have a "season" attribute telling which table to use (historical.tables variable should map table for each seasons)
#' @export
#' @param weekly a data.frame loaded by survey_load_results (weekly survey)
#' @param health.table name of a health status coding view. CAUTION if used, no check is done (is right table used for the right season)
survey_load_health_status = function(weekly, health.table=NULL) {
  h = attr(.Share$epiwork.tables, "health.status")
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

#' Regroup mixed syndromes to non-specific for the 2012's influezanet syndromes set
#' Resulting levels are more pretty : "no.symptom", "ili", "cold", "gastro", "non.specific", "allergy"
#' @export
regroup.syndrome = function(x) {
  ll = levels(x)
  aliases = syndromes.set$grouped$pretty
  n = as.vector(sapply(ll, function(x) {
    aliases[x]
  }))
  f = is.na(n)
  n[f] = ll[f]
  levels(x) <- n
  x
}

# View definition from InfluenzaNet
# SELECT id as pollster_results_weekly_id,
#  case true
#      when "no.sympt"
#          then 'NO-SYMPTOMS'
#
#      when ("sympt.sudden" = 0 or "fever.sudden" = 0)
#       and ("fever" or "chills"  or "highest.temp" = 3 or "highest.temp" = 4 or "highest.temp" = 5 or "asthenia" or "headache" or "pain")
#       and ("sorethroat" or "cough" or "dyspnea")
#          then 'ILI'
#
#      when
#        (
#            (not "fever") and (not "chills")
#            and (("highest.temp" = 0) or ("highest.temp" is null))
#            and ("rhino" or "sneeze" or "wateryeye")
#            and ("sympt.cause" = 2)
#        ) and (
#            case true when "diarrhea" then 1 else 0 end +
#            case true when "nausea" then 1 else 0 end +
#            case true when "vomiting" then 1 else 0 end +
#            case true when "abdopain" then 1 else 0 end >= 2
#        ) then 'ALLERGY-or-HAY-FEVER-and-GASTROINTESTINAL'
#
#      when (not "fever") and (not "chills")
#       and (("highest.temp" = 0) or ("highest.temp" is null))
#       and ("rhino" or "sneeze" or "wateryeye")
#       and ("sympt.cause" = 2)
#          then 'ALLERGY-or-HAY-FEVER'
#
#      when
#        (
#            case true when "rhino" then 1 else 0 end +
#            case true when "sneeze" then 1 else 0 end +
#            case true when "cough" then 1 else 0 end +
#            case true when "sorethroat" then 1 else 0 end >= 2
#              -- note: common cold after all allergy-related branches
#        ) and (
#            case true when "diarrhea" then 1 else 0 end +
#            case true when "nausea" then 1 else 0 end +
#            case true when "vomiting" then 1 else 0 end +
#            case true when "abdopain" then 1 else 0 end >= 2
#        ) then 'COMMON-COLD-and-GASTROINTESTINAL'
#
#      when
#        case true when "rhino" then 1 else 0 end +
#        case true when "sneeze" then 1 else 0 end +
#        case true when "cough" then 1 else 0 end +
#        case true when "sorethroat" then 1 else 0 end >= 2
#          -- note: common cold after all allergy-related branches
#          then 'COMMON-COLD'
#
#      when
#        case true when "diarrhea" then 1 else 0 end +
#        case true when "nausea" then 1 else 0 end +
#        case true when "vomiting" then 1 else 0 end +
#        case true when "abdopain" then 1 else 0 end >= 2
#          then 'GASTROINTESTINAL'
#
#      else 'NON-SPECIFIC-SYMPTOMS'
#  end as status
#FROM pollster_results_weekly"""
#

#' Compute syndrome classification of weekly data according to 2012 InfluenzaNet rules (as defined in the web platform)
#' This function do not require the data to be in the weekly table (i.e. do not use the DB view). So it is useables with
#' generated data or without the weekly id
#' @param r weekly data (as return by survey_load_results)
#' @param as.levels if TRUE returns factors
#' @seealso syndromes.set
syndromes.influenzanet.2012 = function(r, as.levels=F) {
#' syndromes set computation for the 2012's version
 # c('no.sympt','ili', 'allergy.gastro','allergy','cold.gastro', 'cold','gastroenteritis','sympt.other')

  respi = r$sorethroat | r$cough | r$dyspnea
  gastro.score = apply(r[,c('diarrhea','nausea','vomiting','abdopain')], 1, sum)
  cold.score = apply(r[,c('rhino','sneeze','cough','sorethroat')], 1, sum)
  no.temp = is.na(r$highest.temp) | r$highest.temp == 0
  allergic = r$rhino | r$sneeze | r$wateryeye
  believe.allergy = !is.na(r$sympt.cause) & r$sympt.cause == 2
  i = function(x) { !is.na(x) & x }

  x = ifelse( r$no.sympt, 1,
     ifelse( ( i(r$sympt.sudden) | i(r$fever.sudden) ) & (r$fever | r$chills | (!is.na(r$highest.temp) & r$highest.temp %in% c(3:5)) | r$asthenia | r$headache | r$pain) & respi, 2,
      ifelse( !r$fever & !r$chills & no.temp & allergic & (believe.allergy) & gastro.score >=2, 3,
        ifelse(!r$fever & !r$chills & no.temp & allergic & (believe.allergy), 4,
         ifelse( cold.score >= 2 & gastro.score >= 2, 5,
          ifelse( cold.score >= 2, 6,
            ifelse( gastro.score >= 2, 7, 8)
          ))))))
  if(as.levels) {
    x = factor(x, 1:8, syndromes.set$influenzanet.2012$levels)
  }
  x
}

#' Compute the 2011's Influenzanet syndromes set  (NO-SYMPT/ILI/COMMON-COLD/GASTRO/OTHER)
#' @seealso syndromes.influenzanet.2012
#' @param r data.frame of weekly results
syndromes.influenzanet.2011 = function(r, as.levels=F) {
  respi = r$sorethroat | r$cough | r$dyspnea
  gastro = r$nausea | r$vomiting | r$diarrhea | r$abdopain
  x = ifelse( r$no.sympt, 1,
    ifelse(r$sympt.sudden & (r$fever | r$asthenia | r$headache | r$pain) & respi, 2,
      ifelse((!r$sympt.sudden) & (r$fever | respi | r$sneeze), 3,
        ifelse(gastro, 4, 5)
      )
    )
  )
  if(as.levels) {
    x = factor(x, 1:5, syndromes.set$influenzanet.2011$levels)
  }
  x
}

#' Prettify syndrome names from InfluenzaNet DB health status name to another set
#' by default, transform to pretty names (for R) from the same set ()
#' @export
syndromes.prettify = function(x, pretty.set='influenzanet.2012') {
  ll = levels(x)
  from = syndromes.set[[ pretty.set]]
  if( is.null(from) ) {
    stop(paste("Unknown syndrome set ", pretty.set))
  }
  aliases = from$pretty
  if( is.null(aliases) ) {
    stop(paste("syndrome set ", pretty.set," does not handle pretty aliases"))
  }
  n = as.vector(sapply(ll, function(x) {
    aliases[x]
  }))
  f = is.na(n)
  n[f] = ll[f]
  levels(x) <- n
  x
}

#' Syndromes set definitions
#' Defines each known syndrome set, with possible levels, pretty levels (regrouped version)
#' also define a provider function, usable to compute the syndrome classification from weekly data
#' @export
syndromes.set = list(
  'influenzanet.2011'=list(
    levels=c('NO-SYMPT','ILI','COMMON-COLD','GASTRO','OTHER'),
    provider=syndromes.influenzanet.2011
  ),
  'influenzanet.2012'=list(
    levels=c('no.sympt','ili', 'allergy.gastro','allergy','cold.gastro', 'cold','gastroenteritis','sympt.other'),
    pretty=c(
      "COMMON-COLD"="cold",
      "GASTROINTESTINAL"="gastroenteritis",
      "ILI"="ili",
      "NO-SYMPTOMS"="no.sympt",
      "ALLERGY-or-HAY-FEVER"="allergy",
      "ALLERGY-or-HAY-FEVER-and-GASTROINTESTINAL"="allergy.gastro",
      "COMMON-COLD-and-GASTROINTESTINAL"="cold.gastro",
      "NON-SPECIFIC-SYMPTOMS"="sympt.other"
    ),
    provider=syndromes.influenzanet.2012
  ),
  'grouped'=list(
    levels=c("no.symptom", "ili", "cold", "gastro", "non.specific", "allergy"),
    pretty=c(
      "NO-SYMPTOMS"='no.symptom',
      "ILI"='ili',
      "COMMON-COLD"='cold',
      "GASTROINTESTINAL"='gastro',
      "ALLERGY-or-HAY-FEVER-and-GASTROINTESTINAL"= 'non.specific',
      "ALLERGY-or-HAY-FEVER"='allergy',
      "COMMON-COLD-and-GASTROINTESTINAL"='non.specific',
      "NON-SPECIFIC-SYMPTOMS"='non.specific'
    )
  )
)

##
# French syndrome computation function
# @todo: put in a platform's lib

#' Get the ILI French Sentinel network syndrome
#' @param r data.frame
#' @param infer.fever.level = how to infer the fever level when it's missing (low = consider it's low, "high" consider it's high)
#' @param pain.age.limit, if defined ignore "pain" criterium for age under this limit (=consider always true for pain symptom)
#' @export
symptoms.ili.rs = function(r, infer.fever.level="low", pain.age.limit=NA ) {
  sudden = !is.na(r$sympt.sudden) & r$sympt.sudden
  respi = r$sorethroat | r$cough | r$dyspnea | r$sneeze | r$rhino # r$sputum ?
  if(infer.fever.level == "low") {
    # Count only fever with high level. If not given, consider it as low
    # Missing fever level = low => excluded
    fever = r$fever & (!is.na(r$highest.temp) & r$highest.temp >= 4)
  } else {
    # If fever level is not given, exclude only when fever is low (so when unknow, act as it was high)
    # Missing fever level = high => included
    fever = r$fever & !(!is.na(r$highest.temp) & r$highest.temp <= 3)
  }
  if( !is.na(pain.age.limit) ) {
    # ignore pain if age under limit (=> consider always true)
    pain = ifelse( !is.na(r$age) & r$age <= pain.age.limit, TRUE, r$pain)
  } else {
    pain = r$pain
  }
  ifelse( sudden & respi & fever & pain, TRUE, FALSE)
}

#' Compute ili.fever syndrome classification
#' Customization of ecdc
#' @param r weekly data
#' @param level if TRUE take fever level into account, if FALSE only uses symptom "fever"
#' @param include.miss if TRUE include DK and missing fever level values in "highest.temp"
#' @export
symptoms.ili.fever = function(r, level=F, include.miss=F) {
  sudden = !is.na(r$sympt.sudden) & r$sympt.sudden
  respi = r$sorethroat | r$cough | r$dyspnea
  if(level) {
    fever = r$fever & (!is.na(r$highest.temp) & r$highest.temp %in% c(3:5))
	if (include.miss) {
	  rm(fever)
	  fever = r$fever & ( (!is.na(r$highest.temp) & r$highest.temp %in% c(3:6)) | is.na(r$highest.temp) )
	}
  } else {
    fever = r$fever
  }
  ifelse( sudden & respi & fever & (r$pain | r$headache), TRUE, FALSE)
}


#' Compute a generic gastroenteriritis definition (broader than French Sentinel definition)
#' @export
symptoms.gastro = function(r) {
  r$nausea | r$vomiting | r$diarrhea | r$abdopain
}

#'  Compute gastroenteriritis definition according to French Sentinel's definition
#' @export
symptoms.gastro.rs = function(r) {
 r$diarrhea
}

#' Acute Upper Respiratory Infection
#' @param wide.respiratory consider a wider respiratory syndrome if TRUE
#' @param r weekly data
#' @export
symptoms.ira <- function(r, wide.respiratory=F) {
  sudden = !is.na(r$sympt.sudden) & r$sympt.sudden
  if(wide.respiratory) {
    respi = r$cough | r$rhino | r$sneeze | r$sorethroat | r$dyspnea | r$sputum
  } else {
    respi = r$cough | r$rhino | r$sneeze
  }
  sudden & (r$fever | r$chills | r$asthenia | r$headache | r$pain) & (respi)
}

#' Compute GROG (French Influenza Surveillance historical association)'s ARI definition
#' Sudden syndrome with at least one general symptom and respiratory (cough or coryza or rhinitis)
#' @param r weekly data
#' @export
symptoms.grog  <-  function(r) {
  sudden = !is.na(r$sympt.sudden) & r$sympt.sudden
  coryza = r$rhino & r$sneeze & r$wateryeye
  rhinitis = r$rhino & r$sorethroat & r$wateryeye
  respi = r$cough | coryza | rhinitis
  sudden & (r$fever | r$chills | r$asthenia | r$headache | r$pain) & (respi)
}

#' Compute ECDC ILI Definition
#' @param r weekly data
#' @export
symptoms.ecdc <- function(r, with.chills=FALSE) {
  sudden = !is.na(r$sympt.sudden) & r$sympt.sudden
  general = r$fever | r$headache | r$pain | r$asthenia
  if(with.chills) {
    general = general | r$chills
  }
  sudden & general & (r$sorethroat | r$cough | r$dyspnea)
}

#' Compute Influenzanet ILI Definition (ECDC variation)
#' @param r weekly data
#' @export
symptoms.influenzanet <- function(r) {
  sudden = (!is.na(r$sympt.sudden) & r$sympt.sudden) | (!is.na(r$fever.sudden) & r$fever.sudden)
  sudden & (r$fever | r$headache | r$pain | r$asthenia | r$chills) & (r$sorethroat | r$cough | r$dyspnea)
}

#' Compute syndromes classification using several definitions, for French incidence computation
#'
#' Return data.frame of French (rs) syndrome computed for each row of the weekly df
#' @param weekly data.frame() of weekly results (should have the necessary columns to compute it)
#' @param intake intake data, (with age column)
#' @export
syndrome.provider.rs <- function(weekly, intake) {

  # Get age for each participant
  ages = aggregate(age ~ person_id, data=intake, min) # consider only one age by person (the first given)
  ages$age[ ages$age < 0 | ages$age > 120] <- NA
  weekly = merge(weekly, ages, by='person_id', all.x=T)

  r = list()
  r$id = weekly$id # As the weekly has been merged, rows are not in the same order.
  r$ili.gp = weekly$ili & weekly$visit.GP
  r$ili.rs.strict = symptoms.ili.rs(weekly)
  r$ili.rs.pain = symptoms.ili.rs(weekly, pain.age.limit=5)
  r$ili.rs.wide = symptoms.ili.rs(weekly, infer.fever.level="high", pain.age.limit=5)
  r$ili.rs.wide.gp = symptoms.ili.rs(weekly, infer.fever.level="high", pain.age.limit=5)
  r$ili.rs.wide.gp = r$ili.rs.wide.gp & weekly$visit.GP
  r$ili.fever = symptoms.ili.fever(weekly, level=F)
  r$ili.fever.gp = r$ili.fever & weekly$visit.GP
  r$ili.fever.level = symptoms.ili.fever(weekly, level=T)
  r$ili.fever.level.gp = r$ili.fever.level & weekly$visit.GP
  r$ira = symptoms.ira(weekly, wide.respiratory=F)
  r$ira.gp = r$ira & weekly$visit.GP
  r$grog = symptoms.grog(weekly)
  r$ira.wide = symptoms.ira(weekly, wide.respiratory=T) # ARI with wider respiratory syndrome
  r$ira.wide.gp = r$ira.wide & weekly$visit.GP
  r$ili.ecdc = symptoms.ecdc(weekly)
  r$gastro.rs = symptoms.gastro.rs(weekly)
  r$gastro.all = symptoms.gastro(weekly)
  as.data.frame(r)
}
