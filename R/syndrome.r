#####
# Syndromes definitions and computation
#####

#' List of coded values for fever
#' names are pretty labels, values coded
FEVER.CODES = c('<37'=0,'[37,37.5)'=1,'[37.5,38)'=2,'[38,39)'=3,'[39,40)'=4, '>40'=5)

#' Regroup mixed syndromes to non-specific for the 2012's influezanet syndromes set
#'
#' Resulting levels are more pretty : "no.symptom", "ili", "cold", "gastro", "non.specific", "allergy"
#'
#' @param x values to recode
#'
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

#' Compute syndrome classification of weekly data according to 2012's InfluenzaNet rules
#'
#' This function do not require the data to be in the weekly table (i.e. do not use the DB view). So it is useables with
#' generated data or without the weekly id
#' @export
#' @param r weekly data (as return by \code{\link{survey_load_results}})
#' @param as.levels if TRUE returns factors
#' @seealso \code{\link{syndromes.set}}
#'
syndromes_influenzanet_2012 = function(r, as.levels=F) {
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
#' @export
#' @seealso \code{\link{syndromes.set}}
#' @param r data.frame of weekly results
#' @param as.levels return levels instead of numeric values
syndromes_influenzanet_2011 = function(r, as.levels=F) {
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
#'
#' by default, transform to pretty names (for R) from the same set ()
#' @export
#' @param x vector of syndrome name to prettyfy
#' @param pretty.set template name to use, entry in \code{\link{syndromes.set}}
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
#'
#' Defines each known syndromes set, with possible levels, pretty levels (regrouped version)
#' also define a provider function, usable to compute the syndrome classification from weekly data
#'
#'
#' @export
syndromes.set = list(
  'influenzanet.2011'=list(
    levels=c('NO-SYMPT','ILI','COMMON-COLD','GASTRO','OTHER'),
    provider=syndromes_influenzanet_2011
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
    provider=syndromes_influenzanet_2012
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
