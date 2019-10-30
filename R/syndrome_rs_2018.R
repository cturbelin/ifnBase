##
## Syndrome computation : archived version use in 2018 (not used any more will be upgraded)
##

#' Compute syndromes definitions used in Guerrisi C, Euro Surv. 2018.
#'
#' This function embeds in one function to reduce number of available functions since it's provided for archive purpose
#' This function is deprecated.
#' @return list() list of functions
#' @export
#'
#' @details
#' \describe{
#'   \item{compute(weekly, intake)}{returns a data.frame() with a column for each syndrome definition and "id" as weekly survey id}
#' }
#'
syndromes_rs_2018 = function() {

  ##
  # French syndrome computation function

  # Get the ILI French Sentinel network syndrome
  # @param r data.frame
  # @param infer.fever.level = how to infer the fever level when it's missing (low = consider it's low, "high" consider it's high)
  # @param pain.age.limit, if defined ignore "pain" criterium for age under this limit (=consider always true for pain symptom)
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

  # Compute ili.fever syndrome classification
  # Customization of ecdc
  # @param r weekly data
  # @param level if TRUE take fever level into account, if FALSE only uses symptom "fever"
  # @param include.miss if TRUE include DK and missing fever level values in "highest.temp"
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


  # Compute a generic gastroenteriritis definition (broader than French Sentinel definition)
  # @param r data.frame() with syndrom columns
  symptoms.gastro = function(r) {
    r$nausea | r$vomiting | r$diarrhea | r$abdopain
  }

  #  Compute gastroenteriritis definition according to French Sentinel's definition
  # @param r data.frame() with syndrom columns
  symptoms.gastro.rs = function(r) {
    r$diarrhea
  }

  # Acute Upper Respiratory Infection
  # @param r data.frame() with syndrom columns
  # @param wide.respiratory consider a wider respiratory syndrome if TRUE
  symptoms.ira <- function(r, wide.respiratory=F) {
    sudden = !is.na(r$sympt.sudden) & r$sympt.sudden
    if(wide.respiratory) {
      respi = r$cough | r$rhino | r$sneeze | r$sorethroat | r$dyspnea | r$sputum
    } else {
      respi = r$cough | r$rhino | r$sneeze
    }
    sudden & (r$fever | r$chills | r$asthenia | r$headache | r$pain) & (respi)
  }

  # Compute GROG (French Influenza Surveillance historical association)'s ARI definition
  # Sudden syndrome with at least one general symptom and respiratory (cough or coryza or rhinitis)
  # @param r weekly data
  symptoms.grog  <-  function(r) {
    sudden = !is.na(r$sympt.sudden) & r$sympt.sudden
    coryza = r$rhino & r$sneeze & r$wateryeye
    rhinitis = r$rhino & r$sorethroat & r$wateryeye
    respi = r$cough | coryza | rhinitis
    sudden & (r$fever | r$chills | r$asthenia | r$headache | r$pain) & (respi)
  }

  # Compute ECDC ILI Definition
  # @param r weekly data
  # @param with.chills use chills-like symptoms
  symptoms.ecdc <- function(r, with.chills=FALSE) {
    sudden = !is.na(r$sympt.sudden) & r$sympt.sudden
    general = r$fever | r$headache | r$pain | r$asthenia
    if(with.chills) {
      general = general | r$chills
    }
    sudden & general & (r$sorethroat | r$cough | r$dyspnea)
  }

  # Compute syndromes classification using several definitions, for French incidence computation
  #
  # Return data.frame of French (rs) syndrome computed for each row of the weekly df
  # @param weekly data.frame() of weekly results (should have the necessary columns to compute it)
  # @param intake intake data, (with age column)
  compute_syndromes <- function(weekly, intake) {

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

  list(
    "compute"=compute_syndromes,
    "ili.rs"=symptoms.ili.rs,
    "ili.fever"=symptoms.ili.fever,
    "ira"=symptoms.ira,
    "grog"=symptoms.grog,
    "ecdc"=symptoms.ecdc,
    "gastro.rs"=symptoms.gastro.rs,
    "gastro"=symptoms.gastro
  )

}
