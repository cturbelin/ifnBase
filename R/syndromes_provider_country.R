
# # symptoms
# 'no.sympt'='Q1_0',
# 'fever'='Q1_1',
# 'chills'='Q1_2',
# 'rhino'='Q1_3',
# 'sneeze'='Q1_4',
# 'sorethroat'='Q1_5',
# 'cough'='Q1_6',
# 'dyspnea'='Q1_7',
# 'headache'='Q1_8',
# 'pain'='Q1_9',
# 'chestpain'='Q1_10',
# 'asthenia'='Q1_11',
# 'anorexia'='Q1_12',
# 'sputum'='Q1_13',
# 'wateryeye'='Q1_14',
# 'nausea'='Q1_15',
# 'vomiting'='Q1_16',
# 'diarrhea'='Q1_17',
# 'abdopain'='Q1_18',
# 'sympt.other'='Q1_19',


#'
#' Compute country specific classifier for ILI syndrome.
#' @param country country code
#' @param weekly weekly survey data
#' @param intake intake survey data
#' @param season season number see \code{\link{calc_season}}
#' @param use.fever.level if TRUE take into account of the reported fever level
#' @param unknown.has.fever = if TRUE consider that unknown fever level has fever at the right level
#' @param min.syndrome.count in case of imprecise definition about number of symptom, use this number as mininum to consider the syndrom
#'
syndrome_provider_ili_country = function(country, weekly, intake, season, use.fever.level=T, unknown.has.fever=T, min.syndrome.count=1) {

  # Count
  at_least = function(d, columns, n=1) {
    apply(d[, columns, drop=FALSE], 1, function(r) { sum(r) >= n})
  }

  is_sudden = function(r) {
    (!is.na(r$sympt.sudden) & r$sympt.sudden) | (!is.na(r$fever.sudden) & r$fever.sudden)
  }

  sudden = is_sudden(weekly)

  if(use.fever.level) {
    has_fever_38 = weekly$fever & ifelse(is.na(weekly$highest.temp), unknown.has.fever, weekly$highest.temp >= 3)
    has_fever_39 = weekly$fever & ifelse(is.na(weekly$highest.temp), unknown.has.fever, weekly$highest.temp >= 4)
  } else {
    has_fever_38 = weekly$fever
    has_fever_39 = weekly$fever
  }

  ili = NULL


  if(country == "ES") {
    # The present definition of ILI case is the one proposed by the European Union:
    #
    #   - sudden onset of symptoms
    # - at least one of the four general symptoms: fever, malaise, headache, myalgia
    # - at least one of the three respiratory symptoms: cough, sore throat, dispnea
    # - lack of other suspected symptoms

    ili = sudden & at_least(weekly, c('fever', 'asthenia', 'headache', 'pain'), n=1) & at_least(weekly, c('cough', 'sorethroat', 'dyspnea'), n=1)

  }

  if(country == "IE") {
    # Influenza-like illness is characterised by the sudden onset of symptoms with a temperature of 38°C or more,
    # in the absence of any other disease, with at least two of the following: dry cough, headache, sore muscles and a sore throat.
    ili = has_fever_38 & sudden & at_least(weekly, c('cough', 'sorethroat', 'headache', 'pain'), n=2)

  }

  if(country == "IT") {
    # Since 2014-2015 the case definition has been modified to adapt it to the one used by ECDC:
    #   - sudden onset
    # - at least one of these general symptoms: fever, malaise, headache, muscle pain
    # - at least one of these respiratory symptoms: cough, sore throat, heavy breath
    #
    # Before 2014-2015 the definition would include the measurement of the fever:
    #   - sudden onset
    # - fever above 38 °C with at least one of the general symptoms: headache, malaise, chills, asthenia and at
    # least one of these respiratory symptoms: cough, sore throat, nasal congestion
    if(season >= 2014) {
      ili = sudden & at_least(weekly, c('fever', 'asthenia', 'headache', 'pain')) & at_least(weekly, c('cough','sorethroat','dyspnea'))
    } else {
      ili = sudden & has_fever_38 & at_least(weekly, c('headache', 'asthenia', 'chills', 'asthenia')) & at_least(weekly, c('cough','sorethroat','rhino'))
    }
  }

  if(country == "NL") {
    # Not the standardized WHO or EU case definition.
    # The case definition used according to the ‘PEL criteria': acute onset AND rectal temperature >38°C AND
    # at least one of the following symptoms: cough, coryza, sore throat, frontal headache, retrosternal pain, myalgia.
    # Provided by the Member State through a survey.

    ili = sudden & has_fever_38 & at_least(weekly, c('cough','rhino', 'sorethroat','headache','chestpain','pain'), n=1)
    # What about sneezing ?

  }

  if(country == "BE") {
    # Not the standardized WHO or EU ILI case definition. The case definition used is:
    # sudden onset of fever with respiratory symptoms AND general symptoms. Provided by the Member State through a survey
    ili =  sudden & at_least(weekly, c('headache',  'chills', 'asthenia','pain'), n=min.syndrome.count) & at_least(weekly, c('cough','sorethroat','rhino','dyspnea'), n=min.syndrome.count)
  }

  if(country == "SE") {
    ili = weekly$ili
  }

  if(country == "PT") {
    #
    # Início súbito,
    #
    # + 1 dos seguintes sintomas sistémicos:
    # -Febre ou febrícula,
    # -Mal-estar, debilidade, prostração,
    # -Cefaleia,
    # -Mialgias ou dores generalizadas.
    #
    # + 1 dos seguintes sintomas respiratórios:
    # - Tosse,
    # - Dor de garganta ou inflamação da mucosa nasal ou faríngea sem sinais respiratórios relevantes,
    # - Dificuldade respiratória.
    ili = sudden & at_least(weekly, c('fever','chills','asthenia', 'headache','pain'), n=1) & at_least(weekly, c('cough','sorethroat','rhino', 'sneeze','dyspnea'), n=1)

  }

  if(country == "DK") {
    # “The Danish sentinel ILI case def. is;
    # sudden onset of fever or feverishness (chills) AND any symptom of malaise, headache or muscle pain
    # AND at least one of the following symptoms: cough, sore throat or shortness of breath”.

    ili = sudden & at_least(weekly, c('fever','chills')) & at_least(weekly, c('asthenia', 'headache','pain'), n=1) & at_least(weekly, c('cough','sorethroat','dyspnea'), n=1)
  }

  if(country == "FR") {
    # respi = r$sorethroat | r$cough | r$dyspnea
    # fever = r$fever & (!is.na(r$highest.temp) & r$highest.temp %in% c(3:5))
    # Not exact definition, but the one selected on french's data study

    ili = sudden &  at_least(weekly, c('sorethroat','cough','dyspnea'), n=min.syndrome.count) & has_fever_39 & at_least(weekly, c('pain','headache'))

  }

  if(country == "UK") {
    # No specific definition..By default use
    ili = weekly$ili
  }

  ili
}
