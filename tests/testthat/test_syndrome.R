context("Syndromes")

# Symptomes
# "no.sympt", "fever", "chills", "rhino", "sneeze", "sorethroat",
# "cough", "dyspnea", "headache", "pain", "chestpain", "asthenia",
#" "anorexia", "sputum", "wateryeye", "nausea", "vomiting", "diarrhea", "abdopain", "sympt.other"

# Higest temp : mettre le code numérique pas le libellé
# FEVER.CODES = c('<37'=0,'[37,37.5)'=1,'[37.5,38)'=2,'[38,39)'=3,'[39,40)'=4, '>40'=5)

setup({
  platform_define_survey("weekly", template="eu:weekly")

})

# Provide a default values except for the category in test
test_ari = function(test, ...) {
  r = list(...)
  provides = c('general','respi','sudden')
  provides = provides[ !provides %in% test]
  if("general" %in% provides) {
    r$fever = TRUE
  }
  if("respi" %in% provides) {
    r$cough = TRUE
  }
  if("sudden" %in% provides) {
    r$fever.sudden = TRUE
    r$sympt.sudden = TRUE
  }
  r
}


tests = list(
  # Une liste de ce type pour chaque test
  # Weekly indiquer les valeurs, les symptomes non indiqués seront mis à FALSE
  list( # Test with no.sympt
    weekly = list(no.sympt = TRUE, fever.sudden = NA, highest.temp =0, sympt.sudden = NA),
    age = 5,
    expected = list(ili.ecdc=FALSE)
  ),
  list( # Test ili.ecdc with fever.sudden, minimal sympt
    weekly = test_ari("sudden", fever.sudden=TRUE, sympt.sudden = NA),
    age = 5,
    expected = list(ili.ecdc=TRUE)
  ),
  # Test ili.ecdc with fever.sudden
  list(
    weekly = test_ari("sudden", fever.sudden=NA, sympt.sudden = TRUE),
    age = 5,
    expected = list(ili.ecdc=TRUE)
  ),
  list( # Test cough = FALSE
    weekly = test_ari("respi", cough=FALSE),
    age = 5,
    expected = list(ili.ecdc=FALSE)
  ),
  list( # Test cough
    weekly = test_ari("respi", cough=TRUE),
    age = 5,
    expected = list(ili.ecdc=TRUE)
  ),
  list( # Test dyspnea
    weekly =  test_ari("respi", dyspnea=TRUE),
    age = 5,
    expected = list(ili.ecdc=TRUE)
  ),
  list( # Test dyspnea
    weekly = test_ari("respi", dyspnea=FALSE),
    age = 5,
    expected = list(ili.ecdc=FALSE)
  ),
  list( # Test sorethroat
    weekly = test_ari("respi", sorethroat=TRUE),
    age = 5,
    expected = list(ili.ecdc=TRUE)
  ),
  list( # Chills
    weekly = test_ari("general", chills=TRUE),
    age = 20,
    expected = list(ili.ecdc=TRUE)
  ),
  list( # asthenia
    weekly = test_ari("general", asthenia=TRUE),
    age = 20,
    expected = list(ili.ecdc=TRUE)
  ),
  list( # asthenia=FALSE, fever=T
    weekly = test_ari("general", asthenia = FALSE, fever=TRUE),
    age = 20,
    expected = list(ili.ecdc=TRUE)
  ),
  list( # Pain is not counted for <= 5, so no general sympt
    weekly = test_ari("general", pain=TRUE),
    age = 5,
    expected = list(ili.ecdc=FALSE)
  ),
  list( # Pain with age > 5
    weekly =test_ari("general", pain=TRUE),
    age = 20,
    expected = list(ili.ecdc=TRUE, ari=TRUE)
  )

)

test_that("Syndrome provider", {

  symptomes = get_symptoms_aliases()
  provider = SyndromeProviderRS2019$new(pain.age.limit = 5)

  for(i in seq_along(tests)) {
    test = tests[[i]]
    weekly = data.frame(test$weekly, person_id=1L, id=1L)
    intake = data.frame(person_id=1L, age=test$age)

    for(sd in symptomes) {
      if(!hasName(weekly, sd)) {
        weekly[[sd]] = FALSE
      }
    }

    r = provider$compute(weekly=weekly, intake=intake)
    for(sd in names(test$expected)) {
      expect_equal(test$expected[[sd]], r[[sd]], info=i)
    }
  }
})
