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

tests = list(
  # Une liste de ce type pour chaque test
  # Weekly indiquer les valeurs, les symptomes non indiqués seront mis à FALSE
  list( # Test with no.sympt
    weekly = list(no.sympt = TRUE, fever.sudden = NA, highest.temp =0, sympt.sudden = NA),
    age = 5,
    expected = list(ari.ecdc=FALSE)
  ),
  list( # Test ari.ecdc with fever.sudden, minimal sympt
    weekly = list(fever = TRUE, fever.sudden = TRUE, highest.temp =0, sympt.sudden = NA, cough=TRUE),
    age = 5,
    expected = list(ari.ecdc=TRUE)
  ),
  # Test ari.ecdc with fever.sudden
  list(
    weekly = list(fever = TRUE, fever.sudden = NA, highest.temp =0, sympt.sudden = TRUE, cough=TRUE),
    age = 5,
    expected = list(ari.ecdc=TRUE)
  ),
  list( # Test cough = FALSE
    weekly = list(fever = TRUE, fever.sudden = TRUE, highest.temp =0, sympt.sudden = NA, cough=FALSE),
    age = 5,
    expected = list(ari.ecdc=FALSE)
  ),
  list( # Test dyspnea
    weekly = list(fever = TRUE, fever.sudden = NA, highest.temp =0, sympt.sudden = TRUE, dyspnea=TRUE),
    age = 5,
    expected = list(ari.ecdc=TRUE)
  ),
  list( # Test dyspnea
    weekly = list(fever = TRUE, fever.sudden = NA, highest.temp =0, sympt.sudden = TRUE, dyspnea=FALSE),
    age = 5,
    expected = list(ari.ecdc=FALSE)
  ),
  list( # Test sorethroat
    weekly = list(fever = TRUE, fever.sudden = NA, highest.temp =0, sympt.sudden = TRUE, sorethroat=TRUE),
    age = 5,
    expected = list(ari.ecdc=TRUE)
  ),
  list( # Chills
    weekly = list(chills = TRUE, fever.sudden = NA, highest.temp =0, sympt.sudden = TRUE, cough=TRUE),
    age = 20,
    expected = list(ari.ecdc=TRUE)
  ),
  list( # asthenia
    weekly = list(asthenia = TRUE, fever.sudden = NA, highest.temp =0, sympt.sudden = TRUE, cough=TRUE),
    age = 20,
    expected = list(ari.ecdc=TRUE)
  ),
  list( # asthenia=FALSE, fever=T
    weekly = list(asthenia = FALSE, fever=TRUE, fever.sudden = NA, highest.temp =0, sympt.sudden = TRUE, cough=TRUE),
    age = 20,
    expected = list(ari.ecdc=TRUE)
  ),
  list( # Pain is not counted for <= 5, so no general sympt
    weekly = list(pain = TRUE, fever.sudden = NA, highest.temp =0, sympt.sudden = TRUE, cough=TRUE),
    age = 5,
    expected = list(ari.ecdc=FALSE)
  ),
  list( # Pain with age > 5
    weekly = list(pain = TRUE, fever.sudden = NA, highest.temp =0, sympt.sudden = TRUE, cough=TRUE),
    age = 20,
    expected = list(ari.ecdc=TRUE, ari=TRUE)
  )

)

test_that("Syndrome provider", {

  symptomes = get_symptoms_aliases()
  provider = SyndromeProviderRS2019$new(pain.age.limit = 5)

  for(test in tests) {
    weekly = data.frame(test$weekly, person_id=1L, id=1L)
    intake = data.frame(person_id=1L, age=test$age)

    for(sd in symptomes) {
      if(!hasName(weekly, sd)) {
        weekly[[sd]] = FALSE
      }
    }

    r = provider$compute(weekly=weekly, intake=intake)
    for(sd in names(test$expected)) {
      expect_equal(test$expected[[sd]], r[[sd]])
    }
  }
})
