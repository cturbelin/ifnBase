
context("Surveys")

test_that("survey_variable_available", {
  survey = platform_define_survey("test_survey_variable_available",
      mapping=list(
        var1=variable_available("Q1", 2011:2017),
        var2=variable_available("Q2", quo(season > 2015)),
        var3=variable_available("Q3", quo(season==2016)),
        var4=variable_available("Q4", quo(season < 2014)),
        var5="Q5"
      )
  )

  always = c('var5','extra')

  tests = list(
    list(2011, c('var1','var4', always)),
    list(2014, c('var1', always)),
    list(2015, c('var1', always)),
    list(2016, c('var1','var2','var3',always)),
    list(2017, c('var1','var2',always)),
    list(2018, c('var2',always)),
    list(2019, c('var2',always))
  )

  for(i in seq_along(tests)) {
    test = tests[[i]]
    season = test[[1]]
    expect = test[[2]]
    vars = survey_variable_available(c('var1','var2','var3','var4','var5','extra'), survey, season=season)
    expect_setequal(vars, expect)
  }


})
