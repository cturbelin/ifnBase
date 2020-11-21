context("Episode fusion")


setup({
 library(dplyr)
 platform_define_survey("weekly", template="eu:weekly")
})

tests = list(
  strategies = list(
    "highest.temp"=list(
      raw=c(5L, 4L, 3L, 2L, 1L, 0L)
    ),
    "change.routine"=list(
      codes=c('routine.yes','routine.off','routine.no')
    )
  ),
  sets = list(
    list(
      name="highest.temp",
      values = c(1L,2L,3L, NA),
      expected=3L,
      recode=TRUE
    ),
    list(
      # Test with recoded values
      name="highest.temp",
      values = factor(c('[37,37.5)','[37.5,38)','[38,39)', NA)),
      expected='[38,39)'
    ),
    list(
      name="highest.temp",
      values = c(NA,3L),
      expected=3L,
      recode=TRUE
    ),
    list(
      name="highest.temp",
      values = as.integer(c(NA,NA,NA)),
      expected=as.integer(NA)
    ),
    list(
      name="change.routine",
      values=c('routine.no','routine.no','routine.off'),
      expected='routine.off'
    )
  )
)

#strategy = list(vars=tests$strategies)

for(i in seq_along(tests$sets)) {
  test = tests$sets[[i]]

  test_that(paste0("episode_fusion.worst_strategy:",i), {

      v = rlang::list2(!!test$name := test$values)
      data = data.frame(list(person_id=i), episode=1, v)

      strategy.def = tests$strategies[[test$name]]
      v$type = "worst"
      if(!is.null(strategy.def$raw)) {
        v[[test$name]] = raw_value(strategy.def$raw)
      } else {
        v[[test$name]] = factor(strategy.def$codes)
      }

      strategy = do.call(episode_strategy, v)

      r = try( ifnBase:::episode_fusion.worst_strategy(strategy, weekly=data, episode.column='episode'))

      if(is(r, "try-error")) {
        str(data)
        str(strategy.def)
        str(strategy)
        fail(paste("Strategy failed"))
      }


      result = r[[test$name]]
      if(is.na(test$expected)) {
        expect_equivalent(result, NA)
      } else {
        if(is.character(test$expected) && is.factor(result)) {
          result = as.character(result)

        }
        expect_equal(result, test$expected)
      }
      ## Try with recoded values
      if(isTRUE(test$recode)) {
        d = data
        d[[test$name]] = survey_recode(d[[test$name]], variable = test$name, survey="weekly")
        r = ifnBase:::episode_fusion.worst_strategy(strategy, weekly=d, episode.column='episode')
        result = r[[test$name]]
        expected = test$expected

        if(is.na(expected)) {
          expect_equivalent(result, NA)
        } else {
          expected = as.character(survey_recode(expected, variable = test$name, survey="weekly"))
          result = as.character(result)
          expect_equal(result, expected)
        }
      }

  })

}

