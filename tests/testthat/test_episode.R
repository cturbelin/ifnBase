context("Episode fusion")


tests = list(
  strategies = list(
    "highest.temp"=list(
      raw=c(5L, 4L, 3L, 2L, 1L, 0L)
    ),
    "change.routine"=list(
      codes=c('Yes','DNK','No')
    )
  ),
  sets = list(
    list(
      name="highest.temp",
      values = c(1L,2L,3L, NA),
      expected=3L
    ),
    list(
      name="highest.temp",
      values = c(NA,3L),
      expected=3L
    ),
    list(
      name="highest.temp",
      values = as.integer(c(NA,NA,NA)),
      expected=as.integer(NA)
    ),
    list(
      name="change.routine",
      values=c("No","No","DNK"),
      expected="DNK"
    )
  )
)

test_that("episode_fusion.worst_strategy", {

  strategy = list(vars=tests$strategies)

  for(i in seq_along(tests$sets)) {
    test = tests$sets[[i]]
    v = rlang::list2(!!test$name := test$values)
    data = data.frame(list(person_id=i), episode=1, v)

    r = episode_fusion.worst_strategy(strategy, weekly=data, episode.column='episode')

    result = r[[test$name]]
    if(is.na(test$expected)) {
      expect_equivalent(result, NA)
    } else {
      if(is.character(test$expected) && is.factor(result)) {
        result = as.character(result)

      }
      expect_equal(result, test$expected)
    }
  }
})

