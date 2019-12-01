context("Test platform functions")

check_list_mapping = ifnBase:::check_list_mapping

raiser = function() {
  values = list()
  list(
    values = function() {
      return(values)
    },
    raise = function(type, value, problem, message) {
      values[[length(values) + 1]] <<- list(type=type, value=value, problem=problem, message=message)
    }
  )
}

test_that("Check list mapping error override", {
    rr = raiser()
    new = list("var1"="Q1")
    old = list( "var1"="Q3")
    check_list_mapping(new, old, raise=rr$raise)
    err = rr$values()
    expect_length(err, 1)
    err = err[[1]]
    expect_equal(err$type, "error")
    expect_equal(err$value, "Q3")
    expect_equal(err$problem, "conflict")
})


test_that("Check list mapping error duplicate", {
  rr = raiser()
  new = list("var1"="Q1", "var4"="Q1")
  old = list( "var2"="Q2")
  check_list_mapping(new, old, raise=rr$raise)
  err = rr$values()
  expect_length(err, 1)
  err = err[[1]]
  expect_equal(err$type, "error")
  expect_equal(err$value, "Q1")
  expect_equal(err$problem, "duplicate")
})

test_that("Check list mapping error override", {
  rr = raiser()
  new = list("var1"="Q1")
  old = list( "var2"="Q1")
  check_list_mapping(new, old, raise=rr$raise)
  err = rr$values()
  expect_length(err, 1)
  err = err[[1]]
  expect_equal(err$type, "error")
  expect_equal(err$value, "Q1")
  expect_equal(err$problem, "override")
})

test_that("Check list mapping allowing override (only warn)", {
  rr = raiser()
  new = list("var1"=ifnBase::override("Q1"))
  old = list( "var2"="Q1")
  check_list_mapping(new, old, raise=rr$raise)
  err = rr$values()
  expect_length(err, 1)
  err = err[[1]]
  expect_equal(err$type, "warn")
  expect_equal(err$value, override("Q1"))
  expect_equal(err$problem, "override")
})

test_that("Check platform options", {
  flags = c("use.country","first.season.censored", "debug.query")
  for(flag in flags) {
    args = list()
    args[[flag]] = TRUE
    do.call(platform_options, args)
    expect_true(platform_env(flag))
    args[[flag]] = FALSE
    do.call(platform_options, args)
    expect_false(platform_env(flag))
  }

})

test_that("Check use_country", {
  env = platform_env()
  env$COUNTRY_CODES = "FR"
  platform_options(use.country=FALSE)
  func = ifnBase:::can_use_country
  expect_error(func("FR"))
  expect_false(func(NULL))
  platform_options(use.country=TRUE)
  expect_true(func("FR"))
  expect_false(func(NULL))
})

