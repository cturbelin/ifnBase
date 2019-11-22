context("Test week functions")

test_that("iso_yearweek", {
  expect_equal(iso_yearweek(as.Date("2019-01-01")), 201901)
  expect_equal(iso_yearweek(as.Date("2017-01-01")), 201652)
  expect_equal(iso_yearweek(as.Date("2016-01-01")), 201553)
  expect_equal(iso_yearweek(as.Date("2014-01-01")), 201401)
  expect_equal(iso_yearweek(as.Date("2010-01-01")), 200953)
})

test_that("iso_yearweek", {

  for(y in 2000:2019) {

    d = as.Date(paste0(y,"-01-04"))
    yw =  (y * 100 ) + 1
    expect_equal(iso_yearweek(d), yw)
  }
})

test_that("monday_of_week", {
  expect_equal(monday_of_week(201901), as.Date("2018-12-31"))
  expect_equal(monday_of_week(201801), as.Date("2018-01-01"))
  expect_equal(monday_of_week(201701), as.Date("2017-01-02"))
  expect_equal(monday_of_week(201553), as.Date("2015-12-28"))
})

test_that("monday_of_year", {
  tt = list(
    list(2019, "2018-12-31"),
    list(2018, "2018-01-01"),
    list(2017, "2017-01-02"),
    list(2016, "2016-01-04"),
    list(2009, "2008-12-29"),
    list(2001, "2001-01-01"),
    list(2000, "2000-01-03")
  )
  for(t in tt) {
    rr = expect_equal(monday_of_year(t[[1]]), as.Date(t[[2]]))
  }
})
