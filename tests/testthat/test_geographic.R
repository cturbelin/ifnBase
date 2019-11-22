context("Geographic functions")

my_hierarchy =c('zip','nuts3','nuts2','nuts1','country')

platform_geographic_levels(my_hierarchy, table="geo_zip")

test_that("Hierarchy", {
  h = geo_hierarchy()
  expect_equal(h, my_hierarchy)
})

test_that("Geographic navigation", {
  # Upper
  expect_equal(geo_level_nav("zip", "upper"), "nuts3")
  expect_equal(geo_level_nav("nuts2", "upper"), "nuts1")
  expect_equal(geo_level_nav("nuts1", "upper"), "country")

  # Outer bound
  expect_identical(geo_level_nav("country", "upper"), NA)
  expect_equal(geo_level_nav("zip", "lower"), NA)

  # Lower
  expect_equal(geo_level_nav("nuts2", "lower"), "nuts3")
  expect_equal(geo_level_nav("nuts1", "lower"), "nuts2")
  expect_equal(geo_level_nav("nuts3", "lower"), "zip")

  # Wrong value
  expect_error(geo_level_nav("nuts1", "toto"))
})
