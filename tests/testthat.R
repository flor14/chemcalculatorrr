library(testthat)
library(chemcalculatorrr)

test_check("chemcalculatorrr")





# Test the percent_mass function for correct output
test_that("precent_mass is not calculated correctly", {
  expect_equal(percent_mass("H2O", "O"), 88.819)
  expect_equal(percent_mass("H2O", "H"), 5.59)
  expect_equal(percent_mass("H2O", "H2"), 11.181)
  expect_equal(percent_mass("NaOH", "OH"), 42.519)
  expect_equal(percent_mass("H2SO4", "SO4"), 97.946)
  expect_equal(percent_mass("H2SO4", "HSO4"), 98.973)
  expect_equal(percent_mass("Na(H2SO4)3", "NaH2SO4"), 38.165)
})


# Check that error is raised when input is incorrect for percent_mass
test_that("Error should be raised when the input is incorrect") {
  expect_error(percent_mass("H2SO4", "NaSO4"))
  expect_error(percent_mass("H2SO4", "Al"))
  expect_error(percent_mass("H2SO4", "H2SO4C5"))
  expect_error(percent_mass("H2SO4", "H3"))
  expect_error(percent_mass("H2SO4", "H3S5"))
  expect_error(percent_mass(".", "H2O"))
  expect_error(percent_mass("NaOH", "sss"))
  expect_error(percent_mass(3, "O"))
}


