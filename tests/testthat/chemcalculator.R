# Check that compute_mass function for correct output:
testthat("compute_mass is not calculating correctly", {
  expect_equal(compute_mass('H2O'), 18.013)
  expect_equal(compute_mass('C12H22O11'), 342.275)
  expect_equal(compute_mass('Al2(SO4)3'), 342.147)
  expect_equal(compute_mass('(NH4)HS'), 51.107)
})

# Check that compute_mass raises error when input is invalid:
test_that("Error should be raised when the input is incorrect", {
  expect_error(compute_mass(c('H2O')))
  expect_error(compute_mass('CuSO4-5H2O'))
  expect_error(compute_mass('naOH'))
  expect_error(compute_mass('(nH4)HS'))
  expect_error(compute_mass('NaaaaaaaaOH'))
})

# Test the moles_grams_converter function for correct output
test_that("moles_grams_converter() converts grams to moles", {
    expect_equal(moles_grams_converter("H2O", 18.01528, "grams"), 1, tolerance = 1)
})

test_that("moles_grams_converter() converts moles to grams", {
    expect_equal(moles_grams_converter("H2O", 1, "grams"), 18.015, tolerance = 1)
})

test_that("moles_grams_converter() errors if input is 'tons'", {
    expect_error(moles_grams_converter("H2O", 1, "tons"))
})

# Check that error is raised when input is incorrect for moles_grams_converter
test_that("moles_grams_converter() errors if input is not a chemical formula", {
    expect_error(moles_grams_converter("[H2O]", 1, "moles"))
    expect_error(moles_grams_converter("CuSO4-5H2O", 1, "moles"))
    expect_error(moles_grams_converter("CuSO4-5H2O", 1, "moles"))
    expect_error(moles_grams_converter("naOH", 1, "moles"))
    expect_error(moles_grams_converter("(nH4)HS", 1, "moles"))
    expect_error(moles_grams_converter("NaaaaaaaaOH", 1, "moles"))
})


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
