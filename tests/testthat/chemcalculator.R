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

# Test the moles_grams_converter function for correct input
test_that("moles_grams_converter() errors if input is not a chemical formula", {
    expect_error(moles_grams_converter("[H2O]", 1, "moles"))
    expect_error(moles_grams_converter("CuSO4-5H2O", 1, "moles"))
    expect_error(moles_grams_converter("CuSO4-5H2O", 1, "moles"))
    expect_error(moles_grams_converter("naOH", 1, "moles"))
    expect_error(moles_grams_converter("(nH4)HS", 1, "moles"))
    expect_error(moles_grams_converter("NaaaaaaaaOH", 1, "moles"))
})
