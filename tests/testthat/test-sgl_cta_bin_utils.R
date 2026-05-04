## Suite 1 ##
# input
input_data <- 0:10
bin_num <- 5
scale <- new_sgl_scale_linear()
# expected calculated values
expected_bin_limits <- c(-.010, 1.994, 3.998, 6.002, 8.006, 10.010)
expected_bin_indices <- c(1, 1, 2, 2, 3, 3, 3, 4, 4, 5, 5)
expected_bin_centers <- c(0.992, 2.996, 5, 7.004, 9.008)
expected_bin_values <- c(
  0.992, 0.992, 2.996, 2.996, 5, 5, 5, 7.004, 7.004, 9.008, 9.008
)
test_that("bin_limits creates correct bin limits", {
  expect_equal(
    bin_limits(input_data, bin_num),
    expected_bin_limits
  )
})

test_that("bin_indices gives correct bin indices", {
  expect_equal(
    bin_indices(input_data, expected_bin_limits),
    expected_bin_indices
  )
})

test_that("bin_centers gives correct bin centers", {
  expect_equal(
    bin_centers(expected_bin_limits),
    expected_bin_centers
  )
})

test_that("bin_values gives correct bin values", {
  expect_equal(
    bin_values(input_data, bin_num, scale),
    expected_bin_values
  )
})

## Suite 2 ##
# input
input_data <- 10
bin_num <- 5
scale <- new_sgl_scale_linear()
# expected calculated values
expected_bin_limits <- c(9.990, 9.994, 9.998, 10.002, 10.006, 10.010)
expected_bin_indices <- 3
expected_bin_centers <- c(9.992, 9.996, 10.000, 10.004, 10.008)
expected_bin_values <- 10

test_that("bin_limits creates correct bin limits when max and min are equal", {
  expect_equal(
    bin_limits(input_data, bin_num),
    expected_bin_limits
  )
})

test_that("bin_indices gives correct bin index when max and min are equal", {
  expect_equal(
    bin_indices(input_data, expected_bin_limits),
    expected_bin_indices
  )
})

test_that("bin_centers gives correct bin centers when max and min are equal", {
  expect_equal(
    bin_centers(expected_bin_limits),
    expected_bin_centers
  )
})

test_that("bin_values gives correct bin values when max and min are equal", {
  expect_equal(
    bin_values(input_data, bin_num, scale),
    expected_bin_values
  )
})

## Suite 3 ##
# input
input_data <- 0:10
bin_num <- 1
scale <- new_sgl_scale_linear()
# expected calculated values
expected_bin_limits <- c(-.010, 10.010)
expected_bin_indices <- rep(1, 11)
expected_bin_centers <- 5
expected_bin_values <- rep(5, 11)

test_that("bin_limits creates correct bin limits for single bin", {
  expect_equal(
    bin_limits(input_data, bin_num),
    expected_bin_limits
  )
})

test_that("bin_indices gives correct bin index for single bin", {
  expect_equal(
    bin_indices(input_data, expected_bin_limits),
    expected_bin_indices
  )
})

test_that("bin_centers gives correct bin centers for single bin", {
  expect_equal(
    bin_centers(expected_bin_limits),
    expected_bin_centers
  )
})

test_that("bin_values gives correct bin values for single bin", {
  expect_equal(
    bin_values(input_data, bin_num, scale),
    expected_bin_values
  )
})

## Suite 4 ##
# input
input_data <- c(NA, 1:4, NA)
bin_num <- 2
scale <- new_sgl_scale_linear()
# expected calculated values
expected_bin_limits <- c(0.997, 2.5, 4.003)
expected_bin_indices <- c(NA, 1, 1, 2, 2, NA)
expected_bin_centers <- c(1.7485, 3.2515)
expected_bin_values <- c(NA, 1.7485, 1.7485, 3.2515, 3.2515, NA)

test_that("bin_limits creates correct bin limits with missing values", {
  expect_equal(
    bin_limits(input_data, bin_num),
    expected_bin_limits
  )
})

test_that("bin_indices gives correct bin index with missing values", {
  expect_equal(
    bin_indices(input_data, expected_bin_limits),
    expected_bin_indices
  )
})

test_that("bin_centers gives correct bin centers with missing values", {
  expect_equal(
    bin_centers(expected_bin_limits),
    expected_bin_centers
  )
})

test_that("bin_values gives correct bin values with missing values", {
  expect_equal(
    bin_values(input_data, bin_num, scale),
    expected_bin_values
  )
})

## Suite 5 ##
# input
input_data <- rep(NA, 5)
bin_num <- 5
scale <- new_sgl_scale_linear()
# expected calculated values
expected_bin_values <- input_data
test_that("bin_values returns all NA's if all input values are NA's", {
  expect_equal(
    bin_values(input_data, bin_num, scale),
    expected_bin_values
  )
})

## Suite 6 ##
# input
input_data <- 10^(0:10)
bin_num <- 5
scale <- new_sgl_scale_log()
# expected calculated values
expected_scaled_input_data <- 0:10
expected_bin_limits <- c(-.010, 1.994, 3.998, 6.002, 8.006, 10.010)
expected_bin_indices <- c(1, 1, 2, 2, 3, 3, 3, 4, 4, 5, 5)
expected_bin_centers <- c(0.992, 2.996, 5, 7.004, 9.008)
expected_bin_values <- 10^(
  c(0.992, 0.992, 2.996, 2.996, 5, 5, 5, 7.004, 7.004, 9.008, 9.008)
)
test_that("bin_limits creates correct bin limits with log scale", {
  expect_equal(
    bin_limits(expected_scaled_input_data, bin_num),
    expected_bin_limits
  )
})

test_that("bin_indices gives correct bin indices with log scale", {
  expect_equal(
    bin_indices(expected_scaled_input_data, expected_bin_limits),
    expected_bin_indices
  )
})

test_that("bin_centers gives correct bin centers with log scale", {
  expect_equal(
    bin_centers(expected_bin_limits),
    expected_bin_centers
  )
})

test_that("bin_values gives correct bin values with log scale", {
  expect_equal(
    bin_values(input_data, bin_num, scale),
    expected_bin_values
  )
})

## End of Suites ##
