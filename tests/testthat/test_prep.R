
context("Data Preparation")


test_that("Discontinuities (atoms) are detected correctly", {

  # Estimate
  x_test_1 <- c(rep(3,10), seq(1, 100), rep(26, 10))
  x_test_2 <- c(rep(3,10), seq(1, 100), rep(26, 3))

  # Compare results.
  expect_equal(find_atoms(x = x_test_1), c(3, 26))
  expect_equal(find_atoms(x = x_test_2), 3)
  expect_equal(find_atoms(x = x_test_2, min_atom = 0.025), c(3, 26))
})


test_that("Break points for doscrete states are calculated correctly", {

  # Compare results.
  expect_equal(state_breaks(x = seq(-5,10), num_breaks = 4),
               seq(-3.5, 8.5, by = 3.0))
  expect_equal(state_breaks(x = seq(-5,10), num_breaks = 9),
               seq(-4.25, 9.25, by = 1.5))
})

