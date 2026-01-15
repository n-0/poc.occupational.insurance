test_that("probabilities are extracted from table", {
  age_column = c(30, 31, 32)
  some_data = data.frame(
    age = age_column,
    total = c(100000, 100000, 99999),
    active = c(100000, 50000, 50000),
    inactive = c(50000, 49999, 49999),
    inactive_to_active = c(0, 0, 0),
    active_to_inactive = c(50000, 0, 0),
    active_to_death = c(0, 0, 0),
    inactive_to_death = c(0, 1, 0),
    death_total = c(0, 1, 1)
  )

  probs = determine_probs(some_data)
  expect_equal(length(probs$age), 3) 
  expect_equal(probs$age, age_column)
  expect_equal(probs$aipx[1], 0.5) 
  expect_equal(probs$idpx[2], 1/50000) 
  expect_equal(probs$iapx[2], 0) 
})


test_that("transition matrix is inferred from probabilities", {
  age_column = c(30, 31, 32)
  some_data = data.frame(
    age = age_column,
    aipx = c(0.9, 0, 0.5),
    adpx = c(0.1, 0, 0),
    idpx = c(0.2, 0, 0),
    iapx = c(0, 0.5, 0.25)
  )

  tms = transition_matrix_x(30, some_data)
  expect_equal(dim(tms$tmatrix[[1]]), c(3, 3)) 
  expected_matrix_30 = matrix(c(0, 0, 0, 0.9, 0.8, 0, 0.1, 0.2, 1), nrow=3, ncol=3)
  expect_equal(tms$tmatrix[[1]], expected_matrix_30)

  selection_of_ages = c(30, 31)
  tms = transition_matrix_x(selection_of_ages, some_data)
  expect_equal(length(tms), 2) 
  expect_equal(tms$age, selection_of_ages) 
  expect_equal(tms$tmatrix[[1]], expected_matrix_30) 
  expected_matrix_31 = matrix(c(1, 0.5, 0, 0, 0.5, 0, 0, 0, 1), nrow=3, ncol=3)
  expect_equal(tms$tmatrix[[2]], expected_matrix_31) 
})


test_that("transition matrix from selected age after t years", {
  age_column = c(30, 31, 32)
  some_data = data.frame(
    age = age_column,
    aipx = c(0.9, 0, 0.5),
    adpx = c(0.1, 0, 0),
    idpx = c(0.2, 0, 0),
    iapx = c(0, 0.5, 0.25)
  )

  tms = transition_matrix_x_t(30, 0, some_data)
  expect_equal(tms$m[[1]], diag(rep(1, 3)))

  tms = transition_matrix_x_t(30, 1, some_data)
  expect_equal(tms$x[1], 30)
  expect_equal(tms$t[1], 1) 
  expect_equal(dim(tms$m[[1]]), c(3, 3)) 
  expected_matrix_30 = matrix(c(0, 0, 0, 0.9, 0.8, 0, 0.1, 0.2, 1), nrow=3, ncol=3)
  expect_equal(tms$m[[1]], expected_matrix_30)

  selection_of_ages = c(1, 2)
  tms = transition_matrix_x_t(30, selection_of_ages, some_data)
  expect_equal(length(tms), 3) 
  expect_equal(tms$x, c(30, 30))
  expect_equal(tms$t, c(1, 2))
  expect_equal(tms$m[[1]], expected_matrix_30) 
  expected_matrix_31 = matrix(c(1, 0.5, 0, 0, 0.5, 0, 0, 0, 1), nrow=3, ncol=3)
  expect_equal(tms$m[[2]], expected_matrix_30 %*% expected_matrix_31) 
})
