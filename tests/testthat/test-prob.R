test_that("probabilities are correctly inferred", {
  age_column = c(30, 31, 32)
  some_data = data.frame(
    age = age_column,
    total = c(100000, 100000, 99999),
    active = c(100000, 50000, 50000),
    inactive = c(0, 50000, 49999),
    entries_into_work = c(0, 0, 0),
    voluntary_work_exits = c(0, 50000, 0),
    death_active = c(0, 0, 0),
    death_inactive = c(0, 0, 1),
    death_total = c(0, 0, 1)
  )

  probs = determine_probs(some_data)
  expect_equal(length(probs$age), 3) 
  expect_equal(probs$age, age_column) 
  expect_equal(probs$aipx[1], 0.5) 
  expect_equal(probs$idpx[2], 1/50000) 
  expect_equal(probs$iapx[2], 0) 
})
