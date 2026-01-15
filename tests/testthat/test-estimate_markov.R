test_that("Estimated Markov Matrices reflect trends in data (active)", {
    incdec_df <- data.frame(
      age = 20:24,
      active = c(499, 497, 493, 488, 482),
      active_to_inactive = c(1,  2,  2,  3,  3), 
      active_to_death = c(1,  1,  2,  2,  3),
      inactive = c(441, 378, 310, 238, 161),
      inactive_to_active = c( 1,  1,  0,  0,  0),
      inactive_to_death = c(59, 64, 70, 75, 80) 
    )

    expected_20_tm = matrix(c(
        1, 0, 0,
        0.002941091, 0.878105329, 0.1189536,
        0, 0, 1
    ), nrow=3, ncol=3, byrow=TRUE)

    incdec_df_ext = extend_incdec_table(incdec_df)
    est = max_l_softmax_estimate(incdec_df_ext, (20:24))
    tm_df = transition_matrix_from_est(est, 20)

    result_20_tm = tm_df[1,]$t_matrix[[1]]
    expect_equal(result_20_tm, expected_20_tm, tolerance=1e-4)

    tm_df_range = transition_matrix_from_est(est, (20:21))
    expect_equal(nrow(tm_df_range), 2)
    result_20_tm_range = tm_df_range[1,]$t_matrix[[1]]
    expect_equal(result_20_tm_range, result_20_tm, tolerance=1e-4)
})


test_that("(mixed)", {
    incdec_df <- data.frame(
      age = 20:24,
      active = c(494, 483, 446, 414, 371),
      active_to_inactive = c(1,  1,  2,  2,  3), 
      active_to_death = c(15,  20,  40,  35,  45),
      inactive = c(461, 422, 389, 356, 324),
      inactive_to_active = c(10,  10,  5,  5,  5),
      inactive_to_death = c(30, 30, 30, 30, 30) 
    )

    expected_20_tm = matrix(c(
         0.96399023, 0.001790611, 0.03421916,
         0.02239487, 0.912672214, 0.06493291,
         0.00000000, 0.000000000, 1.00000000
    ), nrow=3, ncol=3, byrow=TRUE)

    incdec_df_ext = extend_incdec_table(incdec_df)
    est = max_l_softmax_estimate(incdec_df_ext, (20:24))
    tm_df = transition_matrix_from_est(est, 20)
    result_20_tm = tm_df[1,]$t_matrix[[1]]

    expect_equal(result_20_tm, expected_20_tm, tolerance=1e-4)
})
