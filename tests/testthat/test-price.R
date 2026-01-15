test_that("one time premium fits probabilities", {
  age_column = c(30, 31, 32)
  x = 30
  duration = 2 
  v = 1/1.03
  benefit = 1
  sample_transition_probs = data.frame(
    age = age_column,
    aipx = c(0, 0, 0),
    adpx = c(0.1, 0.1, 0.1),
    idpx = c(0, 0, 0),
    iapx = c(1, 1, 1)
  )
  premium = occupation_insurance_one_time_premium(
    x,
    duration,
    benefit,
    v,
    sample_transition_probs
  )
  expect_equal(premium, 0)

  sample_transition_probs = data.frame(
    age = age_column,
    aipx = c(1, 0, 0),
    adpx = c(0, 0, 0.1),
    idpx = c(0, 0, 0),
    iapx = c(0, 0, 0)
  )
  premium = occupation_insurance_one_time_premium(
    x,
    duration,
    benefit,
    v,
    sample_transition_probs
  )
  expect_equal(premium, sum(v^(1:2)*rep(benefit, 2)))

})


test_that("one time premium fits probabilities inactive start", {
  age_column = c(30, 31, 32)
  x = 30
  duration = 2 
  v = 1/1.03
  benefit = 1
  sample_transition_probs = data.frame(
    age = age_column,
    aipx = c(0, 0, 0),
    adpx = c(0, 0, 0),
    idpx = c(0, 0, 0),
    iapx = c(0.8, 1, 1)
  )

  premium = occupation_insurance_one_time_premium(
    x,
    duration,
    benefit,
    v,
    sample_transition_probs,
    FALSE
  )
  expect_equal(premium, v*benefit*0.2)


  sample_transition_probs = data.frame(
    age = age_column,
    aipx = rep(0, 3),
    adpx = rep(1, 3),
    idpx = rep(1, 3),
    iapx = rep(0, 3)
  )
  premium = occupation_insurance_one_time_premium(
    x,
    duration,
    benefit,
    v,
    sample_transition_probs,
  )
  expect_equal(premium, 0)
  premium = occupation_insurance_one_time_premium(
    x,
    duration,
    benefit,
    v,
    sample_transition_probs,
    FALSE
  )
  expect_equal(premium, 0)
})

test_that("recurrent premium fits probabilities, a.s. stays active", {
  age_column = c(30, 31, 32)
  x = 30
  duration = 2
  v = 1/1.03
  benefit = 1
  sample_transition_probs = data.frame(
    age = age_column,
    aipx = c(0, 0, 0),
    adpx = c(0.1, 0.1, 0.1),
    idpx = c(0, 0, 0),
    iapx = c(1, 1, 1)
  )
  premium = occupation_insurance_recurrent_premium(
    x,
    duration,
    benefit,
    v,
    sample_transition_probs
  )
  expect_equal(premium, 0)

  sample_transition_probs = data.frame(
    age = age_column,
    aipx = c(1, 1, 1),
    adpx = c(0, 0, 0),
    idpx = c(0, 0, 0),
    iapx = c(0, 0, 0)
  )
  premium = occupation_insurance_recurrent_premium(
    x,
    duration,
    benefit,
    v,
    sample_transition_probs
  )
  # pays only at the start and afterwards never again
  # as the insuree becomes unemployed.
  expect_equal(premium, sum(v^(1:2)*rep(benefit, 2)))
})

test_that("recurrent premium fits probabilities", {
  age_column = c(30, 31, 32)
  x = 30
  duration = 2
  v = 1/1.03
  benefit = 1

  sample_transition_probs = data.frame(
    age = age_column,
    aipx = c(0, 0, 0),
    adpx = c(0, 0, 0),
    idpx = rep(0, 3),
    iapx = c(0.8, 1, 1)
  )

  premium = occupation_insurance_recurrent_premium(
    x,
    duration,
    benefit,
    v,
    sample_transition_probs,
    FALSE
  )
  expect_equal(premium, (v*benefit*0.2) / (v*0.8))

  sample_transition_probs = data.frame(
    age = age_column,
    aipx = rep(0, 3),
    adpx = rep(1, 3),
    idpx = rep(1, 3),
    iapx = rep(0, 3)
  )
  premium = occupation_insurance_recurrent_premium(
    x,
    duration,
    benefit,
    v,
    sample_transition_probs,
  )
  expect_equal(premium, 0)
  premium = occupation_insurance_recurrent_premium(
    x,
    duration,
    benefit,
    v,
    sample_transition_probs,
    FALSE
  )
  # never pays a premium nor receives any benefit
  # since insuree dies beforehand.
  expect_equal(premium, NaN)
})
