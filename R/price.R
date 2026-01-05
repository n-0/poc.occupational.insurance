unemployment_insurance_one_time_premium = function(x, duration, benefit, v, transition_probs, active = TRUE) {
    tms = transition_matrix_x_t(x, (1:duration), transition_probs)
    tms_temp <- simplify2array(tms$m)
    tms_array <- aperm(tms_temp, c(3, 1, 2))
    discounts <- v^((1:duration))
    if (active) {
        sum(v*tms_array[,1,2]*rep(benefit, duration))
    } else {
        sum(v*tms_array[,2,2]*rep(benefit, duration))
    }
}


unemployment_insurance_recurrent_premium = function(x, duration, benefit, v, transition_probs, active = TRUE) {
    exp_benefits = unemployment_insurance_one_time_premium(x, duration, benefit, v, transition_probs, active)

    tms = transition_matrix_x_t(x, (0:(duration-1)), transition_probs)
    tms_temp <- simplify2array(tms$m)
    tms_array <- aperm(tms_temp, c(3, 1, 2))
    discounts <- v^((0:(duration-1)))
    premia_discount = ifelse(active, sum(v*tms_array[,1,1]), sum(v*tms_array[,2,1]))
    exp_benefits / premia_discount 
}


unemployment_insurance_recurrent_reserve = function(t, x, duration, benefit, v, transition_probs, active=TRUE) {
    remaining_benefits = unemployment_insurance_one_time_premium(x+t, duration-t, benefit, v, transition_probs, active)
    recurrent_premium = unemployment_insurance_recurrent_premium(x, duration, benefit, v, transition_probs, active)

    tms = transition_matrix_x_t(x+t, (0:(duration -t -1)), transition_probs)
    tms_temp <- simplify2array(tms$m)
    tms_array <- aperm(tms_temp, c(3, 1, 2))
    discounts <- v^((0:(duration -t -1)))
    premia_discount = ifelse(active, sum(v*tms_array[,1,1]), sum(v*tms_array[,2,1]))
    remaining_benefits - recurrent_premium*premia_discount
}
