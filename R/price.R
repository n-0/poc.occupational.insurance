get_array_from_tms_list = function(tms) {
    tms_temp <- simplify2array(tms$m)
    aperm(tms_temp, c(3, 1, 2))
}

#' Computes the single premium paid in advance 
#' for an occupation insurance, which pays 
#' a fixed period at each policy anniversary,
#' if the insuree is not employed. Time units are typically years.
#' @param x - Age of the insuree at policy inception.
#' @param duration - Policy duration.
#' @param benefit - Benefit paid at the end of each year. 
#' @param v - Constant discount factor
#' @param transition_probs - A table of transition probabilities as produced by [determine_probs()]
#' @param active - True if the insuree has an occupation at policy inception.
#' @returns Premium to be paid 
#' @export
occupation_insurance_one_time_premium = function(x, duration, benefit, v, transition_probs, active = TRUE) {
    tms = transition_matrix_x_t(x, (1:duration), transition_probs)
    tms_array = get_array_from_tms_list(tms)
    discounts <- v^((1:duration))
    if (active) {
        sum(discounts*tms_array[,1,2]*rep(benefit, duration))
    } else {
        sum(discounts*tms_array[,2,2]*rep(benefit, duration))
    }
}


#' Computes the constant recurrent/natural premium paid in advance
#' for an occupation insurance, which pays 
#' a fixed period at each policy anniversary,
#' if the insuree is not employed. Time units are typically years.
#' @inheritParams occupation_insurance_one_time_premium
#' @param opt_premium_duration - The no. of times the premium is paid, default is the policy duration. 
#' @returns constant premium to be paid 
#' @export
occupation_insurance_recurrent_premium = function(
    x, 
    duration, 
    benefit,
    v,
    transition_probs,
    active = TRUE,
    opt_premium_duration = NULL
) {
    exp_benefits = occupation_insurance_one_time_premium(x, duration, benefit, v, transition_probs, active)
    premium_duration = if (is.null(opt_premium_duration)) { duration } else { opt_premium_duration }

    tms = transition_matrix_x_t(x, (0:(premium_duration-1)), transition_probs)
    tms_array = get_array_from_tms_list(tms)
    discounts <- v^((0:(premium_duration-1)))
    premia_discount = ifelse(active, sum(discounts*tms_array[,1,1]), utils::tail(discounts*tms_array[,2,1], -1))
    exp_benefits / premia_discount 
}


#' Computes the reserve (depends on state) at time `t`
#' for a policy as given by the parameters.
#' @param t - Time at which the reserve is considered.
#' @inheritParams occupation_insurance_recurrent_premium
#' @param opt_premium_duration - The no. of periods the premium is paid, default is the policy duration.
#' @returns reserve at time `t`.
#' @export
occupation_insurance_recurrent_reserve = function(
    t,
    x,
    duration,
    benefit,
    v,
    transition_probs,
    active=TRUE, 
    opt_premium_duration = NULL) {
    premium_duration = if (is.null(opt_premium_duration)) { duration } else { opt_premium_duration }

    remaining_benefits = occupation_insurance_one_time_premium(x+t, duration-t, benefit, v, transition_probs, active)
    recurrent_premium = occupation_insurance_recurrent_premium(x, duration, benefit, v, transition_probs, active, premium_duration)
    if (premium_duration >= t + 1) {
        tms = transition_matrix_x_t(x+t, (0:(premium_duration -t -1)), transition_probs)
        tms_array = get_array_from_tms_list(tms)
        discounts <- v^((0:(premium_duration -t -1)))
        premia_discount = ifelse(active, sum(discounts*tms_array[,1,1]), sum(discounts*tms_array[,2,1]))
        return(remaining_benefits - recurrent_premium*premia_discount)
    }
    return(remaining_benefits)
}
