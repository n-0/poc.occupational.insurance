#' Determine a table of transition probabilities dependend on age
#' from a increment-decrement table.
#' @details
#' The resulting dataframe has rows for each age `x` with the columns 
#' * aipx - the probability of an occupied individual to become unoccupied.
#' * adpx - the probability of an occupied individual to die
#' * idpx - the probability of an unoccupied individual to die
#' * idpx - the probability of an unoccupied individual to become occupied 
#' Where the probabilities are conditional on the current state and age 
#' and taken with respect to the next time period.
#' @param occupation_data - A data.frame with columns such as in 
#' the `occupation1977stlouis` data set.
#' @export
determine_probs = function(occupation_data) {
    aipx = occupation_data$active_to_inactive / occupation_data$active
    adpx = occupation_data$active_to_death / occupation_data$active
    idpx = occupation_data$inactive_to_death / occupation_data$inactive
    iapx = occupation_data$inactive_to_active / occupation_data$inactive

    data.frame(aipx = round(aipx, 6), adpx = round(adpx, 6), idpx = round(idpx, 6), iapx = round(iapx, 6), age=occupation_data$age)
}


#' Determine a transition probability matrix 
#' for Markov Chain based calculations
#' @param transition_probs - A table of transition probabilities as produced by [determine_probs()]
#' @param x - If a vector then for each age in there, a transition matrix is added. 
#' @returns A transition matrix. 
#' @export
transition_matrix_x = function(x, transition_probs) {
    if (length(x) > 1) {
        tms = as.data.frame(do.call(rbind, lapply(x, transition_matrix_x, transition_probs)))
        return(tms)
    }
    t1probs = transition_probs[transition_probs$age == x,]
    t1matrix = c(1 - t1probs$aipx - t1probs$adpx, t1probs$aipx, t1probs$adpx,
                t1probs$iapx, 1 - t1probs$iapx - t1probs$idpx, t1probs$idpx,
                 0, 0, 1
              ) |> round(6) |> matrix(byrow=TRUE, nrow = 3, ncol = 3)
    data.frame(age = x, tmatrix = I(list(t1matrix)))
}

#' Determine transition probability matrices
#' from starting age x until t
#' @param transition_probs - A table of transition probabilities as produced by [determine_probs()].
#' @param opt_all_tms - Optional one period transition matrices for all ages such as by [transition_matrix_x()].
#' @param x - Starting age
#' @param t - If a vector then the result has a row with a transition matrix for each entry.
#' @returns data.frame of transition matrices and corresponding initial age and time since then.
#' @export
transition_matrix_x_t = function(x, t, transition_probs, opt_all_tms = NULL) {
    last_row = utils::tail(transition_probs, 1)
    if (last_row$age < utils::tail(x + t, 1)) {
        stop(sprintf("Out of range (%f, %f) maximum age for available probabilities", transition_probs[1]$age, last_row$age))
    }
    all_tms = if(is.null(opt_all_tms)) {
        transition_matrix_x(transition_probs$age, transition_probs)
    } else { opt_all_tms }
    if (length(t) > 1) {
        tms = as.data.frame(do.call(rbind, lapply(t, function(t) transition_matrix_x_t(x, t, transition_probs, all_tms))))
        return(tms)
    }
    if (t == 0) { 
        tms = data.frame(x = x, t = 0, m = I(list(diag(rep(1, 3)))))
        return(tms) 
    }
    age_range = all_tms[all_tms$age >= x & all_tms$age < x + t,]
    final_tms = (function(acc, nxt) round(acc %*% nxt, 6)) |>
    Reduce(age_range$tmatrix) |> list()
    data.frame(x = x, t = t, m = I(final_tms))
}
