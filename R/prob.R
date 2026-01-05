the <- new.env(parent = emptyenv())

#' Determine a table of transition probabilities dependend on age
#' @export
determine_probs = function(employment_data) {
    aipx = employment_data$voluntary_work_exits[-1] / employment_data$active 
    aipx = aipx[-length(aipx)] 

    adpx = employment_data$death_active[-1] / employment_data$active
    adpx = adpx[-length(adpx)]

    idpx = employment_data$death_inactive[-1] / employment_data$inactive
    idpx = idpx[-length(idpx)]

    iapx = employment_data$entries_into_work[-1] / employment_data$inactive
    iapx = iapx[-length(iapx)]

    df = data.frame(aipx = aipx, adpx = adpx, idpx = idpx, iapx = iapx, age=employment_data$age[-length(aipx)])
    row.names(df) <- employment_data$age[-length(aipx)];
    df
}


#' Determine a transition probability matrix 
#' for Markov Chain based calculations
#' @export
transition_matrix_x = function(x, transition_probs) {
    if (length(x) > 1) {
        tms = as.data.frame(do.call(rbind, lapply(x, transition_matrix_x, transition_probs)))
        return(tms)
    }
    t1probs = transition_probs[transition_probs$age == x,]
    t1matrix = matrix(c(1 - t1probs$aipx - t1probs$adpx, 
                      t1probs$iapx,
                      0,
                      t1probs$aipx,
                      1 - t1probs$iapx - t1probs$idpx,
                      0,
                      t1probs$adpx,
                      t1probs$idpx,
                      1), nrow = 3, ncol = 3)
    data.frame(age = x, tmatrix = I(list(t1matrix)))
}

the$all_transition_matrices = NULL

#' For efficient caching, fix all one step transition matrices.
#' @export
set_all_transition_matrices = function(x, transition_probs) {
    the$all_transition_matrices = transition_matrix_x(transition_probs$age, transition_probs)
}

#' Obtain the transition matrix for an individual aged x
#' after t years.
#' @export
transition_matrix_x_t = function(x, t, transition_probs) {
    if (the$all_transition_matrices == NULL) { 
        set_all_transition_matrices(x, transition_probs)
    }
    if (length(t) > 1) {
        tms = as.data.frame(do.call(rbind, lapply(t, function(t) transition_matrix_x_t(x, t, transition_probs))))
        return(tms)
    }
    if (t == 0) { 
        tms = data.frame(x = x, t = 0, m = I(list(diag(rep(1, 3)))))
        return(tms) 
    }
    age_range = the$all_transition_matrices_x[the$all_transition_matrices_x$age >= x & the$all_transition_matrices_x$age <= x + t,]
    data.frame(x = x, t = t, m = I(list(Reduce(`%*%`, age_range$tmatrix))))
}
