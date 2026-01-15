#' Extends an inc/decrement table
#' by the  by the number individuals 
#' staying in their state at each age.
#' @param icdc_df - inc/decrement table such as [occupation1977stlouis] dataset
#' @returns extended dataframe
#' @export
extend_incdec_table = function(icdc_df) {
   stayed_active = icdc_df$active - icdc_df$active_to_inactive - icdc_df$active_to_death
   stayed_inactive = icdc_df$inactive - icdc_df$inactive_to_active - icdc_df$inactive_to_death
   cbind(icdc_df, stayed_active, stayed_inactive)
}


#' Estimates parameters via max likelihood
#' for a softmax model of the markov transition matrices
#' dependent on age.
#' @param incdec_df - An inc/decrement table with such as produced by [extend_incdec_table()].
#' @param age_range - The age range for which the model should be estimated.
#' @returns parameters for softmax model.
#' @export
max_l_softmax_estimate <- function(incdec_df, age_range) {

  # Returns matrix of probs rows=ages cols=states
  # @param theta - vector c(alpha2, beta2, alpha3, beta3) (state1 is reference state)
  get_probs <- function(t_vec, theta) {
    # Reshape theta into matrix: 
    # Row 1=State2(inactive), Row 2=State3(death)
    # Cols: 1=Intercept, 2=Slope
    coefs <- matrix(theta, nrow = 2, byrow = TRUE) 
    
    # Linear Predictors (Logits)
    # State 1 (Ref) gets 0
    # States 2,3 get: b0 + b1 * t
    logit_2 <- coefs[1, 1] + coefs[1, 2] * t_vec
    logit_3 <- coefs[2, 1] + coefs[2, 2] * t_vec
    
    # Exponentials
    exp_1 <- rep(1, length(t_vec)) # exp(0)
    exp_2 <- exp(logit_2)
    exp_3 <- exp(logit_3)
    
    sum_exp <- exp_1 + exp_2 + exp_3
   
    # transition probs into state 1,2,3
    cbind(exp_1/sum_exp, exp_2/sum_exp, exp_3/sum_exp)
  }
 
  # Actual max likelihood estimation.
  # @param target_cols - vector of column names for 3 states
  find_parameters_of_col <- function(target_cols) {
    data_sub <- incdec_df[incdec_df$age %in% age_range, ]
    t_vals <- data_sub$age
    
    # transition count matrix: [N_to_State1, N_to_State2, N_to_State3]
    Y_counts <- as.matrix(data_sub[, target_cols])
    
    # N_total: Total individuals starting in this state at each age
    N_total <- rowSums(Y_counts)
    
    nll_fn <- function(theta) {
      P_mat <- get_probs(t_vals, theta)
      
      # Avoid log(0)
      epsilon <- 1e-15
      log_probs <- log(P_mat + epsilon)
      
      likelihood <- sum(Y_counts * log_probs)
      return(-likelihood)
    }
    
    # Gradient of NLL of softmax function for better optimization.
    # @param theta - 
    grad_fn <- function(theta) {
      P_mat <- get_probs(t_vals, theta)
      
      E_counts <- P_mat * N_total
      
      # error = expected - observed
      # only errors for State 2 and State 3 (State 1 is Ref)
      err_2 <- E_counts[, 2] - Y_counts[, 2]
      err_3 <- E_counts[, 3] - Y_counts[, 3]
      
      # Gradients: sum(error * covariate)
      # For b0 (Intercept): Covariate is 1
      d_b02 <- sum(err_2)
      d_b03 <- sum(err_3)
      
      # For Beta (Slope): Covariate is t
      d_b12 <- sum(err_2 * t_vals)
      d_b13 <- sum(err_3 * t_vals)
      
      return(c(d_b02, d_b12, d_b03, d_b13))
    }
    
    init_theta <- rep(0, 4) 
    opt <- stats::optim(init_theta, nll_fn, gr = grad_fn, method = "BFGS")
    return(opt$par)
  }
 
  params_active <- find_parameters_of_col(c("stayed_active", "active_to_inactive", "active_to_death"))
  params_faulty <- find_parameters_of_col(c("inactive_to_active", "stayed_inactive", "inactive_to_death"))
  
  # Columns: Beta0 (Intercept), Beta1 (Slope)
  result <- data.frame(
    row_source = c("Active", "Inactive"),
    
    b0_active = c(0, 0),
    b1_active  = c(0, 0),
    
    b0_inactive = c(params_active[1], params_faulty[1]),
    b1_inactive  = c(params_active[2], params_faulty[2]),
    
    b0_death = c(params_active[3], params_faulty[3]),
    b1_death  = c(params_active[4], params_faulty[4])
  )
  
  return(result)
}

#' Creates a a transition matrix of age t 
#' from a softmax model.
#' @param est - Parameters of softmax model such as [max_l_softmax_estimate()]
#' @param x - ages for which a transition matrix should be created
#' @export
transition_matrix_from_est = function(est, x) {
    if (length(x) > 1) {
        tms = as.data.frame(do.call(rbind, lapply(x, function (y) transition_matrix_from_est(est, y))))
        return(tms)
    }

    b0 = est[c('b0_active', 'b0_inactive', 'b0_death')] |> data.matrix() |> unname()
    b1 = est[c('b1_active', 'b1_inactive', 'b1_death')] |> data.matrix() |> unname()
    normalizer = apply(exp(b1*x)*exp(b0), 1, sum)
    active_inactive_rows = exp(b1*x + b0) / normalizer
    tms = rbind(active_inactive_rows, c(0, 0, 1)) |> list()
    data.frame(age=x, t_matrix=I(tms))
}
