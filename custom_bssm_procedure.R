

is.custom_bssm_procedure = function(object) { 
  inherits(object, "custom_bssm_procedure", FALSE)
}


custom_bssm_procedure_ = function(
    lot_series,
    model,
    aql,
    rql,
    seed
  ) 
{
  stopifnot(exprs = {
    lotsim::is.lot_series(lot_series)
    vek::is_chr_vec_xb1(model)
    lotsim:::is_prop_xy1(aql)
    lotsim:::is_prop_xy1(rql)
    vek::is_int_vec_x1(seed)
    aql < rql
  })
  
  lots_df = as.data.frame(lot_series)
  bad_counts = lots_df$count
  t_len = lotsim::get_num_lot(lot_series)
  batch_size = lots_df$parent_size[1L]
  lot_size = lots_df$size[1L]
  
  stan_model = rstan::stan_model(model_code = model)
  
  data = list(
    T = t_len,
    s = rep(batch_size, t_len),
    n = rep(lot_size, t_len),
    y = bad_counts
  )
  
  fit = rstan::sampling(
    stan_model,
    data = data,
    chains = 4L,
    iter = 2000L,
    warmup = 500L,
    thin = 1L,
    seed = seed,
    control = list(adapt_delta = 0.99, max_treedepth = 17L),
    cores = 4L
  )
  
  list(
    lot_series = lot_series,
    fit = fit
  )
}


custom_bssm_procedure = function(
    lot_series,
    model,
    aql,
    rql,
    seed
  ) 
{
  stopifnot(exprs = {
    lotsim::is.lot_series(lot_series)
    vek::is_chr_vec_xb1(model)
    lotsim:::is_prop_xy1(aql)
    lotsim:::is_prop_xy1(rql)
    vek::is_int_vec_x1(seed)
    aql < rql
  })
  
  object = safely(custom_bssm_procedure_(
    lot_series,
    model,
    aql,
    rql,
    seed
  ))
  
  if (!is.null(object$error) || !is.null(object$warnings)) {
    object$result = object$result %||% list()
    object$result$lot_series = object$result$lot_series %||% lot_series
  }
  
  object$result$args = list(
    model = model,
    aql = aql,
    rql = rql,
    seed = seed
  )
  
  class(object) = c("custom_bssm_procedure", "procedure")
  
  return(object)
}


policy_alpha = function(object) {
  stopifnot(is.custom_bssm_procedure(object))
  
  posterior_samples = rstan::extract(object$result$fit)
  posterior_samples_mu = apply(posterior_samples$theta, 2L, mean)
  mu_ci_lower = apply(posterior_samples$theta, 2L, stats::quantile, probs = .05)
  
  decisions = mu_ci_lower <= object$result$args$aql
  return(decisions)
}


# Makes decisions based on the median of the posterior for theta.
policy_beta = function(object) {
  stopifnot(is.custom_bssm_procedure(object))
  
  posterior_samples = rstan::extract(object$result$fit)
  #theta_est = apply(posterior_samples$theta, 2L, mean)
  theta_est = apply(posterior_samples$theta, 2L, stats::median)
  #theta_est = apply(posterior_samples$theta, 2L, stats::quantile, probs = .3)
  
  decisions = theta_est <= object$result$args$aql
  return(decisions)
}


# Makes a decision for the final batch only, using the posterior predictive.
policy_gamma = function(object) {
  stopifnot(is.custom_bssm_procedure(object))
  
  samples = rstan::extract(object$result$fit)
  median_y_pred = stats::median(samples$y_pred)
  lot_series = object$result$lot_series
  last_lot_count = lotsim::get_count(lot_series) |> utils::tail(1L)
  last_batch_size = lotsim::get_parent_size(lot_series) |> utils::tail(1L)
  y_hat = (last_lot_count + median_y_pred) / last_batch_size
  
  stopifnot(exprs = {
    lotsim:::is_prop_xy1(y_hat)
  })
  
  decision = y_hat <= object$result$args$aql
  return(decision)
}


to_df_row.custom_bssm_procedure = function(
    object,
    policy,
    policy_args = list()
  ) 
{
  stopifnot(exprs = {
    vek::is_chr_vec_xb1(policy)
    policy %in% c("alpha", "beta", "gamma")
    lotsim:::is_list(policy_args)
  })
  
  df = new_df()
  tryCatch({
    if (policy == "alpha")
      decisions = policy_alpha(object)
    else if (policy == "beta")
      decisions = policy_beta(object)
    else if (policy == "gamma")
      decisions = policy_gamma(object)
    else
      stop("invalid state")
    
    if (policy %in% c("alpha", "beta")) {
      df = confusion_df(
        decisions,
        object$result$lot_series,
        object$result$args$aql
      )
      
    } else if (policy == "gamma") {
      true_prop = lotsim::get_true_prop(object$result$lot_series) |>
        utils::tail(1L)
      
      df = new_df(
        decision = decisions,
        true_prop = true_prop
      )
      
    } else {
      stop("invalid state")
    }
  }, condition = \(cond) {
    if (policy %in% c("alpha", "beta"))
      df = get_empty_confusion_df()
    else if (policy == "gamma")
      df = new_df(decision = NA, true_prop = NA_real_)
    else
      stop("invalid sate")
  })
  
  df$e = list(object$error)
  df$w = list(object$warnings)
  df$is_ok = is.null(object$error) && is.null(object$warnings)
  
  row.names(df) = attr(object, "run_id", TRUE)
  return(df)
}


plot.custom_bssm_procedure = function(object, ...) {
  args = list(...)
  add = args$add %||% FALSE
  flip = args$flip %||% TRUE
  lwd = args$lwd %||% 1L
  xlab = args$xlab %||% NULL
  ylab = args$ylab %||% NULL
  xlim = args$xlim %||% NULL
  ylim = args$ylim %||% NULL
  main = args$main %||% NULL
  xaxt = args$xaxt %||% NULL
  yaxt = args$yaxt %||% NULL
  stopifnot(exprs = {
    vek::is_lgl_vec_x1(add)
    vek::is_lgl_vec_x1(flip)
  })
  
  posterior_samples = rstan::extract(object$result$fit)
  theta_median = apply(posterior_samples$theta, 2L, stats::median)
  theta_lower = apply(posterior_samples$theta, 2L, stats::quantile,
                      probs = .05)
  
  theta_upper = apply(posterior_samples$theta, 2L, stats::quantile,
                      probs = .95)
  
  lot_series = object$result$lot_series
  aql = object$result$args$aql
  rql = object$result$args$rql
  
  if (!add) {
    plot(
      lot_series, what = "prop", flip = flip, col = "gray", xlab = xlab,
      ylab = ylab, xlim = xlim, lwd = lwd, ylim = ylim, main = main,
      type = "n", xaxt = xaxt, yaxt = yaxt
    )
  }
  
  at_t = 1:lotsim::get_num_lot(lot_series)
  graphics::polygon(
    x = c(at_t, rev(at_t)),
    y = c(theta_lower, rev(theta_upper)),
    col = grDevices::adjustcolor("#2D708EFF", alpha = .5), border = NA
  )
  
  plot(
    lot_series, what = "prop", flip = flip, col = "gray", xlab = xlab,
    ylab = ylab, xlim = xlim, lwd = lwd, ylim = ylim, main = main, add = TRUE
  )
  
  plot(
    lot_series, what = "true_prop", col = "black", flip = flip, add = TRUE,
    lwd = lwd
  )
    
  #if (flip)
  #  graphics::abline(v = c(aql, rql), col = "black", lwd = lwd, lty = 2L)
  #else
  #  graphics::abline(h = c(aql, rql), col = "black", lwd = lwd, lty = 2L)
  
  #x = if (flip) theta_lower else at_t
  #y = if (flip) at_t else theta_lower
  #graphics::lines(x, y, col = col, lwd = lwd)
  
  #x = if (flip) theta_median else at_t
  #y = if (flip) at_t else theta_median
  #graphics::lines(x, y, col = col, lwd = lwd)
  
  #x = if (flip) theta_upper else at_t
  #y = if (flip) at_t else theta_upper
  #graphics::lines(x, y, col = col, lwd = lwd)
}


get_model1x = function() {
  "
  data {
    int<lower=1> T;  // Number of time points
    int<lower=0> s[T];  // Size of batch
    int<lower=0> n[T];  // Number of sampled items per batch
    int<lower=0> y[T];  // Observed defects for each batch
  }
  
  parameters {
    vector[T] logit_theta_raw;  // Latent state on the logit scale (non-centered)
    real<lower=0> sigma;  // Standard deviation of state transitions
    vector[T-1] delta;  // Transition parameter
  }
  
  transformed parameters {
    vector[T] logit_theta;  // Latent state on the logit scale (centered)
    vector[T] theta;  // Latent state representing defect rate in original scale
  
    // Non-centered parameterization
    logit_theta[1] = -4 + logit_theta_raw[1] * 0.07791937;
    for (t in 2:T) {
      logit_theta[t] = logit_theta[t-1] + delta[t-1] + logit_theta_raw[t] * sigma;
    }
  
    theta = inv_logit(logit_theta);  // Transform back to [0, 1] scale
  }
  
  model {
    logit_theta_raw ~ normal(0, 1);  // Standard normal prior for the non-centered parameter
    sigma ~ normal(0, 0.07791937);  // Prior on the standard deviation
    delta ~ normal(0, 0.1558387);  // Prior on the transition parameter
  
    // Observation equation
    for (t in 1:T) {
      y[t] ~ binomial(n[t], theta[t]);  // Likelihood
    }
  }
  
  generated quantities {
    int y_pred;  // Predicted defects for the remaining items in the last batch
  
    // Generate predicted number of defects for the remaining items in the last batch
    y_pred = binomial_rng(s[T] - n[T], theta[T]);
  }
  "
}
