

is.AcceptanceSampling_lqas_procedure = function(object) { 
  inherits(object, "AcceptanceSampling_lqas_procedure", FALSE)
}


AcceptanceSampling_lqas_procedure_ = function(
    lot_series, 
    aql, 
    rql, 
    alpha, 
    beta
  ) 
{
  stopifnot(exprs = {
    lotsim::is.lot_series(lot_series)
    lotsim:::is_prop_xy1(aql)
    lotsim:::is_prop_xy1(rql)
    lotsim:::is_prop_xy1(alpha)
    lotsim:::is_prop_xy1(beta)
    aql < rql
  })
  
  p0_args = c(aql, 1L - alpha) # TODO check manual again
  p1_args = c(rql, beta)
  lqas_plan = AcceptanceSampling::find.plan(p0_args, p1_args, type = "binomial")
  
  stopifnot(exprs = {
    is.list(lqas_plan)
    vek::is_num_vec_xyz1(lqas_plan$n)
    vek::is_num_vec_xyz1(lqas_plan$c)
    vek::is_num_vec_xyz1(lqas_plan$r)
    lqas_plan$n %% 1L == 0L
    lqas_plan$c %% 1L == 0L
    lqas_plan$r %% 1L == 0L
    lqas_plan$n > 0L
    lqas_plan$c >= 0L
    lqas_plan$r >= 0L
    lqas_plan$n >= lqas_plan$c
  })
  
  lqas_plan$n = as.integer(lqas_plan$n)
  lqas_plan$c = as.integer(lqas_plan$c)
  lqas_plan$r = as.integer(lqas_plan$r)
  
  lqas_plan = structure(
    lqas_plan,
    class = "AcceptanceSampling_lqas_plan",
    type = "binomial",
    aql = aql,
    rql = rql,
    alpha = alpha,
    beta = beta
  )
  
  sampled_lot_series = lotsim::lot_sample(lot_series, size = lqas_plan$n)
  
  bad_counts = lotsim::get_count(sampled_lot_series)
  decision = bad_counts <= lqas_plan$c
  
  list(
    decision = decision,
    lqas_plan = lqas_plan,
    parent_lot_series = lot_series,
    lot_series = sampled_lot_series
  )
}


AcceptanceSampling_lqas_procedure = function(
    lot_series, 
    aql, 
    rql, 
    alpha,
    beta
    )
{
  stopifnot(exprs = {
    lotsim::is.lot_series(lot_series)
    lotsim:::is_prop_xy1(aql)
    lotsim:::is_prop_xy1(rql)
    lotsim:::is_prop_xy1(alpha)
    lotsim:::is_prop_xy1(beta)
    aql < rql
  })
  
  object = safely(
    AcceptanceSampling_lqas_procedure_(lot_series, aql, rql, alpha, beta)
  )
  
  if (!is.null(object$error) || !is.null(object$warnings)) {
    object$result = object$result %||% list()
    object$result$parent_lot_series = object$result$parent_lot_series %||% 
      lot_series
  }
  
  object$result$args = list(
    aql = aql,
    rql = rql,
    alpha = alpha,
    beta = beta
  )
  
  class(object) =  c("AcceptanceSampling_lqas_procedure", "procedure")
  
  # Assess performance.
  conf_df = NULL
  tryCatch({
    conf_df = confusion_df(object$result$decision, object$result$lot_series, aql)
  }, condition = \(cond) {
    conf_df = get_empty_confusion_df()
  })
  
  object$result$confusion_df = conf_df
  
  return(object)
}



to_df_row.AcceptanceSampling_lqas_procedure = function(
    object,
    policy = "alpha",
    policy_args = list()
  )
{
  stopifnot(exprs = {
    vek::is_chr_vec_xb1(policy)
    policy %in% c("alpha", "ewma", "gamma", "ewma_gamma")
    lotsim:::is_list(policy_args)
  })
  
  if (policy %in% c("alpha", "gamma")) {
    df = new_df()
    
    if (policy == "alpha") {
      tryCatch({
        df = confusion_df(
          object$result$decision,
          object$result$lot_series,
          object$result$args$aql
        )
      }, condition = \(cond) {
        df = get_empty_confusion_df()
      })
      
    } else if (policy == "gamma") {
      tryCatch({
        true_prop = lotsim::get_true_prop(object$result$lot_series) |>
          utils::tail(1L)
        
        df = new_df(
          decision = object$result$decision |> utils::tail(1L),
          true_prop = true_prop
        )
      }, condition = \(cond) {
        df = new_df(decision = NA, true_prop = NA_real_)
      })
      
    } else {
      stop("invalid state")
    }
    
    df$e = list(object$error)
    df$w = list(object$warnings)
    df$is_ok = is.null(object$error) && is.null(object$warnings)
    
    row.names(df) = attr(object, "run_id", TRUE)
    return(df)
    
  } else if (policy %in% c("ewma", "ewma_gamma")) {
    a = policy_args$a
    lambda = policy_args$lambda
    stopifnot(exprs = {
      vek::is_num_vec_xyz1(a)
      lotsim:::is_prop_xy1(lambda)
      a > 0L
    })
    
    lot_series = object$result$lot_series
    aql = object$result$args$aql
    rql = object$result$args$rql
    mu = aql * lotsim::get_num_lot(lot_series)
    
    ewma_proc = ewma_procedure(
      lot_series, mu, a = a, lambda = lambda, aql = aql, rql = rql)
    
    df = new_df()
    if (policy == "ewma") {
      tryCatch({
        df = confusion_df(
          ewma_proc$result$decisions,
          lot_series,
          aql
        )
      }, condition = \(cond) {
        df = get_empty_confusion_df()
      })
    } else if (policy == "ewma_gamma") {
      tryCatch({
        df = new_df(
          decision = ewma_proc$result$decisions |> utils::tail(1L),
          true_prop = lotsim::get_true_prop(lot_series) |> utils::tail(1L)
        )
      }, condition = \(cond) {
        df = new_df(decision = NA, true_prop = NA_real_)
      })
    } else {
      stop("invalid state")
    }
    
    df$e = list(object$error)
    df$w = list(object$warnings)
    df$is_ok = is.null(object$error) && is.null(object$warnings)
    
    row.names(df) = attr(object, "run_id", TRUE)
    return(df)
  } else {
    stop("invalid state")
  }
}


plot.AcceptanceSampling_lqas_procedure = function(object, ...) {
  args = list(...)
  policy = args$policy %||% "alpha"
  add = args$add %||% FALSE
  flip = args$flip %||% TRUE
  lwd = args$lwd %||% 1L
  xlab = args$xlab %||% NULL
  ylab = args$ylab %||% NULL
  xlim = args$xlim %||% NULL
  ylim = args$ylim %||% NULL
  main = args$main %||% NULL
  stopifnot(exprs = {
    policy %in% c("alpha", "ewma")
    vek::is_lgl_vec_x1(add)
    vek::is_lgl_vec_x1(flip)
  })
  
  if (policy == "alpha") {
    lot_series = object$result$lot_series
    aql = object$result$args$aql
    rql = object$result$args$rql
    decisions = object$result$decision
    is_correct = decisions == (lotsim::get_true_prop(lot_series) <= aql)
    col = c("red", "gray")[is_correct + 1L]
    prop = lotsim::get_prop(lot_series)
    
    if (!add) {
      plot(
        lot_series, what = "prop", col = "gray", flip = flip, xlim = xlim,
        ylim = ylim, main = main, lwd = lwd, xlab = xlab, ylab = ylab
      )
      
      plot(
        lot_series, what = "true_prop", col = "black", add = TRUE, flip = flip)
      
      if (flip)
        graphics::abline(v = c(aql, rql), lty = 2L, col = "black")
      else  
        graphics::abline(h = c(aql, rql), lty = 2L, col = "black")
    }
    
    at_t = 1:lotsim::get_num_lot(lot_series)
    x = if (flip) prop else at_t
    y = if (flip) at_t else prop
    graphics::points(x, y, pch = 20L, col = col)
    rm(x, y)
    
  } else if (policy == "ewma") {
    lot_series = object$result$lot_series
    aql = object$result$args$aql
    rql = object$result$args$rql
    ewma_proc = ewma_procedure(lot_series, 1.95 , 3., .5, aql = aql, rql = rql)
    plot(ewma_proc, ...)
    
  } else {
    stop("policy not recognized")
  }
}
