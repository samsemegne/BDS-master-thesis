
is.ewma_procedure = function(object) {
  inherits(object, "ewma_procedure", FALSE)
}


ewma_procedure = function(lot_series, mu, a, lambda, ...) {
  stopifnot(exprs = {
    lotsim::is.lot_series(lot_series)
    vek::is_num_vec_xyz1(lambda) 
    vek::is_num_vec_xyz1(mu) 
    vek::is_num_vec_xyz1(a)
    lambda > 0L
    lambda < 1L
    a > 0L
  })
  
  f = function() {
    counts = lotsim::get_count(lot_series)
    vals = ewma(counts, lambda, TRUE)
    ucl = ewma_ucl(counts, mu, a, lambda, TRUE)
    decisions = vals <= ucl
    
    obj = list(
      mu = mu,
      a = a,
      lambda = lambda,
      lot_series = lot_series,
      vals = vals,
      ucl = ucl,
      decisions = decisions
    )
    
    obj = c(obj, list(...))
    return(obj)
  }
  
  object = safely(f())
  
  class(object) = c("ewma_procedure", "procedure")
  return(object)
}


  
# val: a real number
# prev_val: a real number
ewma_step = function(val, prev_val, lambda) {
  stopifnot(exprs = {
    vek::is_num_vec_xyz1(val)
    vek::is_num_vec_xyz1(lambda)
    lambda > 0L
    lambda < 1L
  })
  
  if (!is.null(prev_val))
    stopifnot(vek::is_num_vec_xyz1(prev_val))
  
  if (is.null(prev_val))
    return(val)
  else
    return(lambda * val + (1 - lambda) * prev_val)
}


ewma = function(series, lambda, keep_all) {
  stopifnot(exprs = {
    vek::is_num_vec_xyz(series)
    vek::is_num_vec_xyz1(lambda)
    vek::is_lgl_vec_x1(keep_all)
    length(series) > 0L
    lambda > 0L
    lambda < 1L
  })
  
  all_prev = numeric()
  
  prev_val = NULL
  for (val in series) {
    prev_val = ewma_step(val, prev_val, lambda)
    
    if (keep_all)
      all_prev = c(all_prev, prev_val)
  }
  
  stopifnot(exprs = {
    vek::is_num_vec_xyz1(prev_val)
    vek::is_num_vec_xyz(all_prev)
  })
  
  if (keep_all)
    return(all_prev)
  else
    return(prev_val)
}


# DOI: 10.1080/00224065.1998.11979871
ewma_ucl = function(series, mu, a, lambda, keep_all) {
  stopifnot(exprs = {
    vek::is_num_vec_xyz(series)
    vek::is_num_vec_xyz1(lambda) 
    vek::is_num_vec_xyz1(mu) 
    vek::is_num_vec_xyz1(a) 
    vek::is_lgl_vec_x1(keep_all)
    length(series) > 0L
    lambda > 0L
    lambda < 1L
    a > 0L
  })
  
  t = 1:length(series)
  ucl = mu + a * sqrt((lambda * mu) / (2L - lambda) *
                        (1L - (1L - lambda)^(2L * t)))
  
  if (keep_all)
    return(ucl)
  else
    return(ucl[length(ucl)])
}


plot.ewma_procedure = function(object, ...) {
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
  
  lot_series = object$result$lot_series
  aql = object$result$aql
  rql = object$result$rql
  decisions = object$result$decisions
  is_correct = decisions == (lotsim::get_true_prop(lot_series) <= aql)
  col = c("red", "gray")[is_correct + 1L]
  prop = lotsim::get_prop(lot_series)
  
  plot(
    lot_series, what = "prop", col = "gray", xlim = xlim, lwd = lwd,
    xlab = xlab, ylab = ylab, flip = flip, ylim = ylim, main = main,
    xaxt = xaxt, yaxt = yaxt, add = add
  )
  
  plot(
    lot_series, what = "true_prop", col = "black", flip = flip, add = TRUE,
    lwd = lwd
  )
  
  at_t = 1:lotsim::get_num_lot(lot_series)
  lot_size = lotsim::get_size(lot_series)[1L]
  ucl_prop = object$result$ucl / lot_size
  weighted_prop = object$result$vals / lot_size
  
  x = if (flip) weighted_prop else at_t
  y = if (flip) at_t else weighted_prop
  graphics::lines(x, y, lty = 1L, col = "#2D708EFF", lwd = lwd)
  
  x = if (flip) ucl_prop else at_t
  y = if (flip) at_t else ucl_prop
  graphics::lines(x, y, lty = 2L, col = "#2D708EFF", lwd = lwd)
  
  # Plot decision outcomes
  #x = if (flip) prop else at_t
  #y = if (flip) at_t else prop
  #points(x, y, pch = 20L, col = col)
}