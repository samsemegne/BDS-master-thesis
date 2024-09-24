

simulate_lot_series = function(args) {
  if (args$data_type == "1")
    return(gen_data_type1(args))
  else if (args$data_type == "2")
    return(gen_data_type2(args))
  if (args$data_type == "3")
    return(gen_data_type3(args))
  else if (args$data_type == "4")
    return(gen_data_type4(args))
  else {
    stop("'data_type' not recognized")
  }
}


# Generates runs of alternating quality: good, bad, good, etc.
gen_data_type1 = function(args) {
  gray_zone = args$rql - args$aql
  min_jump = .001 #if (gray_zone > .001) .001 else 0.
  min_run_len = 8L
  k = sample(c(0L, 1L), 1L, FALSE, NULL) # Determines whether series starts good

  true_prop = rep(args$aql, args$t_len) |>
    lotsim::gen_runs(\(prev_vals, i) {
      if (i %% 2L == k) {
        run_len = min_run_len + sample.int(32L - min_run_len, 1L)
        p = -stats::runif(1L, .001, .01)
      }
      else {
        run_len = min_run_len + sample.int(32L - min_run_len, 1L)
        p = stats::runif(1L, min_jump, gray_zone)
      }
      return(rep(p, run_len))
  }) |>
    lotsim::clamp(0L, 1L)

  true_count = as.integer(round(true_prop * args$batch_size))

  lotsim::new_lot_series__shuffle(args$batch_size, true_count)
}


# Generates runs of alternating quality: good, bad, good, etc.
gen_data_type2 = function(args) {
  gray_zone = args$rql - args$aql
  min_jump = if (gray_zone > .001) .001 else 0.

  k = sample(c(0L, 1L), 1L, FALSE, NULL) # Determines whether series starts good

  true_prop = rep(args$aql, args$t_len) |>
    lotsim::gen_runs(\(prev_vals, i) {
      if (i %% 2L == k) {
        # good
        min_run_len = 8L
        run_len = min_run_len + sample.int(32L - min_run_len, 1L)
        p = -stats::runif(1L, .001, .01)
      }
      else {
        # bad
        min_run_len = 8L#8L
        run_len = min_run_len + sample.int(16L - min_run_len, 1L)
        p = stats::runif(1L, min_jump, gray_zone)
      }
      return(rep(p, run_len))
  }) |>
    lotsim::clamp(0L, 1L)

  true_count = as.integer(round(true_prop * args$batch_size))

  lotsim::new_lot_series__shuffle(args$batch_size, true_count)
}


# Generates runs of alternating quality: good, bad, good, etc.
gen_data_type3 = function(args) {
  gray_zone = args$rql - args$aql
  min_jump = if (gray_zone > .001) .001 else 0.

  k = sample(c(0L, 1L), 1L, FALSE, NULL) # Determines whether series starts good

  true_prop = rep(args$aql, args$t_len) |>
    lotsim::gen_runs(\(prev_vals, i) {
      if (i %% 2L == k) {
        # good
        min_run_len = 1L
        run_len = min_run_len + sample.int(8L - min_run_len, 1L)
        p = -stats::runif(1L, .001, .01)
      }
      else {
        # bad
        min_run_len = 1L#8L
        run_len = min_run_len + sample.int(8L - min_run_len, 1L)
        p = stats::runif(1L, min_jump, gray_zone)
      }
      return(rep(p, run_len))
  }) |>
    lotsim::clamp(0L, 1L)

  true_count = as.integer(round(true_prop * args$batch_size))

  lotsim::new_lot_series__shuffle(args$batch_size, true_count)
}


gen_data_type4 = function(args) {
  gray_zone = args$rql - args$aql
  min_jump = if (gray_zone > .001) .001 else 0.

  k = sample(c(0L, 1L), 1L, FALSE, NULL) # Determines whether series starts good

  true_prop = rep(args$aql, args$t_len) |>
    lotsim::gen_runs(\(prev_vals, i) {
      if (i %% 2L == k) {
        # good
        min_run_len = 1L
        run_len = min_run_len + sample.int(8L - min_run_len, 1L)
        p = -stats::runif(1L, .001, .01)
      }
      else {
        # bad
        min_run_len = 1L#8L
        run_len = min_run_len + sample.int(4L - min_run_len, 1L)
        p = stats::runif(1L, min_jump, gray_zone)
      }
      return(rep(p, run_len))
  }) |>
    lotsim::clamp(0L, 1L)

  true_count = as.integer(round(true_prop * args$batch_size))

  lotsim::new_lot_series__shuffle(args$batch_size, true_count)
}


