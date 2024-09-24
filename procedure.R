

is.procedure = function(object) inherits(object, "procedure", FALSE)

to_df_row = function(object, policy, ...) UseMethod("to_df_row")

get_procedure_types = function() {
  c("AcceptanceSampling_lqas_procedure",
    "custom_bssm_procedure",
    "bssm_procedure")
}


save_procedure = function(object, suffix = "", folder = "simulations") {
  stopifnot(exprs = { 
    is.procedure(object)
    vek::is_chr_vec_xb1(folder)
    vek::is_chr_vec_x1(suffix)
    dir.exists(folder)
  })
  
  procedure_type = class(object)[1L]
  run_id = attr(object, "run_id", TRUE) %||%
    stop("procedure has no run_id attr")
  
  path = sprintf("%s/run_%s__%s%s", folder, run_id, procedure_type, suffix)
  
  saveRDS(
    object, 
    file = path,
    ascii = FALSE,
    version = 3L,
    compress = "gzip",
    refhook = NULL
  )
  
  return(invisible(path))
}


load_procedure = function(run_id, procedure_type, suffix = "", folder = "simulations") {
  stopifnot(exprs = {
    vek::is_int_vec_x1(run_id) || vek::is_chr_vec_xb1(run_id)
    vek::is_chr_vec_xb1(procedure_type)
    vek::is_chr_vec_x1(suffix)
    vek::is_chr_vec_xb1(folder)
    dir.exists(folder)
  })
  
  path = sprintf("%s/run_%s__%s%s", folder, run_id, procedure_type, suffix)
  x = readRDS(file = path, refhook = NULL)
  return(x)
}


# Summarizes the results of individual simulation run procedures.
get_results_df = function(
    procedure_type, 
    suffix, 
    policy, 
    policy_args, 
    folder,
    ...
  ) 
{
  stopifnot(exprs = {
    vek::is_chr_vec_xb1(procedure_type)
    vek::is_chr_vec_xb1(policy)
    vek::is_chr_vec_x1(suffix)
    vek::is_chr_vec_xb1(folder)
    lotsim:::is_list(policy_args)
    dir.exists(folder)
    procedure_type %in% get_procedure_types()
  })
  
  run_ids = get_run_ids_in_folder(folder)
  
  cli::cli_alert_info(sprintf(
    "Getting results %s, %s, %s, %s", folder, procedure_type, suffix, policy))
  
  cli::cli_progress_bar(
    total = length(run_ids),
    format = "Results {cli::pb_bar} {pb_percent} ETA:{pb_eta}",
    clear = TRUE
  )
  
  options(cli.progress_show_after = 0L)
  
  df = list()
  for (runid in run_ids) {
    proc_path = sprintf("%s/run_%s__%s%s", 
                        folder, runid, procedure_type, suffix)
    
    if (!file.exists(proc_path)) {
      cli::cli_alert_danger("`%s` file doesn't exist")
      cli::cli_progress_update()
      next
    }
    
    tryCatch({
      proc = load_procedure(runid, procedure_type, suffix, folder)
      proc_row = to_df_row(proc, policy, policy_args)
      df[[length(df) + 1L]] = proc_row
      rm(proc)
    }, error = \(e) {
      e_msg = sprintf("`%s` unexpected error", proc_path)
      cli::cli_alert_danger(e_msg)
      cli::cli_alert_danger(e$message)
    })
    
    cli::cli_progress_update()
  }
  
  cli::cli_process_done()
  
  df = do.call(rbind, df)
  
  structure(
    df,
    procedure_type = procedure_type,
    suffix = suffix,
    policy = policy,
    policy_args = policy_args,
    folder = folder,
    ...
  )
}


get_data_descriptives_df = function(
    procedure_type, 
    suffix, 
    folder,
    study = "main",
    ...
) 
{
  stopifnot(exprs = {
    vek::is_chr_vec_xb1(procedure_type)
    vek::is_chr_vec_x1(suffix)
    vek::is_chr_vec_xb1(folder)
    vek::is_chr_vec_xb1(study)
    dir.exists(folder)
    procedure_type %in% get_procedure_types()
    study %in% c("main", "post")
  })
  
  run_ids = get_run_ids_in_folder(folder)
  
  #cli::cli_alert_info(sprintf(
  #  "Getting results %s, %s, %s, %s", folder, procedure_type, suffix, policy))
  
  cli::cli_progress_bar(
    total = length(run_ids),
    format = "Results {cli::pb_bar} {pb_percent} ETA:{pb_eta}",
    clear = TRUE
  )
  
  options(cli.progress_show_after = 0L)
  
  df = list()
  for (runid in run_ids) {
    proc_path = sprintf("%s/run_%s__%s%s", 
                        folder, runid, procedure_type, suffix)
    
    if (!file.exists(proc_path)) {
      cli::cli_alert_danger("`%s` file doesn't exist")
      cli::cli_progress_update()
      next
    }
    
    tryCatch({
      proc = load_procedure(runid, procedure_type, suffix, folder)
      if (study == "main") {
        proc_row = data_descriptives_df_row(
          proc$result$lot_series,
          proc$result$args$aql
        )
      } else if (study == "post") {
        proc_row = data_descriptives2_df_row(
          proc$result$lot_series,
          proc$result$args$aql
        )
      } else {
        stop("invalid state")
      }
      
      row.names(proc_row) = runid
      
      df[[length(df) + 1L]] = proc_row
      rm(proc)
    }, error = \(e) {
      e_msg = sprintf("`%s` unexpected error", proc_path)
      cli::cli_alert_danger(e_msg)
      cli::cli_alert_danger(e$message)
    })
    
    cli::cli_progress_update()
  }
  
  cli::cli_process_done()
  
  df = do.call(rbind, df)
  
  structure(
    df,
    procedure_type = procedure_type,
    suffix = suffix,
    folder = folder,
    ...
  )
}


data_descriptives2_df_row = function(lot_series, prop_treshold) {
  stopifnot(exprs = { 
    lotsim::is.lot_series(lot_series)
    lotsim:::is_prop_xy1(prop_treshold)
    prop_treshold > 0L
    prop_treshold < 1L
  })
  
  true_prop = lotsim::get_true_prop(lot_series) |> utils::tail(1L)
  prop = lotsim::get_prop(lot_series) |> utils::tail(1L)
  
  return(new_df(
    true_prop = true_prop,
    prop = prop,
    is_acceptable = true_prop <= prop_treshold
  ))
}


data_descriptives_df_row = function(lot_series, prop_treshold) {
  stopifnot(exprs = { 
    lotsim::is.lot_series(lot_series)
    lotsim:::is_prop_xy1(prop_treshold)
    prop_treshold > 0L
    prop_treshold < 1L
  })
  
  true_prop = lotsim::get_true_prop(lot_series)
  prop = lotsim::get_prop(lot_series)
  true_prop_x = true_prop[true_prop > prop_treshold]
  prop_x = prop[prop > prop_treshold]
  
  df = new_df(
    min_true_prop = min(true_prop, na.rm = FALSE),
    max_true_prop = max(true_prop, na.rm = FALSE),
    min_prop = min(prop, na.rm = FALSE),
    max_prop = max(prop, na.rm = FALSE),
    mean_prop = mean(prop),
    mean_true_prop = mean(true_prop),
    sd_prop = stats::sd(prop),
    sd_true_prop = stats::sd(true_prop),
    min_true_prop_x = min(true_prop_x, na.rm = FALSE),
    max_true_prop_x = min(true_prop_x, na.rm = FALSE),
    min_prop_x = min(prop_x, na.rm = FALSE),
    max_prop_x = min(prop_x, na.rm = FALSE),
    mean_prop_x = mean(prop_x),
    mean_true_prop_x = mean(true_prop_x),
    sd_prop_x = stats::sd(prop_x),
    sd_true_prop_x = stats::sd(true_prop_x)
  )
  
  stopifnot(exprs = {
    all(apply(df, 2L, lotsim:::is_prop_xy1, simplify = TRUE), na.rm = FALSE)
  })
  
  df$true_prop = list(true_prop)
  df$prop = list(prop)
  
  stopifnot(nrow(df) == 1L)
  
  return(df)
}


empty_data_descriptives_df_row = function() {
  df = new_df(
    min_true_prop = NA_real_,
    max_true_prop = NA_real_,
    min_prop = NA_real_,
    max_prop = NA_real_,
    mean_prop = NA_real_,
    mean_true_prop = NA_real_,
    sd_prop = NA_real_,
    sd_true_prop = NA_real_,
    min_true_prop_x = NA_real_,
    max_true_prop_x = NA_real_,
    min_prop_x = NA_real_,
    max_prop_x = NA_real_,
    mean_prop_x = NA_real_,
    mean_true_prop_x = NA_real_,
    sd_prop_x = NA_real_,
    sd_true_prop_x = NA_real_
  )
  
  df$true_prop = list()
  df$prop = list()
  
  return(df)
}


get_run_ids_in_folder = function(folder) {
  stopifnot(exprs = {
    vek::is_chr_vec_xb1(folder)
    dir.exists(folder)
  })
  
  file_names = dir(
    path = folder,
    pattern = NULL,
    all.files = FALSE,
    full.names = FALSE,
    recursive = FALSE,
    ignore.case = FALSE,
    include.dirs = FALSE,
    no.. = FALSE
  )
  
  run_ids = lapply(file_names, \(x) {
    parts = strsplit(x, "_", FALSE, FALSE, FALSE)
    return(parts[[1L]][2])
  }) |>
    unlist(FALSE, FALSE) |>
    unique(FALSE)
  
  return(run_ids)
}


confusion_df = function(decisions, lot_series, prop_treshold) {
  stopifnot(exprs = {
    vek::is_lgl_vec_x(decisions)
    lotsim::is.lot_series(lot_series)
    lotsim:::is_prop_xy1(prop_treshold)
    length(decisions) > 0L
    length(decisions) == lotsim::get_num_lot(lot_series)
  })
  
  true_prop = lotsim::get_true_prop(lot_series)
  total = length(decisions)
  
  x = new_df(
    true_pos =  sum(decisions & true_prop <= prop_treshold, na.rm = FALSE),
    true_neg =  sum(!decisions & true_prop > prop_treshold, na.rm = FALSE),
    false_pos = sum(decisions & true_prop > prop_treshold, na.rm = FALSE),
    false_neg = sum(!decisions & true_prop <= prop_treshold, na.rm = FALSE)
  )
  
  stopifnot(nrow(x) == 1L)
  
  x$p_true_pos =  x$true_pos / total
  x$p_true_neg =  x$true_neg / total
  x$p_false_pos = x$false_pos / total
  x$p_false_neg = x$false_neg / total
  
  x$acc = x$true_pos + x$true_neg
  x$p_acc = x$acc / total
  x$sens = x$true_pos / (x$true_pos + x$false_neg)
  x$spec = x$true_neg / (x$true_neg + x$false_pos)
  
  stopifnot(exprs = {
    lotsim:::is_prop_xy(x$p_acc)
    lotsim:::is_prop_xy(x$sens)
    lotsim:::is_prop_xy(x$spec)
    lotsim:::is_prop_xy(x$p_true_pos)
    lotsim:::is_prop_xy(x$p_true_neg)
    lotsim:::is_prop_xy(x$p_false_pos)
    lotsim:::is_prop_xy(x$p_false_neg)
    all(x$true_pos + x$false_pos + x$true_neg + x$false_neg == total,
        na.rm = FALSE)
    
    nrow(x) == 1L
  })
  
  return(x)
}


get_empty_confusion_df = function() {
  new_df(
    true_pos = NA_integer_,
    true_neg = NA_integer_,
    false_pos = NA_integer_,
    false_neg = NA_integer_,
    p_true_pos = NA_real_,
    p_true_neg = NA_real_,
    p_false_pos = NA_real_,
    p_false_neg = NA_real_,
    acc = NA_integer_,
    p_acc = NA_real_,
    sens = NA_real_,
    spec = NA_real_
  )
}


get_warning_messages = function(object, max_len = 64L) {
  stopifnot(exprs = {
    is.procedure(object)
    vek::is_int_vec_x1(max_len)
    max_len > 0L
  })
  
  # Get the warning messages, truncate them if they're long, and remove
  # newlines.
  w = lapply(object$warnings, \(x) {
    k = (x$message %||% "[warning message empty]") |> elipsis(max_len)
    return(gsub("\\n", " ", k))
  }) |>
    unlist(FALSE, FALSE)
  
  # Number each warning.
  w = paste(1:length(w), w, sep = ". ", recycle0 = FALSE)
  return(w)
}

