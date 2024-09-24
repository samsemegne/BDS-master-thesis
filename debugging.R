

remove_broken_procedure_files = function(folder, destination_folder) {
  stopifnot(exprs = {
    vek::is_chr_vec_xb1(folder)
    vek::is_chr_vec_xb1(destination_folder)
    dir.exists(folder)
    dir.exists(destination_folder)
  })
  
  run_ids = get_run_ids_in_folder(folder)
  
  cli::cli_progress_bar(
    total = length(run_ids),
    format = "Fixing {cli::pb_bar} {pb_percent} ETA:{pb_eta}",
    clear = TRUE
  )
  
  for (run_id in run_ids) {
    proc_filenames = c(
      sprintf("run_%s__AcceptanceSampling_lqas_procedure", run_id),
      sprintf("run_%s__custom_bssm_procedure_model1", run_id)
    )
    
    error1 = NULL
    tryCatch({ 
      tmp = load_procedure(run_id, "AcceptanceSampling_lqas_procedure", "", folder)
      rm(tmp)
    }, error = \(e) {
      error1 <<- e
    })
    
    error2 = NULL
    tryCatch({ 
      tmp = load_procedure(run_id, "custom_bssm_procedure", "_model1", folder)
      rm(tmp)
    }, error = \(e) {
      error2 <<- e
    })
    
    if (is.null(error1) && is.null(error2)) {
      cli::cli_progress_update()
      next
    }
    
    if (!is.null(error1))
      cli::cli_alert_info(sprintf("error1: %s", error1$message))
    
    if (!is.null(error2))
      cli::cli_alert_info(sprintf("error2: %s", error2$message))
    
    for (proc_filename in proc_filenames) {
      proc_path = sprintf("%s/%s", folder, proc_filename)
      if (file.exists(proc_path)) {
        proc_destination_path = sprintf(
          "%s/%s_%s", destination_folder, folder, proc_filename)
        
        ok = file.rename(proc_path, proc_destination_path)
        
        if (!ok) {
          cli::cli_alert_danger(sprintf("`%s` Failed to move", proc_path))
        }
        else {
          cli::cli_alert_success(sprintf("Moved `%s`", proc_path))
          cli::cli_alert_success(sprintf("To `%s`", proc_destination_path))
        }
      }
    }
    
    cli::cli_progress_update()
  }
  
  cli::cli_progress_done()
}


find_common_run_ids = function(folder_a, folder_b) {
  stopifnot(exprs = {
    vek::is_chr_vec_xb1(folder_a)
    vek::is_chr_vec_xb1(folder_b)
    dir.exists(folder_a)
    dir.exists(folder_b)
  })
  
  ids_a = get_run_ids_in_folder(folder_a)
  ids_b = get_run_ids_in_folder(folder_b)
  common_ids = intersect(ids_a, ids_b)
  
  if (length(common_ids) == 0L) {
    print("No common run_id's")
  }
  else {
    print("Common run_id's:")
    for (id in common_ids) print(id)
  }
}