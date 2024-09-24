
source("load.R")


f = function(num_runs, data_type) {
  sim_run_args = create_sim_run_params(num_runs, data_type)
  num_runs = nrow(sim_run_args) # temp
  
  cli::cli_progress_bar(
    total = num_runs,
    format = "{cli::pb_bar} {pb_percent} @ {Sys.time()} ETA:{pb_eta}"
  )

  options(cli.progress_show_after = 0L)
  cli::cli_alert_info("Starting simulation, at {Sys.time()}")

  i = 1L
  while (i <= num_runs) {
    tryCatch({
      suppressWarnings({
        sim_run(sim_run_args[i, ])
      })
    }, error = \(e) {
      cli::cli_alert_danger(sprintf("Unexpected error: %s", e$message))
    })

    cli::cli_progress_update()
    i = i + 1L
  }

  cli::cli_process_done()
  cli::cli_alert_info("Finished, at {Sys.time()}")
}


#create_folder_if_it_doesnt_exist()
#f(1L, "1")
#shell(paste('rename', "simulations", "flats_100(3)"))


#create_folder_if_it_doesnt_exist()
#f(2L, "2")
#shell(paste('rename', "simulations", "flats_16-32_100(3)"))
#
#
create_folder_if_it_doesnt_exist()
f(3L, "3")
shell(paste('rename', "simulations", "spikes_8-8_100(3-1)"))
#
#
#create_folder_if_it_doesnt_exist()
#f(4L, "4")
#shell(paste('rename', "simulations", "spikes_4-8_100(3-2)"))


