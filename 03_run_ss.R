
devtools::install_github("r4ss/r4ss") # version 1.40.1

run_ss <- function(dir) {
  curdir <- getwd()
  on.exit(setwd(curdir))
  
  setwd(file.path(curdir, "SS", dir))
  out <- system("ss -nox")
  return(invisible(out))
}

plot_ss <- function(dir) {
  curdir <- getwd()
  on.exit(setwd(curdir))
  
  setwd(file.path(curdir, "SS", dir))
  replist <- r4ss::SS_output(getwd())
  replist$btarg <- replist$minbthresh <- -1
  
  r4ss::SS_plots(replist)
  return(invisible(replist))
}


dir <- "01_base"
dir <- "02_agesel"
run_ss(dir)

replist <- plot_ss(dir)
replist <- r4ss::SS_output(file.path(getwd(), "SS", dir))

replist$timeseries %>% View()
replist$derived_quants %>% View()
replist$estimated_non_dev_parameters
