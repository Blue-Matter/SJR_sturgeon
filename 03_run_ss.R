
devtools::install_github("r4ss/r4ss") # version 1.40.1

run_ss <- function(dir, covar = TRUE, output = FALSE) {
  curdir <- getwd()
  on.exit(setwd(curdir))
  
  setwd(file.path(curdir, "SS", dir))
  if(covar) {
    out <- system("ss -nox", show.output.on.console = output)
  } else {
    out <- system("ss -nox -nohess", show.output.on.console = output)
  }
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
dir <- "05_maxF_high"
dir <- "03_area"
run_ss(dir)

replist <- plot_ss(dir)
replist <- r4ss::SS_output(file.path(getwd(), "SS", dir))

r4ss::SSplotMovementRates(replist)

replist$timeseries %>% View()
replist$derived_quants %>% View()
replist$estimated_non_dev_parameters
replist$movement
