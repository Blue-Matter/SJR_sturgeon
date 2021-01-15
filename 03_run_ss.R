
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

plot_ss <- function(dir, verbose = TRUE) {
  curdir <- getwd()
  on.exit(setwd(curdir))
  
  setwd(file.path(curdir, "SS", dir))
  replist <- r4ss::SS_output(getwd(), verbose = verbose)
  replist$btarg <- replist$minbthresh <- -1
  
  r4ss::SS_plots(replist, verbose = verbose)
  return(invisible(replist))
}


dir <- "01_base_offset"
dir <- "02_agesel"
dir <- "05_maxF_high"
dir <- "03_area"
run_ss(dir)

replist <- plot_ss(dir, FALSE)
replist <- r4ss::SS_output(file.path(getwd(), "SS", dir))

r4ss::SSplotMovementRates(replist)

replist$timeseries %>% View()
replist$derived_quants %>% View()
replist$estimated_non_dev_parameters
replist$movement


## ENSURE catch multiplier works
plot(replist$timeseries$Yr, replist$timeseries$`obs_cat:_3`, typ = 'o')
lines(replist$timeseries$Yr, replist$timeseries$`dead(B):_3`, typ = 'o', col = 'red')

replist$timeseries$`dead(B):_3`/replist$timeseries$`obs_cat:_3`
