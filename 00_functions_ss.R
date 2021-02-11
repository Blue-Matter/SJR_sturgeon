
# For 03_run_ss.R

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

plot_ss <- function(dir, verbose = TRUE, openfile = TRUE) {
  curdir <- getwd()
  on.exit(setwd(curdir))
  
  setwd(file.path(curdir, "SS", dir))
  replist <- r4ss::SS_output(getwd(), verbose = verbose)
  replist$btarg <- replist$minbthresh <- -1
  
  r4ss::SS_plots(replist, verbose = verbose, openfile = openfile)
  return(invisible(replist))
}
