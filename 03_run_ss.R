
#devtools::install_github("r4ss/r4ss") # version 1.40.1
source("00_functions_ss.R")

dirs <- system("ls SS", intern = TRUE)
dirs <- dirs[grepl("03A", dirs) & !grepl("prof", dirs)]

for(i in 1:length(dirs)) {
  #run_ss(dirs[i])
  plot_ss(dirs[i], FALSE, FALSE)
}



dir <- "03A_SSF_0.6Minas"
dir <- "01A_CSF_0.6Minas"
dir <- "02A_CSF_0.6Minas_tags"
run_ss(dir)
run_ss(dir, covar = FALSE)

replist <- plot_ss(dir, FALSE)
replist <- r4ss::SS_output(file.path(getwd(), "SS", dir), covar = FALSE, verbose = FALSE)
r4ss::SS_plots(replist)
r4ss::SSplotSelex(replist, fleets = 1)
r4ss::SSplotMovementRates(replist)

replist$timeseries %>% View()
replist$derived_quants %>% View()
replist$estimated_non_dev_parameters
replist$movement


## ENSURE catch multiplier works
plot(replist$timeseries$Yr, replist$timeseries$`obs_cat:_3`, typ = 'o')
lines(replist$timeseries$Yr, replist$timeseries$`dead(B):_3`, typ = 'o', col = 'red')

replist$timeseries$`dead(B):_3`/replist$timeseries$`obs_cat:_3`


# Get the sex-specific catches from the combined-sex fleet model
SSC_from_CSF("01A_CSF_A_0.6Minas")
SSC_from_SSF("03A_SSF_A_0.6Minas")
