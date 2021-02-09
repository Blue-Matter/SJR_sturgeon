source("00_functions_report_figures.R")

dir <- "03A_SSF_0.6BOF"

r4ss::SS_doRetro(masterdir = file.path(getwd(), "SS"), oldsubdir = dir,
                 years = seq(0, -10, -1), extras = "-nox -nohess")

retroModels <- r4ss::SSgetoutput(dirvec = file.path(getwd(), "SS", dir, "retrospectives", 
                                                    paste0("retro", seq(0, -10, -1))))
retroSummary <- r4ss::SSsummarize(retroModels)
endyrvec <- retroSummary$endyrs + seq(0, -10, -1)

compare_SSB(retroModels[1:7], c("Reference", paste0("Retro ", endyrvec[2:7])), 
            forecast = FALSE, retro_yr = endyrvec[1:7])
ggsave("figures/assess/retro.png", height = 4, width = 7)

#r4ss::SSplotComparisons(retroSummary, endyrvec = endyrvec, legendlabels = paste("Retro", endyrvec))
#r4ss::SSmohnsrho(retroSummary, endyrvec = endyrvec)


dir <- "01A_CSF_0.6BOF"

r4ss::SS_doRetro(masterdir = file.path(getwd(), "SS"), oldsubdir = dir,
                 years = seq(0, -10, -1), extras = "-nox -nohess")

retroModels <- r4ss::SSgetoutput(dirvec = file.path(getwd(), "SS", dir, "retrospectives", 
                                                    paste0("retro", seq(0, -10, -1))))
retroSummary <- r4ss::SSsummarize(retroModels)
endyrvec <- retroSummary$endyrs + seq(0, -10, -1)

compare_SSB(retroModels[1:7], c("Reference", paste0("Retro ", endyrvec[2:7])), 
            forecast = FALSE, retro_yr = endyrvec[1:7])
ggsave("figures/assess/retro_CSF.png", height = 4, width = 7)

# Calculate Mohn's rho
SSB_Mohns_rho <- function(dir, retro_yr) {
  if(is.list(dir)) {
    report <- dir
  } else {
    report <- lapply(dir, function(x) r4ss::SS_output(file.path(getwd(), "SS", x)))
  }
  
  SSB <- Map(function(replist, retro_yr, min_yr) {
    replist$timeseries$SpawnBio[replist$timeseries$Yr <= retro_yr & replist$timeseries$Yr >= min_yr]
  }, replist = report, retro_yr = retro_yr, MoreArgs = list(min_yr = min(retro_yr)))
  
  SSB_ref <- SSB[[which.max(retro_yr)]]
  
  Mohns_rho <- vapply(SSB[-which.max(retro_yr)], function(x) {
    x[length(x)]/SSB_ref[length(x)] - 1
  }, numeric(1))
  
  mean(Mohns_rho)
}
SSB_Mohns_rho(report[1:7], endyrvec[1:7])

