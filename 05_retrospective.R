library(dplyr)
dir <- "01_base"
dir <- "02_agesel"

r4ss::SS_doRetro(masterdir = file.path(getwd(), "SS"), oldsubdir = dir,
                 years = seq(0, -10, -2), extras = "-nox -nohess")

retroModels <- r4ss::SSgetoutput(dirvec = file.path(getwd(), "SS", "retrospectives", paste0("retro", seq(0, -10, -2))))
retroSummary <- SSsummarize(retroModels)
endyrvec <- retroSummary$endyrs + seq(0, -10, -2)
SSplotComparisons(retroSummary, endyrvec = endyrvec, legendlabels = paste("Retro", endyrvec))
