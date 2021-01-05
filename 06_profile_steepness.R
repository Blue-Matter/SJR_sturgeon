

dir <- "01_base_profh"

# Profile steepness
h <- seq(0.25, 0.7, 0.05)
prof <- r4ss::SS_profile(dir = file.path(getwd(), "SS", dir),
                         linenum = 115, profilevec = h, extras = "-nox -nohess")
profilemodels <- r4ss::SSgetoutput(dirvec = file.path(getwd(), "SS", dir), keyvec = 1:length(h))
profilesum <- r4ss::SSsummarize(profilemodels)
r4ss::SSplotProfile(profilesum)

r4ss::SSplotComparisons(profilesum, legendlabels = paste("h =", h))
dev.off()
r4ss::SSplotComparisons(profilesum, legendlabels = paste("h =", h), subplots = 3, btarg = -1, minbthresh = -1)
