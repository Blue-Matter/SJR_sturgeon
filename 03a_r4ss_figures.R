profilemodels <- r4ss::SSgetoutput(dirvec = file.path(getwd(), "SS", dir), keyvec = 1:length(h))
profilesum <- r4ss::SSsummarize(profilemodels)
r4ss::SSplotProfile(profilesum)
dev.off()
# SSB
r4ss::SSplotComparisons(profilesum, legendlabels = paste("h =", h), subplots = 1, btarg = -1, minbthresh = -1)

# depletion
r4ss::SSplotComparisons(profilesum, legendlabels = paste("h =", h), subplots = 3, btarg = -1, minbthresh = -1)

# SPR
r4ss::SSplotComparisons(profilesum, legendlabels = paste("h =", h), subplots = 5, btarg = -1, minbthresh = -1)

# F
r4ss::SSplotComparisons(profilesum, legendlabels = paste("h =", h), subplots = 7, btarg = -1, minbthresh = -1)

# Recruitment
r4ss::SSplotComparisons(profilesum, legendlabels = paste("h =", h), subplots = 9, btarg = -1, minbthresh = -1)

# Index fit
r4ss::SSplotComparisons(profilesum, legendlabels = paste("h =", h), subplots = 13, btarg = -1, minbthresh = -1)


