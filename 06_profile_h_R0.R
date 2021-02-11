
source("00_functions_report_figures.R")

do_profile <- function(dir, linenum, prof_vec) {
  prof <- r4ss::SS_profile(dir = file.path(getwd(), "SS", dir),
                           linenum = linenum, profilevec = prof_vec, extras = "-nox -nohess", 
                           prior_check = FALSE, verbose = FALSE)
  return(invisible(prof))
}


# Profile steepness in the SSF
dir <- "03A_SSF_0.6BOF_profh"
h <- seq(0.45, 0.85, 0.05)
do_profile(dir, 118, h)

profilemodels <- r4ss::SSgetoutput(dirvec = file.path(getwd(), "SS", dir), keyvec = 1:length(h))
profilesum <- r4ss::SSsummarize(profilemodels)

png("figures/assess/profile_h.png", height = 4, width = 6, res = 400, units = "in")
par(mar = c(5, 4, 1, 1))
r4ss::SSplotProfile(profilesum)
abline(v = 0.6, lty = 3)
dev.off()

dir <- "03A_SSF_0.6BOF_profh_downweight_CAL"
h <- seq(0.45, 0.85, 0.05)
do_profile(dir, 118, h)

profilemodels <- r4ss::SSgetoutput(dirvec = file.path(getwd(), "SS", dir), keyvec = 1:length(h))
profilesum <- r4ss::SSsummarize(profilemodels)

png("figures/assess/profile_h_downweight_CAL.png", height = 4, width = 6, res = 400, units = "in")
par(mar = c(5, 4, 1, 1))
r4ss::SSplotProfile(profilesum, legendloc = "topleft")
abline(v = 0.6, lty = 3)
dev.off()

compare_SSB(profilemodels, model_names = paste("h =", format(h)))
ggsave("figures/assess/profile_h_SSB.png", height = 4, width = 7)

compare_SSB(profilemodels, model_names = paste("h =", format(h)), type = "SSBMSY")
ggsave("figures/assess/profile_h_SSBMSY.png", height = 4, width = 7)

compare_SSB(profilemodels, model_names = paste("h =", format(h)), type = "SSB0")
ggsave("figures/assess/profile_h_SSB0.png", height = 4, width = 7)

compare_CAL(profilemodels, model_names = paste("h =", format(h)), fleet = 1, "SJR F")
compare_CAL(profilemodels, model_names = paste("h =", format(h)), fleet = 2, "SJR M")
compare_CAL(profilemodels, model_names = paste("h =", format(h)), fleet = 5, "BOF")

compare_recruitment(profilemodels, model_names = paste("h =", format(h)))
ggsave("figures/assess/profile_h_recruitment.png", height = 4, width = 7)

ref_pt_SSF <- data.frame(`Ref.pt.` = c("F0.1", "F50%"), value = c(0.1, 0.06))
compare_F(profilemodels, model_names = paste("h =", format(h)), type2 = "summary", ylim = c(0, 0.15)) +
  geom_hline(data = ref_pt_SSF, aes(yintercept = value, linetype = `Ref.pt.`))
ggsave("figures/assess/profile_h_F.png", height = 4, width = 7)



# Profile steepness in the CSF
dir <- "01A_CSF_0.6BOF_profh"
h <- seq(0.45, 0.85, 0.05)
do_profile(dir, 116, h)

profilemodels <- r4ss::SSgetoutput(dirvec = file.path(getwd(), "SS", dir), keyvec = 1:length(h))
#profilesum <- r4ss::SSsummarize(profilemodels)
#
#png("figures/assess/profile_h.png", height = 4, width = 6, res = 400, units = "in")
#par(mar = c(5, 4, 1, 1))
#r4ss::SSplotProfile(profilesum)
#abline(v = 0.6, lty = 3)
#dev.off()

compare_SSB(profilemodels, model_names = paste("h =", format(h)))
ggsave("figures/assess/profile_h_SSB_CSF.png", height = 4, width = 7)

compare_SSB(profilemodels, model_names = paste("h =", format(h)), type = "SSBMSY") + 
  labs(y = expression(SSB/SSB[MSY]))
ggsave("figures/assess/profile_h_SSBMSY_CSF.png", height = 4, width = 7)

compare_SSB(profilemodels, model_names = paste("h =", format(h)), type = "SSB0")
ggsave("figures/assess/profile_h_SSB0.png", height = 4, width = 7)

#
#compare_CAL(profilemodels, model_names = paste("h =", format(h)), fleet = 1, "SJR F")
#compare_CAL(profilemodels, model_names = paste("h =", format(h)), fleet = 2, "SJR M")
#compare_CAL(profilemodels, model_names = paste("h =", format(h)), fleet = 5, "BOF")

compare_recruitment(profilemodels, model_names = paste("h =", format(h)))
ggsave("figures/assess/profile_h_recruitment_CSF.png", height = 4, width = 7)

ref_pt_SSF <- data.frame(`Ref.pt.` = c("F0.1", "F50%"), value = c(0.13, 0.06))
compare_F(profilemodels, model_names = paste("h =", format(h)), type2 = "summary", ylim = c(0, 0.15)) +
  geom_hline(data = ref_pt_SSF, aes(yintercept = value, linetype = `Ref.pt.`))
ggsave("figures/assess/profile_h_F_CSF.png", height = 4, width = 7)




# Profile R0
dir <- "03A_SSF_0.6BOF_profR0"
R0 <- seq(1, 3, 0.25)
do_profile(dir, 117, R0)


profilemodels <- r4ss::SSgetoutput(dirvec = file.path(getwd(), "SS", dir), keyvec = 1:length(R0))
profilesum <- r4ss::SSsummarize(profilemodels)

png("figures/assess/profile_R0.png", height = 4, width = 6, res = 400, units = "in")
par(mar = c(5, 4, 1, 1))
r4ss::SSplotProfile(profilesum, profile.string = "R0", profile.label = "log(R0)")
abline(v = 1.74, lty = 3)
dev.off()

# Profile R0 in the CSF
dir <- "01A_CSF_0.6BOF_profR0"
R0 <- seq(1.4, 1.7, 0.05)
do_profile(dir, 115, R0)

profilemodels <- r4ss::SSgetoutput(dirvec = file.path(getwd(), "SS", dir), keyvec = 1:length(R0))
profilesum <- r4ss::SSsummarize(profilemodels)

png("figures/assess/profile_R0_CSF.png", height = 4, width = 6, res = 400, units = "in")
par(mar = c(5, 4, 1, 1))
r4ss::SSplotProfile(profilesum, profile.string = "R0", profile.label = expression(log(R[0])))
#abline(v = 1.52, lty = 3)
dev.off()

## Profile CV length-at-age
#dir <- "03A_SSF_0.6BOF_profCVL"
#CVL <- seq(0.05, 0.15, 0.01)
#do_profile(dir, 73, CVL)
#
#profilemodels <- r4ss::SSgetoutput(dirvec = file.path(getwd(), "SS", dir), keyvec = 1:length(CVL))
#profilesum <- r4ss::SSsummarize(profilemodels)
#r4ss::SSplotProfile(profilesum, profile.string = "CV_old_Fem")
#
## Profile Female M, note Male M = 1.5 Female M
#dir <- "03A_SSF_0.6BOF_profM"
#M <- seq(0.03, 0.16, 0.01)
#do_profile(dir, 67, M)
#
#
#profilemodels <- r4ss::SSgetoutput(dirvec = file.path(getwd(), "SS", dir), keyvec = 1:length(M))
#profilesum <- r4ss::SSsummarize(profilemodels)
#r4ss::SSplotProfile(profilesum, profile.string = "NatM_p_1_Fem")
#
#