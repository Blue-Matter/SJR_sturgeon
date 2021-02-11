source("00_functions_report_figures.R")

dir <- "03A_SSF_0.6BOF"
replist <- r4ss::SS_output(file.path(getwd(), "SS", dir))

# Ref
# FMSY = 0.0830294
# F01 = 0.10
# F20 = 0.273063 
# F30 = 0.147444
# F40 = 0.0913185
# F50 = 0.0600689 
# F60 = 0.0401829

##### Biology
png("figures/biology/maturity_length.png", height = 4, width = 6, res = 400, units = "in")
par(mar = c(5, 4, 1, 1))
plot_mat(replist)
dev.off()

png("figures/biology/maturity_age.png", height = 4, width = 6, res = 400, units = "in")
par(mar = c(5, 4, 1, 1))
plot_mat(replist, "age")
dev.off()

png("figures/biology/growth.png", height = 4, width = 6, res = 400, units = "in")
par(mar = c(5, 4, 1, 1))
r4ss::SSplotBiology(replist, subplots = 1, 
                    labels = c("Length (cm)", "Age (yr)", "Maturity", "Mean weight (kg) in last year",
                               "Spawning output", "Length (cm)", "Natural mortality",
                               "Female weight (kg)", "Female length (cm)", "Fecundity", "Default fecundity label",
                               "Year", "Hermaphroditism transition rate", "Fraction females by age at equilibrium"))
dev.off()

##### Index
png("figures/data/index.png", height = 4, width = 6, res = 400, units = "in")
par(mar = c(5, 4, 1, 1))
r4ss::SSplotIndices(replist, subplots = 1)
dev.off()

##### Catch
png("figures/data/catch_hist.png", height = 4, width = 6, res = 400, units = "in")
par(mar = c(5, 4, 1, 1))
plot_catch(replist, "wt")
dev.off()

png("figures/data/catch_modern.png", height = 4, width = 6, res = 400, units = "in")
par(mar = c(5, 4, 1, 1))
plot_catch(replist, "abun")
dev.off()

# Length data
png("figures/data/CAL_SJR_F.png", height = 4, width = 6, res = 400, units = "in")
r4ss::SSplotComps(replist, datonly = TRUE, showeffN = FALSE, subplots = 1, fleets = 1)
dev.off()

png("figures/data/CAL_SJR_M.png", height = 4, width = 6, res = 400, units = "in")
r4ss::SSplotComps(replist, datonly = TRUE, showeffN = FALSE, subplots = 1, fleets = 2)
dev.off()

png("figures/data/CAL_BOF.png", height = 4, width = 6, res = 400, units = "in")
r4ss::SSplotComps(replist, datonly = TRUE, showeffN = FALSE, subplots = 1, fleets = 5)
dev.off()

# Age
png("figures/data/CAA_BOF.png", height = 4, width = 6, res = 400, units = "in")
r4ss::SSplotComps(replist, datonly = TRUE, kind = "AGE", showeffN = FALSE, subplots = 1, fleets = 5)
dev.off()


##### Selectivity vs. maturity
png("figures/assess/sel_mat_length.png", height = 4, width = 6, res = 400, units = "in")
par(mar = c(5, 4, 1, 1))
plot_sel_mat(replist)
dev.off()

png("figures/assess/sel_mat_age.png", height = 4, width = 6, res = 400, units = "in")
par(mar = c(5, 4, 1, 1))
plot_sel_mat(replist, "age")
dev.off()

##### Plot individual model fits
# Fits in SSF 0.6BOF
png("figures/assess/long_model_index_fit.png", height = 4, width = 6, res = 400, units = "in")
par(mar = c(5, 4, 1, 1))
r4ss::SSplotIndices(replist, subplots = 2)
dev.off()

png("figures/data/long_model_CAL_SJR_F.png", height = 4, width = 6, res = 400, units = "in")
r4ss::SSplotComps(replist, showeffN = FALSE, subplots = 1, fleets = 1)
dev.off()

png("figures/data/long_model_CAL_SJR_M.png", height = 4, width = 6, res = 400, units = "in")
r4ss::SSplotComps(replist, showeffN = FALSE, subplots = 1, fleets = 2)
dev.off()

png("figures/data/long_model_CAL_BOF.png", height = 4, width = 6, res = 400, units = "in")
r4ss::SSplotComps(replist, showeffN = FALSE, subplots = 1, fleets = 5)
dev.off()

# Age
png("figures/data/long_model_CAA_BOF.png", height = 4, width = 6, res = 400, units = "in")
r4ss::SSplotComps(replist, kind = "AGE", showeffN = FALSE, subplots = 1, fleets = 5)
dev.off()

# Summary F
png("figures/assess/long_model_summary_F.png", height = 4, width = 6, res = 400, units = "in")
plot_summary_F(replist, ylim = c(0, 0.12))
dev.off()


compare_CAL(list(replist), "Fit")
ggsave("figures/assess/long_model_CAL_SJR_F2.png", height = 6, width = 7)


compare_CAL(list(replist), "Fit", fleet = 2, fleetname = "SJR M")
ggsave("figures/assess/long_model_CAL_SJR_M2.png", height = 6, width = 7)


compare_CAL(list(replist), "Fit", fleet = 5, fleetname = "BOF")
ggsave("figures/assess/long_model_CAL_BOF.png", height = 3, width = 4)

plot_F_SSF(replist, ylim = c(0, 0.2))
ggsave("figures/assess/long_model_fleet_F.png", height = 4, width = 7)

##### Compare with a suite of models

# Compare 0 - 90 % SJR origin of BOF catch
report <- lapply(c("03A_SSF_zeroBOF", "03A_SSF_0.3BOF", "03A_SSF_0.6BOF", "03A_SSF_0.9BOF"), 
                 function(x) r4ss::SS_output(file.path(getwd(), "SS", x)))

compare_SSB(report, model_names = c("0%", "30%", "60%", "90%") %>% paste("BOF"))
ggsave("figures/assess/compare_SSB_BOF.png", height = 4, width = 7)

compare_SSB(report, model_names = c("0%", "30%", "60%", "90%") %>% paste("BOF"), type = "SSBMSY")
ggsave("figures/assess/compare_SSBMSY_BOF.png", height = 4, width = 7)

compare_SSB(report, model_names = c("0%", "30%", "60%", "90%") %>% paste("BOF"), type = "SSB0")
ggsave("figures/assess/compare_SSB0_BOF.png", height = 4, width = 7)

ref_pt_SSF <- data.frame(`Ref.pt.` = c("F0.1", "F50%"), value = c(0.1, 0.06))

compare_F(report, model_names = c("0%", "30%", "60%", "90%") %>% paste("BOF"), type2 = "summary", 
          ylim = c(0, 0.12), forecast = FALSE) + 
  geom_hline(data = ref_pt_SSF, aes(yintercept = value, linetype = `Ref.pt.`))
ggsave("figures/assess/compare_F_BOF.png", height = 4, width = 7)




# Compare CSF (combined sex fishery) vs SSF (separate sex fishery)
report <- lapply(c("03A_SSF_0.6BOF", "01A_CSF_0.6BOF"), 
                 function(x) r4ss::SS_output(file.path(getwd(), "SS", x)))

compare_SSB(report, model_names = c("SSF (30% BOF)", "CSF (30% BOF)"))
ggsave("figures/assess/compare_SSB_fleet_structure.png", height = 4, width = 7)

compare_SSB(report, model_names = c("SSF (30% BOF)", "CSF (30% BOF)"), type = "SSBMSY")
ggsave("figures/assess/compare_SSBMSY_fleet_structure.png", height = 4, width = 7)

compare_SSB(report, model_names = c("SSF (30% BOF)", "CSF (30% BOF)"), type = "SSB0")
ggsave("figures/assess/compare_SSB0_fleet_structure.png", height = 4, width = 7)


sex_ratio <- SSC_from_CSF("01A_CSF_0.6BOF") 
png("figures/assess/sex_ratio_CSF.png", height = 4, width = 6, res = 400, units = "in")
par(mar = c(5, 4, 1, 1))
plot(ratio_F ~ Yr, sex_ratio$Hist, typ = "o", pch = 16, xlab = "Year", ylab = "Percent Female (by weight)")
dev.off()


ref_pt_SSF <- data.frame(`Ref.pt.` = c("F0.1", "F50%"), value = c(0.1, 0.06))

compare_F(report, model_names = c("SSF (30% BOF)", "CSF (30% BOF)"), type2 = "summary", 
          ylim = c(0, 0.25), forecast = FALSE) +
  geom_hline(data = ref_pt_SSF, aes(yintercept = value, linetype = `Ref.pt.`))
ggsave("figures/assess/compare_F_fleet_structure.png", height = 4, width = 7)


# Long model with tags

report <- lapply(c("03A_SSF_0.6BOF", "03A_SSF_0.6BOF_tags2", "03A_SSF_0.6BOF_tags4", "03A_SSF_0.6BOF_tags4_upweight"), 
                 function(x) r4ss::SS_output(file.path(getwd(), "SS", x)))

compare_SSB(report, model_names = c("No tags", "Tags (delay 2)", "Tags (delay 4)", "Tags (delay 4, upweighted)"))
ggsave("figures/assess/compare_SSB_tags.png", height = 4, width = 7)

compare_SSB(report, model_names = c("0%", "30%", "60%", "90%") %>% paste("BOF"), type = "SSBMSY")
ggsave("figures/assess/compare_SSBMSY_BOF.png", height = 4, width = 7)

compare_SSB(report, model_names = c("0%", "30%", "60%", "90%") %>% paste("BOF"), type = "SSB0")
ggsave("figures/assess/compare_SSB0_BOF.png", height = 4, width = 7)

ref_pt_SSF <- data.frame(`Ref.pt.` = c("F0.1", "F50%"), value = c(0.1, 0.06))

compare_F(report, model_names = c("0%", "30%", "60%", "90%") %>% paste("BOF"), type2 = "summary", 
          ylim = c(0, 0.12), forecast = FALSE) + 
  geom_hline(data = ref_pt_SSF, aes(yintercept = value, linetype = `Ref.pt.`))
ggsave("figures/assess/compare_F_BOF.png", height = 4, width = 7)


# Compare max F
report <- lapply(c("01A_CSF_0.6BOF", "01A_CSF_0.6BOF_maxF1", "01A_CSF_0.6BOF_maxF6"), 
                 function(x) r4ss::SS_output(file.path(getwd(), "SS", x)))

compare_SSB(report, model_names = c("3", "1", "6")) + scale_colour_discrete(name = "Max. F")
ggsave("figures/assess/compare_SSB_maxF.png", height = 4, width = 7)

compare_SSB(report, model_names = c("3", "1", "6"), type = "SSBMSY") + scale_colour_discrete(name = "Max. F")
ggsave("figures/assess/compare_SSBMSY_maxF.png", height = 4, width = 7)

compare_SSB(report, model_names = c("3", "1", "6"), type = "SSB0") + scale_colour_discrete(name = "Max. F")
ggsave("figures/assess/compare_SSB0_maxF.png", height = 4, width = 7)

ref_pt_SSF <- data.frame(`Ref.pt.` = c("F0.1", "F50%"), value = c(0.1, 0.06))

compare_F(report, model_names = c("maxF = 3", "maxF = 1", "maxF = 6"), type2 = "summary", 
          ylim = c(0, 0.5), forecast = FALSE) + 
  geom_hline(data = ref_pt_SSF, aes(yintercept = value, linetype = `Ref.pt.`))
ggsave("figures/assess/compare_F_BOF.png", height = 4, width = 7)

plot_F_CSF(report[[3]])

lapply(report, SSC_from_CSF) %>% lapply(getElement, "proj")
