
# Calculate depletion in the past 3 MGT
MGT <- round(1/0.06 + 26) # MGT = 43

do_depletion_estimates <- function(SSN, MGT = 43) {
  N_spawn <- dplyr::filter(SSN, Year == 2020)
  
  GT3 <- 3 * MGT
  minY <- max(min(SSN$Year), 2020 - GT3)
  print(paste0("minY = ", minY))
  N_denom <- dplyr::filter(SSN, Year == minY)
  
  list(SSN = rbind(N_spawn, N_denom), 
       dep = data.frame(DF = round(N_spawn$SSN_F/N_denom$SSN_F, 2), 
                        DM = round(N_spawn$SSN_M/N_denom$SSN_M, 2),
                        DN = round(N_spawn$SSN/N_denom$SSN, 2),
                        Model = N_denom$Model))
}

# max F, SSF, %BOF
models <- c("01A_CSF_0.6BOF", "01A_CSF_0.6BOF_maxF1", "01A_CSF_0.6BOF_maxF6", "03A_SSF_0.6BOF",
            "01A_CSF_0.3BOF", "01A_CSF_0.9BOF")
m_names <- c("Reference model", "Max. F = 1", "Max. F = 6", "SSF", "30% BOF", "90% BOF")
report <- lapply(models, function(x) r4ss::SS_output(file.path(getwd(), "SS", x)))
SSN <- compare_SSN(report, m_names)

# Profile h
h <- seq(0.45, 0.85, 0.05)
profilemodels <- r4ss::SSgetoutput(dirvec = file.path(getwd(), "SS", "01A_CSF_0.6BOF_profh"), keyvec = 1:length(h))
h_names <- paste("Profile h =", h)

SSN_h <- compare_SSN(profilemodels[h != 0.6], h_names[h != 0.6])

COSEWIC_A <- rbind(SSN, SSN_h) %>% do_depletion_estimates()

write.csv(COSEWIC_A[[1]], file = "tables/SSN_1891_2020.csv")
write.csv(COSEWIC_A[[2]], file = "tables/depletion_1891.csv")
