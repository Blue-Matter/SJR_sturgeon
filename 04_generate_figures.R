library(dplyr)
dir <- "01_base"
dir <- "02_agesel"

replist <- r4ss::SS_output(file.path(getwd(), "SS", dir))

plot_sel_mat <- function(type = c("length", "age")) {
  type <- match.arg(type)
  
  if(type == "length") {
    plot(Mat_len ~ Mean_Size, replist$biology, 
         xlab = "Length (cm)", ylab = "Selectivity/Maturity", typ = "o", pch = 16, lwd = 2)
    lensel <- replist$sizeselex %>% filter(Factor == "Lsel" & Fleet == 1 & Yr == 2020)
    lensel <- lensel[, -c(1:5)] %>% as.matrix()
    matlines(colnames(lensel) %>% as.numeric(), t(lensel), lty = c(2, 4), col = 1, lwd = 2)
    legend("topleft", c("Female Maturity", "Female Selectivity", "Male Selectivity"),
           lwd = 2, lty = c(1, 2, 4), pch = c(16, NA, NA))
  } else {
    plot(Len_Mat * Age_Mat ~ int_Age, replist$endgrowth %>% filter(Sex == 1), 
         xlab = "Age", ylab = "Selectivity/Maturity", typ = "o", pch = 16, lwd = 2)
    agesel <- replist$ageselex %>% filter(Factor == "Asel2" & Fleet == 1 & Yr == 2020)
    agesel <- agesel[, -c(1:7)] %>% as.matrix()
    matlines(colnames(agesel) %>% as.numeric(), t(agesel), lty = c(2, 4), col = 1, lwd = 2)
    legend("topleft", c("Female Maturity", "Female Selectivity", "Male Selectivity"),
           lwd = 2, lty = c(1, 2, 4), pch = c(16, NA, NA))
  }
  invisible()
}

plot_SSB <- function(forecast = TRUE) {
  ts <- replist$timeseries %>% filter(Era != "VIRG")
  if(!forecast) ts <- filter(ts, Era != "FORE")
  SSB <- ts$SpawnBio
  Yr <- ts$Yr
  SSBMSY <- replist$derived_quants$Value[replist$derived_quants$Label == "SSB_MSY"]
  SSB_SPR <- replist$derived_quants$Value[replist$derived_quants$Label == "SSB_SPR"]
  plot(Yr, SSB, typ = "o", ylim = c(0, 1.1 * max(SSB)), xlab = "Year")
  abline(h = c(SSBMSY, SSB_SPR), lty = 2:3)
  legend("topright", c(expression(SSB[MSY]), expression(SSB[50*"%"])), lty = 2:3)
  if(forecast) abline(v = 2020, lty = 3)
}

plot_SSB_SSBMSY <- function(forecast = TRUE) {
  ts <- replist$timeseries %>% filter(Era != "VIRG")
  if(!forecast) ts <- filter(ts, Era != "FORE")
  SSB <- ts$SpawnBio
  Yr <- ts$Yr
  SSBMSY <- replist$derived_quants$Value[replist$derived_quants$Label == "SSB_MSY"]
  plot(Yr, SSB/SSBMSY, typ = "o", ylim = c(0, 1.1 * max(SSB/SSBMSY)), 
       xlab = "Year", ylab = expression(SSB/SSB[MSY]))
  abline(h = 1, lty = 2)
  if(forecast) abline(v = 2020, lty = 3)
}

plot_SSB_SSB0 <- function(forecast = TRUE) {
  ts <- replist$timeseries %>% filter(Era != "VIRG")
  if(!forecast) ts <- filter(ts, Era != "FORE")
  SSB <- ts$SpawnBio
  Yr <- ts$Yr
  SSB0 <- replist$derived_quants$Value[replist$derived_quants$Label == "SSB_unfished"]
  plot(Yr, SSB/SSB0, typ = "o", ylim = c(0, 1.1 * max(SSB/SSB0)), 
       xlab = "Year", ylab = expression(SSB/SSB[0]))
  #abline(h = 1, lty = 2)
  if(forecast) abline(v = 2020, lty = 3)
}


plot_F_FMSY <- function(forecast = TRUE) {
  ts <- replist$timeseries
  if(!forecast) ts <- filter(ts, Era != "FORE")
  FF <- ts[, grepl("F:_", colnames(ts))] %>% rowSums()
  Yr <- ts$Yr
  FMSY <- replist$derived_quants$Value[replist$derived_quants$Label == "annF_MSY"]
  plot(Yr, FF/FMSY, typ = "o", ylim = c(0, 3), xlab = "Year", ylab = expression(F/F[MSY]))
  abline(h = 1, lty = 2)
  if(forecast) abline(v = 2020, lty = 3)
}

plot_F <- function(forecast = TRUE) {
  ts <- replist$timeseries
  if(!forecast) ts <- filter(ts, Era != "FORE")
  FF <- ts[, grepl("F:_", colnames(ts))] %>% rowSums()
  Yr <- ts$Yr
  FMSY <- replist$derived_quants$Value[replist$derived_quants$Label == "annF_MSY"]
  FSPR <- replist$derived_quants$Value[replist$derived_quants$Label == "annF_SPR"]
  plot(Yr, FF, typ = "o", ylim = c(0, 0.5), xlab = "Year", ylab = "F")
  abline(h = c(FMSY, FSPR), lty = 2:3)
  legend("topright", c(expression(F[MSY]), expression(F[50*"%"])), lty = 2:3)
  if(forecast) abline(v = 2020, lty = 3)
}



plot_sel_mat()
plot_sel_mat("age")

plot_SSB()
plot_SSB_SSBMSY()
plot_SSB_SSB0()

plot_F()
plot_F_FMSY()
