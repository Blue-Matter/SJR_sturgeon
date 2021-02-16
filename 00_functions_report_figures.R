plot_mat <- function(type = c("length", "age")) {
  
  #replist_F <- r4ss::SS_output(file.path(getwd(), "SS", "01A_CSF_0.6BOF"))
  #replist_M <- r4ss::SS_output(file.path(getwd(), "SS", "01A_CSF_0.6BOF_male"))
  
  type <- match.arg(type)
  
  if(type == "length") {
    #mat_len <- data.frame(Length = replist_F$biology$Mean_Size, 
    #                      Mat_F = replist_F$biology$Mat_len,
    #                      Mat_M = replist_M$biology$Mat_len)
    #write.csv(mat_len, file = "processed_data/mat_len.csv")
    
    mat_len <- read.csv("processed_data/mat_len.csv")
    matplot(mat_len[, 2], mat_len[, 3:4], xlab = "Length (cm)", ylab = "Maturity", 
            col = "black", typ = "o", pch = 16, lwd = 2, lty = c(1, 3))
    legend("topleft", c("Female", "Male"), lwd = 2, lty = c(1, 3), pch = 16)
  } else {
    #mat_age <- data.frame(Age = dplyr::filter(replist_F$endgrowth, Sex == 1)$int_Age, 
    #                      Mat_F = dplyr::filter(replist_F$endgrowth, Sex == 1)$Len_Mat, 
    #                      Mat_M = dplyr::filter(replist_M$endgrowth, Sex == 2)$Len_Mat)
    #write.csv(mat_age, file = "processed_data/mat_age.csv")
    
    mat_age <- read.csv("processed_data/mat_age.csv")
    matplot(mat_age[, 2], mat_age[, 3:4], xlab = "Age", ylab = "Maturity", 
            typ = "o", col = "black", pch = 16, lty = c(1, 3), lwd = 2)    
    legend("topleft", c("Female", "Male"), lwd = 2, lty = c(1, 3), pch = 16)
  }
  
  invisible()
}

plot_sel_mat <- function(dir, type = c("length", "age"), fleet = c(1, 1, 3), sex = c(1, 2, 1)) { # Fleet # of SJR F, SJR M, BOF
  
  if(is.character(dir)) {
    replist <- r4ss::SS_output(file.path(getwd(), "SS", dir))
  } else if(is.list(dir)) {
    replist <- dir
  } else {
    stop()
  }
  
  type <- match.arg(type)
  
  if(type == "length") {
    plot(Mat_len ~ Mean_Size, replist$biology, 
         xlab = "Length (cm)", ylab = "Selectivity/Maturity", typ = "o", pch = 16, lwd = 2)
    
    lensel <- Map(function(x, y) {
      dplyr::filter(replist$sizeselex, Factor == "Lsel", Fleet == x, Sex == y, Yr == 2020)[, -c(1:5)]
    }, x = fleet, y = sex) %>% do.call(rbind, .)
    
    matlines(colnames(lensel) %>% as.numeric(), t(lensel), lty = c(2, 3, 4), col = 1, lwd = 2)
    legend("topleft", c("Female Maturity", "SJR Selectivity (F)", "SJR Selectivity (M)", "BOF Selectivity (U)"),
           lwd = 2, lty = c(1, 2, 3, 4), pch = c(16, NA, NA, NA))
    
    invisible(lensel)
  } else {
    plot(Len_Mat * Age_Mat ~ int_Age, replist$endgrowth %>% dplyr::filter(Sex == 1), 
         xlab = "Age", ylab = "Selectivity/Maturity", typ = "o", pch = 16, lwd = 2)
    
    agesel <- Map(function(x, y) {
      dplyr::filter(replist$ageselex, Factor == "Asel2", Fleet == x, Sex == y, Yr == 2020)[, -c(1:7)]
    }, x = fleet, y = sex) %>% do.call(rbind, .)
    
    matlines(colnames(agesel) %>% as.numeric(), t(agesel), lty = c(2, 3, 4), col = 1, lwd = 2)
    legend("right", c("Female Maturity", "SJR Selectivity (F)", "SJR Selectivity (M)", "BOF Selectivity (U)"),
           lwd = 2, lty = c(1, 2, 3, 4), pch = c(16, NA, NA, NA))
    invisible(agesel)
  }
}

#plot_mat <- function(dir, type = c("length", "age")) { # Fleet # of SJR F, SJR M, BOF
#  
#  if(is.character(dir)) {
#    replist <- r4ss::SS_output(file.path(getwd(), "SS", dir))
#  } else if(is.list(dir)) {
#    replist <- dir
#  } else {
#    stop()
#  }
#  
#  type <- match.arg(type)
#  
#  if(type == "length") {
#    plot(Mat_len ~ Mean_Size, replist$biology, 
#         xlab = "Length (cm)", ylab = "Maturity", typ = "o", pch = 16, lwd = 2)
#  } else {
#    plot(Len_Mat * Age_Mat ~ int_Age, replist$endgrowth %>% dplyr::filter(Sex == 1), 
#         xlab = "Age", ylab = "Maturity", typ = "o", pch = 16, lwd = 2)
#  }
#  invisible()
#}

plot_catch <- function(replist = r4ss::SS_output(file.path(getwd(), "SS", "03A_SSF_0.6BOF")), type = c("wt", "abun")) {
  type <- match.arg(type)
  cat <- dplyr::filter(replist$timeseries, Era == "TIME")
  
  if(type == "wt") {
    # Historical catch SJR with Bay of Fundy
    plot(NULL, NULL, typ = "n", xlim = range(cat$Yr), ylim = c(0, 50), xlab = "Year", ylab = "Catch (t)")
    abline(h = 0, col = "grey")
    lines(cat$Yr, cat$`obs_cat:_3` + cat$`obs_cat:_4`, lwd = 3)
    lines(cat$Yr, cat$`obs_cat:_5`, col = "red", lwd = 3)
    legend("topleft", c("SJR (pre-2007)", "Bay of Fundy"), col = c("black", "red"), lwd = 3)
  } else {
    # Modern fishery
    cat <- dplyr::filter(cat, Yr >= 2007)
    plot(NULL, NULL, typ = "n", xlim = c(2007, 2020), ylim = c(0, 175), xlab = "Year", ylab = "Catch (numbers)")
    abline(h = 0, col = "grey")
    lines(cat$Yr, 1e3 * cat$`obs_cat:_1`, pch = 16, typ = "o")
    lines(cat$Yr, 1e3 * cat$`obs_cat:_2`, pch = 1, typ = "o", lty = 3)
    legend("bottomright", c("SJR (F)", "SJR (M)"), pch = c(16, 1), lty = c(1, 3))
  }
  invisible()
}


plot_CAL <- function(replist, fleet = 1, fleetname = "SJR F", plotfit = TRUE) {
  
  if(plotfit) {
    CALpred <- dplyr::filter(replist$lendbase, Fleet == fleet) %>% dplyr::select(Yr, Bin, Exp) %>% 
      reshape2::melt(id.vars = c("Yr", "Bin"))
  }
  CALobs <- dplyr::filter(replist$lendbase, Fleet == fleet) %>% dplyr::select(Yr, Bin, Obs) %>% 
    reshape2::melt(id.vars = c("Yr", "Bin"))
  
  if(plotfit) {
    ggplot(CALpred, aes(Bin, value)) + facet_wrap(~Yr) +
      geom_line(data = CALobs) + geom_point(data = CALobs) +
      geom_line(aes(colour = Model)) + geom_point(aes(colour = Model)) + theme_bw() +
      labs(x = "Length (cm)", y = "Proportion") + ggtitle(fleetname)
  } else {
    ggplot(CALobs, aes(Bin, value)) + facet_wrap(~Yr) +
      geom_line() + geom_point() +
      labs(x = "Length (cm)", y = "Proportion") + ggtitle(fleetname)
  }
}

report_table <- function(replist, fleet) {
  ts_B <- replist$timeseries %>% dplyr::filter(Era == "INIT" | Era == "TIME")
  ts_F <- replist$derived_quants[replist$derived_quants$Label %in% paste0("F_", replist$startyr:replist$endyr), ]

  get_SSN <- function() {
    age_mat <- read.csv("processed_data/mat_age.csv")
    NF <- replist$natage %>% dplyr::filter((`Beg/Mid` == "M" & Era == "TIME") | (`Beg/Mid` == "B" & Era == "INIT"), Sex == 1)
    NM <- replist$natage %>% dplyr::filter((`Beg/Mid` == "M" & Era == "TIME") | (`Beg/Mid` == "B" & Era == "INIT"), Sex == 2)
    list(SSN_F = colSums(t(NF[, -c(1:12)] %>% as.matrix()) * age_mat$Mat_F), 
         SSN_M = colSums(t(NM[, -c(1:12)] %>% as.matrix()) * age_mat$Mat_M))
  }
  SSN <- get_SSN()
  Fout <- numeric(length(replist$startyr:replist$endyr))
  Fout[paste0("F_", replist$startyr:replist$endyr) %in% ts_F$Label] <- ts_F$Value
  out <- data.frame(Year = ts_B$Yr, SSB = ts_B$SpawnBio %>% round(1), FM = c(0, Fout) %>% round(2), 
                    SSN_F = SSN[[1]] * 1e3, SSN_M = SSN[[2]] * 1e3) %>% 
    dplyr::mutate(SSN = round(SSN_F + SSN_M, 0), SSN_F = round(SSN_F), SSN_M = round(SSN_M))
  if(!missing(fleet)) {
    out_fleetF <- lapply(fleet, function(x) parse(text = paste0("ts_B$`F:_", x, "`")) %>% eval() %>% round(2)) %>% bind_cols()
    out <- cbind(out, out_fleetF)
  }
  return(out)
}

compare_SSN <- function(report, model_names) {
  Map(function(x, y) report_table(x) %>% dplyr::mutate(Model = y), x = report, y = model_names) %>%
    dplyr::bind_rows()
}


compare_SSB <- function(dir, model_names, type = c("SSB", "SSBMSY", "SSB0"), forecast = TRUE,
                        retro_yr = NA) {
  type <- match.arg(type)
  #if(missing(model_names)) model_names <- dir
  if(is.list(dir)) {
    report <- dir
  } else {
    report <- lapply(dir, function(x) r4ss::SS_output(file.path(getwd(), "SS", x)))
  }
  
  SSB <- Map(function(replist, model_name, retro_yr) {
    ts <- replist$timeseries %>% dplyr::filter(Era != "VIRG")
    if(!forecast) ts <- dplyr::filter(ts, Era != "FORE")
    if(!is.na(retro_yr)) ts <- dplyr::filter(ts, Yr <= retro_yr)
    
    data.frame(Year = ts$Yr, SSB = ts$SpawnBio) %>% 
      dplyr::mutate(SSBMSY = replist$derived_quants$Value[replist$derived_quants$Label == "SSB_MSY"],
                    SSB_SPR = replist$derived_quants$Value[replist$derived_quants$Label == "SSB_SPR"],
                    SSB0 = replist$derived_quants$Value[replist$derived_quants$Label == "SSB_unfished"],
                    Model = model_name)
  }, replist = report, model_name = model_names, retro_yr = retro_yr) %>% do.call(rbind, .)
  
  if(type == "SSB") {
    g <- ggplot(SSB, aes(Year, SSB, colour = Model)) 
  }
  if(type == "SSBMSY") {
    g <- ggplot(SSB, aes(Year, SSB/SSBMSY, colour = Model)) + geom_hline(yintercept = 1, linetype = 3) +
      labs(y = expression(SSB/SSB[MSY]))
  }
  if(type == "SSB0") {
    g <- ggplot(SSB, aes(Year, SSB/SSB0, colour = Model)) + labs(y = expression(SSB/SSB[0]))
  }
  if(forecast) g <- g + geom_vline(xintercept = 2020, linetype = 3)
  g + geom_line(size = 1) + theme_bw() + geom_hline(yintercept = 0, colour = "grey")
}

compare_F <- function(dir, model_names, type = c("F", "FMSY", "F0.1", "FSPR"), type2 = c("summary", "SJR F"),
                      ylim = c(0, 2), forecast = TRUE, fleet = list(c(1, 3), c(1, 2))) {
  type <- match.arg(type)
  type2 <- match.arg(type2)
  #if(missing(model_names)) model_names <- dir
  if(is.list(dir)) {
    report <- dir
  } else {
    report <- lapply(dir, function(x) r4ss::SS_output(file.path(getwd(), "SS", x)))
    if(missing(model_names)) model_names <- dir
  }
  
  FF <- Map(function(replist, model_name, fleet) {
    if(type2 == "SJR F") {
      ts <- replist$timeseries %>% dplyr::filter(Era != "VIRG")
      if(!forecast) ts <- dplyr::filter(ts, Era != "FORE")
      
      FM <- lapply(fleet, function(xx) getElement(ts, paste0("F:_", xx))) %>% do.call(rbind, .) %>% colSums()
      
      data.frame(Year = ts$Yr, FM = FM, Model = model_name)
    } else {
      ts <- replist$derived_quants[replist$derived_quants$Label %in% paste0("F_", replist$startyr:replist$endyr), ]
      if(forecast) {
        tsf <- dplyr::filter(replist$timeseries, Era == "FORE")
      }
      Fout <- numeric(length(replist$startyr:replist$endyr))
      Fout[paste0("F_", replist$startyr:replist$endyr) %in% ts$Label] <- ts$Value
      data.frame(Year = replist$startyr:replist$endyr, FM = Fout) %>% 
        rbind(data.frame(Year = tsf$Yr, FM = tsf$`F:_1`)) %>%
        dplyr::mutate(Model = model_name)
    }
  }, replist = report, model_name = model_names, fleet = fleet) %>% do.call(rbind, .)
  
  if(type == "F") {
    labs <- "Fishing mortality"
    if(type2 == "summary") labs <- "Summary F"
    FF <- dplyr::mutate(FF, Fout = FM)
  } else if(type == "FMSY") {
    labs <- expression(F/F[MSY])
    FF <- dplyr::mutate(FF, Fout = FM/FMSY)
  } else if(type == "F0.1") {
    labs <- expression(F/F[0.1])
    FF <- dplyr::mutate(FF, Fout = FM/F01)
  } else {
    labs <- parse(text = "expression(F/F", 100 * replist$sprtarg, "*\"%\")") %>% eval()
    FF <- dplyr::mutate(FF, Fout = FM/F_SPR)
  }
  
  g <- ggplot(FF, aes(Year, Fout, colour = Model))
  if(type == "FMSY" && type2 == "summary") { 
    g <- g + geom_hline(yintercept = 1, linetype = 3)
  } else {
    if(forecast) g <- g + geom_vline(xintercept = 2020, linetype = 3)
  }
  g + geom_line(size = 1) + theme_bw() + geom_hline(yintercept = 0, colour = "grey") + 
    coord_cartesian(ylim = ylim) + labs(y = labs)
}

compare_CAL <- function(report, model_names, fleet = 1, gender = 1, fleetname = "SJR F", obs_only = FALSE) {
  CALobs <- dplyr::filter(report[[1]]$lendbase, Fleet == fleet, sex == gender) %>% dplyr::select(Yr, Bin, Obs) %>% 
    reshape2::melt(id.vars = c("Yr", "Bin"))
  
  if(!obs_only) {
    CALpred <- Map(function(x, model_name) {
      res <- dplyr::filter(report[[x]]$lendbase, Fleet == fleet, sex == gender) %>% dplyr::select(Yr, Bin, Exp) %>% 
        reshape2::melt(id.vars = c("Yr", "Bin")) %>%
        dplyr::mutate(Model = model_name)
    }, x = 1:length(report), model_name = model_names) %>% dplyr::bind_rows()
    
    ggplot(CALpred, aes(Bin, value)) + facet_wrap(~Yr, scales = "free_y") +
      geom_line(data = CALobs) + geom_point(data = CALobs) +
      geom_line(aes(colour = Model)) + geom_point(aes(colour = Model)) + theme_bw() +
      labs(x = "Length (cm)", y = "Proportion") + ggtitle(fleetname)
  } else {
    ggplot(CALobs, aes(Bin, value)) + facet_wrap(~Yr, scales = "free_y") +
      geom_line() + geom_point() +
      theme_bw() +
      labs(x = "Length (cm)", y = "Proportion") + ggtitle(fleetname)
  }
}

compare_CAA <- function(report, model_names, fleet = 3, gender = 0, fleetname = "BOF", obs_only = FALSE) {
  CAAobs <- dplyr::filter(report[[1]]$agedbase, Fleet == fleet, sex == gender) %>% dplyr::select(Yr, Bin, Obs) %>% 
    reshape2::melt(id.vars = c("Yr", "Bin"))
  
  if(!obs_only) {
    CAApred <- Map(function(x, model_name) {
      res <- dplyr::filter(report[[x]]$agedbase, Fleet == fleet, sex == gender) %>% dplyr::select(Yr, Bin, Exp) %>% 
        reshape2::melt(id.vars = c("Yr", "Bin")) %>%
        dplyr::mutate(Model = model_name)
    }, x = 1:length(report), model_name = model_names) %>% dplyr::bind_rows()
    
    ggplot(CAApred, aes(Bin, value)) + facet_wrap(~Yr, scales = "free_y") +
      geom_line(data = CAAobs) + geom_point(data = CAAobs) +
      geom_line(aes(colour = Model)) + geom_point(aes(colour = Model)) + theme_bw() +
      labs(x = "Length (cm)", y = "Proportion") + ggtitle(fleetname)
  } else {
    ggplot(CAAobs, aes(Bin, value)) + facet_wrap(~Yr, scales = "free_y") +
      geom_line() + geom_point() +
      theme_bw() +
      labs(x = "Length (cm)", y = "Proportion") + ggtitle(fleetname)
  }
}


compare_recruitment <- function(replist, model_names) {
  Rec <- Map(function(replist, model_name) {
    ts <- replist$timeseries %>% dplyr::filter(Era == "TIME")
    data.frame(Year = ts$Yr, R = ts$Recruit_0) %>% 
      dplyr::mutate(Model = model_name)
  }, replist = replist, model_name = model_names) %>% do.call(rbind, .)
  
  ggplot(Rec, aes(Year, R, colour = Model)) + geom_line(size = 1) +
    theme_bw() + geom_hline(yintercept = 0, colour = "grey") + labs(y = "Recruitment")
}


plot_SSB <- function(replist, forecast = TRUE) {
  ts <- replist$timeseries %>% dplyr::filter(Era != "VIRG")
  if(!forecast) ts <- dplyr::filter(ts, Era != "FORE")
  SSB <- ts$SpawnBio
  Yr <- ts$Yr
  SSBMSY <- replist$derived_quants$Value[replist$derived_quants$Label == "SSB_MSY"]
  SSB_SPR <- replist$derived_quants$Value[replist$derived_quants$Label == "SSB_SPR"]
  plot(Yr, SSB, typ = "o", ylim = c(0, 1.1 * max(SSB)), xlab = "Year", ylab = "SSB (t)")
  abline(h = c(SSBMSY, SSB_SPR), lty = 2:3)
  legend("topright", c(expression(SSB[MSY]), expression(SSB[50*"%"])), lty = 2:3)
  if(forecast) abline(v = 2020, lty = 3)
}

plot_SSB_SSBMSY <- function(replist, forecast = TRUE) {
  ts <- replist$timeseries %>% dplyr::filter(Era != "VIRG")
  if(!forecast) ts <- dplyr::filter(ts, Era != "FORE")
  SSB <- ts$SpawnBio
  Yr <- ts$Yr
  SSBMSY <- replist$derived_quants$Value[replist$derived_quants$Label == "SSB_MSY"]
  plot(Yr, SSB/SSBMSY, typ = "o", ylim = c(0, 1.1 * max(SSB/SSBMSY)), 
       xlab = "Year", ylab = expression(SSB/SSB[MSY]))
  abline(h = 1, lty = 2)
  if(forecast) abline(v = 2020, lty = 3)
}

plot_SSB_SSB0 <- function(replist, forecast = TRUE) {
  ts <- replist$timeseries %>% dplyr::filter(Era != "VIRG")
  if(!forecast) ts <- dplyr::filter(ts, Era != "FORE")
  SSB <- ts$SpawnBio
  Yr <- ts$Yr
  SSB0 <- replist$derived_quants$Value[replist$derived_quants$Label == "SSB_unfished"]
  plot(Yr, SSB/SSB0, typ = "o", ylim = c(0, 1.1 * max(SSB/SSB0)), 
       xlab = "Year", ylab = expression(SSB/SSB[0]))
  #abline(h = 1, lty = 2)
  if(forecast) abline(v = 2020, lty = 3)
}


plot_F_FMSY_CSF <- function(replist, forecast = TRUE) {
  ts <- replist$timeseries
  if(!forecast) ts <- dplyr::filter(ts, Era != "FORE")
  FF <- ts[, grepl("F:_", colnames(ts))] %>% rowSums()
  Yr <- ts$Yr
  FMSY <- replist$derived_quants$Value[replist$derived_quants$Label == "annF_MSY"]
  plot(Yr, FF/FMSY, typ = "o", ylim = c(0, 3), xlab = "Year", ylab = expression(F/F[MSY]))
  abline(h = 1, lty = 2)
  if(forecast) abline(v = 2020, lty = 3)
}

plot_F_CSF <- function(replist, forecast = TRUE) {
  ts <- replist$timeseries
  if(!forecast) ts <- dplyr::filter(ts, Era != "FORE")
  FF <- ts[, grepl("F:_", colnames(ts))] %>% rowSums()
  Yr <- ts$Yr
  FMSY <- replist$derived_quants$Value[replist$derived_quants$Label == "annF_MSY"]
  FSPR <- replist$derived_quants$Value[replist$derived_quants$Label == "annF_SPR"]
  plot(Yr, FF, typ = "o", ylim = c(0, 0.5), xlab = "Year", ylab = "F")
  abline(h = c(FMSY, FSPR), lty = 2:3)
  legend("topright", c(expression(F[MSY]), expression(F[50*"%"])), lty = 2:3)
  if(forecast) abline(v = 2020, lty = 3)
}

plot_summary_F <- function(replist, ylim = c(0, 3), F01 = 0.13, SPR = 0.5, F_SPR = 0.06, forecast = TRUE) {
  ts <- replist$derived_quants[replist$derived_quants$Label %in% paste0("F_", replist$startyr:replist$endyr), ]
  
  Fout <- numeric(length(replist$startyr:replist$endyr))
  Fout[paste0("F_", replist$startyr:replist$endyr) %in% ts$Label] <- ts$Value
  out <- data.frame(Year = replist$startyr:replist$endyr, FM = Fout)
  
  if(forecast) {
    out <- rbind(out, 
                 replist$timeseries %>% dplyr::filter(Era == "FORE") %>% dplyr::select(Year = Yr, FM = `F:_1`))
  }
  
  plot(FM ~ Year, out, typ = "n", xlab = "Year", ylab = "Summary F", ylim = ylim)
  abline(h = 0, col = "grey")
  lines(FM ~ Year, out, lwd = 2)
  abline(h = c(F01, F_SPR), lty = 3:4)
  if(forecast) abline(v = 2020, lty = 3)
  legend("topright", c(expression(F[0.1]), 
                       parse(text = paste0("expression(F[", 100 * SPR, "*\"%\"])")) %>% eval()),
         lty = 3:4)
}

plot_F_SSF <- function(replist, forecast = TRUE, ylim = c(0, 3), fleet = list(c(1, 3), c(2, 4), 5)) {
  ts <- replist$timeseries %>% dplyr::filter(Era != "VIRG")
  if(!forecast) ts <- dplyr::filter(ts, Era != "FORE")
  FF <- ts[, grepl("F:_", colnames(ts))]
  F_lab <- c("SJR F", "SJR M", "BOF")
  
  Fout <- Map(function(x, y) {
    data.frame(Year = ts$Yr, F = FF[, x, drop = FALSE] %>% rowSums(), Fishery = y)
  }, x = fleet, y = F_lab) %>% do.call(rbind, .) %>% mutate(Fishery = factor(Fishery, levels = F_lab))
  
  g <- ggplot(Fout, aes(Year, F, colour = Fishery)) + geom_hline(yintercept = 0, colour = "grey") +
    geom_line() + theme_bw() + coord_cartesian(ylim = ylim)
  if(forecast) g <- g + geom_vline(xintercept = 2020, linetype = 3)
  g
  #SJR_F <- FF[, c(1, 3)] %>% rowSums()
  #SJR_M <- FF[, c(2, 4)] %>% rowSums()
  #BOF <- FF[, 5]
  #Yr <- ts$Yr
  #
  #plot(Yr, SJR_M, ylim = ylim, xlab = "Year", ylab = "Fishing mortality", typ = "n")
  #abline(h = 0, col = "grey")
  #matlines(Yr, cbind(SJR_F, SJR_M, BOF), lty = 1, lwd = 2)
  #if(forecast) abline(v = 2020, lty = 3)
  #legend("topright", F_lab, col = 1:3, lwd = 2)
}


# Compare models. Default = combined-sex fleet (CSF) vs. sex-specific fleet (SSF)
compare_models <- function(dir = c("01A_CSF_0.6Minas", "03A_SSF_0.6Minas"), names) {
  if(missing(names)) names <- dir
  out <- dir %>% 
    lapply(function(dir) r4ss::SS_output(file.path(getwd(), "SS", dir), verbose = FALSE))
  
  compare <- r4ss::SSsummarize(out, selgender = 1:2, verbose = FALSE, SpawnOutputUnits = "numbers")
  dev.off()
  r4ss::SSplotComparisons(compare, btarg = -1, minbthresh = -1, legendlabels = names, subplots = 1)
  return(invisible(compare))
}


SSC_from_CSF <- function(dir) {
  
  if(is.character(dir)) {
    replist <- r4ss::SS_output(file.path(getwd(), "SS", dir), covar = FALSE, verbose = FALSE)
  } else {
    replist <- dir
  }
  
  get_catch_series <- function(i, year = replist$startyr:replist$endyr, num = FALSE, ff = 2) {
    FF <- dplyr::filter(replist$ageselex, Factor == "F", Sex == i, Fleet == ff, 
                        Yr %in% year)[, -c(1:7)] %>% as.matrix()
    
    N <- dplyr::filter(replist$natage_annual_2_with_fishery, Sex == i, 
                       Yr %in% year)[, -c(1:3)] %>% as.matrix()
    
    M <- dplyr::filter(replist$M_at_age, Sex == i, 
                       Yr %in% year)[, -c(1:3)] %>% as.matrix()
    M[, 61] <- M[, 60]
    if(nrow(M) < length(year)) M <- rbind(M, M[nrow(M), ])
    
    Z <- FF + M
    CAA <- FF/Z * (1 - exp(-Z)) * N
    
    if(num) {
      Cout <- rowSums(CAA)
    } else {
      wt <- dplyr::filter(replist$ageselex, Factor == "bodywt", Sex == i, Fleet == 2, 
                          Yr %in% year)[, -c(1:7)] %>% as.matrix()
      Cout <- rowSums(CAA * wt)
    }
    return(Cout)
  }
  
  catch_hist <- lapply(1:2, get_catch_series)
  
  Hist <- data.frame(Yr = replist$startyr:replist$endyr, Catch_F = catch_hist[[1]], Catch_M = catch_hist[[2]]) %>%
    mutate(Catch_T = Catch_F + Catch_M, ratio_F = Catch_F/Catch_T)
  
  catch_proj <- lapply(1:2, get_catch_series, year = replist$endyr + 1:replist$nforecastyears, num = TRUE, ff = 1)
  
  proj <- data.frame(Yr = replist$endyr + 1:replist$nforecastyears, 
                     Catch_F = catch_proj[[1]], Catch_M = catch_proj[[2]]) %>%
    mutate(Catch_T = Catch_F + Catch_M, ratio_F = Catch_F/Catch_T)
  
  catch_Minas <- lapply(1:2, get_catch_series, ff = 3)
  
  Minas <- data.frame(Yr = replist$startyr:replist$endyr, 
                      Catch_F = catch_Minas[[1]], Catch_M = catch_Minas[[2]]) %>%
    mutate(Catch_T = Catch_F + Catch_M, ratio_F = Catch_F/Catch_T)
  
  list(Hist = Hist, Minas = Minas, proj = proj)
}


SSC_from_SSF <- function(dir) {
  
  replist <- r4ss::SS_output(file.path(getwd(), "SS", dir), covar = FALSE, verbose = FALSE)
  
  get_catch_series <- function(i, year = replist$startyr:replist$endyr, num = FALSE, ff = 2) {
    FF <- dplyr::filter(replist$ageselex, Factor == "F", Sex == i, Fleet == ff, 
                        Yr %in% year)[, -c(1:7)] %>% as.matrix()
    
    N <- dplyr::filter(replist$natage_annual_2_with_fishery, Sex == i, 
                       Yr %in% year)[, -c(1:3)] %>% as.matrix()
    
    M <- dplyr::filter(replist$M_at_age, Sex == i, 
                       Yr %in% year)[, -c(1:3)] %>% as.matrix()
    M[, 61] <- M[, 60]
    if(nrow(M) < length(year)) M <- rbind(M, M[nrow(M), ])
    
    Z <- FF + M
    CAA <- FF/Z * (1 - exp(-Z)) * N
    
    if(num) {
      Cout <- rowSums(CAA)
    } else {
      wt <- dplyr::filter(replist$ageselex, Factor == "bodywt", Sex == i, Fleet == 2, 
                          Yr %in% year)[, -c(1:7)] %>% as.matrix()
      Cout <- rowSums(CAA * wt)
    }
    return(Cout)
  }
  
  catch_hist <- lapply(1:2, get_catch_series, ff = expression(i + 2))
  
  Hist <- data.frame(Yr = replist$startyr:replist$endyr, Catch_F = catch_hist[[1]], Catch_M = catch_hist[[2]]) %>%
    mutate(Catch_T = Catch_F + Catch_M, ratio_F = Catch_F/Catch_T)
  
  catch_proj <- lapply(1:2, get_catch_series, year = replist$endyr + 1:replist$nforecastyears, num = TRUE, 
                       ff = expression(i))
  
  proj <- data.frame(Yr = replist$endyr + 1:replist$nforecastyears, 
                     Catch_F = catch_proj[[1]], Catch_M = catch_proj[[2]]) %>%
    mutate(Catch_T = Catch_F + Catch_M, ratio_F = Catch_F/Catch_T)
  
  catch_Minas <- lapply(1:2, get_catch_series, ff = 5)
  
  Minas <- data.frame(Yr = replist$startyr:replist$endyr, 
                      Catch_F = catch_Minas[[1]], Catch_M = catch_Minas[[2]]) %>%
    mutate(Catch_T = Catch_F + Catch_M, ratio_F = Catch_F/Catch_T)
  
  list(Hist = Hist, Minas = Minas, proj = proj)
}
