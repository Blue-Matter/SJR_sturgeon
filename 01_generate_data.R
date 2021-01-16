

library(dplyr)
library(readxl)

dir <- getwd()
data_path <- file.path(dir, "data")

startyr <- 1880
endyr <- 2020


# SS assumes units of abundance in 10^3 fish and weight in metric tonnes
generate_catch <- function() {
  
  curr_catch <- readxl::read_excel(file.path(data_path, "sturgeon summary table start updated Dec 16.xlsx")) %>% 
    group_by(year) %>% summarise(Harvest = sum(M_harvest + F_harvest + X_harvest))
  c_df <- data.frame(Year = curr_catch$year, Season = 1, Fleet = 1, Catch = curr_catch$Harvest/1e3, SE = 0.01)
  
  hist_catch <- readxl::read_excel(file.path(data_path, "Table_4_Bradford_2016.xlsx"))
  h_df <- data.frame(Year = hist_catch$Year, Season = 1, Fleet = 2, Catch = hist_catch$SJR, SE = 0.01) %>% 
    dplyr::filter(Catch > 0)
  
  minas_df <- data.frame(Year = hist_catch$Year, Season = 1, Fleet = 3, 
                         Catch = hist_catch$NB_Fundy + hist_catch$NS_Fundy, SE = 0.01) %>% 
    dplyr::filter(Catch > 0)
  
  return(rbind(c_df, h_df, minas_df))
}

catch <- generate_catch()
write.csv(catch, "processed_data/catch.csv")


# SS assumes units of abundance in 10^3 fish and weight in metric tonnes
generate_catch_by_sex <- function() {
  curr_catch <- readxl::read_excel(file.path(data_path, "sturgeon summary table start updated Dec 16.xlsx")) 
  
  sex_ratio_all <- curr_catch %>% summarise(ratio_F = sum(F_harvest)/sum(F_harvest + M_harvest))
  sex_ratio_annual <- curr_catch %>% group_by(year) %>% summarise(ratio_F = sum(F_harvest)/sum(F_harvest + M_harvest))
  sex_ratio_annual$ratio_F[1] <- 0.5
  
  curr_catch <- curr_catch %>% dplyr::left_join(sex_ratio_annual, by = "year") %>% group_by(year) %>%
    summarise(M_harvest = sum(M_harvest + (1 - ratio_F) * X_harvest),
              F_harvest = sum(F_harvest + ratio_F * X_harvest)) 
  
  c_female <- data.frame(Year = curr_catch$year, Season = 1, Fleet = 1, Catch = curr_catch$F_harvest/1e3, SE = 0.01)
  c_male <- data.frame(Year = curr_catch$year, Season = 1, Fleet = 2, Catch = curr_catch$M_harvest/1e3, SE = 0.01)

  hist_catch <- readxl::read_excel(file.path(data_path, "Table_4_Bradford_2016.xlsx"))
  h_female <- data.frame(Year = hist_catch$Year, Season = 1, Fleet = 3, Catch = 0.55 * hist_catch$SJR, SE = 0.01) %>% 
    dplyr::filter(Catch > 0)
  h_male <- data.frame(Year = hist_catch$Year, Season = 1, Fleet = 4, Catch = 0.45 * hist_catch$SJR, SE = 0.01) %>% 
    dplyr::filter(Catch > 0)
  
  minas_df <- data.frame(Year = hist_catch$Year, Season = 1, Fleet = 5, 
                         Catch = hist_catch$NB_Fundy + hist_catch$NS_Fundy, SE = 0.01) %>% 
    dplyr::filter(Catch > 0)
  
  return(rbind(c_female, c_male, h_female, h_male, minas_df))
}

catch_by_sex <- generate_catch_by_sex()
write.csv(catch_by_sex, "processed_data/catch_by_sex.csv")



generate_CAL_SJR <- function(bins = seq(40, 280, 5), month = 6) {
  
  sex_legend <- data.frame(Sex = c("f", "F", "j", "J", "m", "M", "M / J", "SF", "x", "X"),
                           Sout = c("F", "F", "J", "J", "M", "M", "J", "F", "X", "X"))
  
  CAL <- readxl::read_excel(file.path(data_path, "sturgeon summary table start updated Dec 16.xlsx"), sheet = 3, range = "A1:P6346") %>% 
    left_join(sex_legend, by = "Sex") %>% dplyr::filter(Sout == "F" | Sout == "M")
  CAL$`TL(cm)`[is.na(CAL$`TL(cm)`)] <- CAL$`TL(in)`[is.na(CAL$`TL(cm)`)] * 2.54
  CAL <- CAL[!is.na(CAL$`TL(cm)`), ]
  CAL <- dplyr::filter(CAL, `TL(cm)` <= max(bins) & `TL(cm)` >= min(bins))
  
  ### Harvested length comps
  CAL_harvest <- dplyr::filter(CAL, Harvested == 1) %>% 
    mutate(Bin = cut(`TL(cm)`, breaks = bins, labels = bins[-length(bins)], right = FALSE) %>% factor(levels = bins))
  CAL_h <- reshape2::dcast(CAL_harvest, YYYY ~ Sout + Bin, value.var = "Harvested", fun.aggregate = sum, drop = FALSE)
  CAL_hf <- CAL_h[, match(c("YYYY", paste0("F_", bins)), colnames(CAL_h))]
  CAL_hm <- CAL_h[, match(c("YYYY", paste0("M_", bins)), colnames(CAL_h))]
  
  ### Released length comps
  CAL_rel <- dplyr::filter(CAL, Harvested != 1) %>% 
    mutate(Bin = cut(`TL(cm)`, breaks = bins, labels = bins[-length(bins)], right = FALSE) %>% factor(levels = bins),
           Counter = 1L)
  CAL_r <- reshape2::dcast(CAL_rel, YYYY ~ Sout + Bin, value.var = "Counter", fun.aggregate = sum, drop = FALSE)
  CAL_rf <- CAL_r[, match(c("YYYY", paste0("F_", bins)), colnames(CAL_h))]
  CAL_rm <- CAL_r[, match(c("YYYY", paste0("M_", bins)), colnames(CAL_h))]
  
  SAMtool::plot_composition(Year = CAL_h$YYYY, CAL_rf[, -1], CAL_hf[, -1], CAL_bins = bins, annual_yscale = "raw") # Black = release, red = retained
  SAMtool::plot_composition(Year = CAL_h$YYYY, CAL_rm[, -1], CAL_hm[, -1], CAL_bins = bins, annual_yscale = "raw") # Black = release, red = retained
  
  ### Sex ratio
  sexratio_harvest <- CAL_harvest %>% group_by(YYYY, Sout) %>% summarise(n_harvest = n())
  sexratio_rel <- CAL_rel %>% group_by(YYYY, Sout) %>% summarise(n_rel = n())
  
  sex_ratio <- full_join(sexratio_harvest, sexratio_rel, by = c("YYYY", "Sout")) %>% mutate(n = n_rel + n_harvest)
  
  sex_ratio_harvest <- dplyr::filter(sex_ratio, Sout == "F")$n_harvest/dplyr::filter(sex_ratio, Sout == "M")$n_harvest
  sex_ratio_catch <- dplyr::filter(sex_ratio, Sout == "F")$n/dplyr::filter(sex_ratio, Sout == "M")$n
  plot(2007:2020, sex_ratio_harvest)
  plot(2007:2020, sex_ratio_catch)
  
  
  ### Process for SS
  CAL_out <- list(harvest_f = CAL_hf, harvest_m = CAL_hm, rel_f = CAL_rf, rel_m = CAL_rm)
  
  sex_code <- c(1, 2, 1, 2) # Should sex ratio be retained, i.e., 3. No for 2007...
  part_code <- c(2, 2, 1, 1) # 2 = retained, 1 = discard
  
  generate_CAL_SS <- function(xx, sex_code, part_code) {
    if(sex_code == 1) {
      Nsamp <- xx[, grepl("F_", colnames(xx))] %>% rowSums()
    } else {
      Nsamp <- xx[, grepl("M_", colnames(xx))] %>% rowSums()
    }
    data.frame(Year = xx$YYYY, Month = month, Fleet = 1, Sex = sex_code, Partition = part_code, 
               Nsamp = Nsamp) %>% cbind(xx[, -1])
  }
  Map(generate_CAL_SS, xx = list(CAL_h, CAL_h, CAL_r, CAL_r), 
      sex_code = sex_code, part_code = part_code) %>% do.call(rbind, .)
}

# Retain only # Lower length bin at 130 but decrease for Minas bay length comp
CAL_SJR <- generate_CAL_SJR(bins = seq(45, 280, 5)) %>% dplyr::filter(Partition == 2)

head(CAL_SJR)

generate_CAL_Minas <- function(bins = seq(80, 280, 5), month = 6, Year_superperiod = 2007:2010) {
  CAL <- read.csv(file.path(data_path, "Dadswell_Fig_4.csv")) %>% mutate(Bin = (Bin - 1) * 2.5 + 45)
  
  CAL_new <- vapply(1:length(bins), function(i) {
    if(i == 1) {
      out <- dplyr::filter(CAL, Bin <= bins[i])
    } else {
      out <- dplyr::filter(CAL, Bin <= bins[i], Bin > bins[i-1])
    }
    out %>% getElement("N") %>% sum()
  }, numeric(1))
  
  plot(bins, CAL_new, typ = 'o')
  
  out <- data.frame(Year = Year_superperiod, 
                    Month = c(-month, rep(month, length(Year_superperiod) - 2), -month), 
                    Fleet = c(3, rep(-3, length(Year_superperiod) - 1)), 
                    Sex = 0, Partition = 0, 
                    Nsamp = sum(CAL_new))
  
  CAL_out <- matrix(0, nrow = nrow(out), ncol = length(bins) * 2) %>% 
    structure(dimnames = list(NULL, c(paste0("F_", bins), paste0("M_", bins))))
  CAL_out[, 1:length(bins)] <- matrix(CAL_new, nrow(CAL_out), length(bins), byrow = TRUE)
  
  cbind(out, CAL_out) %>% as.data.frame()
}
CAL_Minas <- generate_CAL_Minas(bins = seq(45, 280, 5))


rbind(CAL_SJR, CAL_Minas) %>% write.csv("processed_data/CAL_retain.csv")




generate_CAA_Minas <- function(month = 6, Year_superperiod = 2005:2009) {
  CAA <- read.csv(file.path(data_path, "Dadswell_Fig_7.csv"))
  
  plot(N ~ Age, CAA, typ = 'o')
  
  out <- data.frame(Year = Year_superperiod, 
                    Month = c(-month, rep(month, length(Year_superperiod) - 2), -month), 
                    Fleet = c(3, rep(-3, length(Year_superperiod) - 1)), 
                    Sex = 0, Partition = 0, Age_Err = 0,
                    Lbin_lo = -1, Lbin_hi = -1,
                    Nsamp = sum(CAA$N))
  
  CAL_out <- matrix(0, nrow = nrow(out), ncol = length(CAA$Age) * 2) %>% 
    structure(dimnames = list(NULL, c(paste0("F_", CAA$Age), paste0("M_", CAA$Age))))
  CAL_out[, 1:length(CAA$Age)] <- matrix(CAA$N, nrow(CAL_out), nrow(CAA), byrow = TRUE)
  
  cbind(out, CAL_out) %>% as.data.frame()
}
CAA_Minas <- generate_CAA_Minas()
write.csv(CAA_Minas, "processed_data/CAA_Minas.csv")
