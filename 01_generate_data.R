

library(dplyr)
library(readxl)

dir <- getwd()
data_path <- file.path(dir, "data")

startyr <- 1880
endyr <- 2020


# SS assumes units of abundance in 10^3 fish and weight in metric tonnes
generate_catch <- function() {
  
  curr_catch <- readxl::read_excel(file.path(data_path, "sturgeon_2020.xlsx")) %>% 
    group_by(year) %>% summarise(Harvest = sum(Harvest))
  c_df <- data.frame(Year = curr_catch$year, Season = 1, Fleet = 1, Catch = curr_catch$Harvest/1e3, SE = 0.01)
  
  #curr_catch2 <- readxl::read_excel(file.path(data_path, "sturgeon_2020.xlsx"), sheet = 3) %>% filter(Harvested == 1) %>%
  #  group_by(YYYY) %>% summarise(Catch = sum(Count))

  #c_catch <- numeric(length(startyr:endyr))
  #c_catch[match(curr_catch$year, startyr:endyr)] <- curr_catch$Harvest
  #c_catch[c_catch <= 0] <- 1e-4
  #c_df <- data.frame(Year = startyr:endyr, Season = 1, Fleet = 1, Catch = c_catch, SE = 0.01)
  
  hist_catch <- readxl::read_excel(file.path(data_path, "Table_4_Bradford_2016.xlsx"))
  h_df <- data.frame(Year = hist_catch$Year, Season = 1, Fleet = 2, Catch = hist_catch$Catch, SE = 0.01) %>% filter(Catch > 0)
    
  #h_catch <- numeric(length(startyr:endyr))
  #h_catch[match(hist_catch$Year, startyr:endyr)] <- hist_catch$Catch * 1e3 # Need to convert to proper units
  #h_catch[h_catch <= 0] <- 1e-4
  #h_df <- data.frame(Year = startyr:endyr, Season = 1, Fleet = 2, Catch = h_catch, SE = 0.01)
  
  return(rbind(c_df, h_df))
}

catch <- generate_catch()
write.csv(catch, "processed_data/catch.csv")


generate_cpue <- function() {
  cpue <- readxl::read_excel(file.path(data_path, "sturgeon_2020.xlsx"), sheet = 2) %>% mutate(cpue = catch/nets) %>%
    group_by(year) %>% summarise(catch = sum(catch), nets = sum(nets), obs = mean(cpue), SE = sd(cpue), ln_SE = sd(log(cpue)[!is.infinite(log(cpue))])) %>% 
    mutate(CV = SE/obs)
  return(data.frame(Year = cpue$year, Month = 6, Fleet = 3, Obs = cpue$obs, SE = cpue$ln_SE))
}

cpue <- generate_cpue()
write.csv(cpue, "processed_data/cpue.csv")


generate_CAL <- function(bins = seq(40, 280, 5), month = 6) {
  
  sex_legend <- data.frame(Sex = c("f", "F", "j", "J", "m", "M", "M / J", "SF", "x", "X"),
                           Sout = c("F", "F", "J", "J", "M", "M", "J", "F", "X", "X"))
  
  CAL <- readxl::read_excel(file.path(data_path, "sturgeon_2020.xlsx"), sheet = 3) %>% left_join(sex_legend, by = "Sex") %>% 
    filter(Sout == "F" | Sout == "M")
  CAL$`TL(cm)`[is.na(CAL$`TL(cm)`)] <- CAL$`TL(in)`[is.na(CAL$`TL(cm)`)] * 2.54
  CAL <- CAL[!is.na(CAL$`TL(cm)`), ]
  CAL <- filter(CAL, `TL(cm)` <= max(bins) & `TL(cm)` >= min(bins))
  
  ### Harvested length comps
  CAL_harvest <- filter(CAL, Harvested == 1) %>% 
    mutate(Bin = cut(`TL(cm)`, breaks = bins, labels = bins[-length(bins)], right = FALSE) %>% factor(levels = bins))
  CAL_h <- reshape2::dcast(CAL_harvest, YYYY ~ Sout + Bin, value.var = "Harvested", fun.aggregate = sum, drop = FALSE)
  CAL_hf <- CAL_h[, match(c("YYYY", paste0("F_", bins)), colnames(CAL_h))]
  CAL_hm <- CAL_h[, match(c("YYYY", paste0("M_", bins)), colnames(CAL_h))]
  
  ### Released length comps
  CAL_rel <- filter(CAL, Harvested != 1) %>% 
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
  
  sex_ratio_harvest <- filter(sex_ratio, Sout == "F")$n_harvest/filter(sex_ratio, Sout == "M")$n_harvest
  sex_ratio_catch <- filter(sex_ratio, Sout == "F")$n/filter(sex_ratio, Sout == "M")$n
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

# Retain only
CAL <- generate_CAL(bins = seq(130, 280, 5)) %>% filter(Partition == 2)
write.csv(CAL, "processed_data/CAL_retain.csv")

