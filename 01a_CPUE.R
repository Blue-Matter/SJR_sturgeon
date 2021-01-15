

library(dplyr)
library(readxl)

dir <- getwd()
data_path <- file.path(dir, "data")


# Water flow data from Mactaquac
mac_flow <- readxl::read_excel(file.path(data_path, "MQTDF.xlsx")) %>%
  mutate(year = lubridate::year(DT), month = lubridate::month(DT), day = lubridate::day(DT), 
         ymd = lubridate::ymd(paste0(year, "-", month, "-", day)))

mac_flow_daily <- group_by(mac_flow, ymd) %>% summarise(Flow = mean(Flow, na.rm = TRUE))

# Flow plots
png("figures/data/MQTDF.png", height = 4, width = 6, units = "in", res = 400)
plot(Flow ~ ymd, mac_flow_daily, typ = 'l', xlab = "Date", ylab = "Mactaquac Flow")
dev.off()

mac_flow_daily %>% mutate(year = lubridate::year(ymd), month = lubridate::month(ymd)) %>%
  group_by(year, month) %>% summarise(Flow_mean = mean(Flow)) %>% 
  mutate(ymd = lubridate::ymd(paste0(year, "-", month, "-01"))) %>%
  ggplot(aes(ymd, Flow_mean, colour = as.factor(month))) + geom_point() + geom_line()


#plot(Flow ~ DT, mac_flow, typ = 'l')
#mac_flow_daily %>% dplyr::filter(lubridate::month(ymd) == 5) %>% plot(Flow ~ ymd, ., typ = 'o')

# Water temperature 
evandale_temp <- readxl::read_excel(file.path(data_path, "SJR temperatures at Evandale 2009_2020.xlsx")) %>%
  mutate(year = lubridate::year(`Result Date`), month = lubridate::month(`Result Date`), day = lubridate::day(`Result Date`), 
         ymd = lubridate::ymd(paste0(year, "-", month, "-", day))) %>%
  group_by(ymd) %>% summarise(ETemp = mean(Temp)) %>% mutate(year = lubridate::year(ymd), month = lubridate::month(ymd))

westfield_temp <- readxl::read_excel(file.path(data_path, "VR2_Tx_Westfield_ 2017-2019.xlsx")) %>% 
  dplyr::filter(Description == "Temperature") %>% 
  mutate(year = lubridate::year(`Date and Time (UTC)`), 
         month = lubridate::month(`Date and Time (UTC)`), 
         day = lubridate::day(`Date and Time (UTC)`), 
         ymd = lubridate::ymd(paste0(year, "-", month, "-", day))) %>% 
  group_by(ymd) %>% summarise(WTemp = mean(as.numeric(Data)))

# Plot temperature
plot(ETemp ~ ymd, evandale_temp, typ = 'o')
ggplot(evandale_temp, aes(ymd, ETemp, colour = as.factor(month))) + geom_point()


ggplot(westfield_temp, aes(ymd, WTemp, colour = as.factor(lubridate::month(ymd)))) + geom_point()
plot(WTemp ~ ymd, westfield_temp, typ = 'o')



fishery <- readxl::read_excel(file.path(data_path, "sturgeon summary table start updated Dec 16.xlsx"), 
                              range = "A1:F793", sheet = 2) %>% 
  mutate(cpue = catch/nets, year2 = pmax(year, 2009),
         ymd = lubridate::ymd(paste0(year2, "-", month, "-", day)),
         yy = factor(year), mm = factor(month)) %>%
  dplyr::left_join(mac_flow_daily, by = "ymd") %>%
  dplyr::left_join(evandale_temp %>% mutate(ymd = NULL), by = c("year", "month")) %>% 
  mutate(cpue_plus_tiny = cpue + 1e-4, log_cpue = log(cpue_plus_tiny), Month = as.factor(month),
         ln_Flow = log(Flow), ln_Flow_std = ln_Flow - mean(ln_Flow), ln_Flow_Z = ln_Flow_std/sd(ln_Flow))

# Remove zero sets in May
fishery2 <- dplyr::filter(fishery, !(month == 5 & cpue == 0))

mean_with <- dplyr::filter(fishery, month == 5) %>% group_by(year) %>% summarise(sd = sd(cpue), cpue = mean(cpue))
mean_without <- dplyr::filter(fishery2, month == 5) %>% group_by(year) %>% summarise(sd = sd(cpue), cpue = mean(cpue))

plot(cpue ~ year, mean_with, typ = 'o', xlab = "")
lines(cpue ~ year, mean_without, col = "red", typ = "o")
legend("topright", c("Include zeros in May", "Exclude zeros in May"), col = c("black", "red"), pch = 1)


# number of nets
n_nets <- group_by(fishery, year, month) %>% summarise(nets_per_day = mean(nets), nets = sum(nets)) %>% 
  group_by(year) %>% mutate(percent_nets = nets/sum(nets), Month = as.factor(month))

ggplot(n_nets, aes(year, nets_per_day, colour = Month)) + geom_line() + geom_point() + gfplot::theme_pbs() +
  labs(y = "Nets per day")
ggsave("figures/data/nets_per_day.png", height = 3, width = 4)

# percent of empty nets
empty_nets <- group_by(fishery, year, month) %>% summarise(percent_empty = sum(!cpue)/n()) %>%
  mutate(Month = as.factor(month))
  #group_by(year) %>% mutate(percent_nets = nets/sum(nets), Month = as.factor(month))
ggplot(empty_nets, aes(year, percent_empty, colour = Month)) + geom_line() + geom_point() + 
  facet_wrap(~ Month) + gfplot::theme_pbs() + labs(y = "Empty nets (%)")
ggsave("figures/data/empty_nets.png", height = 3, width = 4)


# CPUE increases with Flow
ggplot(fishery, aes(Flow, cpue, colour = Month)) + geom_point() + gfplot::theme_pbs() + labs(y = "Observed CPUE")
ggsave("figures/data/cpue_vs_flow.png", height = 3, width = 5)

ggplot(fishery, aes(log(Flow), log(cpue), colour = Month)) + 
  geom_point() + gfplot::theme_pbs() + labs(y = "Observed log(CPUE)")
ggsave("figures/data/log_cpue_vs_flow.png", height = 3, width = 5)

# CPUE with temperature
ggplot(fishery, aes(ETemp, cpue, colour = Month)) + 
  geom_point() + gfplot::theme_pbs() + labs(x = "Temperature at Evandale", y = "Observed CPUE")
ggsave("figures/data/cpue_vs_temp.png", height = 3, width = 5)

ggplot(fishery %>% mutate(Month = as.factor(month)), aes(log(ETemp), log(cpue), colour = Month)) + 
  geom_point() + gfplot::theme_pbs()
ggsave("figures/data/log_cpue_vs_temp.png", height = 3, width = 5)

# flow vs month
flow <- group_by(fishery, year, Month) %>% summarise(Flow = mean(Flow))
ggplot(flow, aes(year, Flow, colour = Month)) + geom_line() + geom_point() + 
  gfplot::theme_pbs() + coord_cartesian(ylim = c(0, 60000))
ggsave("figures/data/Flow_month.png", height = 2.5, width = 4)

# Monthly values
ggplot(fishery, aes(year, cpue, colour = Month)) + facet_wrap(~ Month) + geom_point() + 
  gfplot::theme_pbs() + labs(y = "Observed CPUE")
ggsave("figures/data/daily_CPUE.png", height = 3, width = 4)

cpue_val <- fishery2 %>% group_by(year, Month) %>% 
  summarise(lower = exp(mean(log_cpue) - 2 * sd(log_cpue)), 
            upper = exp(mean(log_cpue) + 2 * sd(log_cpue)), 
            obs = mean(log_cpue, na.rm = TRUE) %>% exp())
ggplot(cpue_val, aes(year, obs, colour = Month)) + 
  facet_wrap(~ Month) + 
  geom_line() + geom_point() + geom_linerange(aes(ymin = lower, ymax = upper)) + 
  gfplot::theme_pbs() + coord_cartesian(ylim = c(0, 8)) +
  labs(y = "Observed CPUE")
ggsave("figures/data/CPUE_month.png", height = 3, width = 4)

cpue_val <- fishery2 %>% group_by(year, Month) %>% 
  summarise(lower = mean(log_cpue) - 2 * sd(log_cpue), 
            upper = mean(log_cpue) + 2 * sd(log_cpue), 
            obs = mean(log_cpue, na.rm = TRUE))
ggplot(cpue_val, aes(year, obs, colour = Month)) + 
  facet_wrap(~ Month, scales = "free_y") + 
  geom_line() + geom_point() + geom_linerange(aes(ymin = lower, ymax = upper)) + 
  gfplot::theme_pbs() + labs(y = "Observed log(CPUE)")
ggsave("figures/data/log_CPUE_month.png", height = 3, width = 4)




############ Run some linear models
data_mod <- fishery2 %>% dplyr::filter(mm != 9, year >= 2009)
m1 <- lm(log_cpue ~ as.factor(year), data = data_mod)
m2 <- lm(log_cpue ~ yy + mm, data = data_mod)
m3 <- lm(log_cpue ~ yy * mm, data = data_mod)
m4 <- lm(log_cpue ~ yy + mm + ln_Flow_Z, data = data_mod)
m5 <- lm(log_cpue ~ yy * mm + ln_Flow_Z, data = data_mod)
AIC(m1, m2, m3, m4, m5)

#m2c <- glm(catch ~ yy + mm, data = data_mod, family = "poisson")
#m3c <- glm(catch ~ yy * mm, data = data_mod, family = "poisson")
#m4 <- lm(log(cpue+0.001) ~ as.factor(year) + as.factor(month) + ETemp, data = data_mod)
#m5 <- lm(log(cpue+0.001) ~ as.factor(year) + as.factor(month) + ETemp + Flow_std, data = data_mod)
#m6c <- glm(catch ~ yy + mm + ln_Flow_std, data = data_mod, family = "poisson")
#m7c <- glm(catch ~ yy * mm + ln_Flow_std, data = data_mod, family = "poisson")


# By year
newdata <- data.frame(yy = data_mod$yy, mm = data_mod$mm,
                      ln_Flow_Z = data_mod$ln_Flow_Z)

generate_annual_value <- function(x) mean(x) %>% exp()
generate_lower <- function(x) sd(x) %>% exp()
generate_upper <- function(x) mean(x) %>% exp()

annual_cpue_sd <- data_mod %>% 
  mutate(pred3 = predict(m3, newdata = newdata[, -3]), 
         pred4 = predict(m4, newdata = newdata),
         pred5 = predict(m5, newdata = newdata)) %>%
  group_by(yy) %>% 
  summarise(obs = sd(log_cpue), m3 = sd(pred3), m4 = sd(pred4), m5 = sd(pred5)) %>%
  reshape2::melt(id.vars = "yy", variable.name = "type", value.name = "sd")
matplot(annual_cpue_sd[, -1], typ = 'o')
  

newdata_mu <- data.frame(yy = unique(data_mod$yy), mm = factor(7),
                         ln_Flow_Z = mean(data_mod$ln_Flow_Z, na.rm = TRUE))

annual_cpue_mu <- group_by(data_mod, yy) %>% 
  summarise(obs = mean(log_cpue) %>% exp() %>% `-`(0.001)) %>%
  mutate(m3 = predict(m3, newdata = newdata_mu[, -3]) %>% exp() %>% `-`(0.001), 
         m4 = predict(m4, newdata = newdata_mu) %>% exp() %>% `-`(0.001),
         m5 = predict(m5, newdata = newdata_mu) %>% exp() %>% `-`(0.001))
matplot(annual_cpue_mu[, -1], typ = 'o')

ss_cpue <- reshape2::melt(annual_cpue_mu, id.vars = "yy", variable.name = "type") %>% 
  dplyr::left_join(annual_cpue_sd, by = c("yy", "type"))

write.csv(ss_cpue, "processed_data/cpue_with_zeros.csv")
write.csv(ss_cpue, "processed_data/cpue_without_zeros.csv")

write.csv(ss_cpue, "processed_data/cpue_with_zeros_since_2009.csv")
write.csv(ss_cpue, "processed_data/cpue_without_zeros_since_2009.csv")


# Compare series
series <- c("cpue_with_zeros", "cpue_without_zeros", "cpue_with_zeros_since_2009",
            "cpue_without_zeros_since_2009")
series_df <- lapply(series, function(x) {
  dff <- paste0("processed_data/", x, ".csv") %>% read.csv()
  dff$series <- x
  return(dff)
}) %>% do.call(rbind, .)
ggplot(series_df, aes(yy, value, colour = type)) + facet_wrap(~series) + geom_line() + geom_point() +
  gfplot::theme_pbs()
ggsave("figures/data/compare_cpue_series.png", width = 5, height = 4)
