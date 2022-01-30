

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
plot(Flow ~ ymd, mac_flow_daily, typ = 'l', xlab = "Time", ylab = "Mactaquac flow (cfs)")
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

# number of nets
n_nets <- group_by(fishery, year, month) %>% summarise(nets_per_day = mean(nets), nets = sum(nets)) %>% 
  group_by(year) %>% mutate(percent_nets = nets/sum(nets), Month = as.factor(month))

ggplot(n_nets, aes(year, nets_per_day, colour = Month)) + geom_line() + geom_point() + theme_bw() +
  labs(y = "Nets per day") + labs(x = "Year") + coord_cartesian(ylim = c(0, 12))
ggsave("figures/data/nets_per_day.png", height = 3, width = 4)

# percent of empty nets
empty_nets <- group_by(fishery, year, month) %>% summarise(percent_empty = sum(!cpue)/n()) %>%
  mutate(Month = as.factor(month))
ggplot(empty_nets, aes(year, percent_empty, colour = Month)) + geom_line() + geom_point() + 
  facet_wrap(~ Month) + theme_bw() + labs(y = "Empty nets (%)")
ggsave("figures/data/empty_nets.png", height = 3, width = 4)


# CPUE increases with Flow
ggplot(fishery, aes(Flow, cpue, colour = Month)) + geom_point() + theme_bw() + labs(y = "Observed CPUE")
ggsave("figures/data/cpue_vs_flow.png", height = 3, width = 5)

ggplot(fishery, aes(log(Flow), log(cpue), colour = Month)) + 
  geom_point() + theme_bw() + labs(y = "Observed log(CPUE)")
ggsave("figures/data/log_cpue_vs_flow.png", height = 3, width = 5)

# CPUE vs effort
ggplot(fishery, aes(nets, cpue)) + geom_smooth() + 
  geom_jitter(width = 0.2, height = 0, alpha = 0.8, aes(colour = Month)) +
  theme_bw() + labs(x = "Effort (nets per day)", y = "Observed CPUE")
ggsave("figures/data/cpue_vs_effort.png", height = 3, width = 5)

ggplot(fishery, aes(nets, log(cpue))) + geom_smooth() +
  geom_jitter(width = 0.2, height = 0, alpha = 0.8, aes(colour = Month)) +
  theme_bw() + labs(x = "Effort (nets per day)", y = "Observed log(CPUE)")
ggsave("figures/data/log_cpue_vs_effort.png", height = 3, width = 5)


ggplot(fishery, aes(nets, log(cpue))) + facet_wrap(~Month) + geom_smooth() +
  geom_jitter(width = 0.2, height = 0, alpha = 0.8, aes(colour = Month)) +
  theme_bw() + labs(x = "Effort (nets per day)", y = "Observed log(CPUE)")
ggsave("figures/data/log_cpue_vs_effort_by_month.png", height = 3, width = 5)

# CPUE with temperature
ggplot(fishery, aes(ETemp, cpue, colour = Month)) + 
  geom_point() + theme_bw() + labs(x = "Temperature at Evandale", y = "Observed CPUE")
ggsave("figures/data/cpue_vs_temp.png", height = 3, width = 5)

ggplot(fishery %>% mutate(Month = as.factor(month)), aes(log(ETemp), log(cpue), colour = Month)) + 
  geom_point() + theme_bw()
ggsave("figures/data/log_cpue_vs_temp.png", height = 3, width = 5)

# flow vs month
flow <- group_by(fishery, year, Month) %>% summarise(Flow = mean(Flow))
ggplot(flow, aes(year, Flow, colour = Month)) + geom_line() + geom_point() + 
  theme_bw() + coord_cartesian(ylim = c(0, 60000)) + labs(x = "Year", y = "Mactaquac Flow (cfs)")
ggsave("figures/data/Flow_month.png", height = 2.5, width = 4)

# Monthly values
ggplot(fishery, aes(year, cpue, colour = Month)) + facet_wrap(~ Month) + geom_point() + 
  theme_bw() + labs(y = "Observed CPUE")
ggsave("figures/data/daily_CPUE.png", height = 3, width = 4)

cpue_val <- fishery %>% group_by(year, Month) %>% 
  summarise(lower = exp(mean(log_cpue) - 2 * sd(log_cpue)), 
            upper = exp(mean(log_cpue) + 2 * sd(log_cpue)), 
            obs = mean(log_cpue, na.rm = TRUE) %>% exp())
ggplot(cpue_val, aes(year, obs, colour = Month)) + 
  facet_wrap(~ Month) + 
  geom_line() + geom_point() + geom_linerange(aes(ymin = lower, ymax = upper)) + 
  theme_bw() + coord_cartesian(ylim = c(0, 8)) +
  labs(y = "Observed CPUE")
ggsave("figures/data/CPUE_month.png", height = 3, width = 4)

cpue_val <- fishery %>% group_by(year, Month) %>% 
  summarise(lower = mean(log_cpue) - 2 * sd(log_cpue), 
            upper = mean(log_cpue) + 2 * sd(log_cpue), 
            obs = mean(log_cpue, na.rm = TRUE))
ggplot(cpue_val, aes(year, obs, colour = Month)) + 
  facet_wrap(~ Month, scales = "free_y") + 
  geom_line() + geom_point() + geom_linerange(aes(ymin = lower, ymax = upper)) + 
  theme_bw() + labs(y = "Observed log(CPUE)")
ggsave("figures/data/log_CPUE_month.png", height = 3, width = 4)




############ Run some linear models
data_mod <- fishery %>% dplyr::filter(mm != 9, year >= 2009) %>% mutate(offset = log(nets))

#m1q <- glm(catch ~ as.factor(year), data = data_mod, family = "quasipoisson", offset = offset)
#m2q <- glm(catch ~ yy + mm, data = data_mod, family = "quasipoisson", offset = offset)
#m3q <- glm(catch ~ yy * mm, data = data_mod, family = "quasipoisson", offset = offset)
#m4q <- glm(catch ~ yy + mm + ln_Flow_Z, data = data_mod, family = "quasipoisson", offset = offset)
#m5q <- glm(catch ~ yy * mm + ln_Flow_Z, data = data_mod, family = "quasipoisson", offset = offset)
#AIC(m1q, m2q, m3q, m4q, m5q)

m0nb <- MASS::glm.nb(catch ~ offset(offset), data = data_mod)
m1nb <- MASS::glm.nb(catch ~ 0 + offset(offset) + yy, data = data_mod)
m2nb <- MASS::glm.nb(catch ~ 0 + offset(offset) + yy + mm, data = data_mod)
m3nb <- MASS::glm.nb(catch ~ 0 + offset(offset) + yy * mm, data = data_mod)
m4nb <- MASS::glm.nb(catch ~ 0 + offset(offset) + yy + mm + ln_Flow_Z, data = data_mod)
m5nb <- MASS::glm.nb(catch ~ 0 + offset(offset) + yy * mm + ln_Flow_Z, data = data_mod)
AIC(m0nb, m1nb, m2nb, m3nb, m4nb, m5nb)

m0p <- glm(catch ~ 1, data = data_mod, family = "poisson", offset = offset)
m1p <- glm(catch ~ 0 + yy, data = data_mod, family = "poisson", offset = offset)
m2p <- glm(catch ~ 0 + yy + mm, data = data_mod, family = "poisson", offset = offset)
m3p <- glm(catch ~ 0 + yy * mm, data = data_mod, family = "poisson", offset = offset)
m4p <- glm(catch ~ 0 + yy + mm + ln_Flow_Z, data = data_mod, family = "poisson", offset = offset)
m5p <- glm(catch ~ 0 + yy * mm + ln_Flow_Z, data = data_mod, family = "poisson", offset = offset)
AIC(m0p, m1p, m2p, m3p, m4p, m5p)

aov(m5nb) %>% summary()

comp_mod <- AIC(m0p, m1p, m2p, m3p, m4p, m5p, m0nb, m1nb, m2nb, m3nb, m4nb, m5nb)
comp_mod$delta_AIC <- comp_mod$AIC %>% `-`(min(.)) %>% round(2)
comp_mod$theta <- lapply(list(m0p, m1p, m2p, m3p, m4p, m5p, m0nb, m1nb, m2nb, m3nb, m4nb, m5nb), getElement, "theta") %>%
  sapply(function(x) if(is.null(x)) NA_real_ else x)
write.csv(comp_mod, file = "tables/CPUE_AIC.csv")

png("figures/data/CPUE_model_diagnostics.png", height = 8, width = 6, units = "in", res = 600)
mout <- m5nb
par(mfrow = c(3, 2), mar = c(5, 4, 1.5, 1))
plot(mout, which = 1:3)

resid(mout) %>% hist(main = "Histogram of Residuals", xlab = "Residuals", font.main = 1)
box()

data.frame(Residuals = resid(mout), Year = mout$model$yy) %>% plot(Residuals ~ Year, .)
abline(h = 0, lty = 2)

data.frame(Residuals = resid(mout), Month = mout$model$mm) %>% plot(Residuals ~ Month, .)
abline(h = 0, lty = 2)

dev.off()


# By year
newdata <- expand.grid(yy = unique(data_mod$yy), mm = factor(c(5, 7, 8)),
                       ln_Flow_Z = mean(data_mod$ln_Flow_Z, na.rm = TRUE), 
                       offset = mean(log(data_mod$nets)))

obs_cpue <- summarise(group_by(data_mod, yy), val = mean(log_cpue) %>% exp() %>% `-`(0.001),
                      se = sd(log_cpue)) %>% mutate(Type = "Observed")

annual_cpue <- mutate(newdata, 
                      val = predict(m5nb, newdata = newdata),
                      se = predict(m5nb, newdata = newdata, se.fit = TRUE)$se.fit) %>%
  group_by(yy) %>% summarise(val = mean(val) %>% exp(), se = mean(se)) %>% mutate(Type = "Standardized") %>%
  rbind(obs_cpue) %>% group_by(Type) %>% mutate(yy = as.numeric(yy) + 2006, val = val/mean(val))
write.csv(annual_cpue, "processed_data/cpue_series.csv")

ggplot(annual_cpue, 
       aes(yy, val)) + 
  geom_line(aes(linetype = Type)) + geom_point(aes(shape = Type)) + 
  #geom_linerange(data = dplyr::filter(annual_cpue, Type == "Standardized"),
  #               aes(ymin = log(val) %>% `-`(1.96 * se) %>% exp(),
  #                  ymax = log(val) %>% `+`(1.96 * se) %>% exp())) + 
  scale_linetype_manual(values = c("Observed" = 3, "Standardized" = 1)) +
  scale_shape_manual(values = c("Observed" = 1, "Standardized" = 16)) +
  theme_bw() + labs(x = "Year", y = "Relative CPUE") + coord_cartesian(ylim = c(0, 3)) 
ggsave("figures/data/standardized_CPUE.png", height = 2.5, width = 5)
