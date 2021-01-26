source("00_functions_Brownie.R")

#tag_data <- get_tag_data()
#rownames(tag_data[[2]]) <- 2009:2019
#saveRDS(tag_data, file = "processed_data/tag_table.rds")
tag_data <- readRDS("processed_data/tag_table.rds") 

Br_estM <- Brownie(tag_data, report_rate = 0.5) # Estimate M
Br_0.06M <- Brownie(tag_data, TRUE) # Fix M

png("figures/brownie/F_estM.png", height = 3, width = 4, res = 400, units = "in")
par(mar = c(5, 4, 1, 1))
plot_F_Brownie(Br_estM, c(0, 0.3))
dev.off()

png("figures/brownie/F_fixM.png", height = 3, width = 4, res = 400, units = "in")
par(mar = c(5, 4, 1, 1))
plot_F_Brownie(Br_0.06M, c(0, 0.1))
dev.off()

# Profile M
M <- seq(0.05, 0.3, 0.01)
profile_M <- vapply(M, function(x) {
  Brownie(tag_data, fix_M = TRUE, M = x)$opt$objective
}, numeric(1))

png("figures/brownie/M_profile.png", height = 3, width = 4, res = 400, units = "in")
par(mar = c(5, 4, 1, 1))
plot(M, profile_M, ylab = "Brownie likelihood profile", typ = "o")
dev.off()

png("figures/brownie/fit_estM.png", height = 4, width = 5, res = 400, units = "in")
plot_Brownie_fit(Br_estM)
dev.off()

png("figures/brownie/fit_fixM.png", height = 4, width = 5, res = 400, units = "in")
plot_Brownie_fit(Br_0.06M)
dev.off()

png("figures/brownie/resid_estM.png", height = 3, width = 4, res = 400, units = "in")
par(mar = c(5, 4, 1, 1))
plot_Brownie_resid(Br_estM, bubble_adj = 10)
dev.off()

png("figures/brownie/resid_fixM.png", height = 3, width = 4, res = 400, units = "in")
par(mar = c(5, 4, 1, 1))
plot_Brownie_resid(Br_0.06M, bubble_adj = 10)
dev.off()


# Look at AIC
AIC_Brownie <- function(Br) 2 * (Br$opt$objective + length(Br$SD$par.fixed))
AIC_Brownie(Br_0.06M)
AIC_Brownie(Br_estM)


output <- data.frame(Year = 2009:2020, F = Br_0.06M$report$F %>% round(3), SE = Br_0.06M$SD$sd[2:13] %>% round(3))
write.csv(output, file = "tables/Brownie_F.csv")
