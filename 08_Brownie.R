source("00_functions_Brownie.R")

tag_data <- readRDS("processed_data/tag_table.rds") 

Br_estM <- Brownie(tag_data) # Estimate M
Br_0.06M <- Brownie(tag_data, TRUE) # Fix M
Br_estret_fixM <- Brownie(tag_data, TRUE, fix_retain = FALSE) # remains = 0.8
Br_0.7ret <- Brownie(tag_data, TRUE, retain = 0.7) 
summary(Br_estret_fixM$SD) %>% cbind(`CV` = .[, "Std. Error"]/.[, "Estimate"]) %>% round(2) %>%
  write.csv("tables/Brownie.csv")

png("figures/brownie/F_estM.png", height = 3, width = 4, res = 400, units = "in")
par(mar = c(5, 4, 1, 1))
plot_F_Brownie(Br_estM, c(0, 0.2))
dev.off()

png("figures/brownie/F_fixM.png", height = 3, width = 4, res = 400, units = "in")
par(mar = c(5, 4, 1, 1))
plot_F_Brownie(Br_0.06M, c(0, 0.05))
dev.off()

png("figures/brownie/F_estret_fixM.png", height = 3, width = 4, res = 400, units = "in")
par(mar = c(5, 4, 1, 1))
plot_F_Brownie(Br_estret_fixM, c(0, 0.2))
dev.off()

png("figures/brownie/F_0.7ret.png", height = 3, width = 4, res = 400, units = "in")
par(mar = c(5, 4, 1, 1))
plot_F_Brownie(Br_0.7ret, c(0, 0.3))
dev.off()

# Profile tag loss
retain <- seq(0.6, 1, 0.05)
profile_retain <- vapply(retain, function(x) {
  Brownie(tag_data, fix_M = TRUE, retain = x)$opt$objective
}, numeric(1)) %>% `-`(min(.))

png("figures/brownie/tag_retention_with_fixed_M_profile.png", height = 3, width = 4, res = 400, units = "in")
par(mar = c(5, 4, 1, 1))
plot(retain, profile_retain, xlab = "Chronic tag retention rate", ylab = "Negative log-likelihood", typ = "o")
dev.off()

# Profile M
M <- seq(0.05, 0.3, 0.01)
profile_M <- vapply(M, function(x) {
  Brownie(tag_data, fix_M = TRUE, M = x)$opt$objective
}, numeric(1)) %>% `-`(min(.))
plot(M, profile_M)

# Joint profile with tag loss and M
joint_par <- expand.grid(retain = retain, M = M)
joint_profile <- vapply(1:nrow(joint_par), function(x) {
  Brownie(tag_data, fix_M = TRUE, retain = joint_par$retain[x], M = joint_par$M[x])$opt$objective
}, numeric(1)) %>% `-`(min(.))
joint_par$nll <- joint_profile

nll <- joint_par %>% dplyr::filter(retain >= 0.6, M <= 0.3) %>% reshape2::acast(list("M", "retain"))

png("figures/brownie/joint_profile_retention_M.png", height = 4, width = 6, res = 600, units = "in")
par(mar = c(5, 4, 1, 1))
contour(as.numeric(rownames(nll)), as.numeric(colnames(nll)), nll, 
        levels = 1:5 %>% c(seq(10, 60, 10)), 
        xlab = "Natural mortality", ylab = "Chronic tag retention rate")
#points(joint_par$M[which.min(joint_profile)], joint_par$retain[which.min(joint_profile)], 
#       pch = 16, cex = 1.5)
dev.off()


#png("figures/brownie/M_profile.png", height = 3, width = 4, res = 400, units = "in")
#par(mar = c(5, 4, 1, 1))
#plot(M, profile_M, ylab = "Change in likelihood", typ = "o", ylim = c(0, 10))
#dev.off()

png("figures/brownie/fit_estM.png", height = 4, width = 5, res = 400, units = "in")
plot_Brownie_fit(Br_estM)
dev.off()

png("figures/brownie/fit_fixM.png", height = 4, width = 5, res = 400, units = "in")
plot_Brownie_fit(Br_0.06M)
dev.off()

png("figures/brownie/fit_estret_fixM.png", height = 4, width = 5, res = 400, units = "in")
plot_Brownie_fit(Br_estret_fixM)
dev.off()

png("figures/brownie/fit_estret_fixM_nodiag.png", height = 4, width = 5, res = 400, units = "in")
plot_Brownie_fit(Br_estret_fixM, FALSE)
dev.off()

png("figures/brownie/fit_0.7ret.png", height = 4, width = 5, res = 400, units = "in")
plot_Brownie_fit(Br_0.7ret)
dev.off()

png("figures/brownie/resid_estM.png", height = 3, width = 4, res = 400, units = "in")
par(mar = c(5, 4, 1, 1))
plot_Brownie_resid(Br_estM, bubble_adj = 10)
dev.off()

png("figures/brownie/resid_fixM.png", height = 3, width = 4, res = 400, units = "in")
par(mar = c(5, 4, 1, 1))
plot_Brownie_resid(Br_0.06M, bubble_adj = 10)
dev.off()

png("figures/brownie/resid_estret_fixM.png", height = 3, width = 4, res = 400, units = "in")
par(mar = c(5, 4, 1, 1))
plot_Brownie_resid(Br_estret_fixM, bubble_adj = 10)
dev.off()

png("figures/brownie/resid_0.7ret.png", height = 3, width = 4, res = 400, units = "in")
par(mar = c(5, 4, 1, 1))
plot_Brownie_resid(Br_0.7ret, bubble_adj = 10)
dev.off()

# Look at AIC
AIC_Brownie <- function(Br) 2 * (Br$opt$objective + length(Br$SD$par.fixed))
AIC_Brownie(Br_0.06M)
AIC_Brownie(Br_estM)
AIC_Brownie(Br_estret_fixM)
AIC_Brownie(Br_0.7ret)


output <- data.frame(Year = 2009:2020, F = Br_estret_fixM$report$F %>% round(3), 
                     SE = Br_estret_fixM$SD$sd[2:13] %>% round(3)) %>%
  dplyr::mutate(CV = round(SE/F, 3))
write.csv(output, file = "tables/Brownie_F.csv")
