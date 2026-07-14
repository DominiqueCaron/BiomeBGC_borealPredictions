# Figure 2: Plot of validation metrics for each tower
# 6 panels:
# a) R2 for GPP
# b) RMSE for GPP
# c) Relative bias for GPP
# d) Across-tower GPP fit
# e) R2 for RECO
# f) RMSE for RECO
# g) Relative bias for RECO
# h) Across-tower RECO fit
library(ggplot2)
library(data.table)
library(patchwork)
library(ggpubr)
library(lme4)
source("scripts/fcnt4analysis.R")

towerName <- c(
  "CA-Gro",
  "CA-HPC",
  #"CA-LP1", #remove because disturbed by mountain pine beetle
  "CA-Man",
  "CA-NS1",
  "CA-NS2",
  "CA-NS3",
  "CA-NS4",
  "CA-NS5",
  "CA-Qc2",
  "CA-Qfo",
  "CA-SCC",
  "CA-SF1",
  "CA-SF2",
  "CA-SMC",
  "CA-Ojp",
  #"CA-SJ2", # removed because harvested just before
  "CA-Na1"
)

validationOutputDir <- "~/repos/BiomeBGC_validation/outputs"

# create an empty data.frame to store the results
results <- data.frame(
  towerName = character(),
  estimate = character(),
  nppRMSE = numeric(),
  nppRelBias = numeric(),
  nppR2 = numeric()
)
# loop through each tower and calculate the metrics
for (tower in towerName) {
  dataPath <- file.path(validationOutputDir, tower, "BiomeBGC_validationFluxTower", "validationSummary.csv")
  validationStats <- fread(dataPath)

  # extract R2, RMSE, and relative bias for GPP and RECO at the monthly scale
  out <- validationStats[estimate %in% c("RECO", "GPP") & timescale == "month", 
  .(estimate, R2, RMSE, Bias_perc)]

  out[, towerName := tower]
  results <- rbind(results, out)
}

p2a <- plotValidationMetrics(results, estimateName = "GPP", metric = "R2") + ggtitle("(a)")
p2b <- plotValidationMetrics(results, estimateName = "GPP", metric = "RMSE") + ggtitle("(b)")
p2c <- plotValidationMetrics(results, estimateName = "GPP", metric = "Bias_perc") + ggtitle("(c)")
p2d <- plotValidationMetrics(results, estimateName = "RECO", metric = "R2") + ggtitle("(d)")
p2e <- plotValidationMetrics(results, estimateName = "RECO", metric = "RMSE") + ggtitle("(e)")
p2f <- plotValidationMetrics(results, estimateName = "RECO", metric = "Bias_perc") + ggtitle("(f)")

rowlabel_1 <- wrap_elements(panel = text_grob("GPP", rot = 90, just = "center"))
rowlabel_2 <- wrap_elements(panel = text_grob("RECO", rot = 90, just = "center"))

rowlabel_1 + p2a + p2b + p2c + rowlabel_2 + p2d + p2e + p2f +
  plot_layout(ncol = 4, nrow = 2, axes = "collect_y", widths = c(0.1, 1, 1, 1)) &
  theme(plot.title.position = "plot", 
        axis.text = element_blank(),
        axis.ticks = element_blank())

ggsave("figures/validationMetrics.png", width = 10, height = 6, dpi = 300)

# Alternative figure 2: Show correlation of annual GPP, RECO, and NEE
# create an empty data.frame to store the results
results <- data.frame(
  towerName = character(),
  estimate = character(),
  towerMean = numeric(),
  BGCMean = numeric()
)
# loop through each tower and calculate the metrics
for (tower in towerName) {
  for (estimate in c("GPP", "NEE", "RECO")){
    dataPath <- file.path(validationOutputDir, tower, "BiomeBGC_validationFluxTower", estimate, "annualComparison.csv")
    annualAverages <- fread(dataPath)
    if(nrow(annualAverages) > 0){
    results <- rbind(results,
      data.frame(
        towerName = tower,
        estimate = estimate, 
        tower = annualAverages$fluxTower,
        BBGC = annualAverages$BBGC
      )
    )
    }
  }
}

library(car)
library(MuMIn)
# Model 1: Observed GPP vs predicted GPP
m1_dat <- results[results$estimate == "GPP",]
m1_dat$towerName <- as.factor(m1_dat$towerName)
m1_dat$pred_siteMean <- ave(m1_dat$BBGC, m1_dat$towerName, FUN = mean)
m1_dat$pred_anomaly <- m1_dat$BBGC - m1_dat$pred_siteMean
m1_dat$pred_siteMean_c <- m1_dat$pred_siteMean - mean(m1_dat$pred_siteMean)

m1 <- lmer(tower ~ pred_siteMean_c + pred_anomaly + (1 | towerName) + (0 + pred_anomaly | towerName), data = m1_dat, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(m1)
linearHypothesis(m1, "pred_siteMean_c = 1")
linearHypothesis(m1, "pred_anomaly = 1")
r.squaredGLMM(m1)

# Model 2: Observed RECO vs predicted RECO
m2_dat <- results[results$estimate == "RECO",]
m2_dat$towerName <- as.factor(m2_dat$towerName)
m2_dat$pred_siteMean <- ave(m2_dat$BBGC, m2_dat$towerName, FUN = mean)
m2_dat$pred_anomaly <- m2_dat$BBGC - m2_dat$pred_siteMean
m2_dat$pred_siteMean_c <- m2_dat$pred_siteMean - mean(m2_dat$pred_siteMean)

m2 <- lmer(tower ~ pred_siteMean_c + pred_anomaly + (1 | towerName), data = m2_dat)
summary(m2)
linearHypothesis(m2, "pred_siteMean_c = 1")
linearHypothesis(m2, "pred_anomaly = 1")
r.squaredGLMM(m2)
