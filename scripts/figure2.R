# Figure 2: Plot of validation metrics for each tower
# 6 panels:
# a) R2 for GPP
# b) RMSE for GPP
# c) Relative bias for GPP
# d) R2 for RECO
# e) RMSE for RECO
# f) Relative bias for RECO
library(ggplot2)
library(data.table)
library(patchwork)
library(ggpubr)

towerName <- c(
  "CA-Gro",
  "CA-HPC",
  "CA-LP1",
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
  "CA-SMC"
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

ggsave("figures/validationMetrics.png", scale = 1)
