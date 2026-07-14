# Code to create figure 3:
# 6 panels:
# a) Map of mean temperature (20-year average 2000-2020).
# b) Map of mean annual precipitation (20-year average 2000-2020).
# c) Map of mean vapor pressure deficit (20-year average 2000-2020).
# d) Map of change mean temperature under RCP4.5 (2080-2100 vs 2000-2020).
# e) Map of change mean precipitation under RCP4.5 (2080-2100 vs 2000-2020).
# f) Map of change mean vapor pressure deficit under RCP4.5 (2080-2100 vs 2000-2020).
# g) Map of change mean temperature under RCP4.5 (2080-2100 vs 2000-2020).
# h) Map of change mean precipitation under RCP4.5 (2080-2100 vs 2000-2020).
# i) Map of change mean vapor pressure deficit under RCP4.5 (2080-2100 vs 2000-2020).

# Get package and utils we need
source("scripts/fcnt4analysis.R")
library(reproducible)
library(sf)
library(data.table)
library(rnaturalearth)
library(terra)
library(ggplot2)
library(patchwork)
library(ggpubr)

# Aggregate meteo data
climateData <- fread("data/processed/climate_bioclimatic_indices.csv")
ecod <- unique(climateData[climateData$scenario == "RCP85",]$ecodistrict)
climateData <- climateData[ecodistrict %in% ecod,]

# Panel a-c maps of the mean temperature, precipitation, and vpd (20-year average 2001-2020)
presentClimate <- climateMean(
  climateData,
  eco_boreal,
  yearRange = 2001:2020,
  scenario = "all",
  model = "all"
)

f3a <- climatePlot(
  presentClimate,
  "tmin"
)

# f3b <- climatePlot(
#   presentClimate,
#   "prcp"
# )

f3b <- climatePlot(
  presentClimate,
  "vpd"
)

# Panel d-f maps change of climate under RCP4.5 (20-year average 2081-2100)
rcp45Climate <- climateMean(
  climateData,
  eco_boreal,
  yearRange = 2081:2100,
  scenario = "RCP45",
  model = "all"
)

rcp45ClimateDiff <- rcp45Climate[,("ECODISTRIC")]
rcp45ClimateDiff$tminDiff <- rcp45Climate$tmin - presentClimate$tmin
rcp45ClimateDiff$prcpDiff <- rcp45Climate$prcp - presentClimate$prcp
rcp45ClimateDiff$vpdDiff <- rcp45Climate$vpd - presentClimate$vpd

f3c <- climatePlot(
  rcp45ClimateDiff,
  "tminDiff"
)

# f3e <- climatePlot(
#   rcp45ClimateDiff,
#   "prcpDiff"
# )

f3d <- climatePlot(
  rcp45ClimateDiff,
  "vpdDiff"
)


# Panel g-i maps change of climate under RCP8.5 (20-year average 2081-2100)
rcp85Climate <- climateMean(
  climateData,
  eco_boreal,
  yearRange = 2081:2100,
  scenario = "RCP85",
  model = "all"
)

rcp85ClimateDiff <- rcp85Climate[,("ECODISTRIC")]
rcp85ClimateDiff$tminDiff <- rcp85Climate$tmin - presentClimate$tmin
rcp85ClimateDiff$prcpDiff <- rcp85Climate$prcp - presentClimate$prcp
rcp85ClimateDiff$vpdDiff <- rcp85Climate$vpd - presentClimate$vpd

f3e <- climatePlot(
  rcp85ClimateDiff,
  "tminDiff"
)

# f3h <- climatePlot(
#   rcp85ClimateDiff,
#   "prcpDiff"
# )

f3f <- climatePlot(
  rcp85ClimateDiff,
  "vpdDiff"
)

rowlabel_1 <- wrap_elements(panel = text_grob("Present", rot = 90, just = "right"))
rowlabel_2 <- wrap_elements(panel = text_grob("RCP 4.5", rot = 90, just = "right"))
rowlabel_3 <- wrap_elements(panel = text_grob("RCP 8.5", rot = 90, just = "right"))
col_label_1 <- wrap_elements(panel = text_grob("Minimum temperature"))
col_label_2 <- wrap_elements(panel = text_grob("Vapor pressure deficit"))

temperature_plot <- col_label_1 /
  (f3a + ggtitle("(a)")) /
  (f3c + ggtitle("(c)")) /
  (f3e + ggtitle("(e)")) +
  plot_layout(heights = c(0.1, 1, 1, 1)) &
  theme(legend.position = 'right',
        plot.title.position = "plot",
        legend.title.position = "top",
        axis.text = element_blank(),
        axis.ticks = element_blank())

vpd_plot <- col_label_2 /
  (f3b + ggtitle("(b)")) /
  (f3d + ggtitle("(d)")) /
  (f3f + ggtitle("(f)")) +
  plot_layout(heights = c(0.1, 1, 1, 1)) &
  theme(legend.position = 'right',
        plot.title.position = "plot",
        legend.title.position = "top",
        axis.text = element_blank(),
        axis.ticks = element_blank())


rowLabels <- rowlabel_1 / rowlabel_2 / rowlabel_3

(rowLabels | temperature_plot | vpd_plot) + plot_layout(width = c(0.1, 1, 1))
ggsave("figures/climatePlot.png", width = 10, height = 10)
