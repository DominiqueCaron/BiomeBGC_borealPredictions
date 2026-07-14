# Code to generate figure 4: Maps of GPP and NPP standard deviation for the present and future (RCP 4.5 and RCP 8.5) scenarios
# a) Map of NPP standard deviation (20-year average 2001-2010)
# b) Map of % change in NPP standard deviation RCP 4.5 (20-year average 2081-2100)
# c) Map of % change in NPP standard deviation RCP 8.5 (20-year average 2081-2100)
source("scripts/fcnt4analysis.R")
library(reproducible)
library(data.table)
library(terra)
library(rnaturalearth)
library(ggplot2)
library(tidyterra)
library(patchwork)

# Load processed NPP data
npp_sd_present <- rast("data/processed/NPP_present_sd.tif")
rcp45lyrs <- grepl("RCP45", names(npp_sd_present))
npp_sd_rcp45 <- rast("data/processed/NPP_RCP45_sd.tif")
npp_sd_rcp85 <- rast("data/processed/NPP_RCP85_sd.tif")

# Map Each
fig5a <- plotNPP(npp_sd_present, stat = "sd")
fig5b <- plotNPPchange(present = npp_sd_present[[rcp45lyrs]], future = npp_sd_rcp45, type = "relative", stat = "sd")
fig5c <- plotNPPchange(present = npp_sd_present[[!rcp45lyrs]], future = npp_sd_rcp85, type = "relative", stat = "sd")

npp_sd_plots <- ((fig5a + ggtitle("(a) 2001-2030")) |
                   (fig5b+ ggtitle("(b) RCP 4.5")) |
                   (fig5c + ggtitle("(c) RCP 8.5"))) & theme(legend.position = "bottom", legend.title.position = "top", legend.key.height = unit(8, "pt"))

ggsave("figures/NPPVariationResults.png", scale = 1, width= 9, height = 3)
