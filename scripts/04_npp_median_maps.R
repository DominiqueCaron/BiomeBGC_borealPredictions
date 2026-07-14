# Code to generate figure 4: Maps of GPP and NPP for the present and future (RCP 4.5 and RCP 8.5) scenarios
# a) Map of NPP (20-year average 2001-2010)
# b) Map of % change in NPP RCP 4.5 (20-year average 2081-2100)
# c) Map of % change in NPP RCP 8.5 (20-year average 2081-2100)
source("scripts/fcnt4analysis.R")
library(reproducible)
library(data.table)
library(terra)
library(rnaturalearth)
library(ggplot2)
library(tidyterra)
library(patchwork)

# Load processed NPP data
npp_present <- rast("data/processed/NPP_present.tif")
rcp45lyrs <- grepl("RCP45", names(npp_present))
npp_rcp45 <- rast("data/processed/NPP_RCP45.tif")
npp_rcp85 <- rast("data/processed/NPP_RCP85.tif")

# Map Each
fig4a <- plotNPP(npp_present)
fig4b <- plotNPPchange(present = npp_present[[rcp45lyrs]], future = npp_rcp45, type = "relative")
fig4c <- plotNPPchange(present = npp_present[[!rcp45lyrs]], future = npp_rcp85, type = "relative")

npp_plots <- ((fig4a + ggtitle("(a) 2001-2030")) |
                (fig4b+ ggtitle("(b) RCP 4.5")) |
                (fig4c + ggtitle("(c) RCP 8.5"))) & theme(legend.position = "bottom", legend.title.position = "top", legend.key.height = unit(8, "pt"))


ggsave("figures/NPPresults.png", scale = 1, width= 9, height = 3)
