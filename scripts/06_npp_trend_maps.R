source("scripts/fcnt4analysis.R")
library(reproducible)
library(data.table)
library(terra)
library(rnaturalearth)
library(ggplot2)
library(tidyterra)
library(patchwork)

# Load processed NPP data
npp_present <- rast("data/processed/NPP_present_slope.tif")
rcp45lyrs <- grepl("RCP45", names(npp_present))
npp_rcp45 <- rast("data/processed/NPP_RCP45_slope.tif")
npp_rcp85 <- rast("data/processed/NPP_RCP85_slope.tif")

# Map Each
fig7a <- plotNPP(npp_present, stat = "trend")
fig7b <- plotNPP(npp_rcp45, stat = "trend")
fig7c <- plotNPP(npp_rcp85, stat = "trend")

npp_plots <- ((fig7a + ggtitle("(a) 2001-2030")) |
                (fig7b+ ggtitle("(b) RCP 4.5")) |
                (fig7c + ggtitle("(c) RCP 8.5"))) & theme(legend.position = "bottom", legend.title.position = "top", legend.key.height = unit(8, "pt"))


ggsave("figures/NPPSloperesults.png", scale = 1, width= 9, height = 3)
