# Code to generate figure 4: Maps of GPP and NPP for the present and future (RCP 4.5 and RCP 8.5) scenarios
# a) Map of GPP (10-year average 2001-2010)
# b) Map of GPP RCP 4.5 (10-year average 2090-2100)
# c) Map of GPP RCP 8.5 (10-year average 2090-2100)
# d) Map of NPP (10-year average 2001-2010)
# e) Map of NPP RCP 4.5 (10-year average 2090-2100)
# f) Map of NPP RCP 8.5 (10-year average 2090-2100)
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
npp_rcp45 <- rast("data/processed/NPP_RCP45.tif")
npp_rcp85 <- rast("data/processed/NPP_RCP85.tif")

# Map Each
fig4a <- plotNPP(npp_present)
fig4b <- plotNPP(npp_rcp45)
fig4c <- plotNPP(npp_rcp85)

npp_plots <- (fig4a + ggtitle("(a) 2011-2020")) /
  (fig4b+ ggtitle("(b) 2091-2100: RCP 4.5")) /
  (fig4c + ggtitle("(c) 2091-2100: RCP 8.5")) +
  plot_layout(guides = 'collect')


# Map differences
fig4d <- plotNPPchange(present = npp_present, future = npp_rcp45)
fig4f <- plotNPPchange(present = npp_present, future = npp_rcp85)

npp_change_plots <- (plot_spacer()) /
  (fig4d + ggtitle("(d) Difference: RCP 4.5")) /
  (fig4f + ggtitle("(f) Difference: RCP 8.5")) +
  plot_layout(guides = 'collect', heights = c(1, 1, 1)) 

(npp_plots | npp_change_plots) &
  theme(legend.position = 'bottom', 
        legend.key.width = unit(25, "pt"),
        plot.title.position = "panel",
        legend.title.position = "top",
        axis.text = element_blank(),
        axis.ticks = element_blank())
ggsave("figures/NPPresults.png", scale = 1)
