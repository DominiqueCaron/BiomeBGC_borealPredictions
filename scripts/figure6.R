# Code to create figure 6:
# 3 panels:
# a) Map of limiting factors present (10-year average 2000-2010).
# b) Map of limiting factors RCP 4.5 (10-year average 2000-2010). RCP4.5, average 3 scenarios
# c) Map of limiting factors RCP 8.5 (10-year average 2090-2100). RCP8.5, average 3 scenarios
source("fcnt4analysis.R")
library(reproducible)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(patchwork)
library(ggpubr)
library(ggtern)
library(tricolore)

fig6a <- mapClimaticControl(out, eco_boreal, 2001:2010) + ggtitle("(a) Present") + theme(axis.text = element_blank(), axis.ticks = element_blank())
fig6b <- mapClimaticControl(out, eco_boreal, 2091:2100, scenario = "RCP45") + ggtitle("(b) RCP 4.5") + theme(axis.text = element_blank(), axis.ticks = element_blank())
fig6c <- mapClimaticControl(out, eco_boreal, 2091:2100, scenario = "RCP85") + ggtitle("(c) RCP 8.5") + theme(axis.text = element_blank(), axis.ticks = element_blank())

fig6a / fig6b / fig6c
ggsave("climaticControls.png", scale = 1)
