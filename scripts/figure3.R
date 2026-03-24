# Code to create figure 3:
# 6 panels:
# a) Map of mean temperature (10-year average 2000-2010).
# b) Map of mean annual precipitation (10-year average 2000-2010).
# c) Map of mean temperature (10-year average 2090-2100). RCP4.5, average 3 scenarios
# d) Map of mean annual precipitation (10-year average 2090-2100). RCP4.5, average 3 scenarios
# e) Map of mean temperature (10-year average 2090-2100). RCP8.5, average 3 scenarios
# f) Map of mean annual precipitation (10-year average 2090-2100). RCP8.5, average 3 scenarios

# Get package and utils we need
source("fcnt4analysis.R")
library(reproducible)
library(sf)
library(rnaturalearth)
library(ggplot2)
library(patchwork)
library(ggpubr)

# Aggregate meteo data

# Panel a) and b) maps of the mean temperature and precipitation (10-year average 2000-2010)
f3ab <- mapClimate(
  out,
  eco_boreal,
  yearRange = 2000:2010,
  scenario = "all",
  model = "all"
)

# Panel c) and d) maps of the mean temperature and precipitation under RCP4.5 (10-year average 2090-2100)
f3cd <- mapClimate(
  out,
  eco_boreal,
  yearRange = 2090:2100,
  scenario = "RCP45",
  model = "all"
)

# Panel e) and f) maps of the mean temperature and precipitation under RCP4.5 (10-year average 2090-2100)
f3ef <- mapClimate(
  out,
  eco_boreal,
  yearRange = 2090:2100,
  scenario = "RCP85",
  model = "all"
)

rowlabel_1 <- wrap_elements(panel = text_grob("Present", rot = 90, just = "right"))
rowlabel_2 <- wrap_elements(panel = text_grob("RCP 4.5", rot = 90, just = "right"))
rowlabel_3 <- wrap_elements(panel = text_grob("RCP 8.5", rot = 90, just = "right"))
col_label_1 <- wrap_elements(panel = text_grob("Minimum temperature"))
col_label_2 <- wrap_elements(panel = text_grob("Annual precipitation"))

temperature_plot <- col_label_1 /
  (f3ab[[1]] + ggtitle("(a)")) /
  (f3cd[[1]] + ggtitle("(c)")) /
  (f3ef[[1]] + ggtitle("(e)")) +
  plot_layout(guides = 'collect', heights = c(0.1, 1, 1, 1)) &
  theme(legend.position = 'bottom', 
        legend.key.width = unit(25, "pt"),
        plot.title.position = "plot", 
        legend.title.position = "top",
        axis.text = element_blank(),
        axis.ticks = element_blank())

precipitation_plot <- col_label_2 / 
  (f3ab[[2]] + ggtitle("(b)")) /
  (f3cd[[2]] + ggtitle("(d)")) /
  (f3ef[[2]] + ggtitle("(f)")) +
  plot_layout(guides = 'collect', heights = c(0.1, 1, 1, 1)) &
  theme(legend.position = 'bottom',
        legend.key.width = unit(25, "pt"),
        plot.title.position = "plot",
        legend.title.position = "top",
        axis.text = element_blank(),
        axis.ticks = element_blank())


rowLabels <- rowlabel_1 / rowlabel_2 / rowlabel_3


(rowLabels | temperature_plot | precipitation_plot) + plot_layout(width = c(0.1, 1, 1))
ggsave("climatePlot.png")
