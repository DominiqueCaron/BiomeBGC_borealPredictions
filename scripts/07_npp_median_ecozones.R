library(terra)
library(reproducible)
library(exactextractr)
library(ggplot2)
library(data.table)
library(dplyr)
library(rnaturalearth)
library(sf)
library(cowplot)

# Load processed NPP data
npp_present <- rast("data/processed/NPP_present.tif")
npp_rcp45 <- rast("data/processed/NPP_RCP45.tif")
npp_rcp85 <- rast("data/processed/NPP_RCP85.tif")

#
ecozones <- reproducible::prepInputs(
  url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
  targetFile = "ecozones.shp",
  destinationPath = "inputs",
  fun = "sf::st_read",
  cropTo = npp_present,
  projectTo = npp_present
)
ecozones <- aggregate(ecozones, by = list(ZONE_NAME = ecozones$ZONE_NAME), FUN = mean)
ecozones$ZONE_NAME[ecozones$ZONE_NAME == "Boreal PLain"] <- "Boreal Plain"
N_pixels <- exact_extract(npp_present[[1]], ecozones, fun = c('count'),
                          append_cols = 'ZONE_NAME')

npp_present_df <- exact_extract(mean(npp_present), ecozones,
                                fun = "quantile",
                                quantiles = c(0.25, 0.50, 0.75),
                                append_cols = 'ZONE_NAME')

npp_rcp45_df <- exact_extract(mean(npp_rcp45), ecozones,
                              fun = "quantile",
                              quantiles = c(0.25, 0.50, 0.75),
                              append_cols = 'ZONE_NAME')

npp_rcp85_df <- exact_extract(mean(npp_rcp85), ecozones,
                              fun = "quantile",
                              quantiles = c(0.25, 0.50, 0.75),
                              append_cols = 'ZONE_NAME')

results <- data.frame(ecozone = rep(N_pixels$ZONE_NAME, 3),
                      nPix = rep(N_pixels$count, 3),
                      scenario = rep(c("present", "rcp45", "rcp85"), each = 15),
                      npp_median = c(npp_present_df$q50, npp_rcp45_df$q50, npp_rcp85_df$q50) * 365 * 1000,
                      npp_lower = c(npp_present_df$q25, npp_rcp45_df$q25, npp_rcp85_df$q25) * 365 * 1000,
                      npp_upper = c(npp_present_df$q75, npp_rcp45_df$q75, npp_rcp85_df$q75) * 365 * 1000
)

results <- results[results$nPix > 8* 10^5,]

# Create a map of the ecozone
# get data for a base map
canada <- ne_countries(country = "canada", scale = 10) |>
  sf::st_transform(crs(ecozones))
usa <- ne_countries(country = "United States of America", scale = 10) |>
  sf::st_transform(crs(ecozones))

# some setup
xlims <- c(-2339839, 3010580)
ylims <- c(-200000, 3000000)

basemap <- ggplot() +
  geom_sf(data = canada, fill = "white") +
  geom_sf(data = usa, fill = "white") +
  theme(
    panel.background = element_rect(fill = "lightblue1"),
    panel.grid = element_line(color = "#00000010")
  )

# Add IDs
ecozones <- ecozones %>%
  filter(ZONE_NAME %in% results$ecozone) %>%
  arrange(ZONE_NAME) %>%
  mutate(ID = 1:n())

# Representative point for labels
# st_point_on_surface guarantees the point is inside the polygon
label_pts <- st_point_on_surface(ecozones)

map <- basemap +
  geom_sf(data = ecozones,
          aes(fill = ZONE_NAME),
          alpha = 0.4,
          colour = "black",
          linewidth = 0.3) +
  geom_sf_text(data = label_pts,
               aes(label = ID),
               size = 4,
               fontface = "bold") +
  coord_sf(xlim = xlims, ylim = ylims, expand = FALSE) +
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position = "none", axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_rect(color = "black"))

# Create the bar plot
results$ecozone[results$ecozone == "Boreal PLain"] <- "Boreal Plain"
eczns <- unique(results$ecozone)
ecozone_labs <- paste0("(", 1:length(eczns), ") ", eczns)
results$ecozone <- factor(results$ecozone, levels = eczns, labels = ecozone_labs)
results$scenario <- factor(results$scenario, levels = c("present", "rcp45", "rcp85"), labels = c("2001-2030", "RCP 4.5", "RCP 8.5"))


fig6 <- ggplot(results) +
  geom_col(
    aes(x = ecozone, y = npp_median, group = scenario, fill = scenario),
    color = "grey10",
    position = position_dodge()
  ) +
  geom_linerange(
    aes(
      x = ecozone,
      ymin = npp_lower,
      ymax = npp_upper,
      group = scenario
    ),
    position = position_dodge(width = 0.9)
  ) +
  scale_y_continuous(limits = c(0,900)) +
  labs(x = "Ecozone", y = "NPP (gC/m²/yr)", fill = NULL) +
  scale_fill_brewer(type = "qual", palette = "Oranges") +
  theme_classic()

ggdraw(fig6) +
  draw_plot(
    map,
    x = 0.54,     # left position (0-1)
    y = 0.65,     # bottom position (0-1)
    width = 0.35, # fraction of figure width
    height = 0.35 # fraction of figure height
  )

ggsave("figures/spatialResults.png")
