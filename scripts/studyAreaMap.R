# Create a map of the boreal forest showing leading species (SCANFI 2020)

library(sf)
library(ggplot2)
library(terra)
library(tidyterra)
library(ggspatial)
library(rnaturalearth)
library(ggnewscale)
library(ggrepel)
library(data.table)
ecoregions <- reproducible::prepInputs(
  url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/region/ecoregion_shp.zip",
  targetFile = "ecoregions.shp",
  destinationPath = "inputs",
  fun = "sf::st_read"
)

borealForest <- reproducible::prepInputs(
  url = "https://d278fo2rk9arr5.cloudfront.net/downloads/boreal.zip",
  targetFile = "NABoreal.shp",
  destinationPath = "inputs",
  fun = "sf::st_read"
)

ecoregions <- st_transform(ecoregions, 3978)
borealForest <- st_transform(borealForest, 3978)

ecoregions <- st_make_valid(ecoregions)
borealForest <- st_make_valid(borealForest)

borealForest <- st_union(borealForest[
  borealForest$TYPE == "BOREAL" | borealForest$TYPE == "B_ALPINE",
])

# use this to loop across ecoregions
eco_boreal <- st_intersection(ecoregions, borealForest)

studyArea <- eco_boreal |> st_union() |> st_simplify(dTolerance = 5e3)
rtm <- terra::rast(terra::vect(studyArea), res = c(250, 250))
terra::crs(rtm) <- terra::crs(terra::vect(studyArea))
rtm[] <- 1
rtm <- terra::mask(rtm, terra::vect(studyArea))

# dominant species
dominantSpecies <- reproducible::prepInputs(
  url = "https://opendata.nfis.org/downloads/forest_change/CA_Tree_Species_Classification_2020.zip",
  destinationPath = "~/inputs"
)
dominantSpecies <- reproducible::cropTo(dominantSpecies, rtm) |>
  reproducible::Cache(cachePath = normalizePath("cache"))
dominantSpecies <- reproducible::projectTo(
  dominantSpecies,
  rtm,
  method = "near"
) |>
  reproducible::Cache(cachePath = normalizePath("cache"))
dominantSpecies <- dominantSpecies * rtm
dominantSpecies <- subst(dominantSpecies, 0, NA)
dominantSpecies <- subst(dominantSpecies, c(5, 6, 9, 27, 28), 1001)
dominantSpecies <- subst(dominantSpecies, c(13, 16, 26, 35, 36), 2001)

dominantSpecies <- as.factor(dominantSpecies)
cls <- data.frame(
  id = c(LandR::sppEquivalencies_CA$NTEMS_Species_Code, 1001, 2001),
  c(
    LandR::sppEquivalencies_CA$EN_generic_full,
    "Other broadleaf",
    "Other needleleaf"
  )
) |>
  na.omit() |>
  data.table::as.data.table() |>
  unique(by = "id")
levels(dominantSpecies) <- cls


# towers
lat <- c(
  48.2167,
  68.3203,
  55.1119,
  55.880,
  55.8792,
  55.9058,
  55.9117,
  55.9144,
  55.8631,
  49.7598,
  49.6925,
  61.3079,
  54.4850,
  54.2539,
  63.1534
)
lon <- c(
  -82.1556,
  -133.5188,
  -122.8414,
  -98.481,
  -98.4839,
  -98.5247,
  -98.3822,
  -98.3806,
  -98.4850,
  -74.5711,
  -74.3421,
  -121.2992,
  -105.8176,
  -105.8775,
  -123.2522
)
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

towers <- data.frame(lat, lon, towerName, shape = "Flux tower")
towers <- st_as_sf(x = towers, coords = c("lon", "lat"), crs = 4326)
towers <- st_transform(towers, 3978)

# Canada
canada_map <- ne_countries(country = "canada", scale = 10) |>
  sf::st_transform(crs(eco_boreal))
# Usa
usa_map <- ne_countries(country = "United States of America", scale = 10) |>
  sf::st_transform(crs(eco_boreal))
# Lakes
lakes <- reproducible::prepInputs(
  url = "https://ftp.maps.canada.ca/pub/nrcan_rncan/vector/canvec/shp/Hydro/canvec_15M_CA_Hydro_shp.zip",
  targetFile = "waterbody_2.shp",
  projectTo = rtm,
  destinationPath = "inputs",
)
lakes <- lakes |> st_union()


xlims <- c(-2339839, 3010580)
ylims <- c(-200000, 3000000)

towerLabels <- data.frame(st_coordinates(towers), towerName = towerName)


ggplot() +
  geom_sf(data = usa_map, fill = "grey95", color = NA) +
  geom_sf(data = canada_map, fill = "grey95", color = NA) +
  geom_spatraster(data = dominantSpecies, alpha = 0.95) + # no interpolation
  scale_fill_manual(
    "Dominant species",
    na.translate = FALSE,
    values = c(
      "#A10300",
      "#FF0000",
      "#DD00FF",
      "#93D4FF",
      "#00479E",
      "#C8FF00",
      "#02AD24",
      "#FF00B6",
      "orange",
      "turquoise"
    ),
    guide = guide_legend(
      title.position = "top",
      ncol = 2,
      byrow = TRUE,
      keyheight = unit(8, "pt")
    )
  ) +
  geom_sf(data = studyArea, color = "black", linewidth = 0.2, fill = NA) +
  geom_sf(data = lakes, fill = "lightblue1", color = NA, alpha = 0.6) +
  geom_sf(
    data = towers,
    aes(shape = shape),
    size = 3,
    color = "black",
    fill = "white",
    stroke = 0.8
  ) +
  geom_label_repel(
    data = towerLabels,
    aes(x = X, y = Y, label = towerName),
    size = 3,
    box.padding = 0.8,
    point.padding = 0.6,
    segment.color = "black",
    segment.size = 0.35,
    min.segment.length = 0,
    direction = "both",
    force = 6,
    max.overlaps = Inf,
    seed = 42
  ) +
  scale_shape_manual(NULL, values = 24) +
  annotation_scale(width_hint = 0.2, text_cex = 1) +
  annotation_north_arrow(
    location = "tr",
    style = north_arrow_fancy_orienteering
  ) +
  coord_sf(xlim = xlims, ylim = ylims, expand = FALSE) +
  guides(
    fill = guide_legend(order = 1, ncol = 2),
    shape = guide_legend(order = 2)
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.background = element_rect(fill = "lightblue1", color = NA),
    panel.grid = element_line(color = "#00000010"),
    legend.key = element_rect(fill = "white", color = NA),
    legend.box.background = element_rect(
      fill = alpha("white", 0.7),
      color = NA
    ),
    legend.position = c(0.9, 0.53), # move legend off the main raster area
    legend.justification = c(1, 0),
    legend.title.align = 0.5,
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

ggsave("figures/studyAreaMap.png", width = 8, height = 10, dpi = 300)
