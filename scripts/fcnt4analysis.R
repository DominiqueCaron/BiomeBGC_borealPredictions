iVPD <- function(VPD, VPDmin = 750, VPDmax = 4000) {
  ivpd <- as.integer(VPD <= VPDmin)
  to_calc <- VPD < VPDmax & VPD > VPDmin
  num <- VPD[to_calc] - VPDmin
  denom <- VPDmax - VPDmin
  ivpd[to_calc] <- 1 - (num / denom)
  return(ivpd)
}

iTmin <- function(Tmin, TminMin = -2, TminMax = 5) {
  itmin <- as.integer(Tmin >= TminMax)
  to_calc <- Tmin < TminMax & Tmin > TminMin
  num <- Tmin[to_calc] - TminMin
  denom <- TminMax - TminMin
  itmin[to_calc] <- (num / denom)
  return(itmin)
}

iPhoto <- function(Photo, PhotoMin = 10, PhotoMax = 11) {
  iphoto <- as.integer(Photo >= PhotoMax)
  to_calc <- Photo < PhotoMax & Photo > PhotoMin
  num <- Photo[to_calc] - PhotoMin
  denom <- PhotoMax - PhotoMin
  iphoto[to_calc] <- (num / denom)
  return(iphoto)
}

metRead <- function(fileName, nHeaderLines = 4) {
  # Always the same 9 variables
  colNames <- c(
    "year",
    "yday",
    "tmax",
    "tmin",
    "tday",
    "prcp",
    "vpd",
    "srad",
    "daylen"
  )

  # Read data, skip header
  metData <- read.table(fileName, skip = nHeaderLines, col.names = colNames)

  return(metData)
}


mapClimate <- function(
  dt,
  polygons,
  yearRange,
  scenario = "all",
  model = "all"
) {
  # Only keep the rows of the model and scenario we want to plot
  if (model != "all") {
    rowToKeep <- dt$model == model
    dt <- dt[rowToKeep, ]
  }
  if (scenario != "all") {
    rowToKeep <- dt$scenario == scenario
    dt <- dt[rowToKeep, ]
  }
  # Keep years we want to plot
  dt <- dt[year %in% yearRange, ]
  
  dt <- dt[,
    .(tmin = mean(tmin), prcp = mean(prcp)),
    by = .(ecodistrict)
  ]

  # get data for a base map
  canada <- ne_countries(country = "canada", scale = 10) |>
    sf::st_transform(crs(polygons))
  usa <- ne_countries(country = "United States of America", scale = 10) |>
    sf::st_transform(crs(polygons))

  # some setup
  xlims <- c(-2339839, 3010580)
  ylims <- c(-725354.2, 3000000)

  basemap <- ggplot() +
    geom_sf(data = canada, fill = "white") +
    geom_sf(data = usa, fill = "white") +
    theme(
      panel.background = element_rect(fill = "lightblue1"),
      panel.grid = element_line(color = "#00000010")
    )

  # Map tmin
  sfObj <- merge(
    polygons,
    dt,
    by.x = "ECODISTRIC",
    by.y = "ecodistrict"
  )
  tmin_plot <- basemap +
    geom_sf(data = sfObj, aes(fill = tmin, colour = tmin)) +
    coord_sf(xlim = xlims, ylim = ylims, expand = FALSE) +
    scale_fill_distiller(
      name = NULL,
      palette = "RdYlBu",
      limits = c(-20, 10),
      breaks = c(-20, -10, 0, 10),
      labels = c("-20°C", "-10°C", "0°C", "10°C")
    ) +
    scale_colour_distiller(
     name = NULL,
      palette = "RdYlBu",
      limits = c(-20, 10),
      breaks = c(-20, -10, 0, 10),
      labels = c("-20°C", "-10°C", "0°C", "10°C")
    )
  prcp_plot <- basemap +
    geom_sf(data = sfObj, aes(fill = prcp, colour = prcp)) +
    coord_sf(xlim = xlims, ylim = ylims, expand = FALSE) +
    scale_fill_distiller(
      name = NULL,
      palette = "YlGnBu",
      direction = 1,
      limits = c(0, 100),
      breaks = c(0, 25, 50, 75, 100),
      labels = c("0cm", "25cm", "50cm", "75cm", "100cm")
    ) +
    scale_colour_distiller(
      name = NULL,
      palette = "YlGnBu",
      direction = 1,
      limits = c(0, 100),
      breaks = c(0, 25, 50, 75, 100),
      labels = c("0cm", "25cm", "50cm", "75cm", "100cm")
    )
  return(list(tmin = tmin_plot, prcp = prcp_plot))
}


### CREATE FUNCTION TO PLOT CLIMATE DIFFERENCE BETWEEN TWO TIME STEP
#   # Map the difference
#   dt_wide <- dcast(dt, ecodistrict ~ year, value.var = c("prcp", "tmin"))
#   dt_wide$tmin_dif <- dt_wide$`tmin_2090-2100` - dt_wide$`tmin_2000-2010`
#   dt_wide$prcp_dif <- dt_wide$`prcp_2090-2100` - dt_wide$`prcp_2000-2010`
#   sfObj <- merge(eco_boreal, dt_wide, by.x = "ECODISTRIC", by.y = "ecodistrict")
#   tmin_diff <- basemap +
#     geom_sf(data = sfObj, aes(fill = tmin_dif, colour = tmin_dif)) +
#     coord_sf(xlim = xlims, ylim = ylims, expand = FALSE) +
#     scale_fill_distiller(
#       "Difference in minimum temperature",
#       palette = "RdBu",
#       limits = c(-10, 10)
#     ) +
#     scale_colour_distiller(
#       "Difference in minimum temperature",
#       palette = "RdBu",
#       limits = c(-10, 10)
#     )
#   prcp_diff <- basemap +
#     geom_sf(data = sfObj, aes(fill = prcp_dif, colour = prcp_dif)) +
#     coord_sf(xlim = xlims, ylim = ylims, expand = FALSE) +
#     scale_fill_distiller(
#       "Difference in annual precipitation",
#       palette = "BrBG",
#       direction = 1,
#       limits = c(-20, 20)
#     ) +
#     scale_colour_distiller(
#       "Difference in annual precipitation",
#       palette = "BrBG",
#       direction = 1,
#       limits = c(-20, 20)
#     )

#   return(list(tmin_t1, tmin_t2, tmin_diff, prcp_t1, prcp_t2, prcp_diff))
# }

mapClimaticControl <- function(
  dt,
  polygons,
  yearRange,
  scenario = "all",
  model = "all"
) {
  if (model != "all") {
    rowToKeep <- dt$model == model
    dt <- dt[rowToKeep, ]
  }
  if (scenario != "all") {
    rowToKeep <- dt$scenario == scenario
    dt <- dt[rowToKeep, ]
  }
  # Keep years we want to plot
  dt <- dt[year %in% yearRange, ]
  dt <- dt[,
    .(
      ivpd = (365 - mean(ivpd)) / 365,
      itmin = (365 - mean(itmin)) / 365,
      iphoto = (365 - mean(iphoto)) / 365
    ),
    by = .(ecodistrict)
  ]

  # get data for a base map
  canada <- ne_countries(country = "canada", scale = 10) |>
    sf::st_transform(crs(eco_boreal))
  usa <- ne_countries(country = "United States of America", scale = 10) |>
    sf::st_transform(crs(eco_boreal))

  # some setup
  xlims <- c(-2339839, 56e5)
  ylims <- c(-725354.2, 3000000)

  basemap <- ggplot() +
    geom_sf(data = canada, fill = "white") +
    geom_sf(data = usa, fill = "white") +
    theme(
      panel.background = element_rect(fill = "lightblue1"),
      panel.grid = element_line(color = "#00000010")
    )

  # Map the 2000-2010 range
  sfObj <- merge(
    polygons,
    dt,
    by.x = "ECODISTRIC",
    by.y = "ecodistrict"
  )
  # generate a color key
  tric <- Tricolore(sfObj, "iphoto", "itmin", "ivpd", breaks = Inf, show_data = FALSE)
  sfObj$rgb <- tric$rgb

  # map
  map_climControl <- basemap +
    geom_sf(data = sfObj, aes(fill = rgb, colour = rgb)) +
    coord_sf(xlim = xlims, ylim = ylims, expand = FALSE) +
    scale_fill_identity("") +
    scale_colour_identity("")

  # add ternary plot
 legendGrob <- ggplotGrob(
    tric$key +
      geom_point(data = sfObj, aes(iphoto, itmin, ivpd), size = 0.3) +
      labs(
        x = "Light\nlimited",
        y = "Temp.\nlimited",       # shorter label reduces clipping risk
        z = "Water\nlimited"
      ) +
      theme_transparent() +
      theme(
        # Axis titles — make them larger
        tern.axis.title.L = element_text(size = 8),
        tern.axis.title.R = element_text(size = 8),
        tern.axis.title.T = element_text(size = 8),
        tern.axis.text.L  = element_blank(),
        tern.axis.text.R  = element_blank(),
        tern.axis.text.T  = element_blank(),
        tern.axis.arrow.L = element_blank(),
        tern.axis.arrow.R = element_blank(),
        tern.axis.arrow.T = element_blank(),
        tern.axis.ticks.major.L = element_blank(),
        tern.axis.ticks.major.R = element_blank(),
        tern.axis.ticks.major.T = element_blank(),
        tern.axis.ticks.minor.L = element_blank(),
        tern.axis.ticks.minor.R = element_blank(),
        tern.axis.ticks.minor.T = element_blank(),
        tern.panel.expand = 0.7
      )
  )

  # Disable clipping so labels outside the grob boundary are not cut off
  legendGrob$layout$clip[legendGrob$layout$name=="panel"] <- "off"

  mapOut <- map_climControl +
    annotation_custom(
      legendGrob,
      xmin = 22e5, xmax = 60e5,
      ymin = -10e5, ymax = 30e5   
    )

  return(mapOut)
}

 
combineResults <- function(vars, ecoregions, outputPath, yearRange, model, scenario, summarize = FALSE) {

  if (length(model) > 1 | length(scenario) > 1) {
    # Create a df of model and scenario combinations to loop through
    runs <- expand.grid(
      model = model,
      scenario = scenario
    )

    # Prepare an empty list to store rasters for each run
    runOut <- list()
    for (i in 1:nrow(runs)) {
      message(
        "Processing model ",
        runs$model[i],
        " and scenario ",
        runs$scenario[i]
      )
      runOut[[i]] <- combineResults(
        vars = vars,
        ecoregions = ecoregions,
        outputPath = outputPath,
        yearRange = yearRange,
        model = runs$model[i],
        scenario = runs$scenario[i]
      )
    }

    # align to a template if geometries differ
    template <- runOut[[1]]
    aligned <- lapply(runOut, function(r) {
      if (!terra::compareGeom(r, template, stopOnError = FALSE)) {
        terra::resample(r, template)
      } else {
        r
      }
    })

    # make a single SpatRaster (stack)
    outRaster <- terra::rast(aligned)

    # If summarize is TRUE, calculate the mean across the stack
    if (summarize) {
      outRaster <- terra::app(outRaster, mean)
      names(outRaster) <- paste0(vars, "_", "mean")
    }
  } else {
    raster_list <- list()
    for (j in 1:length(ecoregions)) {
      iecoregion <- ecoregions[j]
      # define the path were the outputs are
      folderPath <- file.path(outputPath, iecoregion, scenario, model)

      # check if there are data
      if (length(list.files(folderPath)) != 0) {
        # get the raster
        pixelGroupMap <- rast(file.path(folderPath, "pixelGroupMap.tif"))

        # get the data
        annualAverages <- qs2::qs_read(file.path(
          folderPath,
          "annualAverages.qs"
        ))

        # filter years to keep the range
        yearRangeAvgs <- annualAverages[year %in% yearRange]

        # calculate across-year average
        yearRangeAvgs <- yearRangeAvgs[,
          .(value = mean(get(vars))),
          by = pixelGroup
        ]

        # Create a lookup vector to switch pixelgroup to the variable of interest
        max_id <- max(yearRangeAvgs$pixelGroup)
        lookup <- rep(NA_real_, max_id)
        lookup[yearRangeAvgs$pixelGroup] <- yearRangeAvgs$value

        # apply lookup
        rast_value <- app(pixelGroupMap, function(x) lookup[x])

        raster_list[[j]] <- rast_value
      }
    }
    # remove NULL rasters
    raster_list <- raster_list[-which(sapply(raster_list, is.null))]

    # combine the rasters
    outRaster <- mosaic(sprc(raster_list))

    # add metadata to the raster
    names(outRaster) <- paste0(vars, "_", model, "_", scenario)
  }

  return(outRaster)
}
