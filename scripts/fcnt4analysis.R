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


climatePlot <- function(
    sfObj,
    colToPlot
) {
  
  # get data for a base map
  canada <- ne_countries(country = "canada", scale = 10) |>
    sf::st_transform(crs(sfObj))
  usa <- ne_countries(country = "United States of America", scale = 10) |>
    sf::st_transform(crs(sfObj))
  
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
  
  if (colToPlot == "tmin"){
    limits = c(-16, 6)
    breaks = c(-15, -10, -5, 0, 5)
    palette = "RdYlBu"
    direction = -1
    legendName = "Annual average (°C)"
  } else if (colToPlot == "prcp") {
    limits = c(0, 250)
    breaks = c(0, 50, 100, 150, 200, 250)
    palette = "YlGnBu"
    direction = 1
    legendName = "Annual sum (cm)"
  } else if (colToPlot == "vpd"){
    limits = c(0, 600)
    breaks = c(0, 200, 400, 600)
    palette = "YlOrBr"
    direction = 1
    legendName = "Annual average (Pa)"
  } else if (colToPlot == "tminDiff") {
    limits = c(-12, 12)
    breaks = c(-10, -5, 0, 5, 10)
    palette = "RdBu"
    direction = -1
    legendName = "Change (°C)"
  } else if (colToPlot == "prcpDiff") {
    limits = c(-75, 75)
    breaks = c(-50, -25, 0, 25, 50)
    palette = "BrBG"
    direction = 1
    legendName = "Change (cm)"
  } else if (colToPlot == "vpdDiff") {
    limits = c(-450, 450)
    breaks = c(-300, -150, 0, 150, 300)
    palette = "PuOr"
    direction = -1
    legendName = "Change (Pa)"
  }
  
  
  out_plot <- basemap +
    geom_sf(data = sfObj, aes(fill = get(colToPlot), colour = get(colToPlot))) +
    coord_sf(xlim = xlims, ylim = ylims, expand = FALSE) +
    scale_fill_distiller(
      name = legendName,
      palette = palette,
      limits = limits,
      breaks = breaks,
      direction = direction
    ) +
    scale_colour_distiller(
      name = legendName,
      palette = palette,
      limits = limits,
      breaks = breaks,
      direction = direction
    )
  return(out_plot)
}

climateMean <- function(
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
           .(tmin = mean(tmin), prcp = mean(prcp), vpd = mean(vpd)),
           by = .(ecodistrict)
  ]
  
  # Map tmin
  sfObj <- merge(
    polygons,
    dt,
    by.x = "ECODISTRIC",
    by.y = "ecodistrict"
  )
  
  return(sfObj)
}

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
  ylims <- c(-200000, 3000000)
  
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
  tric <- Tricolore(
    sfObj,
    "iphoto",
    "itmin",
    "ivpd",
    breaks = Inf,
    show_data = FALSE
  )
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
        y = "Temp.\nlimited", # shorter label reduces clipping risk
        z = "Water\nlimited"
      ) +
      theme_transparent() +
      theme(
        # Axis titles — make them larger
        tern.axis.title.L = element_text(size = 8),
        tern.axis.title.R = element_text(size = 8),
        tern.axis.title.T = element_text(size = 8),
        tern.axis.text.L = element_blank(),
        tern.axis.text.R = element_blank(),
        tern.axis.text.T = element_blank(),
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
  legendGrob$layout$clip[legendGrob$layout$name == "panel"] <- "off"
  
  mapOut <- map_climControl +
    annotation_custom(
      legendGrob,
      xmin = 22e5,
      xmax = 60e5,
      ymin = -10e5,
      ymax = 30e5
    )
  
  return(mapOut)
}


combineResults <- function(
    vars,
    ecoregions,
    outputPath,
    yearRange,
    model,
    scenario,
    fun = "mean"
) {
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
        scenario = runs$scenario[i],
        fun = fun
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
        if (fun == "sd"){
          yearRangeAvgs <- yearRangeAvgs[,
                                         .(value = sd(get(vars))),
                                         by = pixelGroup
          ]
        } else {
          yearRangeAvgs <- yearRangeAvgs[,
                                         .(value = median(get(vars))),
                                         by = pixelGroup
          ]
        }
        
        
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


getSlopes <- function(
    vars,
    ecoregions,
    outputPath,
    yearRange,
    model,
    scenario
){
  
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
      runOut[[i]] <- getSlopes(
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
        setDT(annualAverages)
        annualAverages <- annualAverages[year %in% yearRange]
        
        setorder(annualAverages, pixelGroup, year)
        slopes <- annualAverages[, .(slope = senSlope(get(vars))), by = pixelGroup]
        
        # Create a lookup vector to switch pixelgroup to the variable of interest
        max_id <- max(slopes$pixelGroup)
        lookup <- rep(NA_real_, max_id)
        lookup[slopes$pixelGroup] <- slopes$slope
        
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

senSlope <- function(x){
  slope <- sens.slope(x)
  if(is.na(slope$p.value) | slope$p.value > 0.05){
    return(0)
  } else {
    return(slope$estimates)
  }
}

plotNPP <- function(nppRaster, stat = "mean") {
  if (nlyr(nppRaster) > 1) {
    nppRaster <- app(nppRaster, mean)
  }
  
  # convert kgC/m2/day to gC/m2/yr
  nppRaster <- nppRaster * 365 * 1000
  
  # get data for a base map
  canada <- ne_countries(country = "canada", scale = 10) |>
    sf::st_transform(crs(nppRaster))
  usa <- ne_countries(country = "United States of America", scale = 10) |>
    sf::st_transform(crs(nppRaster))
  
  # some setup
  xlims <- c(-2339839, 3010580)
  ylims <- c(-200000, 3000000)
  
  lims = c(0, 800)
  brks = c(0, 200, 400, 600, 800)
  labs = c("0", "200", "400", "600", "800")
  legendTitle <- "NPP (gC/m²/yr)"
  pal = "YlGn"
  
  if (stat == "sd"){
    lims = c(0, 150)
    brks = c(0, 50, 100, 150)
    labs = c("0", "50", "100", "150")
    legendTitle <- "NPP SD (gC/m²/yr)"
  } else if (stat == "trend") {
    lims = c(-3, 3)
    brks = c(-3, -1.5, 0, 1.5, 3)
    labs = c("-3", "-1.5", "0", "+1.5", "+3")
    legendTitle <- "NPP slope (gC/m²/yr²)"
    pal = "RdYlGn"
  }
  
  basemap <- ggplot() +
    geom_sf(data = usa, fill = "grey95", color = NA) +
    geom_sf(data = canada, fill = "grey95", color = NA)
  
  npp_plot <- basemap +
    geom_spatraster(
      data = nppRaster,
      aes(fill = !!sym(names(nppRaster)[1])),
      show.legend = TRUE
    ) +
    coord_sf(xlim = xlims, ylim = ylims, expand = FALSE) +
    scale_fill_distiller(
      name = legendTitle,
      palette = pal,
      direction = 1,
      limits = lims,
      oob = scales::squish,
      breaks = brks,
      labels = labs,
      na.value = "transparent"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.background = element_rect(fill = "lightblue1", color = NA),
      panel.grid = element_line(color = "#00000010"),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
  
  return(npp_plot)
}


plotNPPchange <- function(present, future, type = "absolute", stat = "mean") {
  if (nlyr(present) > 1) {
    present <- app(present, mean)
  }
  if (nlyr(future) > 1) {
    future <- app(future, mean)
  }
  
  # convert kgC/m2/day to gC/m2/yr
  present <- present * 365 * 1000
  future <- future * 365 * 1000
  
  if (!identical(crs(present), crs(future))) {
    future <- terra::project(future, crs(present))
  }
  # align geometry (extent/resolution/rows/cols)
  if (!terra::compareGeom(present, future, stopOnError = FALSE)) {
    future <- terra::resample(future, present, method = "bilinear")
  }
  
  # calculate difference
  npp_diff <- future - present
  legendTitle <- expression(Delta * " NPP (gC/m²/yr)")
  lims <- c(-200, 200)
  labs <- c("-200", "-100", "0", "100", "200")
  brks <- c(-200, -100, 0, 100, 200)
  
  if (stat == "sd"){
    lims <- c(-50, 50)
    labs <- c("-50", "-25", "0", "+25", "+50")
    brks <- c(-50, -25, 0, 25, 50)
    legendTitle <- expression(Delta * " NPP SD (gC/m²/yr)")
  }
  
  # transformation
  if (type == "relative"){
    npp_diff <- npp_diff / present * 100
    legendTitle <- expression(Delta * " NPP (%)")
    lims <- c(-60, 60)
    labs <- c("-60", "-30", "0", "+30", "+60")
    brks <- c(-60, -30, 0, 30, 60)
    
    if (stat == "sd"){
      lims <- c(-100, 100)
      labs <- c("-100", "-50", "0", "+50", "+100")
      brks <- c(-100, -50, 0, 50, 100)
      legendTitle <- expression(Delta * " NPP SD (%)")
    }
  }
  
  # get data for a base map
  canada <- ne_countries(country = "canada", scale = 10) |>
    sf::st_transform(crs(npp_diff))
  usa <- ne_countries(country = "United States of America", scale = 10) |>
    sf::st_transform(crs(npp_diff))
  
  # some setup
  xlims <- c(-2339839, 3010580)
  ylims <- c(-200000, 3000000)
  
  basemap <- ggplot() +
    geom_sf(data = usa, fill = "grey95", color = NA) +
    geom_sf(data = canada, fill = "grey95", color = NA)
  
  npp_plot <- basemap +
    geom_spatraster(
      data = npp_diff,
      aes(fill = !!sym(names(npp_diff)[1])),
      show.legend = TRUE
    ) +
    coord_sf(xlim = xlims, ylim = ylims, expand = FALSE) +
    scale_fill_distiller(
      name = legendTitle,
      palette = "RdYlGn",
      direction = 1,
      limits = lims,
      oob = scales::squish,
      breaks = brks,
      labels = labs,
      na.value = "transparent"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.background = element_rect(fill = "lightblue1", color = NA),
      panel.grid = element_line(color = "#00000010"),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
  
  return(npp_plot)
}

plotValidationMetrics <- function(
    dt,
    estimateName = "GPP",
    metric = "R2"
) {
  
  # some metric-specific formatting
  optimValue <- ifelse(metric == "R2", 1, 0)
  xLab <- ifelse(metric == "R2", "R²", ifelse(metric == "RMSE", "RMSE (gC/m²/day)", "Relative bias (%)"))
  
  xLims <- range(dt[, get(metric)], na.rm = TRUE)
  xLims[1] <- min(xLims[1], optimValue)
  xLims[2] <- max(xLims[2], optimValue)
  
  dt <- dt[estimate == estimateName, ]
  
  p <- ggplot(dt, aes(y = towerName, x = get(metric))) +
    geom_point(color = "steelblue4", size = 2.5) +
    geom_vline(
      xintercept = optimValue,
      color = "black",
      linetype = "dashed",
      linewidth = 0.4
    ) +
    scale_x_continuous(
      limits = xLims,
      n.breaks = 6,
      expand = expansion(mult = 0.02)
    ) +
    scale_y_discrete(limits=rev) +
    theme_minimal(base_size = 12) +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_text(size = 9, colour = "black"),
      axis.text.x = element_text(size = 10, colour = "black"),
      axis.title.x = element_text(size = 11),
      panel.grid.major.y = element_line(color = "#00000020"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "#00000020"),
      panel.border = element_rect(fill = NA, color = "black", linewidth = 0.4),
      plot.title = element_text(size = 13, face = "bold", hjust = 0),
      plot.caption = element_text(size = 8),
      plot.margin = margin(5, 10, 5, 5)
    ) +
    labs(x = xLab)
  
  return(p)
}
