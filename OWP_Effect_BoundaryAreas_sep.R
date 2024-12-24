#### Loop for radii around OWP ####

library(rgeos)
library(sf)
library(sp)
library(raster)
library(rgdal)
library(ggplot2)

#' Load all packages and our support file.
library(easypackages) #' Package to load all packages.
libraries("dplyr", "readr",
          "lattice", "ggplot2", "reshape", "gratia",  "lubridate", 
          "plyr", "cowplot", "naniar", "visdat", "ggmap","rgl", "sp", 
          "gstat", "DHARMa", "lubridate", "scales", "MASS", 
          "raster", "dismo", "splancs",  "reshape", "gstat", 
          "ggmap", "rgdal", "fields", "rgeos", "tidyr", "mgcv", "glmmTMB", "stringr")


#' Choose bird and retrieve ZAG data
#Bird = "RB"
#setwd(paste("C:/Users/grund006/OneDrive - Wageningen University & Research/Anne WMR/Gemini_birds_spatstat/Modelling/Output",Bird,sep="/"))
#load("RB_V4_ZAG.RData")

setwd("~/W/IMARES/DATA/GroteStern/TEMPORARY") #contains RData

Bird = "GM"
#setwd(paste("C:/Users/grund006/OneDrive - Wageningen University & Research/Anne WMR/Gemini_birds_spatstat/Modelling/Output",Bird,sep="/"))
load("GM_V4_ZAG.RData")

Bird = "AUK"
#setwd(paste("C:/Users/grund006/OneDrive - Wageningen University & Research/Anne WMR/Gemini_birds_spatstat/Modelling/Output",Bird,sep="/"))
load("AUK_V4_ZAG.RData")


## Set proper working directory for storing results
setwd("~/W/IMARES/TT/Zeevogels/Public/Gemini HiDef surveys BioConsult/Modelling/Output")

#' Support functions

# Function to generate a random point within the bounding box of a given area
# Function to translate a geometry to a new location based on a point
translate_to_point <- function(polygon_sf, new_center) {
  # Extract the current centroid of the polygon
  old_center <- st_coordinates(st_centroid(st_geometry(polygon_sf)))[1,]
  # Compute the translation vector
  translation_vector <- matrix(c(new_center["X"] - old_center["X"], new_center["Y"] - old_center["Y"]), ncol = 2)
  
  # Debug print
  #print(paste("Translation Vector: ", translation_vector))
  
  # Translate the polygon and return the new object
  new_polygon_sf <- st_geometry(polygon_sf) + translation_vector
  
  # Debug print
  #print(paste("New Polygon: ", new_polygon_sf))
  
  st_sf(geometry = new_polygon_sf, crs = st_crs(polygon_sf))
}

# Function to translate a geometry to a new location based on a point
#translate_to_point <- function(polygon_sf, new_center) {
# Extract the current centroid of the polygon
#old_center <- st_coordinates(st_centroid(st_geometry(polygon_sf)))[1,]
# Compute the translation vector
#translation_vector <- matrix(c(new_center["X"] - old_center["X"], new_center["Y"] - old_center["Y"]), ncol = 2)
# Translate the polygon and return the new object
# new_polygon_sf <- st_geometry(polygon_sf) + translation_vector
# st_sf(geometry = new_polygon_sf, crs = st_crs(polygon_sf))
#}

#' Function to generate a square polygon around a center point with a 
#' specific area
generate_square_polygon <- function(center, area) {
  s <- sqrt(area)
  
  # Define the corners based on the side length and center
  bottom_left  <- c(center['X'] - s/2, center['Y'] - s/2)
  bottom_right <- c(center['X'] + s/2, center['Y'] - s/2)
  top_left     <- c(center['X'] - s/2, center['Y'] + s/2)
  top_right    <- c(center['X'] + s/2, center['Y'] + s/2)
  
  # Create the square polygon
  square <- st_polygon(list(matrix(c(bottom_left, bottom_right, top_right, top_left, bottom_left), 
                                   ncol=2, byrow=TRUE)))
  st_sfc(square, crs = st_crs(StudyArea_sf_buffered))
}


#' Function to generate a square polygon around a center point with a 
#' specific area
generate_square_polygon <- function(center, area) {
  s <- sqrt(area)
  
  # Define the corners based on the side length and center
  bottom_left  <- c(center['X'] - s/2, center['Y'] - s/2)
  bottom_right <- c(center['X'] + s/2, center['Y'] - s/2)
  top_left     <- c(center['X'] - s/2, center['Y'] + s/2)
  top_right    <- c(center['X'] + s/2, center['Y'] + s/2)
  
  # Create the square polygon
  square <- st_polygon(list(matrix(c(bottom_left, bottom_right, top_right, top_left, bottom_left), 
                                   ncol=2, byrow=TRUE)))
  st_sfc(square, crs = st_crs(StudyArea_sf_buffered))
}




#rad = 1 for trial run

for(rad in 8:10){
  
  # inner:
  Area4_sf_buffered <- st_buffer(Area4_sf_poly, dist = rad-1) # this is Area 4 with a buffer of dist
  # outer:
  Area4_sf_buffered2 <- st_buffer(Area4_sf_buffered, dist = 1) # this is Area 4 with a buffer of dist
  
  
  ## AG: We can take Area4_sf_buffered
  ## where dist = rad - 1
  ## so that you get: 01 km, 1-2 km, 2-3km etc. buffer zones.
  ## but then you have to use Area4_st_buffered as your basis instead of Area4_sf_poly
  ## for looping purposes
  
  #' Step 1.
  #' Create a buffer of X km around Area4
  #' Use a buffer of 1 km. You can change this in the dist argument
  
  #Area4_sf_bufferedX <- st_buffer(Area4_sf_poly, dist = rad)
  ## AG:
  Area4_sf_bufferedX <- st_buffer(Area4_sf_buffered, dist = rad-1)
  
  #' Use a buffer of 2 km. You can change this in the dist argument
  #Area4_sf_bufferedY <- st_buffer(Area4_sf_poly, dist = rad)
  ## AG:
  Area4_sf_bufferedY <- st_buffer(Area4_sf_buffered, dist = rad)
  
  #' Step 2:
  #' Calculate Area5 = Area4Buffer - Area4
  # Area5_sf_poly <- st_difference(Area4_sf_buffered, Area4_sf_poly)
  # Calc outer - inner
  Area5_sf_poly <- st_difference(Area4_sf_buffered2, Area4_sf_buffered)
  
  
  #' Step 3: Plot results.
  jpeg(paste( Bird, paste(rad, "km_BoundaryTest.jpeg", sep=""), sep="_"), width=1000, height=1000, quality=100)
  plot(st_geometry(StudyArea_sf_buffered), border='black', add=FALSE, axes=T, col="lightgrey",
       xlab="Xkm", ylab = "Ykm", main=paste("Boundary effect", paste(rad-1, rad, sep="-"), "km", sep=" "))
  plot(st_geometry(Area4_sf_poly), add=TRUE, cex = 0.1, col="lightgrey")
  plot(st_geometry(Area5_sf_poly), add=TRUE, col = "red", cex = 0.1)
  plot(st_geometry(Area4_sf_buffered2), border='red', add=TRUE)
  plot(st_geometry(Area4_sf_buffered), border='red', add=TRUE)
  plot(st_geometry(Area1_sf_poly), add=TRUE, col = "grey", cex = 0.1)
  plot(st_geometry(Area3_sf_poly), add=TRUE, col = "grey", cex = 0.1)
  dev.off()
  
  
  #' But after another email....if this is not what you want but a ring 
  #' around the windpark, then use:
  #' Finally, we need an extra area, called Area5, that is goes from
  #' the outer part of Area4 to a buffer of X km around this outer area.
  #' To get this area, carry out the following steps:
  #'  1. Create a buffer of X km around Area4. Create another buffer of
  #'     Y km around Area 4. Make sure than Y > X. 
  #'     Call this Area4_sf_bufferedX and Area4_sf_bufferedY
  #'  2. Calculate Area5 = Area4_sf_bufferedY - Area4_sf_bufferedX
  #'  3. Plot to double check
  #' Step 1.
  #' Create a buffer of X km around Area4
  #' Use a buffer of 1 km. You can change this in the dist argument
  #' 
  
  
  #* Subsection 12.3: Step 4: Predict the ZAG expected values for each area----
  
  #' Calculate the average predicted density in each of these 4 areas. 
  #' Call this: ExpZagA1... ExpZagA4
  
  #' We have the predicted values from the ZAG in MyData$Pred.
  #' We need to figure out the row numbers of MyData that are in each
  #' area.
  
  
  
  #' Convert MyData to a sf object.
  MyData.sp <- st_as_sf(MyData, 
                        coords = c("Xkm", "Ykm"), 
                        crs = "+proj=utm +zone=31 +north +ellps=WGS84 +units=km  +datum=WGS84")
  print(st_crs(MyData.sp))
  
  st_crs(MyData.sp) <- st_crs(Area1_sf_poly)
  
  
  print(st_crs(Area4_sf_poly))
  print(st_crs(Area5_sf_poly))
  
  print(st_crs(MyData.sp))
  print(st_crs(StudyArea_sf_buffered))
  
  
  #' Determine which points in MyData are contained within each area.
  ContainedArea5 <- st_contains(Area5_sf_poly, MyData.sp)
  contained_indicesArea4 <- unique(unlist(ContainedArea4))
  contained_indicesArea5 <- unique(unlist(ContainedArea5))
  
  #' And this is a check of area 4.
  #' Plot the study area and the spatial polygon for areas 1, 2, 3 and 4.
  jpeg(paste( Bird, paste(rad, "km_BoundaryTestV2.jpeg", sep=""), sep="_"), width=1000, height=1000, quality=100)
  ggplot() +
    geom_sf(data = StudyArea_sf_buffered, 
            fill = "transparent", 
            color = "blue") + 
    geom_sf(data = Area1_sf_poly,  
            fill = "gold",
            col = "gold") +
    geom_sf(data = Area3_sf_poly,  
            fill = "gold",
            col = "gold") +
    geom_sf(data = Area2_sf_poly,  
            fill = "lightyellow",
            col = "lightyellow") +
    #geom_sf(data = Area4_sf_poly,  
    #       fill = "gold",
    #      col = "gold") +
    geom_sf(data = Area5_sf_poly,  
            fill = "red",
            col = "darkred") +
    theme_minimal() +
    labs(title = paste("Boundary effect", paste(rad-1, rad, sep="-"), "km", sep=" ")) +
    geom_point(data = WD2, 
               aes(x = Xkm, y = Ykm),
               size = 0.01,
               col = grey(0.5),
               alpha = 0.5) +
    geom_point(data = MyData[contained_indicesArea4,], 
               aes(x = Xkm, y = Ykm),
               size = 0.1,
               col = "red",
               alpha = 0.5) 
  
  dev.off()
  
  #' Now get the average expected values for each area.
  #' This is for the observed data and the applied model
  
  ExpZagA4 <- mean(MyData[contained_indicesArea4, "Pred"])
  ExpZagA5 <- mean(MyData[contained_indicesArea5, "Pred"])
  
  

  
  MyArea <- "Area5" 
  
  if (MyArea == "Area5"){
    Area_sf_poly.out <- Area4_sf_bufferedY #ExpZagA5 #outside ring
    Area_sf_poly.in <- Area4_sf_bufferedX # inside ring
    Area_sf_poly <- Area4_sf_bufferedY
    
    ## AG: maybe, if we work with the boundary only, 
    ## we should be using a squared shape otherwise it is hard to find a place 
    ## within studyarea where there is no intersections...
    ##
    ## AG: We do not want the random area to be within Area4 or the buffer
    ## so we use the outer shape polygon of the buffer so that everything within that
    ## cannot be included in the random sample areas.
    
    ## AG: To increase computation power (it takes over 3 days to run after rad=5)
    ## We should crop the area from which points are sampled!!
    ## We use the study area and outside ring.
    SamplingArea_sf = st_difference( StudyArea_sf_buffered, Area4_sf_bufferedY )
    
    Title <- "Buffer"
    #' Test statistic per survey
    TestStatPerSurvey <- tapply(MyData[contained_indicesArea5, "Pred"],
                                FUN = mean,
                                INDEX = MyData[contained_indicesArea5, "Survey"])
  }
  
  #' To randomly place a new area within a StudyArea_sf while
  #'  ensuring it doesn't intersect with the existing Area_sf_poly, we 
  #'  use the following approach:
  
  #' Calculate the bounding box of the StudyArea_sf.
  #' Randomly pick a point within that bounding box.
  #' Translate the new Area_sf_poly so that its centroid matches the randomly picked point.
  #' Check for intersections.
  #' If an intersection is found, go back to step 2.
  #' Create space for the vector that will contain the average
  #' biomass for the new area, based on simulated values.
  
  ExpZagAreaSim <- vector(length = NSim)
  ExpZagByArea  <- matrix(nrow = 8, ncol = NSim)
  
  pb <- txtProgressBar(min = 0, max = NSim, style = 3)
  
  for (k in 1:NSim){
    #' Initialize variables
    max_attempts <- 1000
    attempt <- 1
    Overlap <- FALSE
    IsWithinStudyArea <- TRUE
    GoOn <- TRUE
    
    #' WHILE LOOP
    #' Begin while loop to attempt placement
    #' 
    while(attempt <= max_attempts && GoOn) {
      
      #' Generate a random point within StudyArea_sf
      #random_point <- generate_random_point(StudyArea_sf_buffered)
      random_point <- generate_random_point(SamplingArea_sf)
      new_center   <- st_coordinates(random_point)[1,]
      
      ## AG: to avoid problems with the weird buffer shapes and random sampling
      ## we get the area of the buffer in question with [rad]
      ## then just generate a polygon with the same surface area
      # Initial approach that uses the shape of the Area5:
      ## initial (relocates the buffer ring shape)
      ##new_Area_sf_poly <- translate_to_point(Area_sf_poly, new_center)
      ## new approach:
      a <- as.numeric(st_area(Area_sf_poly.out) - st_area(Area_sf_poly.in))    ## surface area of Area5 with rad [rad]
      #ww <- sqrt(a)/2  ## Widths of buffers needed to produce desired areas    
      #xmin = as.numeric(new_center[1] - ww)
      #xmax = as.numeric(new_center[1] + ww)
      # ymin = as.numeric(new_center[2] - ww)
      #ymax = as.numeric(new_center[2] + ww)
      #my_buffer_box = data.frame(cbind(x = c(xmin, xmax, xmax, xmin), y=c(ymin, ymin, ymax, ymax)))
      #' Function to generate a square polygon around a center point with a 
      #' specific area
      new_Area_sf_poly = generate_square_polygon(new_center, a)
      
      #' Convert the box to a sf object.
      # MyBox.sp <- st_as_sf(my_buffer_box, 
      #     coords = c("x", "y"), 
      #    crs = "+proj=utm +zone=31 +north +ellps=WGS84 +units=km  +datum=WGS84")
      #print(st_crs(MyBox.sp))
      # st_crs(MyBox.sp) <- st_crs(StudyArea_sf_buffered)
      # new_Area_sf_poly <- st_geometry(MyBox.sp, crs=crs(StudyArea_sf_buffered))
      
      #plot(new_Area_sf_poly)
      ## AG: The rest stays the same.
      #new_Area_sf_poly <- translate_to_point(new_Area_sf_poly, new_center)
      
      #' Check if the newly generated polygon is completely within the study area.
      within_study_area <- st_within(new_Area_sf_poly, StudyArea_sf_buffered)
      IsWithinStudyArea <- length(within_study_area[[1]]) > 0
      
      #' Check that new polygon does not intersect with existing Area_sf_poly
      overlap_list <- st_intersects(new_Area_sf_poly, Area_sf_poly, sparse = FALSE)
      Overlap <- overlap_list[1,1]
      
      #' Update placement validity
      if(IsWithinStudyArea && !Overlap) {
        #cat("Successfully placed new polygon on attempt", attempt, "\n")
        GoOn <- FALSE
      } else {
        #cat("Attempt", attempt, "failed. Trying again.\n")
      }
      
      attempt <- attempt + 1
    }
    
    
    #' Plot new polygon
    Plot <- FALSE
    if (Plot) {
      Z <- st_coordinates(random_point)[1,]
      Z <- as.data.frame(t(as.matrix(Z)))
      p <- ggplot() +
        geom_sf(data = StudyArea_sf_buffered, 
                fill = "transparent", 
                color = "red") + 
        geom_sf(data = Area_sf_poly,  
                fill = "transparent",
                col = "red") +
        geom_sf(data = new_Area_sf_poly,
                fill = "blue",
                col = "blue",
                alpha =0.5) +
        theme_minimal() +
        labs(title = "Study area") +
        geom_point(data = Z, aes(x = X, y = Y),
                   size = 5,
                   col = "blue",
                   alpha = 0.5) 
      print(p)
    } #' END Plot if
    
    #' Assess which points from MyData are in the new polygon
    ContainedAreaNEW <- st_contains(new_Area_sf_poly, MyData.sp)
    
    #' Extract indices of the points that are in the new polygon.
    contained_indicesAreaNEW <- unique(unlist(ContainedAreaNEW))
    
    Plot <- FALSE
    if (Plot) {
      #' Check
      #' Plot the study area and the spatial polygon for areas 1, 2, 3.
      ggplot() +
        geom_sf(data = StudyArea_sf_buffered, 
                fill = "transparent", 
                color = "red") + 
        geom_sf(data = new_Area_sf_poly,  
                fill = "yellow",
                col = "yellow") +
        theme_minimal() +
        labs(title = "Study area") +
        geom_point(data = MyData[contained_indicesAreaNEW,], 
                   aes(x = Xkm, y = Ykm),
                   size = 0.1,
                   col = "red",
                   alpha = 0.5) 
      #' That seems to be ok.
    }
    #' Calculate the average biomass from the model, for the new
    #' area. Use the posterior simulated values for this.
    ExpZagAreaSim[k] <- mean(ExpZAG1000[contained_indicesAreaNEW, k])
    
    #' Calculate the average biomass per survey, for the new area.
    #' Use posterior simulated values for this.
    ExpZagByArea[,k] <- tapply(ExpZAG1000[contained_indicesAreaNEW, k], 
                               FUN = mean, 
                               INDEX = MyData[contained_indicesAreaNEW, "Survey"])
    
    
    setTxtProgressBar(pb, k)  #Progress bar
  }
  close(pb)   #Close progress bar
  
  #* Subsection 12.5: Step 12: Make a histogram----
  
  #' 12. For each 1000 values of SimArea1, SimArea2, SimArea3, SimArea4,
  #'     SimArea5,
  #'     make  a histogram. 
  
  ResultsSIM <- data.frame(ExpZag = ExpZagAreaSim[1:NSim])
  
  p <- ggplot() + 
    geom_histogram(data = ResultsSIM,
                   aes(x = ExpZag),
                   alpha = 0.5,
                   color="darkblue", 
                   fill="lightblue",
                   bins = 35) +
    geom_vline(aes(xintercept = TestStatistic), color = "red") +
    xlab("Simulated density") + 
    ggtitle(Title) +
    theme(text = element_text(size = 15))
  p
  #' Plot TestStatistic as a vertical line.
  
  #* Subsection 12.6: Step 13: Get a formal test----
  
  
  #' 13 Compare to the NSim ExpZagA1... ExpZagA5. Count how often
  #'    ExpZagSimA1 is larger than ExpZagA1, and divide by 1000. That
  #'    is your test statistic that assesses whether birds avoid Area1.
  #'    Repeat for other areas.
  
  #' Bayesian p-value to test whether the average in the selected windpark area
  #' is larger than the NSim simulated one.
  
  mean(ResultsSIM$ExpZag > TestStatistic)
  
  #' For my random seed, I get 0.938 for area 3 (buitengaats).
  #' That means that if we select 2000 random areas in the study area
  #' of the same size as the Area 3, then in 93.8% of these selected areas 
  #' the mean predicted biomass is larger than in area 3 (buitengaats). 
  #' That, of course, is a strong evidence of a windpark effect.
  #' And if you want to compare the mean value per survey with the simulated mean
  #' values per survey, then use:
  #' We can do the same exercise per survey.
  BayesianPValuePerArea <- vector(length = 8)
  for (i in 1:8){
    BayesianPValuePerArea[i] <- mean(ExpZagByArea[i,] > TestStatPerSurvey[i])
  }
  names(BayesianPValuePerArea) <- LETTERS[1:8]
  BayesianPValuePerArea
  
  #' A 'No windpark effect' translates as a Bayesian p-value of about 0.5.
  #' You can see here, that:
  #'  - We have a strong avoidance effect in surveys F and G. 
  #'  - We have hardly any effects in surveys B, D and H.
  #'  - The lower predicted biomass in Survey A.
  
  
  ## Store the results
  #Bird = "RB"
  setwd(paste("~/W/IMARES/TT/Zeevogels/Public/Gemini HiDef surveys BioConsult/Modelling/Output",Bird,sep="/"))
  file_conn = file(paste(Bird, Title, paste(paste(rad-1, rad, sep="-"),"km_BOUNDARYeffect_stats.txt", sep="_"), sep="_"))
  writeLines(c(
    Title,
    Bird,
    ">>> Per survey A tm H:",
    BayesianPValuePerArea ,
    ">>> Mean:", 
    mean(ResultsSIM$ExpZag > TestStatistic),
    "'No windpark effect' translates as a Bayesian p-value of about 0.5."
  ), file_conn)
  close(file_conn)
  
  ## End loop
}



