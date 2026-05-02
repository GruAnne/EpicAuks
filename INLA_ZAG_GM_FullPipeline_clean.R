#'    Highland Statistics Ltd.
#'    www.highstat.com
#'
#'    This program is distributed in the hope that it will be useful,
#'    but WITHOUT ANY WARRANTY; without even the implied warranty of
#'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#'    GNU General Public License for more details.





# Section 1: Data description----


#' Project:  AIG-MEG Gemini - Aerial surveys - OWP Bird Habitat use

#' code       = Survey code (month; survey; year)
#' CRUISENO   = similar to survey code, but different code
#' POSITION   = position ID for each observation (where one obs. is 8 cameras combined)
#'              LAT / LONG Given per POSITION; this is the middle of all the 8 cameras
#' Area       = sum of area (km2) analysed/annotated of the 8 cameras on one POSITION
#'              We use area to get the bird densities
#' SEQ_DAY   = sequential day of study period, where survey 1 = day 1, 
#'             so SEQ_DAY is the number of days passed since start of surveys

#' Guillemonts.n; Auks.n; Razorbill.n = number of birds observed in image
#' GM = number of birds per km2 in image ( = [Any bird group].n / Area  )



# Section 2: Import the data----

#* Subsection 2.1: Load the data----

#' Set the working directory.

setwd("~/Gemini_birds_spatstat")
source("HighstatLibV13.R")


setwd("~/Gemini_birds_spatstat/Opgewerkte datasets")
WD2 <- read.csv(file = "AllGeminiDataForModelling_densities.csv",
                fileEncoding = "UTF-8-BOM",
                header = TRUE,
                na.strings = "NA",
                stringsAsFactors = TRUE,
                dec = ".")

WD2 <- as.data.frame(WD2)


#* Subsection 2.2: Load the packages----

#' Load all packages and our support file.
library(easypackages) #' Package to load all packages.
libraries("dplyr", "readr",
          "lattice", "ggplot2", "reshape", "gratia",  "lubridate", 
          "plyr", "cowplot", "naniar", "visdat", "ggmap","rgl", "sp", 
          "gstat", "DHARMa", "lubridate", "scales", "MASS", 
          "raster", "dismo", "splancs", "INLA", "reshape", "gstat", 
          "ggmap", "rgdal", "fields", "rgeos", "tidyr", "mgcv", "glmmTMB", "stringr")




# Section 3: Prepare the data----

#' We are going to use these as smoothers. Better discretize them. 
WD2$Depth.grp         <- inla.group(WD2$Depth, n = 30, method = "quantile")
WD2$Dist2Park.grp     <- inla.group(WD2$Dist2Park, n = 30, method = "quantile")
WD2$Dist2ShipLane.grp <- inla.group(WD2$Dist2ShipLane, n = 30, method = "quantile")
WD2$VesselDens.grp    <- inla.group(WD2$VesselDens, n = 30, method = "quantile")


#' A zero-altered Gamma model (ZAG) is a combination of a Bernoulli GLM
#' (or GAM) and a Gamma GLM (or GAM). The Bernoulli distribution is 
#' used for the analysis of absence and presence data, whereas the 
#' Gamma distribution is for strictly positive continuous data (i.e. 
#' data without zeros). The ZAG model can be executed as a combined model, 
#' but also in two separate steps. We will adopt the latter approach. 

#' For the Bernoulli part we need binary data. We will code 1 as presence
#' and 0 as absence.
#' Prepare data for the Bernoulli part of the ZAG.

WD2$GM01 <- ifelse(WD2$GM > 0, yes = 1 , no = 0)


#' Identify the survey
WD2$Survey <- factor(WD2$code,
                     levels = c("M03_S01_22", "M10_S01_22", "M12_S01_22",
                                "M01_S01_23", "M01_S02_23", "M02_S01_23", 
                                "M02_S02_23", "M03_S01_23"),
                     labels = c("A", "B", "C", "D",
                                "E", "F", "G", "H")) 
#' You can change the labels yourself. Note: INLA does not like fancy  names with -.
table(WD2$Survey)

#' We also need Survey as a numerical variable starting at 1.
WD2$SurveyNum <- as.numeric(WD2$Survey)

#' This is what we have:
unique(WD2$SurveyNum)
head(WD2[, c("code", "Survey", "SurveyNum")])


#' To execute the Gamma GAM we can either create a new data set consisting 
#' of only the non-zero biomass data, or we can set all density data that is 
#' equal to 0 to the value `NA`. The first approach means that we have 
#' to redefine the mesh and all other (standardized) variables.


WD2Pos <- subset(WD2, GM > 0)
dim(WD2)
dim(WD2Pos)


#' We need a row number for the Cleveland dotplots
WD2$RowID <- 1:nrow(WD2)


#' This is code from the report to get the contour lines of the wind park.
#' It is copy-paste.
setwd("~/Gemini_birds_spatstat/Modelling")
WindPark <- read.csv(file = "WindParkLocation.csv", 
                     header = TRUE,
                     stringsAsFactors = TRUE,
                     dec = ".")

PXY.utm <- LongLatToUTM(x = WindPark$Longitude, 
                        y = WindPark$Latitude, 
                        zone = 31,
                        Hemisphere = "north")
WindPark$Xkm <- PXY.utm$X / 1000
WindPark$Ykm <- PXY.utm$Y / 1000

WindPark$Long <- WindPark$Longitude
WindPark$Lat  <- WindPark$Latitude


WindPark.ZE <- subset(WindPark, Park == "ZeeEnergie")
WindPark.BU <- subset(WindPark, Park == "Buitengaats")

#' Get the corner locations of ZeeEnergie
Loc <- cbind(WindPark.ZE$Long, WindPark.ZE$Lat)

#' Convert this into a spatial polygon: ParkZEPoly.SP
idCorners <- c(7, 39, 46, 75)
ParkZEPoly     <- Polygon(Loc[idCorners,], hole = FALSE)
ParkZEPoly.SP  <- SpatialPolygons(list(Polygons(list(ParkZEPoly), ID = '1')))

#' And in UTM:
Loc <- cbind(WindPark.ZE$Xkm, WindPark.ZE$Ykm)
ParkZEPoly.utm     <- Polygon(Loc[idCorners,], hole = FALSE)
ParkZEPoly.SPutm  <- SpatialPolygons(list(Polygons(list(ParkZEPoly.utm), ID = '1')))

#' Do the same for Buitengaats
#' Get the corner locations of Buitengaats
Loc <- cbind(WindPark.BU$Long, WindPark.BU$Lat)

#' Convert this into a spatial polygon: ParkZEPoly.SP
idCorners <- c(7, 38, 46, 75)
ParkBUPoly     <- Polygon(Loc[idCorners,], hole = FALSE)
ParkBUPoly.SP  <- SpatialPolygons(list(Polygons(list(ParkBUPoly), 
                                                ID = '1')))

#' Now for UTM:
Loc <- cbind(WindPark.BU$Xkm, WindPark.BU$Ykm)
ParkBUPoly.utm  <- Polygon(Loc[idCorners,], hole = FALSE)
ParkBUPoly.SPutm <- SpatialPolygons(list(Polygons(list(ParkBUPoly.utm), 
                                                  ID = '1')))

#' Glue the two windpark polygons together
Park.SP    <- union(ParkZEPoly.SP, ParkBUPoly.SP)
Park.SPutm <- union(ParkZEPoly.SPutm, ParkBUPoly.SPutm)



#' Now get the two shipping lanes

#' Get Distance to shipping lane
setwd("~/Gemini_birds_spatstat/Modelling/Alain data en scripts Count based (V1)")
DSN <- "ShapefileShippinglanenorth/Shipping lane north_region.shp"
ShipLaNe.North <- readOGR(dsn = DSN, 
                          layer = "Shipping lane north_region",
                          verbose = FALSE)


DSN <- "ShapefileShippinglanesouth/Shipping lane south_region.shp"
ShipLaNe.South <- readOGR(dsn = DSN, 
                          layer = "Shipping lane south_region",
                          verbose = FALSE)


#' Right now ShipLaNe.North and ShipLaNe.South is so-called SpatialPolygonsDataFrame
#' It can be converted into a data frame with
#' the fortify function
ShipLaNe.North.df <- fortify(ShipLaNe.North)
ShipLaNe.South.df <- fortify(ShipLaNe.South)


#' And we convert the coordinate (which are called lat and long,
#' but which are UTM coordinates) to Xkm and Ykm
ShipLaNe.North.df$Xkm <- ShipLaNe.North.df$long  / 1000
ShipLaNe.North.df$Ykm <- ShipLaNe.North.df$lat   / 1000

ShipLaNe.South.df$Xkm <- ShipLaNe.South.df$long / 1000
ShipLaNe.South.df$Ykm <- ShipLaNe.South.df$lat  / 1000

#' Extract the UTM coordinates of the contour lines
LocNorth <- ShipLaNe.North.df[,c("Xkm", "Ykm")]
LocSouth <- ShipLaNe.South.df[,c("Xkm", "Ykm")]

#' Get rid of a few rows
LocNorth <- ShipLaNe.North.df[-21, c("Xkm", "Ykm")]
LocSouth <- ShipLaNe.South.df[c(1:16,19,20,21,1), c("Xkm", "Ykm")]

#' The objects with the coordinates for the shipping lines are:
#' LocNorth
#' LocSouth

#' Spatial polygon for North
LocNorthPoly <- Polygon(LocNorth, hole = FALSE)
LocNorth.SP  <- SpatialPolygons(list(Polygons(list(LocNorthPoly), ID = '1')))

#' Spatial polygon for South
LocSouthhPoly <- Polygon(LocSouth, hole = FALSE)
LocSouth.SP   <- SpatialPolygons(list(Polygons(list(LocSouthhPoly), ID = '1')))

#' Combine those two spatial polygons.
Ships.SP <- union(LocNorth.SP, LocSouth.SP)








# Section 4: Data exploration----

#' See Razorbills_Bernoulli_V2.R and Razorbills_Gamma_V2.R





# Section 5: ZAG GAM model formulation----


#' We will fit a ZAG GAM. This consists of a Bernoulli GAM applied to
#' the 0-1 data and a Gamma GAM applied to the presence-only data. Once
#' these two models have been fitted, we can combine them for prediction
#' of the density.

#' The Bernoulli is defined by:

#'   RB01_is ~ Bernoulli(pi_is)
#'   E[RB01_is] = pi_is
#'   var[RB01_is] = (1 - pi_is) * pi_is

#'   logit(pi_is) = Intercept + Covariates + Dependency


#' In the second step of the analysis, we remove all the zeros from the 
#' data and apply a Gamma GAM with the spatial-temporal replicate 
#' correlation on the presence-only data. This model is defined in 
#' below.

#'   RB_is ~ Gamma(mu_is, r)
#'   E[RB01_is] = mu_is
#'   var[RB01_is] =  mu_is^2 / r

#'   log(mu_is)   = Intercept + Covariates + Dependency



#' Once both models have been fitted, the components of the 
#' individual models can be used to calculate the mean and variance of 
#' the zero-altered Gamma model. The full expression of the ZAG model is 
#' presented below. If the purpose of the analysis is to predict 
#' densities data, then we need to extract the relevant parts of the 
#' Bernoulli and Gamma models, and obtain the mean and variance of the 
#' ZAG. If we 'only' want to know which covariates are important, or if 
#' we want to detect a disturbance effect due to a windpark, then 
#' assembling the ZAG is not needed, and we can focus on the two 
#' individual components of the ZAG.


#' RB_is ~ ZAG (mu_is, pi_is, r)
#' E[RB_is]   = pi_is * mu_is 
#' var[RB_is] = \frac{ \pi_{is} \times r +  \pi_{is} -  \pi_{is}^2 \times r}{r} \times \mu_{is} ^2

#' log(mu_is)  = Intercept + Covariates + Dependency
#' logit(pi_is) = Intercept + Covariates + Dependency





# Section 6: Execute optimal Bernoulli GAM with spatial-temporal dependency in INLA----


#' We will first run the optimal Bernoulli GAM with spatial-temporal 
#' dependency. This was the replicate model

#' Get the spatial coordinates for all sites.
Loc <- cbind(WD2$Xkm, WD2$Ykm)


#' Define the mesh
RangeGuess <- 5  #' RangeGuess <- 5 
MaxEdge    <- RangeGuess / 5
Bound      <- inla.nonconvex.hull(Loc, convex = -0.075)
mesh       <- inla.mesh.2d(boundary = Bound, 
                           max.edge = c(1, 5) * MaxEdge, 
                           cutoff   = MaxEdge / 5)
par(mfrow = c(1,1), mar = c(0, 0, 0, 0))
plot(mesh, asp = 1, main = "")
points(Loc, col = 2, pch = 16, cex = 0.2)

#' Mesh size 
mesh$n  #' That is workable.

#' For later:
mesh01 <- mesh

#' Discretize the covariates
WD2$Depth.grp         <- inla.group(WD2$Depth, n = 30, method = "quantile")
WD2$Dist2Park.grp     <- inla.group(WD2$Dist2Park, n = 30, method = "quantile")
WD2$Dist2ShipLane.grp <- inla.group(WD2$Dist2ShipLane, n = 30, method = "quantile")
WD2$VesselDens.grp    <- inla.group(WD2$VesselDens, n = 30, method = "quantile")


#' Determine the number of observations.  
N  <- nrow(WD2)


#' Survey will go into the model as a factor. R-INLA requires manual coding
#' of the levels. And you can't have '-' in the names of the levels. The
#' underbar '_' may work.
Covar <- model.matrix(~ Survey, data = WD2)
Covar <- as.data.frame(Covar)  #' Essential!


#' Define a vector that identifies the blocking structure.
Repl     <- WD2$SurveyNum
Repl


#' How many do we have? Should be 8.
NRepl    <- length(unique(Repl)) #' Number of groups
NRepl


#' Define the projector matrix.
Arepl <- inla.spde.make.A(mesh, 
                          loc = Loc, 
                          repl = Repl)


#' Define the SPDE.
spdeRepl <- inla.spde2.pcmatern(mesh,  
                                constr = TRUE,
                                prior.range = c(5, 0.05), 
                                prior.sigma = c(2, 0.05))


#' Define the SRF
w01Repl <- inla.spde.make.index('w01', 
                                n.spde = mesh$n, 
                                n.repl = NRepl)



#' This is the stack for the Bernoulli GAM with spatial-temporal replicate
#' correlation.
Stack.BernSpatTemp <- inla.stack(
  tag = "Fit01",
  data = list(GM01 = WD2$GM01),  
  A = list(1, 1, 1, 1, 1, 1, Arepl),                  
  effects = list(   
    Intercept         = rep(1, N),                      #' Intercept
    Covar             = Covar[,-1],                     #' Survey without intercept
    Depth.grp         = MyStd(WD2$Depth.grp),           #' Covariate
    Dist2Park.grp     = MyStd(WD2$Dist2Park.grp),       #' Covariate
    VesselDens.grp    = MyStd(WD2$VesselDens.grp),      #' Covariate
    Dist2ShipLane.grp = MyStd(WD2$Dist2ShipLane.grp),   #' Covariate
    w01               = w01Repl))                       #' SRF


#' Executing the Bernoulli GAM with replicate dependency
BernSpatTemp1 <- inla(GM01 ~ -1 + Intercept + 
                        SurveyB + SurveyC + SurveyD + 
                        SurveyE + SurveyF + SurveyG + SurveyH +
                        f(w01, model = spdeRepl, replicate = w01.repl),
                      data = inla.stack.data(Stack.BernSpatTemp),
                      control.predictor = list(A = inla.stack.A(Stack.BernSpatTemp),
                                               link = 1,
                                               compute = FALSE),
                      control.compute = list(config = TRUE),
                      #'Faster calculation:
                      control.inla = list(strategy='gaussian', 
                                          int.strategy = 'eb'), 
                      family = "binomial")





# Section 7: Posterior simulation Bernoulli replicate GAM----

#' Use posterior simulation to get 10000 sets of simulated 
#' regression parameters and w's.
NSim      <- 1000
SimData01 <- inla.posterior.sample(n = NSim, result = BernSpatTemp1)


#' Here is some fancy code to grab the last 8 values.
#' Custom function
MyGrep <- function(x, SimulatedData){ 
  # SimulatedData is the name of the object containing the simulation results
  # x is an element of BetasInModel
  names.SimData <- attributes(SimulatedData[[1]]$latent)$dimnames[[1]]
  names.SimData[grep(x, names.SimData)] 
}


#' What are the names of the regression parameters?
BetasInModel <- rownames(BernSpatTemp1$summary.fixed)

#' Get their location in the object with simulation results.
BetaNames    <- unlist(lapply(BetasInModel, 
                              FUN = MyGrep, 
                              SimulatedData = SimData01))
MyID     <- function(x){ which(rownames(SimData01[[1]]$latent) == x) }
RowsBeta <- lapply(BetaNames, MyID)
RowsBeta <- as.numeric(RowsBeta)
RowsBeta
#' If RowsBeta gives NA  in a moment, then use this:
#' BetaNames <- rownames(BernSpatTemp1$summary.fixed)


#' Now the spatial correlation random effects.
#' Determine the names of the random effects
names.SimData <- attributes(SimData01[[1]]$latent)$dimnames[[1]] 
wNames <- names.SimData[grep("w01", names.SimData)]  
wNames


# Determine on which rows the random effects are
RowsW <- lapply(wNames, MyID)
RowsW <- as.numeric(RowsW)
RowsW

#' If these are not numbers, then type:  SimData01[[1]]$latent and
#' see how you need to change the paste function above
#' These are the row numbers in SimData01[[1]]$latent that 
#' contain the simulated random effect values


#' For later:
RowsBeta01 <- RowsBeta
RowsW01    <- RowsW



#' Start simulation.
N     <- nrow(WD2)
X.sim <- model.matrix(~ 1 + Survey, data = WD2)
X.sim <- as.matrix(X.sim)
#' A.sim <- as.matrix(Arepl)


compute_probabilities <- function(i, X.sim, Arepl, SimData01, RowsBeta, RowsW) {
  Betas  <- SimData01[[i]]$latent[RowsBeta]
  wk     <- SimData01[[i]]$latent[RowsW]
  FixedPart   <- X.sim %*% Betas
  SpatialPart <- (Arepl %*% wk)[,1]
  ExpEta      <- exp(FixedPart + SpatialPart)
  ExpEta / (1 +  ExpEta)
}

Pi2.i <- simplify2array(lapply(1:NSim, compute_probabilities, 
                               X.sim = X.sim, 
                               Arepl = Arepl, 
                               SimData01 = SimData01, 
                               RowsBeta = RowsBeta, 
                               RowsW = RowsW))
class(Pi2.i)
Pi.i <- drop(Pi2.i)  #' Convert the array back to a matrix
head(Pi.i[1:10, 1:10])









# Section 8: Execute optimal Gamma GAM with spatial-temporal dependency in INLA----


#' Get the spatial coordinates for all sites.
Loc <- cbind(WD2Pos$Xkm, WD2Pos$Ykm)


#' Define the mesh
RangeGuess <- 5  #' RangeGuess <- 5
MaxEdge    <- RangeGuess / 5
Bound      <- inla.nonconvex.hull(Loc, convex = -0.075)
mesh       <- inla.mesh.2d(boundary = Bound, 
                           max.edge = c(1, 5) * MaxEdge, 
                           cutoff   = MaxEdge / 5)
par(mfrow = c(1,1), mar = c(0, 0, 0, 0))
plot(mesh, asp = 1, main = "")
points(Loc, col = 2, pch = 16, cex = 0.2)

#' Mesh size 
mesh$n  #' That is workable.


#' For later:
meshGamma <- mesh


#' Determine the number of observations.  
N  <- nrow(WD2Pos)


#' Survey will go into the model as a factor. R-INLA requires manual coding
#' of the levels. And you can't have '-' in the names of the levels. The
#' underbar '_' may work.
Covar <- model.matrix(~ Survey, data = WD2Pos)
Covar <- as.data.frame(Covar)  #' Essential!


#' Matern correlation and SR
#' Define a vector that identifies the blocking structure.
Repl     <- WD2Pos$SurveyNum
Repl


#' How many do we have? Should be 8
NRepl    <- length(unique(Repl)) #' Number of groups
NRepl


#' Define the projector matrix.
Arepl <- inla.spde.make.A(mesh, 
                          loc = Loc, 
                          repl = Repl)


#' Define the SPDE
spdeRepl <- inla.spde2.pcmatern(mesh,  
                                constr = TRUE,
                                prior.range = c(5, 0.05), 
                                prior.sigma = c(2, 0.05))


#' Define the SRF
wRepl <- inla.spde.make.index('w', 
                              n.spde = mesh$n, 
                              n.repl = NRepl)


#' This is the stack for the Bernoulli GAM with spatial-temporal replicate
#' correlation.
Stack.GammaSpatTemp <- inla.stack(
  tag = "Fit",
  data = list(GM = WD2Pos$GM),  
  A = list(1, 1, 1, 1, 1, 1, Arepl),                  
  effects = list(   
    Intercept         = rep(1, N),                         #' Intercept
    Covar             = Covar[,-1],                        #' Survey without intercept
    Depth.grp         = MyStd(WD2Pos$Depth.grp),           #' Covariate
    Dist2Park.grp     = MyStd(WD2Pos$Dist2Park.grp),       #' Covariate
    VesselDens.grp    = MyStd(WD2Pos$VesselDens.grp),      #' Covariate
    Dist2ShipLane.grp = MyStd(WD2Pos$Dist2ShipLane.grp),   #' Covariate
    w                 = wRepl))                            #' SRF


#' Executing the Bernoulli GAM with replicate dependency
GammaSpatTemp1 <- inla(GM ~ -1 + Intercept + 
                         SurveyB + SurveyC + SurveyD + 
                         SurveyE + SurveyF + SurveyG + SurveyH +
                         f(w, model = spdeRepl, replicate = w.repl),
                       data = inla.stack.data(Stack.GammaSpatTemp),
                       control.predictor = list(A = inla.stack.A(Stack.GammaSpatTemp),
                                                link = 1,
                                                compute = FALSE),
                       control.compute = list(dic = TRUE, 
                                              waic = TRUE,
                                              config = TRUE),
                       #'Faster calculation:
                       control.inla = list(strategy='gaussian', 
                                           int.strategy = 'eb'), 
                       family = "gamma")



# Section 9: Posterior simulation of the replicate GAM----


#' Use posterior simulation to get NSim sets of simulated regression 
#' parameters and w's.
SimDataPos <- inla.posterior.sample(n = NSim, result = GammaSpatTemp1)

#' The object `SimDataPos` contains NSim sets of intercepts and $w_k$ values.
#' We have this object: SimDataPos


#' What are the names of the regression parameters?
BetasInModel <- rownames(GammaSpatTemp1$summary.fixed)

#' Get their location in the object with simulation results.
BetaNames    <- unlist(lapply(BetasInModel, 
                              FUN = MyGrep, 
                              SimulatedData = SimDataPos))
MyID     <- function(x){ which(rownames(SimDataPos[[1]]$latent) == x) }
RowsBeta <- lapply(BetaNames, MyID)
RowsBeta <- as.numeric(RowsBeta)
RowsBeta
#' If RowsBeta gives NA  in a moment, then use this:
#' BetaNames <- rownames(GammaSpatTemp1$summary.fixed)



#' Now the spatial correlation random effects.
#' Determine the names of the random effects
names.SimData <- attributes(SimDataPos[[1]]$latent)$dimnames[[1]] 
wNames <- names.SimData[grep("w", names.SimData)]  
wNames

# Determine on which rows the random effects are
RowsW <- lapply(wNames, MyID)
RowsW <- as.numeric(RowsW)
RowsW


#' If these are not numbers, then type:  SimDataPos[[1]]$latent and
#' see how you need to change the paste function above
#' These are the row numbers in SimDataPos[[1]]$latent that 
#' contain the simulated random effect values


#' For later:
RowsBetaPos <- RowsBeta
RowsWPos    <- RowsW



#' Calculate predicted values
#' The model is: mu = exp(X * beta + A * w) 
#' For one set of simulation, let's calculate it.
Betas <- SimDataPos[[1]]$latent[RowsBeta]
wk    <- SimDataPos[[1]]$latent[RowsW]

#Get the matching X matrix
X.sim <- model.matrix(~ 1 + Survey,
                      data = WD2Pos)

X.sim <- as.matrix(X.sim)
#A.sim <- as.matrix(Arepl)


# This is the r parameter
r <- GammaSpatTemp1$summary.hyperpar["Precision parameter for the Gamma observations", "mean"]
Shape <- r


#' Simulate 1000 times
N  <- nrow(WD2Pos)
#mu.i    <- matrix(nrow = N, ncol = NSim)


compute_GammaMeans <- function(i, X.sim, Arepl, SimDataPos, RowsBeta, RowsW) {
  Betas  <- SimDataPos[[i]]$latent[RowsBeta]
  wk     <- SimDataPos[[i]]$latent[RowsW]
  FixedPart   <- X.sim %*% Betas
  SpatialPart <- (Arepl %*% wk)[,1]
  exp(FixedPart + SpatialPart)
}

mu.i <- simplify2array(lapply(1:NSim, compute_GammaMeans, 
                              X.sim = X.sim, 
                              Arepl = Arepl, 
                              SimDataPos = SimDataPos, 
                              RowsBeta = RowsBeta, 
                              RowsW = RowsW))
class(mu.i)
mu.i <- drop(mu.i)  #' Convert the array back to a matrix
head(mu.i[,1:10])




# Section 10: Calculate ZAG expected values----

#' The Bernoulli model gave us the Pi.
N    <- nrow(WD2)
Pi   <- BernSpatTemp1$summary.fitted.values[1:N, "mean"] #' Fitted values from the model

#' The Gamma model gave us the mu and r. 
N  <- nrow(WD2Pos)
mu <- GammaSpatTemp1$summary.fitted.values[1:N, "mean"] #' Fitted values from the model
r <-  GammaSpatTemp1$summary.hyperpar["Precision parameter for the Gamma observations", "mean"]



#' Problem: Look at the number of observations in Pi and mu:
nrow(WD2)
length(Pi)
length(mu)

#' The Pi_i and the mu_i are not of the same dimension. We have the same 
#' problem with the ZAP model, and we will apply the same solution. 
#' We will predict the mu_i values at those rows where we have an 0 for 
#' RK in `WD2`.


WD2$GM01

#' Get the X matrix based on the full data.
X.full <- model.matrix(~ 1 + Survey, data = WD2)
X.full <- as.matrix(X.full)

#' Get the A matrix based on the full data set.
#' All locations:
LocFull <- WD2[,c("Xkm", "Ykm")]
LocFull <- as.matrix(LocFull)

#' Replicate index for all locations:
ReplFull <- WD2$SurveyNum

#' A projector matrix using the mesh for the Gamma model,
#' but with respect to all locations.
A.full <- inla.spde.make.A(mesh, 
                           loc = LocFull, 
                           repl = ReplFull)

#' For each set of simulated regression parameters from the Gamma
#' model, calculate mu = exp(Fixed part + random part)
N       <- nrow(WD2)

mu.full <- matrix(nrow = N, ncol = NSim)
for (i in 1:NSim){
  FixedPart   <- X.full %*% SimDataPos[[i]]$latent[RowsBeta]
  wk          <- SimDataPos[[i]]$latent[RowsW]
  SpatialPart <- (A.full %*% wk)[,1]
  mu.full[,i] <- exp(FixedPart + SpatialPart)
}







#' In summary, we now have:
#'   Pi values for each site....simulated 1000 times.
#'   mu values for each site...simulated 1000 times
nrow(WD2)
dim(Pi.i)
dim(mu.full)


#' We can now calculate NSim times the ZAG expected values.
ExpZAG <- matrix(nrow = N, ncol = NSim)
for (i in 1:NSim){
  ExpZAG[,i] <- Pi.i[,i] * mu.full[,i]
}


#' Each row is now a site...and contains 1000x a ZAG expected value.
#' We can take the 2.5%, 50% and 97.5% quantiles.  


#' Calculate the 95% CIs
Results <- t(apply(ExpZAG, 1, quantile, c(0.025, 0.5, 0.975)))
Results <- data.frame(Results)
head(Results)

#' Give the columns appropriate names.
names(Results) <- c("SeLo", "Pred", "SeUp")
head(Results)

#' So..the Pred column contains our ZAG expected values.
hist(Results$Pred)

#plot(x = Results$Pred,
#     y = WD2$RB)




# Section 11: Visualise the ZAG expected values----



#' We are going to plot the ZAG expected values.
#' Problem: The geom_raster stuff wants to have values on a nice grid. 
#' We don't have that...yet. 
#' Plan:
#'   1. Create a MyData with a nice grid.
#'   2. On these grid values, calculate the Pi and the mu,
#'      for each simulated set of betas.
#'   3. Calculate the 2.5%, 50% and 97.5% quantils.

#' Here we go.
#' 1. Create a MyData with a nice grid.
MyData <- expand.grid(Survey = levels(WD2$Survey),
                      Xkm    = seq(from = min(WD2$Xkm), to = max(WD2$Xkm), length = 100),
                      Ykm    = seq(from = min(WD2$Ykm), to = max(WD2$Ykm), length = 100))
MyData$SurveyNum <- as.numeric(MyData$Survey)




#' 2. On these grid values, calculate the Pi and the mu,
#'      for each simulated set of betas.

#' Get the X matrix based on the full data.
#' This is the same for the Bernoulli and Gamma models in
#' this analysis
X.full <- model.matrix(~ 1 + Survey, data = MyData)
X.full <- as.matrix(X.full)

#' All locations. This is the same for the Bernoulli and
#' Gamma models.
LocFull <- MyData[,c("Xkm", "Ykm")]
LocFull <- as.matrix(LocFull)

#' Replicate index for all locations. This is the same
#' for the Bernoulli and Gamma models.
ReplFull <- MyData$SurveyNum


#' A projector matrix using the mesh for the Gamma model,
#' but with respect to all locations. This are different for
#' the Bernoulli and Gamma models, as the mesh differs.
A01 <- inla.spde.make.A(mesh01, 
                        loc = LocFull, 
                        repl = ReplFull)

APos <- inla.spde.make.A(meshGamma, 
                         loc = LocFull, 
                         repl = ReplFull)


#' Define space for the simulated expected ZAG values
#' ExpZAG1000 <- matrix(nrow = nrow(MyData), ncol = NSim)

#' Run loop
#' for (i in 1: NSim){
#'   #' Bernoulli part:
#'   Betas01       <- SimData01[[i]]$latent[RowsBeta01]
#'   wk01          <- SimData01[[i]]$latent[RowsW01]
#'   FixedPart01   <- X.full %*% Betas01
#'   SpatialPart01 <- A01 %*% wk01
#'   Pi            <- exp(FixedPart01 + SpatialPart01) / (1 +  exp(FixedPart01 + SpatialPart01))
#'   Pi            <- as.vector(Pi)
#'   #'
#'   #' Gamma part:
#'   BetasPos       <- SimDataPos[[i]]$latent[RowsBetaPos]
#'   wkPos          <- SimDataPos[[i]]$latent[RowsWPos]
#'   FixedPartPos   <- X.full %*% BetasPos
#'   SpatialPartPos <- APos %*% wkPos
#'   #' Add them up and predict on the response scale
#'   mu <- exp(FixedPartPos + SpatialPartPos)
#'   mu <- as.vector(mu)
#'   #' 
#'   #' Expected ZAG values calculated on the MyData grid
#'   ExpZAG1000[,i] <- Pi * mu
#' }

#' Alternative faster code.
compute_exp_ZAG <- function(i, SimData01, RowsBeta01, RowsW01, X.full, A01, SimDataPos, RowsBetaPos, RowsWPos, APos) {
  #' Bernoulli part:
  Betas01       <- SimData01[[i]]$latent[RowsBeta01]
  wk01          <- SimData01[[i]]$latent[RowsW01]
  FixedPart01   <- X.full %*% Betas01
  SpatialPart01 <- A01 %*% wk01
  Pi            <- exp(FixedPart01 + SpatialPart01) / (1 + exp(FixedPart01 + SpatialPart01))
  
  #' Gamma part:
  BetasPos       <- SimDataPos[[i]]$latent[RowsBetaPos]
  wkPos          <- SimDataPos[[i]]$latent[RowsWPos]
  FixedPartPos   <- X.full %*% BetasPos
  SpatialPartPos <- APos %*% wkPos
  mu             <- exp(FixedPartPos + SpatialPartPos)
  
  #' Expected ZAG values
  return(Pi * mu)
}

results <- lapply(1:NSim, compute_exp_ZAG, 
                  SimData01 = SimData01, 
                  RowsBeta01 = RowsBeta01, 
                  RowsW01 = RowsW01, 
                  X.full = X.full, 
                  A01 = A01, 
                  SimDataPos = SimDataPos, 
                  RowsBetaPos = RowsBetaPos, 
                  RowsWPos = RowsWPos, 
                  APos = APos)



ExpZAG1000 <- do.call(cbind, results)
dim(ExpZAG1000)



#'   3. Calculate the 2.5%, 50% and 97.5% quantils.

#' Each row is now a grid in MyData. And it contains 1000x a ZAG expected value.
#' We can take the 2.5%, 50% and 97.5% quantiles.  
Results <- t(apply(ExpZAG1000, 1, quantile, c(0.025, 0.5, 0.975)))
Results <- data.frame(Results)
head(Results)

#' Give the columns appropriate names.
names(Results) <- c("SeLo", "Pred", "SeUp")
head(Results)

#' Add the results to MyData
MyData$Pred <- Results$Pred
MyData$SeLo <- Results$SeLo
MyData$SeUp <- Results$SeUp




#This is needed to get the survey label in the top-left corner in each
#panel.
SurveyNames <- as.character(levels(MyData$Survey))
SurveyData <- data.frame(Survey = SurveyNames,
                         x      = rep(min(MyData$Xkm), length(SurveyNames)),
                         y      = rep(max(MyData$Ykm), length(SurveyNames)))
SurveyData$Survey <- factor(SurveyData$Survey)
SurveyData



#' And plot the whole thing. I am using the log of Pred as colour scale.
w <- ggplot() +
  geom_raster(data = MyData, aes(x = Xkm, y = Ykm, fill = log(Pred))) +
  coord_fixed(ratio = 1) + 
  theme_bw() +
  xlab("Xkm") + ylab("Ykm") +
  geom_path(data = fortify(ParkBUPoly.SPutm), 
            aes(x = long, y = lat)) +
  geom_path(data = fortify(ParkZEPoly.SPutm), 
            aes(x = long, y = lat)) + labs(fill="Log (n/km2)") + scale_fill_gradient2(name = "Log (n/km2)",
                                                                                      limits = c(-4, 4),
                                                                                      midpoint = 0,  #<--- CHANGE
                                                                                      high = "forestgreen", 
                                                                                      #mid = "blue", 
                                                                                      low = "red",
                                                                                      na.value = "transparent")
w <- w + facet_wrap(~Survey, ncol = 3)
w <- w +  theme(strip.background = element_blank(),
                strip.text.x = element_blank(),
                # axis.title.x=element_blank(),
                # axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                # axis.title.y=element_blank(),
                #axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black"))
w <- w + geom_text(data = SurveyData,
                   aes(x = x + 3, 
                       y = y-3, 
                       label = Survey),
                   size = 3)
w


# save it
Bird = "GM"
setwd(paste("~/Gemini_birds_spatstat/Modelling/Output",Bird,sep="/"))
pdf(file = paste(Bird, "FullZAG_PredictedValues_persurvey", sep="_"), height=8, width=12)
w
dev.off()



#### Now let's correct for the total nuber of birds per survey and plot that
MyData = MyData %>% ## GM
  mutate(Pred = as.numeric(Pred),
         My_total = ifelse(Survey == "A", 97, ifelse(
           Survey == "B", 639, ifelse(
             Survey == "C", 3489, ifelse(
               Survey == "D", 118, ifelse(
                 Survey == "E", 33, ifelse(
                   Survey == "F", 1136, ifelse(
                     Survey == "G", 305, ifelse(
                       Survey == "H", 1036, ".00000001"
                     )) ) ))  ) )))%>%
  mutate(My_total = as.numeric(My_total),
         Pred2 = Pred / My_total)

w <- ggplot() +
  geom_raster(data = MyData, aes(x = Xkm, y = Ykm, fill = (100*Pred2))) +
  coord_fixed(ratio = 1) + 
  theme_bw() +
  xlab("Xkm") + ylab("Ykm") +
  geom_path(data = fortify(ParkBUPoly.SPutm), 
            aes(x = long, y = lat)) +
  geom_path(data = fortify(ParkZEPoly.SPutm), 
            aes(x = long, y = lat))  + scale_fill_gradient2(name = "100%*(n/km2)/Ntotal",
                                                            limits = c(0, 4),
                                                            midpoint = 1,  #<--- CHANGE
                                                            high = "forestgreen", 
                                                            #mid = "blue", 
                                                            low = "red",
                                                            na.value = "transparent")
w <- w + facet_wrap(~Survey, ncol = 3)
w <- w +  theme(strip.background = element_blank(),
                strip.text.x = element_blank(),
                # axis.title.x=element_blank(),
                # axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                # axis.title.y=element_blank(),
                #axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black"))
w <- w + geom_text(data = SurveyData,
                   aes(x = x + 3, 
                       y = y-3, 
                       label = Survey),
                   size = 3)
w


# save it
Bird = "GM"
setwd(paste("~/Gemini_birds_spatstat/Modelling/Output",Bird,sep="/"))
pdf(file = paste(Bird, "FullZAG_PredictedValues_persurvey_STANDARDIZED", sep="_"), height=8, width=12)
w
dev.off()






#### NEW ####

## If you have ran it before:
load("GM_V4_ZAG.RData")

#'START ADJUSTED CODE:   9 October 2023
MyData <- expand.grid(Xkm    = seq(from = min(WD2$Xkm), to = max(WD2$Xkm), length = 100),
                      Ykm    = seq(from = min(WD2$Ykm), to = max(WD2$Ykm), length = 100),
                      Survey = levels(WD2$Survey))
MyData$SurveyNum <- as.numeric(MyData$Survey)

MyData$ID <- rep(1:10000, 8)
head(MyData, 25)


#' The 1.1 means that it is grid 1 1 
#' The 1.2 means that it is grid 1 2 
#' In a moment, we want to take the average of all 1.1 values,
#' and of all the 1.2 values, etc.


#' 2. On these grid values, calculate the Pi and the mu,
#'      for each simulated set of betas.

#' Get the X matrix based on the full data.
#' This is the same for the Bernoulli and Gamma models in
#' this analysis
X.full <- model.matrix(~ 1 + Survey, data = MyData)
X.full <- as.matrix(X.full)

#' All locations. This is the same for the Bernoulli and
#' Gamma models.
LocFull <- MyData[,c("Xkm", "Ykm")]
LocFull <- as.matrix(LocFull)

#' Replicate index for all locations. This is the same
#' for the Bernoulli and Gamma models.
ReplFull <- MyData$SurveyNum


#' A projector matrix using the mesh for the Gamma model,
#' but with respect to all locations. This are different for
#' the Bernoulli and Gamma models, as the mesh differs.
A01 <- inla.spde.make.A(mesh01, 
                        loc = LocFull, 
                        repl = ReplFull)

APos <- inla.spde.make.A(meshGamma, 
                         loc = LocFull, 
                         repl = ReplFull)


## Now we do the same, but for the mean and standard error over all surveys instead of pred/SElow/SEup per survey
compute_exp_ZAG2 <- function(i, SimData01, RowsBeta01, RowsW01, X.full, A01, SimDataPos, RowsBetaPos, RowsWPos, APos, ID) {
  #' Bernoulli part:
  Betas01       <- SimData01[[i]]$latent[RowsBeta01]
  wk01          <- SimData01[[i]]$latent[RowsW01]
  FixedPart01   <- X.full %*% Betas01
  SpatialPart01 <- A01 %*% wk01
  Pi            <- exp(FixedPart01 + SpatialPart01) / (1 + exp(FixedPart01 + SpatialPart01))
  
  #' Gamma part:
  BetasPos       <- SimDataPos[[i]]$latent[RowsBetaPos]
  wkPos          <- SimDataPos[[i]]$latent[RowsWPos]
  FixedPartPos   <- X.full %*% BetasPos
  SpatialPartPos <- APos %*% wkPos
  mu             <- exp(FixedPartPos + SpatialPartPos)
  
  #' Expected ZAG values
  Fit <- Pi * mu
  MeanPerGridCell <- tapply(Fit, FUN=mean, INDEX = ID)
  return(MeanPerGridCell)
  
}

results2 <- lapply(1:NSim, compute_exp_ZAG2, 
                   SimData01 = SimData01, 
                   RowsBeta01 = RowsBeta01, 
                   RowsW01 = RowsW01, 
                   X.full = X.full, 
                   A01 = A01, 
                   SimDataPos = SimDataPos, 
                   RowsBetaPos = RowsBetaPos, 
                   RowsWPos = RowsWPos, 
                   APos = APos,
                   ID = MyData$ID)

ExpZAG1000_Means <- do.call(cbind, results2)
dim(ExpZAG1000_Means)

#' Each row is now a grid in MyData. And it contains 1000x a ZAG expected value.
#' We can take the 2.5%, 50% and 97.5% quantiles. 

# First the old stuff:
Results <- t(apply(ExpZAG1000_Means, 1, quantile, c(0.025, 0.5, 0.975)))
Results <- data.frame(Results)
head(Results)
names(Results) <- c("SeLo", "Pred", "SeUp") ## now, Pred is the means

# now add the SD
Results$SD <-  apply(ExpZAG1000_Means, 1, sd)
head(Results)

#' Add spatial coordinates to Results.
grid.xy <- subset(MyData, select = c("Xkm", "Ykm"), subset = Survey == "A")
head(grid.xy)
dim(grid.xy)

Results$Xkm <- grid.xy$Xkm
Results$Ykm <- grid.xy$Ykm
head(Results)
dim(Results)


#' And plot the whole thing. I am using the log of Pred as colour scale.
w2 <- ggplot() +
  geom_raster(data = MyData, aes(x = Xkm, y = Ykm, fill = log(MeansPred))) +
  coord_fixed(ratio = 1) + 
  theme_bw() +
  xlab("Xkm") + ylab("Ykm") +
  geom_path(data = fortify(ParkBUPoly.SPutm), 
            aes(x = long, y = lat)) +
  geom_path(data = fortify(ParkZEPoly.SPutm), 
            aes(x = long, y = lat)) + labs("Log")



#' And plot the whole thing. I am using the log of Pred as colour scale.
W2 = ggplot() +
  geom_raster(data = Results, 
              aes(x = Xkm, y = Ykm, fill = (SD))) + # you can take the log or value
  coord_fixed(ratio = 1) + 
  theme_bw() +
  xlab("Xkm") + ylab("Ykm") +
  geom_path(data = fortify(ParkBUPoly.SPutm), 
            aes(x = long, y = lat)) +
  geom_path(data = fortify(ParkZEPoly.SPutm), 
            aes(x = long, y = lat))  + 
  scale_fill_gradient2(name = "",
                       #limits = c(0, 1),
                       #midpoint = 10,  #<--- CHANGE
                       high = "forestgreen", 
                       #mid = "blue", 
                       low = "red",
                       na.value = "transparent") +
  xlim(range(Loc[,1])) + ylim(range(Loc[,2]))
w2


# save it
Bird = "GM"
setwd(paste("~/Gemini_birds_spatstat/Modelling/Output",Bird,sep="/"))
pdf(file = paste(Bird, "FullZAG_MeanPredValues_means", sep="_"), height=8, width=12)
w2
dev.off()



## Now the SD
#' And plot the whole thing. I am using the log of Pred as colour scale.
w3 = ggplot() +
  geom_raster(data = Results, 
              aes(x = Xkm, y = Ykm, fill = log(SD))) +
  coord_fixed(ratio = 1) + 
  theme_bw() +
  xlab("Xkm") + ylab("Ykm") +
  geom_path(data = fortify(ParkBUPoly.SPutm), 
            aes(x = long, y = lat)) +
  geom_path(data = fortify(ParkZEPoly.SPutm), 
            aes(x = long, y = lat))  + 
  scale_fill_gradient2(name = "",
                       #limits = c(0, 1),
                       #midpoint = 10,  #<--- CHANGE
                       high = "blue", 
                       #mid = "blue", 
                       low = "white",
                       na.value = "transparent") +
  xlim(range(Loc[,1])) + ylim(range(Loc[,2]))

w3

# save it
Bird = "GM"
setwd(paste("~/Gemini_birds_spatstat/Modelling/Output",Bird,sep="/"))
pdf(file = paste(Bird, "FullZAG_MeanPredValues_SD", sep="_"), height=8, width=12)
w3
dev.off()









#### OWP EFFECT ####


# Section 12: Simulation study to check for for a windpark effect----


#' Sampling locations:
ggplot() +
  geom_point(data = WD2, 
             aes(x = Xkm, y = Ykm),
             size = 0.5,
             col = grey(0.5)) +
  coord_fixed(ratio = 1) + 
  theme_bw() +
  xlab("Xkm") + ylab("Ykm") +
  geom_path(data = fortify(ParkBUPoly.SPutm), 
            aes(x = long, y = lat)) +
  geom_path(data = fortify(ParkZEPoly.SPutm), 
            aes(x = long, y = lat)) 


#'  1. Define an non-convex area around the study area. 
#'  2. Define this as a polygon with the name: StudyArea.
#'  3. Define:
#'       Area1 as the most left wind park.
#'       Area2 as the space between the two parks.
#'       Area3 as the right wind park.
#'       Area4 as the Area1+Area2+Area3
#'  4. Calculate the average predicted density in each
#'     of these 4 areas. Call this:
#'        ExpZagA1... ExpZagA4
#'  5. Pick a random point within study area.
#'  6. Using this random point, define an area of the same size 
#'     as Area4. Call it: SimArea4..standing for simulated Area4.
#'  7. Check whether SimArea4 falls within StudyArea.
#'  8. If no, go back to 6. If yes, go to 9.
#'  9. Split up SimArea4 into SimArea1, SimArea2, SimArea3.
#' 10. Calculate  the average density for all 4 areas. Call it:
#'      ExpZagSimA1... ExpZagSimA4
#' 11. Repeat steps 4 to 10 for 1000 sets of simulated regression
#'     parameters and spatial-temporal terms.
#' 12. For each 1000 values of SimArea1, SimArea2, SimArea3, SimArea4,
#'     make  a histogram. 
#' 13 Compare to the 1000 ExpZagA1... ExpZagA4. Count how often
#'    ExpZagSimA1 is larger than ExpZagA1, and divide by 1000. That
#'    is your test statistic that assesses whether birds avoid Area1.
#'    Repeat for other areas.




#* Subsection 12.1: Steps 1 and 2: Define an non-convex area around the study area----
library(sf)

#' Put a convex hull around the sampling locations. 
Loc <- WD2[, c("Xkm", "Ykm")]
Loc_sf <- st_as_sf(Loc, coords = c("Xkm", "Ykm"), 
                   crs = 32631)  # Using UTM Zone 31N as an example


ConvexHull_sf <- st_convex_hull(st_union(Loc_sf))

#' Buffer it with 1 km.
StudyArea_sf_buffered <- st_buffer(ConvexHull_sf, dist = 1)
print(StudyArea_sf_buffered)

plot(st_geometry(StudyArea_sf_buffered), border='red', add=FALSE)
plot(st_geometry(Loc_sf), add=TRUE, cex = 0.1)




#' Plot the results.
ggplot() +
  geom_sf(data = StudyArea_sf_buffered, 
          fill = "transparent", 
          color = "red") + 
  theme_minimal() +
  labs(title = "Study area") +
  geom_point(data = WD2, 
             aes(x = Xkm, y = Ykm),
             size = 0.5,
             alpha = 0.5) 




#* Subsection 12.2: Step 3: Define areas Area1...Area4----

#' Area1 is the most left wind park.
#' Area2 is the space between the two parks.
#' Area3 is the right wind park.
#' Area4 as the Area1+Area2+Area3


#' Define Area1, Area2, Area3, Area4


#' Area1 = ZeeEnergie
#' Get the corner locations of ZeeEnergie..in UTM
Loc <- cbind(WindPark.ZE$Xkm, WindPark.ZE$Ykm)

#' Make it a full polygon
idCorners <- c(7, 39, 46, 75)
FourCorners <- Loc[idCorners,]
FourCornersFullPolygon <- rbind(FourCorners,
                                FourCorners[1,])
FourCornersFullPolygon #' First row equals last row


#' Convert to a sf object, and then to a spatial lines object.
line <- st_linestring(as.matrix(FourCornersFullPolygon))
lines <- st_sfc(list(line), crs = 32631) # UTM zone 31
Area1_sf <- st_sf(geometry = lines)

#' Code added later due to some st_contains() errors
#' Convert line to polygon..and then to spatial polygon object
Area1_poly <- st_polygon(list(as.matrix(FourCornersFullPolygon)))
Area1_sf_poly <- st_sf(geometry = st_sfc(Area1_poly), crs = 32631) # UTM zone 31


#' Plot the study area and the spatial polygon for ZeeEnergie (Area1_sf).
st_crs(StudyArea_sf_buffered) <- 32631  # If it's in UTM Zone 31N

ggplot() +
  geom_sf(data = StudyArea_sf_buffered, 
          fill = "transparent", 
          color = "red") + 
  geom_sf(data = Area1_sf,  
          fill = "transparent",
          col = "blue") +
  theme_minimal() +
  labs(title = "Study area") +
  geom_point(data = WD2, 
             aes(x = Xkm, y = Ykm),
             size = 0.1,
             col = grey(0.5),
             alpha = 0.5) 


#' Do the same for Buitengaats. Area3 = Buitengaats
#' Get the corner locations of Buitengaats .....UTM
Loc <- cbind(WindPark.BU$Xkm, WindPark.BU$Ykm)

#' Make it a full polygon
idCorners <- c(7, 38, 46, 75)
FourCorners <- Loc[idCorners,]
FourCornersFullPolygon <- rbind(FourCorners,
                                FourCorners[1,])
FourCornersFullPolygon #' First row equals last row


#' Convert to a sf object, and then to a spatial lines object.
line <- st_linestring(as.matrix(FourCornersFullPolygon))
lines <- st_sfc(list(line), crs = 32631) # UTM zone 31
Area3_sf <- st_sf(geometry = lines)


#' Code added later due to some st_contains() errors
#' Convert line to polygon..and then to spatial polygon object
Area3_poly <- st_polygon(list(as.matrix(FourCornersFullPolygon)))
Area3_sf_poly <- st_sf(geometry = st_sfc(Area3_poly), crs = 32631) # UTM zone 31


#' Plot the study area and the spatial polygon for ZeeEnergie
ggplot() +
  geom_sf(data = StudyArea_sf_buffered, 
          fill = "transparent", 
          color = "red") + 
  geom_sf(data = Area1_sf,  
          fill = "transparent",
          col = "blue") +
  geom_sf(data = Area3_sf,  
          fill = "transparent",
          col = "purple") +
  theme_minimal() +
  labs(title = "Study area") +
  geom_point(data = WD2, 
             aes(x = Xkm, y = Ykm),
             size = 0.1,
             col = grey(0.5),
             alpha = 0.5) 




#' Define Area2: Area in the middle

#' Get the corners from Area1 again
LocZE <- cbind(WindPark.ZE$Xkm, WindPark.ZE$Ykm)
idCornersZE <- c(7, 39, 46, 75)
FourCornersZE <- LocZE[idCornersZE,]
FourCornersFullPolygonZE <- rbind(FourCornersZE,
                                  FourCornersZE[1,])

CornersArea1 <- FourCornersFullPolygonZE
CornersArea1


#' Get the corners from Area3 again
LocBU <- cbind(WindPark.BU$Xkm, WindPark.BU$Ykm)
idCornersBU <- c(7, 38, 46, 75)
FourCornersBU <- LocBU[idCornersBU,]
FourCornersFullPolygonBU <- rbind(FourCornersBU,
                                  FourCornersBU[1,])
CornersArea3 <- FourCornersFullPolygonBU 
CornersArea3


#' Then this is Area2:
CornersArea2 <- rbind(CornersArea1[3,],
                      CornersArea3[4,],
                      CornersArea3[1,],
                      CornersArea1[2,],
                      CornersArea1[3,]) #<- Make it a full polygon
CornersArea2


#' Convert to a sf object, and then to a spatial lines object.
line <- st_linestring(as.matrix(CornersArea2))
lines <- st_sfc(list(line), crs = 32631) # UTM zone 31
Area2_sf <- st_sf(geometry = lines)


#' Code added later due to some st_contains() errors
#' Convert line to polygon..and then to spatial polygon object
Area2_poly <- st_polygon(list(as.matrix(CornersArea2)))
Area2_sf_poly <- st_sf(geometry = st_sfc(Area2_poly), crs = 32631) # UTM zone 31



#' And let's do Area4 (entire area) as well:
CornersArea4 <- rbind(CornersArea1[4,],
                      CornersArea1[3,],
                      CornersArea3[4,],
                      CornersArea3[3,],
                      CornersArea3[2,],
                      CornersArea3[1,],
                      CornersArea1[2,],
                      CornersArea1[1,],
                      CornersArea1[4,]) #<- Make it a full polygon
CornersArea4

#' Double check: Seems to be all ok.
plot(CornersArea4, type = "l")
lines(CornersArea1, col = 2)
lines(CornersArea3, col = 3)
lines(CornersArea2, col = 3)



#' Convert to a sf object, and then to a spatial lines object.
line <- st_linestring(as.matrix(CornersArea4))
lines <- st_sfc(list(line), crs = 32631) # UTM zone 31
Area4_sf <- st_sf(geometry = lines)

#' Code added later due to some st_contains() errors
#' Convert line to polygon..and then to spatial polygon object
Area4_poly <- st_polygon(list(as.matrix(CornersArea4)))
Area4_sf_poly <- st_sf(geometry = st_sfc(Area4_poly), crs = 32631) # UTM zone 31


#' Check CRS. If all of these return 32631 or equivalent, then you 
#' can be confident that they are in the same CRS. 
st_crs(StudyArea_sf_buffered)
st_crs(Area1_sf_poly)
st_crs(Area2_sf_poly)
st_crs(Area3_sf_poly)
st_crs(Area4_sf_poly)





#' Plot the study area and the spatial polygon for areas 1, 2, 3 and 4.
ggplot() +
  geom_sf(data = StudyArea_sf_buffered, 
          fill = "transparent", 
          color = "red") + 
  geom_sf(data = Area1_sf,  
          fill = "transparent",
          col = "blue") +
  geom_sf(data = Area3_sf,  
          fill = "transparent",
          col = "purple") +
  geom_sf(data = Area2_sf,  
          fill = "green",
          col = "green") +
  geom_sf(data = Area4_sf,  
          fill = "yellow",
          col = "yellow") +
  theme_minimal() +
  labs(title = "Study area") +
  geom_point(data = WD2, 
             aes(x = Xkm, y = Ykm),
             size = 0.1,
             col = grey(0.5),
             alpha = 0.5) 



#* Subsection 12.3: Step 4: Predict the ZAG expected values for each area----

#' Calculate the average predicted density in each of these 4 areas. 
#' Call this: ExpZagA1... ExpZagA4

#' We have the predicted values from the ZAG in MyData$Pred.

#' Convert MyData to a sf object.
MyData.sp <- st_as_sf(MyData, 
                      coords = c("Xkm", "Ykm"), 
                      crs = "+proj=utm +zone=31 +north +ellps=WGS84 +units=km  +datum=WGS84")
print(st_crs(MyData.sp))

st_crs(MyData.sp) <- st_crs(Area1_sf_poly)



#' For Area1 to Area4:
#' Determine which points in MyData are contained within each area.
ContainedArea1 <- st_contains(Area1_sf_poly, MyData.sp)
ContainedArea2 <- st_contains(Area2_sf_poly, MyData.sp)
ContainedArea3 <- st_contains(Area3_sf_poly, MyData.sp)
ContainedArea4 <- st_contains(Area4_sf_poly, MyData.sp)


#' Extract indices of the points that are in the study area.
contained_indicesArea1 <- unique(unlist(ContainedArea1))
contained_indicesArea2 <- unique(unlist(ContainedArea2))
contained_indicesArea3 <- unique(unlist(ContainedArea3))
contained_indicesArea4 <- unique(unlist(ContainedArea4))

#' Example:
contained_indicesArea1



#' Plot the study area and the spatial polygon for areas 1, 2, 3.
ggplot() +
  geom_sf(data = StudyArea_sf_buffered, 
          fill = "transparent", 
          color = "red") + 
  geom_sf(data = Area1_sf_poly,  
          fill = "transparent",
          col = "blue") +
  geom_sf(data = Area3_sf_poly,  
          fill = "transparent",
          col = "purple") +
  geom_sf(data = Area2_sf_poly,  
          fill = "green",
          col = "green") +
  geom_sf(data = Area4_sf_poly,  
          fill = "yellow",
          col = "yellow") +
  theme_minimal() +
  labs(title = "Study area") +
  geom_point(data = WD2, 
             aes(x = Xkm, y = Ykm),
             size = 0.05,
             col = grey(0.5),
             alpha = 0.5) +
  geom_point(data = MyData[contained_indicesArea1,], 
             aes(x = Xkm, y = Ykm),
             size = 0.1,
             col = "red",
             alpha = 0.5) +
  geom_point(data = MyData[contained_indicesArea3,], 
             aes(x = Xkm, y = Ykm),
             size = 0.1,
             col = "blue",
             alpha = 0.5) +
  geom_point(data = MyData[contained_indicesArea2,], 
             aes(x = Xkm, y = Ykm),
             size = 0.1,
             col = "yellow",
             alpha = 0.5) 
#' That seems to be ok.


#' And this is a check of area 4.
#' Plot the study area and the spatial polygon for areas 1, 2, 3 and 4.
ggplot() +
  geom_sf(data = StudyArea_sf_buffered, 
          fill = "transparent", 
          color = "red") + 
  geom_sf(data = Area1_sf_poly,  
          fill = "transparent",
          col = "blue") +
  geom_sf(data = Area3_sf_poly,  
          fill = "transparent",
          col = "purple") +
  geom_sf(data = Area2_sf_poly,  
          fill = "green",
          col = "green") +
  geom_sf(data = Area4_sf_poly,  
          fill = "yellow",
          col = "yellow") +
  theme_minimal() +
  labs(title = "Study area") +
  geom_point(data = WD2, 
             aes(x = Xkm, y = Ykm),
             size = 0.05,
             col = grey(0.5),
             alpha = 0.5) +
  geom_point(data = MyData[contained_indicesArea4,], 
             aes(x = Xkm, y = Ykm),
             size = 0.1,
             col = "red",
             alpha = 0.5) 


#' Now get the average expected values for each area.
#' This is for the observed data and the applied model
ExpZagA1 <- mean(MyData[contained_indicesArea1, "Pred"])
ExpZagA2 <- mean(MyData[contained_indicesArea2, "Pred"])
ExpZagA3 <- mean(MyData[contained_indicesArea3, "Pred"])
ExpZagA4 <- mean(MyData[contained_indicesArea4, "Pred"])



#' Support functions
# Function to generate a random point within the bounding box of a given area
generate_random_point <- function(study_area) {
  bb <- st_bbox(study_area)
  random_point <- st_point(c(runif(1, bb['xmin'], bb['xmax']), runif(1, bb['ymin'], bb['ymax'])))
  st_sfc(random_point, crs = st_crs(study_area))
}


# Function to translate a geometry to a new location based on a point
translate_to_point <- function(polygon_sf, new_center) {
  # Extract the current centroid of the polygon
  old_center <- st_coordinates(st_centroid(st_geometry(polygon_sf)))[1,]
  
  # Compute the translation vector
  translation_vector <- matrix(c(new_center["X"] - old_center["X"], new_center["Y"] - old_center["Y"]), ncol = 2)

  # Translate the polygon and return the new object
  new_polygon_sf <- st_geometry(polygon_sf) + translation_vector

  st_sf(geometry = new_polygon_sf, crs = st_crs(polygon_sf))
}







#* Subsection 12.4: Steps 4 to 11 Area 3 (owp) ----

#'  5. Pick a random point within study area.
#'  6. Using this random point, define an area of the same size 
#'     as the selected area.
#'     ----> NOTE: we repeat this 4 times, for Area1-4
#'     ----> If you have a single park instead of a windpark, you can just run it once.

#MyArea <- "Area1" 
#MyArea <- "Area2" 
MyArea <- "Area3" 
#MyArea <- "Area4" 

if (MyArea == "Area1"){
  Area_sf_poly <- Area1_sf_poly
  TestStatistic <- ExpZagA1
  Title <- "ZeeEnergie"
  #' Test statistic per survey
  TestStatPerSurvey <- tapply(MyData[contained_indicesArea1, "Pred"],
                              FUN = mean,
                              INDEX = MyData[contained_indicesArea1, "Survey"])
}

if (MyArea == "Area2"){
  Area_sf_poly <- Area2_sf_poly
  TestStatistic <- ExpZagA2
  Title <- "Middle area"
  #' Test statistic per survey
  TestStatPerSurvey <- tapply(MyData[contained_indicesArea2, "Pred"],
                              FUN = mean,
                              INDEX = MyData[contained_indicesArea2, "Survey"])
  
}

if (MyArea == "Area3"){
  Area_sf_poly <- Area3_sf_poly
  TestStatistic <- ExpZagA3
  Title <- "Buitengaats"
  #' Test statistic per survey
  TestStatPerSurvey <- tapply(MyData[contained_indicesArea3, "Pred"],
                              FUN = mean,
                              INDEX = MyData[contained_indicesArea3, "Survey"])
}

if (MyArea == "Area4"){
  Area_sf_poly <- Area4_sf_poly
  TestStatistic <- ExpZagA4
  Title <- "Entire area"
  #' Test statistic per survey
  TestStatPerSurvey <- tapply(MyData[contained_indicesArea4, "Pred"],
                              FUN = mean,
                              INDEX = MyData[contained_indicesArea4, "Survey"])
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
  #' Begin while loop to attempt placement
  while(attempt <= max_attempts && GoOn) {
    
    #' Generate a random point within StudyArea_sf
    random_point <- generate_random_point(StudyArea_sf_buffered)
    new_center   <- st_coordinates(random_point)[1,]
    new_Area_sf_poly <- translate_to_point(Area_sf_poly, new_center)
    
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
  } 
  
  #' Assess which points from MyData are in the new polygon
  ContainedAreaNEW <- st_contains(new_Area_sf_poly, MyData.sp)
  
  #' Extract indices of the points that are in the new polygon.
  contained_indicesAreaNEW <- unique(unlist(ContainedAreaNEW))
  
  Plot <- FALSE
  if (Plot) {
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


#' 13 Compare to the RB ExpZagA1... ExpZagA4. Count how often
#'    ExpZagSimA1 is larger than ExpZagA1, and divide by 1000. That
#'    is your test statistic that assesses whether birds avoid Area1.
#'    Repeat for other areas.

#' Bayesian p-value to test whether the average in the selected windpark area
#' is larger than the RB simulated one.

mean(ResultsSIM$ExpZag > TestStatistic)

#' ---  OUTPUT INTERPRETATION EXAMPLE ---- 
#' 0.938 for RB Area 3
#' [1] 0.976 for GM Area 3, for example.
#' That means that if we select 2000 random areas in the study area
#' of the same size as the Area 3, then in 97.6% of these selected areas 
#' the mean predicted biomass is larger than in area 3 (buitengaats). 
#' That, of course, is a strong evidence of a windpark effect.


#' We can do the same exercise per survey.
BayesianPValuePerArea <- vector(length = 8)
for (i in 1:8){
  BayesianPValuePerArea[i] <- mean(ExpZagByArea[i,] > TestStatPerSurvey[i])
}
names(BayesianPValuePerArea) <- LETTERS[1:8]
BayesianPValuePerArea

#' ---  OUTPUT INTERPRETATION EXAMPLE ---- 

# RB Area 3
#'A      B      C      D      E      F      G      H 
#'0.2820 0.6335 0.7750 0.6115 0.8240 0.8505 0.9165 0.4505 
#'GM Area 3
#'    A     B     C     D     E     F     G     H 
#0.306 0.552 0.961 0.610 0.839 0.936 0.883 0.925 

#' A 'No windpark effect' translates as a Bayesian p-value of about 0.5.
#' You can see here, that:
#'  - We have a strong avoidance effect in surveys F and G. 
#'  - We have hardly any effects in surveys B, D and H.
#'  - The lower predicted biomass in Survey A.






#* Subsection 12.4: (REPEAT) Steps 4 to 11 Area 4 (all)----


#'  --- Area1-4 are outlines of the OWP, but we have a twin offshore wind park,
#'  consisting of two similarly sized OWP areas, with an almost equal space in between.
#'  so we can run tests on either park by itself (1-2),
#'  or use only the space in between , or both OWPs, or use both + the space in between....

#' Now we repeat the above 3x for the other areas

#MyArea <- "Area1" 
#MyArea <- "Area2" 
#MyArea <- "Area3" 
MyArea <- "Area4" 

if (MyArea == "Area1"){
  Area_sf_poly <- Area1_sf_poly
  TestStatistic <- ExpZagA1
  Title <- "ZeeEnergie"
  #' Test statistic per survey
  TestStatPerSurvey <- tapply(MyData[contained_indicesArea1, "Pred"],
                              FUN = mean,
                              INDEX = MyData[contained_indicesArea1, "Survey"])
}

if (MyArea == "Area2"){
  Area_sf_poly <- Area2_sf_poly
  TestStatistic <- ExpZagA2
  Title <- "Middle area"
  #' Test statistic per survey
  TestStatPerSurvey <- tapply(MyData[contained_indicesArea2, "Pred"],
                              FUN = mean,
                              INDEX = MyData[contained_indicesArea2, "Survey"])
  
}

if (MyArea == "Area3"){
  Area_sf_poly <- Area3_sf_poly
  TestStatistic <- ExpZagA3
  Title <- "Buitengaats"
  #' Test statistic per survey
  TestStatPerSurvey <- tapply(MyData[contained_indicesArea3, "Pred"],
                              FUN = mean,
                              INDEX = MyData[contained_indicesArea3, "Survey"])
}

if (MyArea == "Area4"){
  Area_sf_poly <- Area4_sf_poly
  TestStatistic <- ExpZagA4
  Title <- "Entire area"
  #' Test statistic per survey
  TestStatPerSurvey <- tapply(MyData[contained_indicesArea4, "Pred"],
                              FUN = mean,
                              INDEX = MyData[contained_indicesArea4, "Survey"])
}

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
  
  #' Begin while loop to attempt placement
  while(attempt <= max_attempts && GoOn) {
    
    #' Generate a random point within StudyArea_sf
    random_point <- generate_random_point(StudyArea_sf_buffered)
    new_center   <- st_coordinates(random_point)[1,]
    new_Area_sf_poly <- translate_to_point(Area_sf_poly, new_center)
    
    
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
  } 
  
  #' Assess which points from MyData are in the new polygon
  ContainedAreaNEW <- st_contains(new_Area_sf_poly, MyData.sp)
  
  #' Extract indices of the points that are in the new polygon.
  contained_indicesAreaNEW <- unique(unlist(ContainedAreaNEW))
  
  Plot <- FALSE
  if (Plot) {
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



#* Subsection 12.5 (REPEAT): Step 12: Make a histogram----



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


#* Subsection 12.6: (REPEAT) Step 13: Get a formal test----


mean(ResultsSIM$ExpZag > TestStatistic)

BayesianPValuePerArea <- vector(length = 8)
for (i in 1:8){
  BayesianPValuePerArea[i] <- mean(ExpZagByArea[i,] > TestStatPerSurvey[i])
}
names(BayesianPValuePerArea) <- LETTERS[1:8]
BayesianPValuePerArea






#* Subsection 12.4: (REPEAT): Steps 4 to 11 Area 1 ----

MyArea <- "Area1" 
#MyArea <- "Area2" 
#MyArea <- "Area3" 
#MyArea <- "Area4" 

if (MyArea == "Area1"){
  Area_sf_poly <- Area1_sf_poly
  TestStatistic <- ExpZagA1
  Title <- "ZeeEnergie"
  #' Test statistic per survey
  TestStatPerSurvey <- tapply(MyData[contained_indicesArea1, "Pred"],
                              FUN = mean,
                              INDEX = MyData[contained_indicesArea1, "Survey"])
}

if (MyArea == "Area2"){
  Area_sf_poly <- Area2_sf_poly
  TestStatistic <- ExpZagA2
  Title <- "Middle area"
  #' Test statistic per survey
  TestStatPerSurvey <- tapply(MyData[contained_indicesArea2, "Pred"],
                              FUN = mean,
                              INDEX = MyData[contained_indicesArea2, "Survey"])
  
}

if (MyArea == "Area3"){
  Area_sf_poly <- Area3_sf_poly
  TestStatistic <- ExpZagA3
  Title <- "Buitengaats"
  #' Test statistic per survey
  TestStatPerSurvey <- tapply(MyData[contained_indicesArea3, "Pred"],
                              FUN = mean,
                              INDEX = MyData[contained_indicesArea3, "Survey"])
}

if (MyArea == "Area4"){
  Area_sf_poly <- Area4_sf_poly
  TestStatistic <- ExpZagA4
  Title <- "Entire area"
  #' Test statistic per survey
  TestStatPerSurvey <- tapply(MyData[contained_indicesArea4, "Pred"],
                              FUN = mean,
                              INDEX = MyData[contained_indicesArea4, "Survey"])
}



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
  while(attempt <= max_attempts && GoOn) {
    
    #' Generate a random point within StudyArea_sf
    random_point <- generate_random_point(StudyArea_sf_buffered)
    new_center   <- st_coordinates(random_point)[1,]
    new_Area_sf_poly <- translate_to_point(Area_sf_poly, new_center)
    
    
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



#* Subsection 12.5 : (REPEAT): Step 12: Make a histogram----


ResultsSIM <- data.frame(ExpZag = ExpZagAreaSim[1:NSim])

p <- ggplot() + 
  geom_histogram(data = ResultsSIM,
                 aes(x = ExpZag),
                 alpha = 0.5,
                 color="darkblue", 
                 fill="lightblue",
                 bins = 50) +
  geom_vline(aes(xintercept = TestStatistic), color = "red", lwd=1.1) +
  xlab("Simulated bird density (n/km2)") + ylab("Simulations")+
  ggtitle(paste("Area:",Title, sep=" ")) +
  theme(text = element_text(size = 15))+theme_bw()
p
#' Plot TestStatistic as a vertical line.

## Save
Bird = "GM"
setwd(paste("~/Gemini_birds_spatstat/Modelling/Output",Bird,sep="/"))
pdf(file = paste(Bird, paste("OWPSims", Title, sep=":"), sep="_"), height=8, width=12)
p
dev.off()





#* Subsection 12.6: (REPEAT): Step 13: Get a formal test----


#' 13 Compare to the RB ExpZagA1... ExpZagA4. Count how often
#'    ExpZagSimA1 is larger than ExpZagA1, and divide by 1000. That
#'    is your test statistic that assesses whether birds avoid Area1.
#'    Repeat for other areas.

#' Bayesian p-value to test whether the average in the selected windpark area
#' is larger than the RB simulated one.

mean(ResultsSIM$ExpZag > TestStatistic)

#' We can do the same exercise per survey.
BayesianPValuePerArea <- vector(length = 8)
for (i in 1:8){
  BayesianPValuePerArea[i] <- mean(ExpZagByArea[i,] > TestStatPerSurvey[i])
}
names(BayesianPValuePerArea) <- LETTERS[1:8]
BayesianPValuePerArea


## Now store the results
Bird = "GM"
setwd(paste("~/Gemini_birds_spatstat/Modelling/Output",Bird,sep="/"))

file_conn = file( paste(Bird, Title, "OWP_effect_stats.txt", sep="_"))
writeLines(c(
  Title,
  Bird,
  ">>> Per survey A tm H:",
  BayesianPValuePerArea ,
  ">>> Mean:", 
  mean(ResultsSIM$ExpZag > TestStatistic)
), file_conn)
close(file_conn)




#* Subsection 12.4: (REPEAT) Steps 4 to 11 Area 2 ----

#'  5. Pick a random point within study area.
#'  6. Using this random point, define an area of the same size 
#'     as the selected area.

#MyArea <- "Area1" 
MyArea <- "Area2" 
#MyArea <- "Area3" 
#MyArea <- "Area4" 

if (MyArea == "Area1"){
  Area_sf_poly <- Area1_sf_poly
  TestStatistic <- ExpZagA1
  Title <- "ZeeEnergie"
  #' Test statistic per survey
  TestStatPerSurvey <- tapply(MyData[contained_indicesArea1, "Pred"],
                              FUN = mean,
                              INDEX = MyData[contained_indicesArea1, "Survey"])
}

if (MyArea == "Area2"){
  Area_sf_poly <- Area2_sf_poly
  TestStatistic <- ExpZagA2
  Title <- "Middle area"
  #' Test statistic per survey
  TestStatPerSurvey <- tapply(MyData[contained_indicesArea2, "Pred"],
                              FUN = mean,
                              INDEX = MyData[contained_indicesArea2, "Survey"])
  
}

if (MyArea == "Area3"){
  Area_sf_poly <- Area3_sf_poly
  TestStatistic <- ExpZagA3
  Title <- "Buitengaats"
  #' Test statistic per survey
  TestStatPerSurvey <- tapply(MyData[contained_indicesArea3, "Pred"],
                              FUN = mean,
                              INDEX = MyData[contained_indicesArea3, "Survey"])
}

if (MyArea == "Area4"){
  Area_sf_poly <- Area4_sf_poly
  TestStatistic <- ExpZagA4
  Title <- "Entire area"
  #' Test statistic per survey
  TestStatPerSurvey <- tapply(MyData[contained_indicesArea4, "Pred"],
                              FUN = mean,
                              INDEX = MyData[contained_indicesArea4, "Survey"])
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
  while(attempt <= max_attempts && GoOn) {
    
    #' Generate a random point within StudyArea_sf
    random_point <- generate_random_point(StudyArea_sf_buffered)
    new_center   <- st_coordinates(random_point)[1,]
    new_Area_sf_poly <- translate_to_point(Area_sf_poly, new_center)
    
    
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



#* Subsection 12.5: (REPEAT) Step 12: Make a histogram----

#' 12. For each 1000 values of SimArea1, SimArea2, SimArea3, SimArea4,
#'     make  a histogram. 

ResultsSIM <- data.frame(ExpZag = ExpZagAreaSim[1:NSim])

p <- ggplot() + 
  geom_histogram(data = ResultsSIM,
                 aes(x = ExpZag),
                 alpha = 0.5,
                 color="darkblue", 
                 fill="lightblue",
                 bins = 50) +
  geom_vline(aes(xintercept = TestStatistic), color = "red", lwd=1.1) +
  xlab("Simulated bird density (n/km2)") + ylab("Simulations")+
  ggtitle(paste("Area:",Title, sep=" ")) +
  theme(text = element_text(size = 15))+theme_bw()
p
#' Plot TestStatistic as a vertical line.

## Save
Bird = "GM"
setwd(paste("~/Gemini_birds_spatstat/Modelling/Output",Bird,sep="/"))
pdf(file = paste(Bird, paste("OWPSims", Title, sep=":"), sep="_"), height=8, width=12)
p
dev.off()





#* Subsection 12.6: (REPEAT) Step 13: Get a formal test----

mean(ResultsSIM$ExpZag > TestStatistic)


#' We can do the same exercise per survey.
BayesianPValuePerArea <- vector(length = 8)
for (i in 1:8){
  BayesianPValuePerArea[i] <- mean(ExpZagByArea[i,] > TestStatPerSurvey[i])
}
names(BayesianPValuePerArea) <- LETTERS[1:8]
BayesianPValuePerArea

## Now store the results
Bird = "GM"
setwd(paste("~/Gemini_birds_spatstat/Modelling/Output",Bird,sep="/"))

file_conn = file( paste(Bird, Title, "OWP_effect_stats.txt", sep="_"))
writeLines(c(
  Title,
  Bird,
  ">>> Per survey A tm H:",
  BayesianPValuePerArea ,
  ">>> Mean:", 
  mean(ResultsSIM$ExpZag > TestStatistic)
), file_conn)
close(file_conn)



#* Subsection 12.4: (REPEAT) Steps 4 to 11 Area 3 ----

#'  5. Pick a random point within study area.
#'  6. Using this random point, define an area of the same size 
#'     as the selected area.

#MyArea <- "Area1" 
#MyArea <- "Area2" 
MyArea <- "Area3" 
#MyArea <- "Area4" 

if (MyArea == "Area1"){
  Area_sf_poly <- Area1_sf_poly
  TestStatistic <- ExpZagA1
  Title <- "ZeeEnergie"
  #' Test statistic per survey
  TestStatPerSurvey <- tapply(MyData[contained_indicesArea1, "Pred"],
                              FUN = mean,
                              INDEX = MyData[contained_indicesArea1, "Survey"])
}

if (MyArea == "Area2"){
  Area_sf_poly <- Area2_sf_poly
  TestStatistic <- ExpZagA2
  Title <- "Middle area"
  #' Test statistic per survey
  TestStatPerSurvey <- tapply(MyData[contained_indicesArea2, "Pred"],
                              FUN = mean,
                              INDEX = MyData[contained_indicesArea2, "Survey"])
  
}

if (MyArea == "Area3"){
  Area_sf_poly <- Area3_sf_poly
  TestStatistic <- ExpZagA3
  Title <- "Buitengaats"
  #' Test statistic per survey
  TestStatPerSurvey <- tapply(MyData[contained_indicesArea3, "Pred"],
                              FUN = mean,
                              INDEX = MyData[contained_indicesArea3, "Survey"])
}

if (MyArea == "Area4"){
  Area_sf_poly <- Area4_sf_poly
  TestStatistic <- ExpZagA4
  Title <- "Entire area"
  #' Test statistic per survey
  TestStatPerSurvey <- tapply(MyData[contained_indicesArea4, "Pred"],
                              FUN = mean,
                              INDEX = MyData[contained_indicesArea4, "Survey"])
}



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

  #' Begin while loop to attempt placement
  while(attempt <= max_attempts && GoOn) {
    
    #' Generate a random point within StudyArea_sf
    random_point <- generate_random_point(StudyArea_sf_buffered)
    new_center   <- st_coordinates(random_point)[1,]
    new_Area_sf_poly <- translate_to_point(Area_sf_poly, new_center)
    
    
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



#* Subsection 12.5: (REPEAT) Step 12: Make a histogram----

#' 12. For each 1000 values of SimArea1, SimArea2, SimArea3, SimArea4,
#'     make  a histogram. 

ResultsSIM <- data.frame(ExpZag = ExpZagAreaSim[1:NSim])

p <- ggplot() + 
  geom_histogram(data = ResultsSIM,
                 aes(x = ExpZag),
                 alpha = 0.5,
                 color="darkblue", 
                 fill="lightblue",
                 bins = 50) +
  geom_vline(aes(xintercept = TestStatistic), color = "red", lwd=1.1) +
  xlab("Simulated bird density (n/km2)") + ylab("Simulations")+
  ggtitle(paste("Area:",Title, sep=" ")) +
  theme(text = element_text(size = 15))+theme_bw()
p
#' Plot TestStatistic as a vertical line.

## Save
Bird = "GM"
setwd(paste("~/Gemini_birds_spatstat/Modelling/Output",Bird,sep="/"))
pdf(file = paste(Bird, paste("OWPSims", Title, sep=":"), sep="_"), height=8, width=12)
p
dev.off()





#* Subsection 12.6: (REPEAT): Step 13: Get a formal test----


mean(ResultsSIM$ExpZag > TestStatistic)

#' We can do the same exercise per survey.
BayesianPValuePerArea <- vector(length = 8)
for (i in 1:8){
  BayesianPValuePerArea[i] <- mean(ExpZagByArea[i,] > TestStatPerSurvey[i])
}
names(BayesianPValuePerArea) <- LETTERS[1:8]
BayesianPValuePerArea


## Now store the results
Bird = "GM"
setwd(paste("~/Gemini_birds_spatstat/Modelling/Output",Bird,sep="/"))

file_conn = file( paste(Bird, Title, "OWP_effect_stats.txt", sep="_"))
writeLines(c(
  Title,
  Bird,
  "--> Per survey A tm H:",
  BayesianPValuePerArea ,
  "--> Mean:", 
  mean(ResultsSIM$ExpZag > TestStatistic)
), file_conn)
close(file_conn)


#* Subsection 12.4 (REPEAT): Steps 4 to 11 Area 4 ----


#MyArea <- "Area1" 
#MyArea <- "Area2" 
#MyArea <- "Area3" 
MyArea <- "Area4" 

if (MyArea == "Area1"){
  Area_sf_poly <- Area1_sf_poly
  TestStatistic <- ExpZagA1
  Title <- "ZeeEnergie"
  #' Test statistic per survey
  TestStatPerSurvey <- tapply(MyData[contained_indicesArea1, "Pred"],
                              FUN = mean,
                              INDEX = MyData[contained_indicesArea1, "Survey"])
}

if (MyArea == "Area2"){
  Area_sf_poly <- Area2_sf_poly
  TestStatistic <- ExpZagA2
  Title <- "Middle area"
  #' Test statistic per survey
  TestStatPerSurvey <- tapply(MyData[contained_indicesArea2, "Pred"],
                              FUN = mean,
                              INDEX = MyData[contained_indicesArea2, "Survey"])
  
}

if (MyArea == "Area3"){
  Area_sf_poly <- Area3_sf_poly
  TestStatistic <- ExpZagA3
  Title <- "Buitengaats"
  #' Test statistic per survey
  TestStatPerSurvey <- tapply(MyData[contained_indicesArea3, "Pred"],
                              FUN = mean,
                              INDEX = MyData[contained_indicesArea3, "Survey"])
}

if (MyArea == "Area4"){
  Area_sf_poly <- Area4_sf_poly
  TestStatistic <- ExpZagA4
  Title <- "Entire area"
  #' Test statistic per survey
  TestStatPerSurvey <- tapply(MyData[contained_indicesArea4, "Pred"],
                              FUN = mean,
                              INDEX = MyData[contained_indicesArea4, "Survey"])
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

  #' Begin while loop to attempt placement
  while(attempt <= max_attempts && GoOn) {
    
    #' Generate a random point within StudyArea_sf
    random_point <- generate_random_point(StudyArea_sf_buffered)
    new_center   <- st_coordinates(random_point)[1,]
    new_Area_sf_poly <- translate_to_point(Area_sf_poly, new_center)
    
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
  } 
  
  #' Assess which points from MyData are in the new polygon
  ContainedAreaNEW <- st_contains(new_Area_sf_poly, MyData.sp)
  
  #' Extract indices of the points that are in the new polygon.
  contained_indicesAreaNEW <- unique(unlist(ContainedAreaNEW))
  
  Plot <- FALSE
  if (Plot) {
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



#* Subsection 12.5:(REPEAT) Step 12: Make a histogram----

#' 12. For each 1000 values of SimArea1, SimArea2, SimArea3, SimArea4,
#'     make  a histogram. 

ResultsSIM <- data.frame(ExpZag = ExpZagAreaSim[1:NSim])

p <- ggplot() + 
  geom_histogram(data = ResultsSIM,
                 aes(x = ExpZag),
                 alpha = 0.5,
                 color="darkblue", 
                 fill="lightblue",
                 bins = 50) +
  geom_vline(aes(xintercept = TestStatistic), color = "red", lwd=1.1) +
  xlab("Simulated bird density (n/km2)") + ylab("Simulations")+
  ggtitle(paste("Area:",Title, sep=" ")) +
  theme(text = element_text(size = 15))+theme_bw()
p
#' Plot TestStatistic as a vertical line.

## Save
Bird = "GM"
setwd(paste("~/Gemini_birds_spatstat/Modelling/Output",Bird,sep="/"))
pdf(file = paste(Bird, paste("OWPSims", Title, sep=":"), sep="_"), height=8, width=12)
p
dev.off()


#* Subsection 12.6: (REPEAT)Step 13: Get a formal test----

#' 13 Compare to the RB ExpZagA1... ExpZagA4. Count how often
#'    ExpZagSimA1 is larger than ExpZagA1, and divide by 1000. That
#'    is your test statistic that assesses whether birds avoid Area1.
#'    Repeat for other areas.

#' Bayesian p-value to test whether the average in the selected windpark area
#' is larger than the RB simulated one.

mean(ResultsSIM$ExpZag > TestStatistic)

#' EXAMPLE OUTPUT interpretation:
#' 0.938 for RB Area 3
#' [1] 0.976 for GM Area 3, 0.998 Area 4, 0.938 Area 2 (center)

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

#' Now store the results
Bird = "GM"
setwd(paste("~/Gemini_birds_spatstat/Modelling/Output",Bird,sep="/"))
file_conn = file( paste(Bird, Title, "OWP_effect_stats.txt", sep="_"))
writeLines(c(
  Title,
  Bird,
  ">>> Per survey A tm H:",
  BayesianPValuePerArea ,
  ">>> Mean:", 
  mean(ResultsSIM$ExpZag > TestStatistic)
), file_conn)
close(file_conn)


#' For my random seed, I get 0.938 for area 3 (buitengaats).
#' That means that if we select 2000 random areas in the study area
#' of the same size as the Area 3, then in 93.8% of these selected areas 
#' the mean predicted biomass is larger than in area 3 (buitengaats). 
#' That, of course, is a strong evidence of a windpark effect.


## Store RData
save.image("GM_V4_ZAG.RData")


#### Loop for testing OWP effect in buffer layers radii around OWP ####

Bird = "GM"
setwd(paste("~/Gemini_birds_spatstat/Modelling/Output",Bird,sep="/"))
load("GM_V4_ZAG.RData")

for(rad in 1:10){
  # inner:
  Area4_sf_buffered <- st_buffer(Area4_sf_poly, dist = rad-1) # this is Area 4 with a buffer of dist
  # outer:
  Area4_sf_buffered2 <- st_buffer(Area4_sf_buffered, dist = 1) # this is Area 4 with a buffer of dist
  
  #  We can take Area4_sf_buffered
  ## where dist = rad - 1
  ## so that you get boundaries of : 0.0001-1 km around the OWF, 1-2 km, 2-3km etc. buffer zones.
  ## but then you have to use Area4_st_buffered as your basis instead of Area4_sf_poly
  ## for looping purposes
  
  #' Step 1.
  #' Create a buffer of X km around Area4
  #' Use a buffer of 1 km. You can change this in the dist argument
  Area4_sf_bufferedX <- st_buffer(Area4_sf_buffered, dist = rad-1)
  Area4_sf_bufferedY <- st_buffer(Area4_sf_buffered, dist = rad)
  
  #' Step 2:
  #' Calculate difference Area5 = Area4Buffer - Area4
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
  #' Use a buffer of 1 km. You can change this radius in the "dist" argument

  
  #* Subsection 12.x: Step 4: Predict the ZAG expected values for each area----
  
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

  
  #' Support functions
  # Function to generate a random point within the bounding box of a given area
  generate_random_point <- function(study_area) {
    bb <- st_bbox(study_area)
    random_point <- st_point(c(runif(1, bb['xmin'], bb['xmax']), runif(1, bb['ymin'], bb['ymax'])))
    st_sfc(random_point, crs = st_crs(study_area))
  }
  
  # Function to translate a geometry to a new location based on a point
  translate_to_point <- function(polygon_sf, new_center) {
    # Extract the current centroid of the polygon
    old_center <- st_coordinates(st_centroid(st_geometry(polygon_sf)))[1,]
    # Compute the translation vector
    translation_vector <- matrix(c(new_center["X"] - old_center["X"], new_center["Y"] - old_center["Y"]), ncol = 2)
    # Translate the polygon and return the new object
    new_polygon_sf <- st_geometry(polygon_sf) + translation_vector
    st_sf(geometry = new_polygon_sf, crs = st_crs(polygon_sf))
  }
  

  MyArea <- "Area5" 
  
  if (MyArea == "Area5"){
    Area_sf_poly.out <- Area4_sf_bufferedY 
    Area_sf_poly.in <- Area4_sf_bufferedX 
    Area_sf_poly <- Area4_sf_bufferedY
    Title <- "Buffer"
    TestStatPerSurvey <- tapply(MyData[contained_indicesArea5, "Pred"],
                                FUN = mean,
                                INDEX = MyData[contained_indicesArea5, "Survey"])
  }
  
  #' To randomly place a new area within a StudyArea_sf while
  #'ensuring it doesn't intersect with the existing Area_sf_poly, we 
  #'use the following approach:
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
    
    #' Begin while loop to attempt placement
    while(attempt <= max_attempts && GoOn) {
      
      #' Generate a random point within StudyArea_sf
      random_point <- generate_random_point(StudyArea_sf_buffered)
      new_center   <- st_coordinates(random_point)[1,]
      
      ## To avoid problems with the weird buffer shapes and random sampling
      ## we get the area of the buffer in question with [rad]
      ## then generate a polygon with the same surface area

      a <- as.numeric(st_area(Area_sf_poly.out) - st_area(Area_sf_poly.in))    ## surface area of Area5 with rad [rad]
      ww <- sqrt(a)/2  ## Widths of buffers needed to produce desired areas    
      xmin = as.numeric(new_center[1] - ww)
      xmax = as.numeric(new_center[1] + ww)
      ymin = as.numeric(new_center[2] - ww)
      ymax = as.numeric(new_center[2] + ww)
      my_buffer_box = data.frame(cbind(x = c(xmin, xmax, xmax, xmin), y=c(ymin, ymin, ymax, ymax)))
      
      #' Convert the box to a sf object.
      MyBox.sp <- st_as_sf(my_buffer_box, 
                           coords = c("x", "y"), 
                           crs = "+proj=utm +zone=31 +north +ellps=WGS84 +units=km  +datum=WGS84")
      #print(st_crs(MyBox.sp))
      st_crs(MyBox.sp) <- st_crs(StudyArea_sf_buffered)
      new_Area_sf_poly <- st_geometry(MyBox.sp, crs=crs(StudyArea_sf_buffered))
      
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
    } 
    
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
  #' Plots TestStatistic as a vertical line.
  
  
  #* Subsection 12.6: Step 13: Get a formal test for OWP effect----

  #' 13 Compare to the NSim ExpZagA1... ExpZagA5. Count how often
  #'    ExpZagSimA1 is larger than ExpZagA1, and divide by 1000. That
  #'    is your test statistic that assesses whether birds avoid Area1.
  #'    Repeat for other areas.
  #' Generate a "Bayesian p-value" to test whether the average in the selected windpark area
  #' is larger than the NSim simulated one.
  
  mean(ResultsSIM$ExpZag > TestStatistic)
  
  #' Interpretation similar as for the OWP itself
  BayesianPValuePerArea <- vector(length = 8)
  for (i in 1:8){
    BayesianPValuePerArea[i] <- mean(ExpZagByArea[i,] > TestStatPerSurvey[i])
  }
  names(BayesianPValuePerArea) <- LETTERS[1:8]
  BayesianPValuePerArea
  
  #' a Bayesian p-value of about 0.5 translated to 'No windpark effect' 
  #' You can see here, that:
  #'  - We have a strong avoidance effect in surveys F and G. 
  #'  - We have hardly any effects in surveys B, D and H.
  #'  - The lower predicted biomass in Survey A.
  
  ## Store the results
  Bird = "GM"
  setwd(paste("~/Gemini_birds_spatstat/Modelling/Output",Bird,sep="/"))
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

