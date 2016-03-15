## Used for testing only
## Creates an obejct of class "simulation"

library(raster)

# Create Class ------------------------------------------------------------

# Create "Simulations" class
setClass("Simulations", slots =
           c(Realisations="RasterBrick",
             Mean = "RasterLayer",
             Standard.Deviation = "RasterLayer",
             Most.Likely.Class = "RasterLayer",
             Class.Probabilities = "RasterBrick",
             Quantiles = "RasterBrick")
           )


# Load data ---------------------------------------------------------------

# Initialise rasterbrick
zlatibor.brick <- brick()

# Load data to create object
for (i in 1:100){
  #input <- Insert directory
  #input <- paste("D:/DamianoLuzzi-Thesis-DO-NOT-REMOVE/spup/data/zlatibor_dem_simulations/DEMsim", i, ".asc", sep = "")
  DEM <- raster(input)
  zlatibor.brick <- addLayer(zlatibor.brick, DEM)
}

# Convert stack to brick
zlatibor.brick <- brick(zlatibor.brick)

# Calculate mean, sd and quantiles
std<-calc(zlatibor.brick, fun = sd, na.rm = T)
mean <- mean(zlatibor.brick, na.rm = T)
quantiles <- calc(zlatibor.brick, fun = function(x) {quantile(x,
                    probs = c(.05,.25, .5, .75, .95),na.rm=TRUE)} )


# Create object of class Simulations --------------------------------------

simulations <- new("Simulations", Realisations = zlatibor.brick, Mean = mean,
                   Standard.Deviation = std, Quantiles = quantiles)
