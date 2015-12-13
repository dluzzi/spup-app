## Used for testing only
## Creates an obejct of class "simulation"
## This is used as an input for the plotuncert function
library(raster)

# Create Class ------------------------------------------------------------

# Create "Simulations" class
setClass("Simulations", slots =
           c(Realisations="RasterBrick",
             Mean = "RasterLayer",
             Standard.Deviation = "RasterLayer")
           )


# Load data ---------------------------------------------------------------

# Initialise rasterbrick
zlatibor.brick <- brick()

# Load data to create object
for (i in 1:100){
  input <- paste("/Users/damianoluzzi/Desktop/Scripting/spup/data/zlatibor_dem_simulations/DEMsim", i, ".asc", sep = "")
  DEM <- raster(input)
  zlatibor.brick <- addLayer(zlatibor.brick, DEM)
}

# Convert stack to brick
zlatibor.brick <- brick(zlatibor.brick)

# Calculate mean and sd
std<-calc(zlatibor.brick, fun = sd, na.rm = T)
mean <- mean(zlatibor.brick, na.rm = T)


# Create object of class Simulations --------------------------------------

simulations <- new("Simulations", Realisations = zlatibor.brick, Mean = mean,
                   Standard.Deviation = std)
