#' # from https://github.com/mhpob/TelemetryR/blob/master/R/CBinterp.R# Interpolate variables over the Chesapeake Bay
#' 
#' \code{CBinterp} interpolates Lat-Long georeferenced data across the surface
#' of the Chesapeake.
#' 
#' This function converts long-lat data to UTM-referenced data, interpolates
#' across a grid of the required resolution, and returns the interpolated values
#' as long-lat-referenced data.
#' 
#' @param data Data frame or Numeric. Vector of water quality values.
#' @param coordinates Numeric. Vector of Long/Lat locations of provided data.
#' @param res Numeric. Resolution of the desired interpolated values in
#'    kilometers.
#' @return This function returns an object of class \code{SpatialPointsDataFrame}.
#'    Data slot contains columns of longitude, latitude, interpolated value at
#'    that location, and the standard error of the interpolated value.
#' @seealso \code{\link{chesapeake}}
#' @export
#' @examples
#' coords <- matrix(c(seq(-77.4, -75.5, length.out = 1000),
#'                         seq(36.7, 39.4, length.out = 1000)), ncol = 2)
#' coords <- apply(coords, 2, sample, size = 100)
#' fake.data <- rnorm(100, 15, 2)
#' resolution <- 2
#' interpolated <- CBinterp(data = fake.data, coordinates = coords,
#'                          res = resolution)



#CBinterp <- function(data, coordinates, res = 2){
# load packages ----------------------------------------------------------------
library(sp)
library(tidyverse)
library(gdistance)
library(SpatialTools) 
options(stringsAsFactors = F)
# read in the data -------------------------------------------------------------
load("./Data/chesapeake_salinity_data.rda") #spatial CB data (whole bay)
#read in some data
water_qual_dat <- read.csv("./Data/CBF_water_qual/CBSeg_TWQM/CBSeg2003_Main_Jan1990toDec1994.csv")
# manipulate data --------------------------------------------------------------
water_qual_dat$SampleDate <- as.Date(water_qual_dat$SampleDate, format = "%m/%d/%Y")

sal_1990 <- water_qual_dat %>% 
            filter(Parameter == "SALINITY") %>% #just get salinity
            filter(Layer == "S ") %>% #just use surface
            mutate(Year = lubridate::year(SampleDate)) %>% #make a year column
            filter(Year == 1990)
#start with these for now.
            
data <- sal_1990$MeasureValue
coordinates <- data.frame(lon = sal_1990$Longitude, lat = sal_1990$Latitude)
#(Will probably want to take some of the points out??)
res <- 2
plot(chesapeake) #just to see.

# read in coordinates 
    # pb <- txtProgressBar(max = 8, style = 3)
if(is.data.frame(data) == F) data <- as.data.frame(data)

water_qual <- sp::SpatialPointsDataFrame(coords = coordinates,
    data = data,
    proj4string = sp::CRS('+proj=longlat'))
water_qual <- sp::spTransform(water_qual,
    sp::CRS('+proj=utm +zone=18 +datum=NAD83 +units=km'))
water_qual@data <- water_qual@data %>%
    dplyr::mutate(reasting = res * round(water_qual@coords[, 1] / res),
        rnorthing = res * round(water_qual@coords[, 2] / res)) %>%
    dplyr::group_by(reasting, rnorthing) %>%
    dplyr::summarize(median = median(data)) %>% #take the seasonal median (may not want to do?)
    as.data.frame()
water_qual@coords <- as.matrix(water_qual@data[, c('reasting','rnorthing')])

# setTxtProgressBar(pb, 1)

# Make grid to interpolate over, make into spatial object with same
# projection as shapefile. Use SpatialPixels to more-easily convert to
# raster later.
grid <- expand.grid(
    seq(min(water_qual@data$reasting), max(water_qual@data$reasting), res),
    seq(min(water_qual@data$rnorthing), max(water_qual@data$rnorthing), res))
grid <- sp::SpatialPixelsDataFrame(points = grid[, 1:2], data = grid,
    tolerance = 0.99,
    proj4string =
        sp::CRS('+proj=utm +zone=18 +datum=NAD83 +units=km'))

# Select points that lie within water and label them as such.
grid_water <- grid[chesapeake,]
grid_water$water <- 'YES'

# Label whether the points are within water, make it harder to interpolate over
# land (i.e., conductance of near-0 (land) v near-1 (water)).
grid@data <- grid@data %>%
    dplyr::left_join(grid_water@data, by = c('Var1', 'Var2')) %>%
    dplyr::mutate(water = ifelse(is.na(water), 'NO', water),
        cond = ifelse(water == 'NO', 0.01, 0.99)) %>%
    dplyr::select(cond)
## Create a raster of data
ches.ras <- raster::raster(grid, layer = 1)

# setTxtProgressBar(pb, 2)

## Create transition matrix that represets a pairwise product of
## cells' conductance
ches.trans <- gdistance::transition(ches.ras, function(x) x[1] * x[2], 8)
ches.trans <- gdistance::geoCorrection(ches.trans)

# calculate the least- cost distance between points.
dist <- gdistance::costDistance(ches.trans, water_qual)
dist <- as.matrix(as.dist(dist, diag = T, upper = T)) #transition matrix.

# setTxtProgressBar(pb, 3)

## Interpolation steps
# Use transition matrix to calculate corrected distances (as the fish swim)
pred.dist <- gdistance::costDistance(ches.trans, grid_water)
pred.dist <- as.matrix(as.dist(pred.dist, diag = TRUE, upper = TRUE))

# setTxtProgressBar(pb, 4)

# Calculate distances between observed and predicted values
op.dist <- gdistance::costDistance(ches.trans, water_qual, grid_water)
op.dist <- as.matrix(op.dist, diag = TRUE, upper = TRUE)

# setTxtProgressBar(pb, 5)

# Fit geostatistical model for the data
vg <- geoR::variog(coords = water_qual@data[, c('reasting','rnorthing')],
    data = water_qual@data[, c('reasting','rnorthing', 'median')], #note: may need to change median name if I don't want to use median values!
    max.dis = 600, dists.mat = dist, messages = F)
#This calculates the empirical variogram: this can be used? a modeled variogram
#is not necessary? To me it seems like it should be... (KD)
#likfit() can be used to estimate variogram model parameters
# Variogram model examples -------------------------------------------------
# from http://leg.ufpr.br/geoR/geoRdoc/geoRintro.html
# # Fitting models with nugget fixed to zero
# ml <- likfit(s100, ini = c(1,0.5), fix.nugget = T)
# reml <- likfit(s100, ini = c(1,0.5), fix.nugget = T, method = "RML")
# ols <- variofit(bin1, ini = c(1,0.5), fix.nugget = T, weights="equal")
# wls <- variofit(bin1, ini = c(1,0.5), fix.nugget = T)
# 
# # Fitting models with a fixed value for the nugget
# ml.fn <- likfit(s100, ini = c(1,0.5), fix.nugget = T, nugget = 0.15)
# reml.fn <- likfit(s100, ini = c(1,0.5), fix.nugget = T, nugget = 0.15, method = "RML")
# ols.fn <- variofit(bin1,ini = c(1,0.5), fix.nugget = T, nugget = 0.15, weights="equal")
# wls.fn <- variofit(bin1, ini = c(1,0.5), fix.nugget = T, nugget = 0.15)
# 
# # Fitting models estimated nugget
# ml.n <- likfit(s100, ini = c(1,0.5), nug = 0.5)
# reml.n <- likfit(s100, ini = c(1,0.5), nug = 0.5, method = "RML")
# ols.n <- variofit(bin1, ini = c(1,0.5), nugget=0.5, weights="equal")
# wls.n <- variofit(bin1, ini = c(1,0.5), nugget=0.5)
# 
# # Now, plotting fitted models against empirical variogram
# par(mfrow = c(1,3))
# plot(bin1, main = expression(paste("fixed ", tau^2 == 0)))
# lines(ml, max.dist = 1)
# lines(reml, lwd = 2, max.dist = 1)
# lines(ols, lty = 2, max.dist = 1)
# lines(wls, lty = 2, lwd = 2, max.dist = 1)
# legend(0.5, 0.3, legend=c("ML","REML","OLS","WLS"),lty=c(1,1,2,2),lwd=c(1,2,1,2), cex=0.7)
# 
# plot(bin1, main = expression(paste("fixed ", tau^2 == 0.15)))
# lines(ml.fn, max.dist = 1)
# lines(reml.fn, lwd = 2, max.dist = 1)
# lines(ols.fn, lty = 2, max.dist = 1)
# lines(wls.fn, lty = 2, lwd = 2, max.dist = 1)
# legend(0.5, 0.3, legend=c("ML","REML","OLS","WLS"), lty=c(1,1,2,2), lwd=c(1,2,1,2), cex=0.7)
# 
# plot(bin1, main = expression(paste("estimated  ", tau^2)))
# lines(ml.n, max.dist = 1)
# lines(reml.n, lwd = 2, max.dist = 1)
# lines(ols.n, lty = 2, max.dist = 1)
# lines(wls.n, lty =2, lwd = 2, max.dist = 1)
# legend(0.5, 0.3, legend=c("ML","REML","OLS","WLS"), lty=c(1,1,2,2), lwd=c(1,2,1,2), cex=0.7)
# 
# par(par.ori)
# Ordinary Kriging -----------------------------------------------------------
# KD thinks that this is a better idea to do, because it is simpler.

# 
# UK method----------------------------------------------------------------
# ML fit (KD SAYS Possible errors? not sure if its a problem.)
# 
# Estimates covariance parameters of spatial covariance functions.
    invisible(capture.output( # I don't know that I want these
        vpar <- SpatialTools::maxlik.cov.sp(as.matrix(cbind(1,
            water_qual@data[, c('reasting','rnorthing')])),
            water_qual@data[, 'median'],
            coords = as.matrix(water_qual@data[, c('reasting','rnorthing')]),
            sp.type = "matern", range.par = 600, error.ratio = 0.5,
            D = dist, reml = T)
    ))
    # need to check the convergence code: (what to do if this did not converge)
    if(vpar$convergence != 0){
        print("Did not converge.")
    }
    
    # Define spatial structure of prediction matrix from fitted spatial model, vpar.
    V0 <- SpatialTools::cov.sp(coords = as.matrix(
        water_qual@data[, c('reasting','rnorthing')]),
        sp.type = "matern", sp.par = vpar$sp.par,
        error.var = vpar$error.var, smoothness = vpar$smoothness,
        finescale.var = 0,
        pcoords = as.matrix(grid_water@data[,1:2]),
        D = dist, Dp = pred.dist, Dop = op.dist)
    
    # setTxtProgressBar(pb, 6)
    
    # Apply spatial structure nd model to predict values using universal kriging.
    # ohter options could be krige.sk (simple kriging), krige.ok (ordinary kriging)
    # Maybe ask Mike if he knows why universal kriging was chosen?
    krige <- SpatialTools::krige.uk(water_qual@data[, 'median'],
        V = V0$V, Vop = V0$Vop, Vp = V0$Vp,
        X = as.matrix(cbind(1,
            water_qual@data[, c('reasting','rnorthing')])), #the column of 1's means an intercept is desired.
        Xp = as.matrix(cbind(1, grid_water@data[,1:2])), nsim = 0) #no conditional simulation is done because nsim < 1.
    
    # setTxtProgressBar(pb, 7)
    
    grid_water[['value']] <- krige$pred
    grid_water[['se']] <- krige$mspe #mean squared prediction error of the responses.
    #KD: I think this is actually the variance (if the estimator is unbiased),
    #not the standard error. The root mean squared error (sq root of this value),
    #for an unbiased estimator, is the standard error. (not that it matters
    # too much for my application.
    
    
    # transform  grid_water back to lon/lat
    # # Maybe don't want to do this? (or only do to use observations.)
    # suppressWarnings(
    #     grid_water <- sp::spTransform(grid_water, sp::CRS('+proj=longlat'))
    # )
    # #this following part prevents us from being able to plot, so probably don't want.
    grid_water@data <- cbind(grid_water@coords,
        grid_water@data[, c('value', 'se')])

    # setTxtProgressBar(pb, 8)
    # close(pb)
    # grid_water #included at end to return from function
    # 
    # 
##Try plotting functions------------------------------------------------------
plot(grid_water["value"])
plot(chesapeake, add = T) # could also potentially use a bay map that does not have any 
#segments, or just use NOAA codes if restricting to data within MD (Not sure if 
# we want to do this or not?)
# 
plot(grid_water["se"]) #not actually se, variance. Suspiciously small, not sure why.
# does this mean the algorithim may not have converged?
# 
# 
# Questions and further code to devlop -----------------------------------------

# Questions for Mike O'Brien
# A sample (empirical) variogram is used as the basis for kriging: is this valid?
# Doesn't a modeled one normally need to be used?
# Why was ordinary Kriging chosen? Is there a good reason for it over other interpolation
# methods?
# Is there any reason to be concern about a ML estimate not being found (i.e, the
# algorithim did not converge?)
# Why is the "se" (actually mean squared error, or variance if the estimator is
# unbiased) so small? 

# Next steps:
#  1. Examine data (is most of the bay sampled simultaneously every 2 weeks?)
#  2. Use this data examination do inform what values should be interpolated together.
#  3. modify code so that it is kriging the way I am interested in (make into a function)
#  4. Create a new function that pulls salinity values of interest from the kriging
#  5. Results (compare with oyster data, perhaps?)
#  6. Average these values (or do some sort of weighted average incorporating the
#        estimates of uncertainty?)

