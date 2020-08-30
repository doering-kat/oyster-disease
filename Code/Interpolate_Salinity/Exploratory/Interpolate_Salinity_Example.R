# from https://github.com/mhpob/TelemetryR/blob/master/R/CBinterp.R
# 
#' Interpolate variables over the Chesapeake Bay
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

CBinterp <- function(data, coordinates, res = 2){
    pb <- txtProgressBar(max = 8, style = 3)
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
        dplyr::summarize(median = median(data)) %>%
        as.data.frame()
    water_qual@coords <- as.matrix(water_qual@data[, c('reasting','rnorthing')])
    
    setTxtProgressBar(pb, 1)
    
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
    
    setTxtProgressBar(pb, 2)
    
    ## Create transition matrix that represets a pairwise product of
    ## cells' conductance
    ches.trans <- gdistance::transition(ches.ras, function(x) x[1] * x[2], 8)
    ches.trans <- gdistance::geoCorrection(ches.trans)
    
    dist <- gdistance::costDistance(ches.trans, water_qual)
    dist <- as.matrix(as.dist(dist, diag = T, upper = T))
    
    setTxtProgressBar(pb, 3)
    
    ## Interpolation steps
    # Use transition matrix to calculate corrected distances (as the fish swim)
    pred.dist <- gdistance::costDistance(ches.trans, grid_water)
    pred.dist <- as.matrix(as.dist(pred.dist, diag = TRUE, upper = TRUE))
    
    setTxtProgressBar(pb, 4)
    
    # Calculate distances between observed and predicted values
    op.dist <- gdistance::costDistance(ches.trans, water_qual, grid_water)
    op.dist <- as.matrix(op.dist, diag = TRUE, upper = TRUE)
    
    setTxtProgressBar(pb, 5)
    
    # Fit geostatistical model for the data
    vg <- geoR::variog(coords = water_qual@data[, c('reasting','rnorthing')],
        data = water_qual@data[, c('reasting','rnorthing', 'median')],
        max.dis = 600, dists.mat = dist, messages = F)
    # ML fit
    invisible(capture.output(
        vpar <- SpatialTools::maxlik.cov.sp(as.matrix(cbind(1,
            water_qual@data[, c('reasting','rnorthing')])),
            water_qual@data[, 'median'],
            coords = as.matrix(water_qual@data[, c('reasting','rnorthing')]),
            sp.type = "matern", range.par = 600, error.ratio = 0.5,
            D = dist, reml = T)
    ))
    
    # Define spatial structure of prediction matrix from fitted spatial model
    V0 <- SpatialTools::cov.sp(coords = as.matrix(
        water_qual@data[, c('reasting','rnorthing')]),
        sp.type = "matern", sp.par = vpar$sp.par,
        error.var = vpar$error.var, smoothness = vpar$smoothness,
        finescale.var = 0,
        pcoords = as.matrix(grid_water@data[,1:2]),
        D = dist, Dp = pred.dist, Dop = op.dist)
    
    setTxtProgressBar(pb, 6)
    
    # Apply spatial structure nd model to predict values
    krige <- SpatialTools::krige.uk(water_qual@data[, 'median'],
        V = V0$V, Vop = V0$Vop, Vp = V0$Vp,
        X = as.matrix(cbind(1,
            water_qual@data[, c('reasting','rnorthing')])),
        Xp = as.matrix(cbind(1, grid_water@data[,1:2])), nsim = 0)
    
    setTxtProgressBar(pb, 7)
    
    grid_water[['value']] <- krige$pred
    grid_water[['se']] <- krige$mspe
    suppressWarnings(
        grid_water <- sp::spTransform(grid_water, sp::CRS('+proj=longlat'))
    )
    grid_water@data <- cbind(grid_water@coords,
        grid_water@data[, c('value', 'se')])
    setTxtProgressBar(pb, 8)
    close(pb)
    grid_water
}