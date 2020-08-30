# Filter Kriging Functions
# Use this to filter Kriging output

n_in_Poly <- function(df, polygon) {
    # specify required pkgs ----------------------------------------------------
    require(dplyr)
    require(sp)
    
    # Manipulate the data into spatial obj -------------------------------------
    #subset the dataframe
    df <- df %>% 
            dplyr::select(Longitude, Latitude, MeasureValue) %>% 
            na.omit() %>% 
            dplyr::select(Longitude, Latitude) %>% 
            dplyr::distinct() #remove duplicate rows. (want to know the number of distinct locations.)
    df_coords <- data.frame(lon = df$Longitude, lat = df$Latitude)
    #make the dataframe into a spatial data set.
    df_sp <- sp::SpatialPoints(coords = df_coords, proj4string = sp::CRS('+proj=longlat'))
    
    #convert df_sp to the same CRS as the polygon. 
    CRS_poly <- polygon@proj4string # get the polygong CRS
    df_sp <- sp::spTransform(df_sp,CRS_poly)
    
    # Count number of points in polygon ----------------------------------------
    count <- over(df_sp, polygon) #find if each point is in the polygon or not
    #(returns as NA if not in the polygon)
    count <- length(na.omit(count)) #count only those in the polygon.
    # return count -------------------------------------------------------------
    return(count)
}
