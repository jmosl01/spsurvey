spbalance_calc <- function(samp_sites, pop_sites, type = "population") {
  
# working with the sampled sites
  
  ## take the sampled sites coordinates
  samp_coords <- st_coordinates(samp_sites)
  ## name them X and Y
  colnames(samp_coords) <- c("X", "Y")
  ## isolate X
  samp_xcoord <- samp_coords[, "X"]
  ## isolate Y
  samp_ycoord <- samp_coords[, "Y"]
  ## recover the sample size
  n <- nrow(samp_sites)
  
# and the population sites
  
  ## find the bounding box of the population
  pop_bbox <- st_bbox(pop_sites)
  ## recover the population size
  N <- nrow(pop_sites)
  
# spatial balance with respect to the population
  
  if (type == "population") {
  
  ## making the dirichlet polygon
  tiles <- deldir(x = samp_xcoord, 
                  y = samp_ycoord, 
                  rw = pop_bbox[c("xmin", "xmax", "ymin", "ymax")]) %>% 
              ## storing it as a tile.list()
              tile.list(.)
  ## using lapply instead of a loop
  sftess <- lapply(tiles, function(t) {
    ## finding the number of points in the bounding polygon
    npol <- length(t$x)
    ## binding the coordinates
    return(cbind(c(t$x[1], t$x[npol:1]), 
                 c(t$y[1], t$y[npol:1])) %>% 
             ## storing as a list
             list(.) %>%
             ## storing as a st polygon
             st_polygon(.))
             }) %>% 
    ## adding in the popoulation level crs
    st_sfc(crs = st_crs(pop_sites)) %>%
    ## adding in poly as a variable and storing as an sf object
    st_sf(poly = 1:n, geometry = .) %>%
    ## joining the polygon bounds with the population data
    st_join(., pop_sites)

# calculate counts 
  
  ## for points, population geometry must be point or multipoint
  if(all(st_geometry_type(pop_sites) %in% c("POINT", "MULTIPOINT"))) {
    ## storing a dummy variable to index counts by
    sftess$point_mdm <- 1
    ## summing over each polygon
    counts <- with(sftess, tapply(point_mdm, poly, sum))
    ## setting NA's equal to zero
    counts[is.na(counts)] <- 0
  }
  
# making proportions and expected quantities
    ## proportions
    props <- counts / sum(counts)
    
    ### could put wgt/sum(wgt) here
    ## making expected counts and proportions
    expected_counts <- N / n 
    expected_props <- 1 / n
    
    
# metrics
    ## calculating pielou with 0 indicating perfect spatial balance and larger values indicating poor spatial balance (1 is max)
    pielou <- 1 + sum(props * log(props)) / log(n)
    ## calculating chi square statsitic that has a chi square distribution, larger is worse spatial balance
    chisq <- sum((counts - expected_counts)^2 / expected_counts)


# returning the results when stored by the user
    invisible(list(pielou = pielou, chisq = chisq, counts = counts))
  }  
}