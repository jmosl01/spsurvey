#' Title
#'
#' Description
#'
#' @param sampled_sub 
#' @param population_sub 
#' @param pop_bbox 
#' @param spb_type 
#' @param population_metrics 
#' @param geography_metrics 
#'
#' @return
#' @export
#'
#' @examples
spbal_calc <- function(sampled_sub, population_sub, pop_bbox, spb_type, population_metrics, geography_metrics) {
  
# working with the sampled sites
  
  ## take the sampled sites coordinates
  samp_coords <- st_coordinates(sampled_sub)
  ## name them X and Y
  colnames(samp_coords) <- c("X", "Y")
  ## isolate X
  samp_xcoord <- samp_coords[, "X"]
  ## isolate Y
  samp_ycoord <- samp_coords[, "Y"]
  ## recover the sample size
  n <- nrow(sampled_sub)
  
# and the population sites
  

  ## recover the population size
  N <- nrow(population_sub)
  
# spatial balance with respect to the population
  
  if ("population" %in% spb_type) {
  
  ## making the dirichlet polygon
  tiles <- deldir(x = samp_xcoord, 
                  y = samp_ycoord, 
                  rw = pop_bbox) %>% 
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
    st_sfc(crs = st_crs(population_sub)) %>%
    ## adding in poly as a variable and storing as an sf object
    st_sf(poly = 1:n, geometry = .) %>%
    ## joining the polygon bounds with the population data
    st_join(., population_sub)

# calculate counts 
  
  ## for points, population geometry must be point or multipoint
  if(all(st_geometry_type(population_sub) %in% c("POINT", "MULTIPOINT"))) {
    ## storing a dummy variable to index counts by
    sftess$point_mdm <- 1
    ## summing over each polygon
    counts <- with(sftess, tapply(point_mdm, poly, sum))
    ## setting NA's equal to zero
    counts[is.na(counts)] <- 0
  }
  
  # ## for lines, have not completetly vetted code
  # } else if(all(st_geometry_spb_type(sfframe) %in% c("LINESTRING", "MULTILINESTRING"))) {
  #   sftess$length_mdm <- as.numeric(st_length(sftess))
  #   extent <- with(sftess, tapply(length_mdm, poly, sum))
  #   extent[is.na(extent)] <- 0
  #   ## for polygons, have not completetly vetted code
  # } else {
  #   sftess$area_mdm <- as.numeric(st_area(sftess))
  #   extent <- with(sftess, tapply(area_mdm, poly, sum))
  #   extent[is.na(extent)] <- 0
  # }
  
# making proportions and expected quantities
    ## proportions
    props <- counts / sum(counts)
    
    ### could put wgt/sum(wgt) here
    ## making expected counts and proportions
    ## expected_counts <- N / n 
    expected_props <- 1 / n
    
    
# metrics
    

    ## calculating pielou with 0 indicating perfect spatial balance and larger values indicating poor spatial balance (1 is max)
    if ("pielou" %in% population_metrics){ 
    pielou <- 1 + sum(props * log(props)) / log(n)
    names(pielou) <- "pielou"
    } else pielou <- NULL
    ## calculating chi square statsitic that has a chi square distribution, larger is worse spatial balance
    #chisq <- sum((counts - expected_counts)^2 / expected_counts)
    if ("chisq" %in% population_metrics) {
      #chisq <- sum((counts - expected_counts)^2 / expected_counts)
      chisq <- sum((props - expected_props)^2 / expected_props)
      names(chisq) <- "chisq"
    } else chisq <- NULL
    if ("abserr" %in% population_metrics) {
      abserr <- sum(abs(props - expected_props) / expected_props)
      names(abserr) <- "abserr"
    } else abserr <- NULL
    if ("simpsons" %in% population_metrics) {
      simpsons <- sum(props^2) - expected_props
      names(simpsons) <- "simpsons"
    } else simpsons <- NULL

    population_metrics <- c(pielou, chisq, abserr, simpsons)
    population_metrics <- population_metrics[sort(names(population_metrics))]
# returning the results when stored by the user
    invisible(list(population_metrics = population_metrics, counts = counts))
  }  
}

