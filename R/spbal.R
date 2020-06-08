################################################################################
# Function: spbal
# Programmer: Michael Dumelle
# Date: July 8, 2020
# Last Revised: July 8, 2020

#' Spatial Balance
#'
#' This function computes the spatial balance of a design with respect 
#' to the population or geography.  Spatial balance with respect to the
#' population measures the extent to which a sample is a miniature of 
#' the population using Dirichlet Tesselations of the sample
#' and counting the number of population units in each tesselation.
#' Spatial balance with respect to the population measures the geographic
#' spread in the sample using distance measures. 
#' 
#' @param dsgn a list specifying the design
#' @param sampled_sites an sf object of the sampled sites
#' @param population_sites an sf object of the population sites
#' @param population_strata a character vector specifying the name
#' of the column in population_sites corresponding to the strata
#' assignment of the population
#' @param spb_type a character vector indicating "population" or 
#' "geography" spatial balance metrics  
#' @param population_metrics a character vector indicating the 
#' requested population spatial balance metrics (default is Pielou's)
#' @param geography_metrics a character vector indicating the 
#' requested population spatial balance metrics
#'
#' @return A list containing the spatial balance metrics for all strata and, 
#' if the spatial balance is with respect to geography, the number of 
#' population units in each Dirichlet Tesselations
#' @export
#'
#' @examples
spbal <- function(dsgn, sampled_sites, population_sites, population_strata, spb_type = "population", 
                  population_metrics = c("pielou"), geography_metrics = NULL) {
  
  # finding the population bounding box, this needs to be reordered to 
  # use in the Dirichlet Tesselation
  pop_bbox <- st_bbox(population_sites)[c("xmin", "xmax", "ymin", "ymax")]
  
  # storing the names of the strata to iterate through later
  strata <- names(dsgn)
  

  
  # checking for equal probabilities within each strata
  if (all(sapply(dsgn, function(s) s$seltype) == "equal")) {
    
    # making the strata index equal "None" if no strata 
    if (all(strata == "None")) {
      strata_index <-  strata
    } else {
      # making the strata index equal to the strata index
      # in the original population frame
      strata_index <- population_sites[[population_strata]]
    }
    
    # computing the spatial balance metrics for each strata
    results <- lapply(strata, function(s) {
      # iterating through each strata (right now, for a single panel) and computing the spatial balance metrics
      # using the spbal_calc() function
      spbal_calc(sampled_sub = sampled_sites[sampled_sites$stratum == s & sampled_sites$panel == "PanelOne", ], 
                 population_sub = population_sites[strata_index == s, ],
                 pop_bbox = pop_bbox,
                 spb_type = spb_type, population_metrics = population_metrics, 
                 geography_metrics = geography_metrics)
    })
    
  }
  # giving the list elements names
  names(results) <- strata
  
  # returning the results
  invisible(results)
}


  
  
  

  
  