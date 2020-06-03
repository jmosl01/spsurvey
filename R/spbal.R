spbal <- function(dsgn, sampled_sites, population_sites, population_strata, spb_type = "population", 
                  population_metrics = c("pielou", "chisq"), geography_metrics = NULL) {
  
  # finding the population bounding box
  pop_bbox <- st_bbox(population_sites)[c("xmin", "xmax", "ymin", "ymax")]
  
  # naming the strata
  strata <- names(dsgn)
  

  
  # checking for equal probabilities within each strata
  if (all(sapply(dsgn, function(s) s$seltype) == "equal")) {
    
    # making the strata index equal "None" if no strata 
    if (all(strata == "None")) {
      strata_index <-  strata
    } else {
      # population strata vector assignment
      strata_index <- population_sites[[population_strata]]
    }
    
    # computing the spatial balance metrics for each strata
    results <- lapply(strata, function(s) {
      spbal_calc(sampled_sub = sampled_sites[sampled_sites$stratum == s & sampled_sites$panel == "PanelOne", ], 
                 population_sub = population_sites[strata_index == s, ],
                 pop_bbox = pop_bbox,
                 spb_type = spb_type, population_metrics = population_metrics, 
                 geography_metrics = geography_metrics)
    })
    
  }
  # giving the list elements names
  names(results) <- strata
  
  # returning the reslts
  invisible(results)
}


  
  
  

  
  