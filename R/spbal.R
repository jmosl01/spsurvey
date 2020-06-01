spbal <- function(dsgn, sampled_sites, population_sites, type = "population", 
                  population_metrics = c("pielou", "chisq"), geography_metrics = NULL) {
  strata <- names(dsgn)
  pop_bbox <- st_bbox(population_sites)[c("xmin", "xmax", "ymin", "ymax")]
  
  if (all(strata == "None")) {
    
    if (dsgn$None$seltype == "equal") {
    results <- spbal_calc(sampled_sub = sampled_sites, population_sub = population_sites, 
                          pop_bbox = pop_bbox, type = type, 
                          population_metrics = population_metrics, geography_metrics = geography_metrics)
    }
  }
  
  if (all(strata != "None")) {
    if (any(sort(strata) != sort(unique(population_sites$ST)))) stop("strata in sample and strata in population are not equal")
    if (all(sapply(Stratdsgn, function(x) x$seltype) == "equal")) {
    
    results <- lapply(strata, function(s) {
      spbal_calc(sampled_sub = sampled_sites[sampled_sites$stratum == s, ], 
                 population_sub = population_sites[population_sites$ST == s, ],
                 pop_bbox = pop_bbox,
                 type = type, population_metrics = population_metrics, geography_metrics = geography_metrics)
    })
    names(results) <- strata
    }
  }
  
  invisible(results)
}
  
  