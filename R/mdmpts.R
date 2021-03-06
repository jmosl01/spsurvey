################################################################################
# File: mdmpts.r
# Programmer: Tony Olsen
# Date: October 15, 2002
#'
#' Internal Function: GRTS Multipliers for Multi-Density Categories for Points
#'
#' @param mdcaty Vector of multi-density category groups for each element in
#'   sample frame.
#'
#' @param n.desired Expected sample size for each category.  Row names must
#'   match category names in mdcaty.
#'
#' @return Numeric vector of multipliers that is same length as mdcaty.
#'
#' @author Tony Olsen \email{Olsen.Tony@epa.gov}
#'
#' @export
################################################################################

mdmpts <- function(mdcaty,n.desired) {

   gsum <- table(mdcaty)
   catmatch <- match(names(n.desired),names(gsum),nomatch=0)
   piden <- n.desired/gsum[catmatch]
   mdmpts <- rep(NA,length(mdcaty))
   for(i in names(n.desired))
       mdmpts[mdcaty == i] <- piden[i]

   mdmpts
}


