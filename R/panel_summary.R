################################################################################
# Function: panel_summary
# Programmer: Tony Olsen
# Date: March 14, 2019
#'
#' Summary Characteristics of a Revisit Panel Design
#'
#' Revisit panel design characteristics are summarized: number of panels, number
#' of time periods, total number of sample events for the revisit design, total
#' number of sample events for each panel, total number of sample events for
#' each time period and cumulative number of unique units sampled by time
#' periods.
#'
#' @param  paneldsgn Two-dimensional array with dimnames specifying revisit
#'   panel design. Typically, array is output from revisit_dsgn, revisit_bibd or
#'   revisit_rand functions.
#'
#' @param  visitdsgn Two-dimensional array with same dimensions as paneldsgn
#'   specifying the number of times a sample unit is sampled at each time
#'   period. Default is visitdsgn=NULL, where default assumes that a sample unit
#'   will be sampled only once at each time period.
#'
#' @details The revisit panel design and the visit design (if present) are
#'   summarized. Summaries can be useful to know the effort required to complete
#'   the survey design. See the values returned for the summaries that are
#'   produced.
#'
#' @return List of six elements.
#'   \itemize{ \item n.panel - number of panels in revisit design
#'
#'   \item n.period - number of time periods in revisit design
#'
#'   \item n.total - total number of sample events across all panels and all
#'   time periods, accounting for visitdsgn, that will be sampled in the revisit
#'   design
#'
#'   \item n.periodunit - Vector of the number of time periods a unit will be
#'   sampled in each panel
#'
#'   \item n.unitpnl - Vector of the number of sample units, accounting for
#'   visitdsgn, that will be sampled in each panel
#'
#'   \item n.unitperiod - Vector of the number of sample units, accounting for
#'   visitdsgn, that will be sampled during each time period
#'
#'   \item ncum.unit - Vector of the cumulative number of unique units that will
#'   be sampled in time periods up to and including the current time period.
#'   }
#'
#' @author Tony Olsen \email{Olsen.Tony@epa.gov}
#'
#' @seealso 
#'   \describe{
#'     \item{\code{\link{revisit_dsgn}}}{create a panel revisit design}
#'     \item{\code{\link{revisit_bibd}}}{create a balanced incomplete block
#'       panel revisit design}
#'     \item{\code{\link{revisit_rand}}}{create a revisit design with random
#'       assignment to panels and time periods}
#'     \item{\code{\link{power.dsgn}}}{power calculation for multiple panel
#'       designs}
#'     \item{\code{\link{cov.panel.dsgn}}}{covariance matrix for a panel design}
#'     \item{\code{\link{plot_powerpaneldesign}}}{plot power curves for panel
#'       designs}
#'   }
#'
#' @keywords survey
#'
#' @examples
#' # Serially alternating panel revisit design summary
#' sa.dsgn <- revisit_dsgn(20, panels=list(SA60N=list(n=60, pnl_dsgn = c(1, 4),
#'                         pnl_n=NA, start_option="None")), begin=1 )
#' panel_summary(sa.dsgn)
#'
#' # Add visit design where first panel is sampled twice at every time period
#' sa.visit <- sa.dsgn
#' sa.visit [sa.visit > 0] <- 1
#' sa.visit [1, sa.visit[1,] > 0] <- 2
#' panel_summary(sa.dsgn, sa.visit)
#'
#' @export
################################################################################

panel_summary <- function (paneldsgn, visitdsgn = NULL) {

  n.pan <- dim (paneldsgn)[1]
  n.period <- dim (paneldsgn)[2]

  # determine the cumulative number of unique sample units by sampling occasion
  used <- rep(FALSE, n.pan)
  tot <- vector("numeric", length=n.period)
  for (i in 1:n.period) {
    units <- paneldsgn[,i] > 0
    new <- used + units
    tot[i] <- sum(paneldsgn[new == 1,i])
    used <- new
  }
  n.unique_cum <- cumsum (tot)
  names(n.unique_cum) <- dimnames(paneldsgn)[[2]]

  # summarize number of sample results by panel and by time period
  # incorporate multiple times unit is sampled if sample units for a time period are
  # sampled more than once.
  ifelse (!is.null (visitdsgn), vis <- visitdsgn * paneldsgn, vis <- paneldsgn)
  n.unitpnl <- apply(vis, 1, sum)
  n.unitperiod <- apply(vis, 2, sum)
  n.total <- sum(n.unitperiod)
  # number of times a sample unit is visited in each panel
  tmp <- array(0, c(n.pan, n.period))
  tmp[paneldsgn > 0] <- 1
  ifelse (!is.null (visitdsgn), vis <- visitdsgn * tmp, vis <- tmp)
  n.periodunit <- apply(vis, 1, sum)
  names (n.periodunit) <- dimnames(paneldsgn)[[1]]

  # create list of results
  rslt <- list(n.panel = n.pan,
               n.period = n.period,
               n.total = n.total,
               n.periodunit = n.periodunit,
               n.unitpnl = n.unitpnl,
               n.unitperiod = n.unitperiod,
               ncum.unit = n.unique_cum)
  return (rslt)
}
