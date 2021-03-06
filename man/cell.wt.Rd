% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cell.wt.R
\name{cell.wt}
\alias{cell.wt}
\title{Last Revised: September 24, 2018}
\usage{
cell.wt(cel, xc, yc, dx, dy, pts)
}
\arguments{
\item{cel}{= the index value for a cell.}

\item{xc}{= x-coordinates that define the cells.}

\item{yc}{= y-coordinates that define the cells.}

\item{dx}{= width of the cells along the x-axis.}

\item{dy}{= width of the cells along the y-axis.}

\item{pts}{= an `sf` data frame containing x-coordinates and y-coordinates in the geometry list-column, and mdm
values.}
}
\value{
The total inclusion probability for a cell.
}
\description{
Total Inclusion Probablity for Matrix of Cells
}
\details{
Calculates the total inclusion probability for a cell. Used to evaluate
spatial balance of a survey design realization.
}
\author{
Tony Olsen  \email{Olsen.Tony@epa.gov}

Marc Weber  \email{Weber.Marc@epa.gov}
}
\keyword{survey}
