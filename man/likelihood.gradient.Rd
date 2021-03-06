% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/opti.R
\name{likelihood.gradient}
\alias{likelihood.gradient}
\title{Gradient of the likelihood of the GEV and GP distribution}
\usage{
likelihood.gradient(parameters, x.in, model = c("gev", "gpd"))
}
\arguments{
\item{parameters}{Numerical vector containing the location, scale,
and shape parameters for the GEV model or the scale and shape
parameters for the GP one.}

\item{x.in}{Time series of class \pkg{xts} or numerical vector
containing the extreme events.}

\item{model}{String determining whether to calculate the initial
parameters of the GEV ("gev") or GP ("gpd") function. Default =
"gev".}
}
\value{
Numerical vector containing the derivative of the negative
  log-likelihood in (location, scale, shape for GEV) or (scale,
  shape for GP) direction.
}
\description{
Calculates the gradient of the negative log
  likelihood of the GEV or GP function.
}
\seealso{
Other optimization: \code{\link{fit.gev.default}},
  \code{\link{fit.gev.list}}, \code{\link{fit.gev.xts}},
  \code{\link{fit.gev}}, \code{\link{fit.gpd.default}},
  \code{\link{fit.gpd.list}}, \code{\link{fit.gpd.xts}},
  \code{\link{fit.gpd}},
  \code{\link{likelihood.augmented}},
  \code{\link{likelihood.gradient.augmented}},
  \code{\link{likelihood.initials}},
  \code{\link{likelihood}}
}
\author{
Philipp Mueller
}
\concept{optimization}
