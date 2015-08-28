\name{prostate}
\alias{prostate}
\docType{data}
\title{
Prostate cancer dataset
}
\description{
The \code{prostate} dataset was obtained from a study carried in Le (2006). This study was conducted to evaluate the clinical usefulness of the level of acid phosphatase in blood serum (APBS) x 100 to predict whether
nodal involvement is present. 
}

\usage{data(prostate)}
\format{
  A data frame with 53 observations on the following 2 variables.
  \describe{
    \item{\code{marker}}{level of acid phosphatase in blood serum. Numeric vector}
    \item{\code{status}}{true disease status (absence/presence of nodal involvement). Numerical vector (0=absence, 1=presence)}
  }
}
\source{
Le, C.T. (2006). A solution for the most basic optimization problem associated with an
ROC curve. \emph{Statistical Methods in Medical Research} \bold{15}, 571--584.
}
\references{
Le, C.T. (2006). A solution for the most basic optimization problem associated with an
ROC curve. \emph{Statistical Methods in Medical Research} \bold{15}, 571--584.
}
\examples{
data(prostate)
summary(prostate)
}
\keyword{datasets}
