\name{melanoma}
\alias{melanoma}
\docType{data}
\title{
Melanoma dataset
}
\description{
The \code{melanoma}  dataset was obtained from Venkatram and Begg (1996). This study was conducted to assess the clinical usefulness of a clinical scoring scheme without dermoscope (CSS) used by dermatologists in order to determine if a suspicious pigmented lesion on the skin is a melanoma.
}
\usage{data(melanoma)}
\format{
  A data frame with 72 observations on the following 2 variables.
  \describe{
    \item{\code{X}}{clinical scoring scheme without dermoscope. Numerical vector.}
    \item{\code{group}}{true disease status (absence/presence of melanoma). Numerical vector (0=absence, 1=presence)}
  }
}

\source{
Venkatraman, ES., Begg, CB. (1996). A distribution-free procedure for comparing receiver
operating characteristic curves from a paired experiment. \emph{Biometrika} \bold{83}, 835--848.
}
\references{
Venkatraman, ES., Begg, CB. (1996). A distribution-free procedure for comparing receiver
operating characteristic curves from a paired experiment. \emph{Biometrika} \bold{83}, 835--848.
}
\examples{
data(melanoma)
summary(melanoma)
}
\keyword{datasets}
