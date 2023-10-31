\name{elastase}
\alias{elastase}
\docType{data}
\title{
Leukocyte Elastase dataset
}
\description{
The \code{elastase} dataset was obtained from a study carried out in the Cardiology Department at the Galicia General Hospital (Santiago de Compostela, Spain). This study (Amaro et al., 1995) was conducted to assess the clinical usefulness of leukocyte elastase determination in the diagnosis of coronary artery disease (CAD).
}
\usage{data(elastase)}
\format{
  A data frame with 141 observations on the following 3 variables.
  \describe{
   \item{\code{elas}}{leukocyte elastase. Numerical vector}
    \item{\code{status}}{true disease status (absence/presence of coronary artery disease). Numerical vector (0=absence, 1=presence)}
    \item{\code{gender}}{patient's gender. Factor with \code{Male} and \code{Female} levels}
  }
}

\source{
Amaro, A., Gude, F., \enc{González}{Gonzalez}-Juanatey, R., Iglesias, C., \enc{Fernández}{Fernandez}-\enc{Vázquez}{Vazquez}, F., \enc{García}{Garcia}-\enc{Acuña}{Acuna}, J. and Gil, M. (1995). Plasma leukocyte elastase concentration in angiographically diagnosed coronary artery disease. \emph{European Heart Journal} \bold{16}, 615--622.
}
\references{
Amaro, A., Gude, F., \enc{González}{Gonzalez}-Juanatey, R., Iglesias, C., \enc{Fernández}{Fernandez}-\enc{Vázquez}{Vazquez}, F., \enc{García}{Garcia}-\enc{Acuña}{Acuna}, J. and Gil, M. (1995). Plasma leukocyte elastase concentration in angiographically diagnosed coronary artery disease. \emph{European Heart Journal} \bold{16}, 615--622.
}
\examples{
data(elastase)
summary(elastase)
}
\keyword{datasets}
