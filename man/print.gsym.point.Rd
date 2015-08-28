\name{print.gsym.point}
\alias{print.gsym.point}
\title{
Print method for gsym.point objects
}

\description{
Default print method for objects fitted with \code{gsym.point()} function. A short summary is printed with: the call to the \code{gsym.point()} function for each categorical covariate level
(if the \code{categorical.cov} argument of the \code{gsym.point()} function is not NULL).
}
\usage{
%print.gsym.point(x, digits = max(3L, getOption("digits") - 3L), ...)
\method{print}{gsym.point}(x, digits = max(3L, getOption("digits") - 3L), ...)

}

\arguments{
  \item{x}{
     an object of class \code{gsym.point} as produced by \code{gsym.point()} function.
}
  \item{digits}{
     controls number of digits printed in the output.
}
  \item{\dots}{
     further arguments passed to or from other methods.
}
}

\author{
	\enc{Mónica}{Monica} \enc{López}{Lopez}-\enc{Ratón}{Raton}, Carmen Cadarso-\enc{Suárez}{Suarez}, Elisa M. Molanes-\enc{López}{Lopez} and Emilio \enc{Letón}{Leton}
}

\seealso{
 \code{\link{gsym.point}}, \code{\link{summary.gsym.point}}
}

\examples{
library(GsymPoint)
data(elastase)

###########################################################
# Empirical Likelihood Method ("GPQ"): 
###########################################################

gsym.point.GPQ.elastase<-gsym.point(methods = "GPQ", data = elastase, marker = "elas", 
status = "status", tag.healthy = 0, categorical.cov = NULL, CFN = 1, CFP = 1, 
control = control.gsym.point(), confidence.level = 0.95, trace = FALSE, 
seed = FALSE, value.seed = 3) 

gsym.point.GPQ.elastase

print(gsym.point.GPQ.elastase)
}