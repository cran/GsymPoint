\name{plot.gsym.point}
\alias{plot.gsym.point}

\title{
Default plotting of a gsym.point object
}
\description{
On the basis of a \code{\link{gsym.point}} object, it plots the Receiver Operating Characteristic (ROC) curve and the line \eqn{y = 1-\rho t}.
}
\usage{
%plot.gsym.point(x, legend = TRUE, ...)
\method{plot}{gsym.point}(x, legend = TRUE, ...)
}

\arguments{
 \item{x}{
    an object of class \code{gsym.point} as produced by \code{gsym.point()} function.
}
  \item{legend}{
	a logical value. If it is TRUE, a legend of the AUC value is shown on the plot. By default it is TRUE.
}
  \item{\dots}{
    further arguments passed to or from other methods.
}
}
\author{
	\enc{Mónica}{Monica} \enc{López}{Lopez}-\enc{Ratón}{Raton}, Carmen Cadarso-\enc{Suárez}{Suarez}, Elisa M. Molanes-\enc{López}{Lopez} and Emilio \enc{Letón}{Leton}
}

\seealso{
 \code{\link{gsym.point}}, \code{\link{control.gsym.point}}
}

\examples{
library(GsymPoint)

data(melanoma)

###########################################################
# Generalized Pivotal Quantity Method ("GPQ"): 
###########################################################

gsym.point.GPQ.melanoma<-gsym.point(methods = "GPQ", data = melanoma,
marker = "X", status = "group", tag.healthy = 0, categorical.cov = NULL, 
CFN = 1, CFP = 1, control = control.gsym.point(),confidence.level = 0.95, 
trace = FALSE, seed = FALSE, value.seed = 3)

plot(gsym.point.GPQ.melanoma)


data(prostate)

###########################################################
# Generalized Pivotal Quantity Method ("GPQ"): 
###########################################################

gsym.point.GPQ.prostate <- gsym.point (methods = "GPQ", data = prostate,
marker = "marker", status = "status", tag.healthy = 0, categorical.cov = NULL, 
CFN = 1, CFP = 1, control = control.gsym.point(), confidence.level = 0.95, 
trace = FALSE, seed = FALSE, value.seed = 3)

plot(gsym.point.GPQ.prostate)


data(elastase)

###########################################################
# Generalized Pivotal Quantity Method ("GPQ"): 
###########################################################

gsym.point.GPQ.elastase <- gsym.point(methods = "GPQ", data = elastase, 
marker = "elas", status = "status", tag.healthy = 0, categorical.cov = NULL, 
CFN = 1, CFP = 1, control = control.gsym.point(), confidence.level = 0.95, 
trace = FALSE, seed = FALSE, value.seed = 3) 

plot(gsym.point.GPQ.elastase)

}