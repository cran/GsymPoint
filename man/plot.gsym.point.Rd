\name{plot.gsym.point}
\alias{plot.gsym.point}

\title{
Default plotting of a gsym.point object
}
\description{
On the basis of a \code{\link{gsym.point}} object, it is used to plot the Receiver Operating Characteristic (ROC) curve, the line \eqn{y = 1-\rho t} and the optimal ROC coordinates associated to the Generalized Symmetry point.
}
\usage{

## S3 method for class 'optimal.cutpoints'
\method{plot}{gsym.point}(x, xlab, ylab, main, ...)


}

\arguments{
  \item{x}{
	an object of class \code{gsym.point} as produced by the \code{gsym.point()} function
}
  \item{xlab}{
	the x axis label of the plot. By default this label is set to "False Positive Rate" 
}
  \item{ylab}{
	the y axis label of the plot. By default this label is set to "True Positive Rate"
}
  \item{main}{
	the title of the plot. By default this title is set to "Empirical ROC Curve and line \eqn{y = 1-\rho x}"                    
}
  \item{\dots}{
	further arguments passed to or from other methods
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
trace = FALSE, seed = FALSE, value.seed = 3, verbose = FALSE)

plot(gsym.point.GPQ.melanoma)


data(prostate)

###########################################################
# Generalized Pivotal Quantity Method ("GPQ"): 
###########################################################

gsym.point.GPQ.prostate <- gsym.point (methods = "GPQ", data = prostate,
marker = "marker", status = "status", tag.healthy = 0, categorical.cov = NULL, 
CFN = 1, CFP = 1, control = control.gsym.point(), confidence.level = 0.95, 
trace = FALSE, seed = FALSE, value.seed = 3, verbose = FALSE)

plot(gsym.point.GPQ.prostate)


data(elastase)

###########################################################
# Generalized Pivotal Quantity Method ("GPQ"): 
###########################################################

gsym.point.GPQ.elastase <- gsym.point(methods = "GPQ", data = elastase, 
marker = "elas", status = "status", tag.healthy = 0, categorical.cov = NULL, 
CFN = 1, CFP = 1, control = control.gsym.point(), confidence.level = 0.95, 
trace = FALSE, seed = FALSE, value.seed = 3, verbose = FALSE) 

plot(gsym.point.GPQ.elastase)

}






























