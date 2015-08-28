\name{summary.gsym.point}
\alias{summary.gsym.point}

\title{
Summary method for gsym.point objects
}

\description{
Produces a summary of a \code{gsym.point} object. The following is printed: the matched call to the \code{gsym.point()} main function; the area under the ROC curve (AUC) estimate; the Generalized Symmetry point obtained with the method(s) selected and the point estimates of the associated sensitivity and specificity indexes with their corresponding confidence intervals. All this information will be shown for each categorical covariate level (if the \code{categorical.cov }argument in the \code{gsym.point()} function is not NULL). 
}

\usage{
%summary.gsym.point(object, ...)
\method{summary}{gsym.point}(object, ...)
}
\arguments{
  \item{object}{
      an object of class \code{gsym.point} as produced by \code{gsym.point()} function.
}
  \item{\dots}{
      further arguments passed to or from other methods. None are used in this method.
}
}
\details{
The \code{summary.gsym.point}  function produces a list of summary information for a fitted \code{gsym.point} object. The result depends on the two arguments, namely, \code{methods}, and \code{categorical.cov} of the \code{gsym.point()} function used in the Generalized Symmetry point computing process.
}

\value{
Returns an object of class "summary.gsym.point" with the same components as the \code{gsym.point} function (see \code{\link{gsym.point}})     
}
\author{
	\enc{Mónica}{Monica} \enc{López}{Lopez}-\enc{Ratón}{Raton}, Carmen Cadarso-\enc{Suárez}{Suarez}, Elisa M. Molanes-\enc{López}{Lopez} and Emilio \enc{Letón}{Leton}
}

\seealso{
 \code{\link{gsym.point}}
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

summary(gsym.point.GPQ.melanoma)


data(prostate)

###########################################################
# Generalized Pivotal Quantity Method ("GPQ"): 
###########################################################

gsym.point.GPQ.prostate <- gsym.point (methods = "GPQ", data = prostate,
marker = "marker", status = "status", tag.healthy = 0, categorical.cov = NULL, 
CFN = 1, CFP = 1, control = control.gsym.point(), confidence.level = 0.95, 
trace = FALSE, seed = FALSE, value.seed = 3)

summary(gsym.point.GPQ.prostate)


data(elastase)

###########################################################
# Generalized Pivotal Quantity Method ("GPQ"): 
###########################################################

gsym.point.GPQ.elastase <- gsym.point(methods = "GPQ", data = elastase, 
marker = "elas", status = "status", tag.healthy = 0, categorical.cov = NULL, 
CFN = 1, CFP = 1, control = control.gsym.point(), confidence.level = 0.95, 
trace = FALSE, seed = FALSE, value.seed = 3) 

summary(gsym.point.GPQ.elastase)





}