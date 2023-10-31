\name{control.gsym.point}
\alias{control.gsym.point}

\title{Control of the Generalized Symmetry point computing process}

\description{control.gsym.point is used to set various parameters that control the Generalized Symmetry point computing process.
}

\usage{control.gsym.point(B = 499, c_sampling = 0.25, c_F = 0.25, c_ELq = 0.25, 
c_R = 0.25, I = 2500)}

\arguments{
  \item{B}{
a numerical value meaningful only in the "EL" method. It specifies the number of simulations in the Empirical Likelihood method. The default value is 499.
}
  \item{c_sampling}{
 a numerical value meaningful only in the "EL" method. It specifies the constant needed for resampling in the Empirical Likelihood method. The default value is 0.25.
}
  \item{c_F}{
a numerical value meaningful only in the "EL" method. It specifies the constant needed for estimating the distribution in the Empirical Likelihood method. The default value is 0.25.
}
  \item{c_ELq}{
a numerical value meaningful only in the "EL" method. It specifies the constant needed for estimating the empirical likelihood function in the Empirical Likelihood method. The default value is 0.25.
}
  \item{c_R}{
  a numerical value meaningful only in the "EL" method. It specifies the constant needed for estimating the ROC Curve in the Empirical Likelihood method. The default value is 0.25.
}
  \item{I}{
a numerical value meaningful only in the "GPQ" method. It specifies the number of replicates in the Generalized Pivotal Quantity method. The default value is 2500.
}
}

\details{
The value yielded by this function is used as the control argument of the \code{gsym.point()}function.
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

# How to set the number of replicates I equal to 2000:

gsym.point.GPQ.melanoma<-gsym.point(methods = "GPQ", data = melanoma,
marker = "X", status = "group", tag.healthy = 0, categorical.cov = NULL, 
CFN = 1, CFP = 1, control = control.gsym.point(I = 2000),
confidence.level = 0.95, trace = FALSE, seed = FALSE, value.seed = 3, verbose = FALSE)

summary(gsym.point.GPQ.melanoma)


data(prostate)

###########################################################
# Empirical Likelihood Method ("EL")
###########################################################

# How to set the number of resamples B equal to 99:

gsym.point.GPQ.prostate <- gsym.point (methods = "GPQ", data = prostate,
marker = "marker", status = "status", tag.healthy = 0, categorical.cov = NULL, 
CFN = 1, CFP = 1, control = control.gsym.point(B=99), confidence.level = 0.95, 
trace = FALSE, seed = FALSE, value.seed = 3, verbose = FALSE)

summary(gsym.point.GPQ.prostate)

}
