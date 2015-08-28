\name{control.gsym.point}
\alias{control.gsym.point}

\title{Controlling the Generalized Symmetry point computing process}

\description{Used to set various parameters controlling the Generalized Symmetry point computing process
}

\usage{
control.gsym.point(B = 499, c_sampling = 0.25, c_F = 0.25, c_ELq = 0.25, 
c_R = 0.25, I = 2500)
}

\arguments{
  \item{B}{
a numerical value meaningful only in the "EL" method. It specifies the number of bootstrap resamples in the Empirical Likelihood method. The default value is 499.
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
\enc{M�nica}{Monica} \enc{L�pez}{Lopez}-\enc{Rat�n}{Raton}, Carmen Cadarso-\enc{Su�rez}{Suarez}, Elisa M. Molanes-\enc{L�pez}{Lopez} and Emilio \enc{Let�n}{Leton}
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

# Change the number of replicates:

gsym.point.GPQ.melanoma<-gsym.point(methods = "GPQ", data = melanoma,
marker = "X", status = "group", tag.healthy = 0, categorical.cov = NULL, 
CFN = 1, CFP = 1, control = control.gsym.point(I = 2000),
confidence.level = 0.95, trace = FALSE, seed = FALSE, value.seed = 3)

summary(gsym.point.GPQ.melanoma)


data(prostate)

###########################################################
# Empirical Likelihood Method ("EL")
###########################################################

# Change the number of bootstrap resamples:

gsym.point.EL.prostate <- gsym.point (methods = "EL", data = prostate,
marker = "marker", status = "status", tag.healthy = 0, categorical.cov = NULL, 
CFN = 1, CFP = 1, control = control.gsym.point(B=99), confidence.level = 0.95, 
trace = FALSE, seed = FALSE, value.seed = 3)

summary(gsym.point.EL.prostate)
}

