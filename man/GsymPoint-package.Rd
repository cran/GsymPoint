\name{GsymPoint-package}
\alias{GsymPoint-package}
\alias{GsymPoint}
\docType{package}
\title{
Estimation of the Generalized Symmetry Point, an Optimal Cutpoint in Continuous Diagnostic Tests
}
\description{
Continuous biomarkers or diagnostic tests are often used to discriminate between healthy and diseased populations. In clinical practice, it is useful to select an appropriate cutpoint or discrimination value \eqn{c} which defines the positive and negative test results. 
Several methods for selecting optimal cutpoints in diagnostic tests in the sense of a specific optimality criterion have been proposed in the literature depending on the underlying reason for this choice (see for example, Youden, 1950; Pepe, 2003; Liu, 2012; Rota and Antolini, 2014). 
One of the best-known methods is based on the Symmetry point, also known in the literature as the point of equivalence (Greiner et al., 1995; Defreitas et al., 2004; Adlhoch et al., 2011), defined as the point where the sensitivity and specificity measures are equal. Taking into account the costs associated to the false positives and false negatives misclassifications, the Generalized Symmetry point can be defined. This package allows the user to compute the Generalized Symmetry point as the optimal cutpoint for a diagnostic test or continuous marker. The two methods introduced in \enc{López}{Lopez}-\enc{Ratón}{Raton} et al. (2015) for estimating the Generalized Symmetry point and its sensitivity and specificity accuracy measures have been implemented in this package. One of them is based on the Generalized Pivotal Quantity (Weerahandi, 1993; 1995) and the other based on Empirical Likelihood (Thomas and Grunkemeier, 1975). Numerical and graphical outputs for these two methods are easily obtained.
}

\details{
\tabular{ll}{
Package: \tab GsymPoint\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-07-15\cr
License: \tab GPL\cr
}

The GsymPoint package only requires a data-entry file, which must, at minimum, contain the following variables: diagnostic marker; disease status (diseased/healthy); and whether adjustment is to be made for any categorical covariate of interest, a variable that indicates the levels of this covariate. A standard-type data input structure is used, with each row of the database indicating a patient/case and each column referring to a variable. 

The most important functions are \code{gsym.point()},\code{control.gsym.point()},\code{summary.gsym.point()} and \code{plot.gsym.point()} functions.  
The \code{gsym.point()} function computes the Generalized Symmetry point and its corresponding sensitivity and specificity indexes, according to the method selected. The two methods can be chosen simultaneously for estimating the optimal cutpoint.
The \code{control.gsym.point()} function is used to set several parameters that are specific of each method, such as the number of bootstrap resamples or the constants needed for the Empirical Likelihood method.
The \code{summary.gsym.point()} and \code{plot.gsym.point()} functions produce numerical and graphical outputs, respectively. 
}

\author{
\enc{Mónica}{Monica} \enc{López}{Lopez}-\enc{Ratón}{Raton}, Carmen Cadarso-\enc{Suárez}{Suarez}, Elisa M. Molanes-\enc{López}{Lopez} and Emilio \enc{Letón}{Leton}

Maintainer: \enc{Mónica}{Monica} \enc{López}{Lopez}-\enc{Ratón}{Raton} <monica.lopez.raton@gmail.com> 
}
\references{
Adlhoch, C., Kaiser, M., Hoehne, M., Mas Marques, A., Stefas, I., Veas, F. and Ellerbrok, H. (2011). Highly sensitive detection of the group A Rotavirus using Apolipoprotein H-coated ELISA plates compared to quantitative real-time PCR. \emph{Virology Journal} \bold{8}, 63.

Defreitas, G.A., Zimmern, P.E., Lemack, G.E. and Shariat, S.F. (2004). Refining diagnosis of anatomic female bladder outlet obstruction: comparison of pressureflow study parameters in clinically obstructed women with those of normal controls. \emph{Urology} \bold{64}, 675-679.

Greiner, M., Sohr, D. and \enc{Göbel}{Gobel}, P. (1995). A modified ROC analysis for the selection of cut-off values and the definition of intermediate results of serodiagnostic tests. \emph{Journal of Immunological Methods} \bold{185}, 123-132.

Liu, X. (2012). Classification accuracy and cut point selection. \emph{Statistics in Medicine} \bold{31(23)}, 2676-2686.

\enc{López}{Lopez}-\enc{Ratón}{Raton}, M., Cadarso-\enc{Suárez}{Suarez}, C., Molanes-\enc{López}{Lopez}, E.M. and \enc{Letón}{Leton}, E. (2015). Confidence intervals for the Symmetry point: an optimal cutpoint in continuous diagnostic tests. (Submitted).

Pepe, M.S. (2003). \emph{The statistical evaluation of medical tests for classification and prediction}. Oxford University Press, New York.

Rota, M. and Antolini, L. (2014). Finding the optimal cut-point for Gaussian and Gamma distributed biomarkers. \emph{Computational Statistics and Data Analysis} \bold{69},1-14.

Thomas, D.R. and Grunkemeier, G.L. (1975). Confidence interval estimation of survival probabilities for censored data. \emph{Journal of the American Statistical Association} \bold{70}, 865-871.

Weerahandi, S. (1993). Generalized confidence intervals. \emph{Journal of the American Statistical Association} \bold{88}, 899-905. 

Weerahandi, S. (1995). \emph{Exact statistical methods for data analysis}. Springer-Verlag, New York. 

Youden, W.J. (1950). Index for rating diagnostic tests. \emph{Cancer} \bold{3}, 32-35.

}
