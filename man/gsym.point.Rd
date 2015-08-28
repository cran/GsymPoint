\name{gsym.point}
\alias{gsym.point}
\title{
Constructing confidence intervals for the Generalized Symmetry point and its accuracy measures through two methods
}
\description{
gsym.point constructs confidence intervals for the Generalized Symmetry point and its accuracy measures sensitivity and specificity for a continuous diagnostic test using two methods: the Generalized Pivotal Quantity method and the Empirical Likelihood method.
}

\usage{
gsym.point (methods, data, marker, status, tag.healthy, categorical.cov = NULL, 
CFN = 1, CFP = 1, control = control.gsym.point(), confidence.level = 0.95, 
trace = FALSE, seed = FALSE, value.seed = 3)
}

\arguments{
  \item{methods}{
a character vector selecting the method(s) to be used for estimating the Generalized Symmetry point and its accuracy measures. The possible options are: "GPQ", "EL", or both: c("GPQ","EL") or c("EL","GPQ").
}  
\item{data}{
    a data frame containing all needed variables: the diagnostic marker, the true disease status and when it is neccesary, the categorical covariate.
}
  \item{marker}{
    a character string with the name of the diagnostic test variable.
}
  \item{status}{
    a character string with the name of the variable that distinguishes healthy from diseased individuals.
}
  \item{tag.healthy}{
    the value codifying healthy individuals in the \code{status} variable.
}
  \item{categorical.cov}{
    a character string with the name of the categorical covariate according to which the Generalized Symmetry point (optimal cutpoint) is to be calculated. The default is NULL (no categorical covariate is considered in the analysis).
}
  \item{CFN}{
    a numerical value. It specifies the cost of a false negative decision. The default value is 1.
}
  \item{CFP}{
    a numerical value. It specifies the cost of a false positive decision. The default value is 1.
}
  \item{control}{
    output of the \code{\link{control.gsym.point}} function, which controls the whole optimal-cutpoint calculation process.
}
  \item{confidence.level}{
    a numerical value with the confidence level for the construction of the confidence intervals. The default value is 0.95.
}
  \item{trace}{
    a logical value. If TRUE, information on progress is shown. The default is FALSE.
}
  \item{seed}{
    a logical value. If TRUE, a seed is fixed for generating the trials in the computation of the confidence intervals. The default is FALSE.
}
  \item{value.seed}{
  If \code{seed} is TRUE, it is the numerical value for the fixed seed. The default value is 3.
}

}

\details{
The Symmetry point \eqn{c_{S}} satisfies the equality \eqn{p(c_{S}) = q(c_{S})}, where \eqn{p} and \eqn{q} denote, respectively, the specificity (or true negative fraction) and sensitivity (or true positive fraction). Geometrically, it is the point where the ROC curve and the line \eqn{y = 1 - x} (the perpendicular to the positive diagonal line) intersect, and it can also be seen as the point that maximizes simultaneously both types of correct classifications (Riddle and Stratford, 1999; Gallop et al., 2003) corresponding, therefore, to the probability of correctly classifying any subject, whether it is healthy or diseased (\enc{Jiménez}{Jimenez}-Valverde et al., 2012; 2014).

Taking into account the costs associated to the false positives and false negatives misclassifications \eqn{C_{FP}} and \eqn{C_{FN}}, an extension of the Symmetry point, the Generalized Symmetry point \eqn{c_{GS}}, can be defined as follows (\enc{López}{Lopez}-\enc{Ratón}{Raton} et al., 2015):
\deqn{\rho (1-p(c_{GS})) = 1-q(c_{GS})}
where \eqn{\rho = \frac{C_{FP}}{C_{FN}}} is the relative loss (cost) of a false positive classification as compared with a false negative classification.
Analogously to the Symmetry point, \eqn{c_{GS}} is obtained graphically by the intersection point between the ROC curve and the line \eqn{y = 1 - \rho x}.

In this package, the two methods proposed in \enc{López}{Lopez}-\enc{Ratón}{Raton} et al. (2015) for estimating the Generalized Symmetry point and its sensitivity and specificity indexes are available:

\code{"GPQ"}: Method based on the Generalized Pivotal Quantity (Weerahandi, 1993; 1995; Lai et al., 2012). It assumes that the diagnostic test on both groups or a monotone Box-Cox transformation is Normal distributed. So, the Generalized Symmetry point \eqn{c_{GS}} can be estimated from the following equation:
\deqn{\Phi(a+b\Phi^{-1}(t)) = 1-\rho t  \Leftrightarrow  \Phi \left(\frac{\Phi^{-1}(1-\rho t)-a}{b}\right)-t=0}
where \eqn{a=\frac{\mu_1-\mu_0}{\sigma_1}}, \eqn{b=\frac{\sigma_0}{\sigma_1}}, \eqn{t=1-p(c_{GS})} and \eqn{\Phi} denotes the standard Normal cumulative distribution function (cdf), with \eqn{\mu_i} and \eqn{\sigma_i}, i = 0,1, the mean and standard deviation of healthy (\eqn{i}=0) and diseased (\eqn{i}=1) populations, respectively.
To check the assumption of normality, the Shapiro-Wilk test is used with a significance level of 5\%.

\code{"EL"}: Method based on the Empirical Likelihood (Thomas and Grunkemeier, 1975). It takes into account that \eqn{c_{GS}} can be seen as two specific quantiles, the \eqn{p(c_{GS})}-th quantile of the healthy population and the \eqn{\rho (1-q(c_{GS}))}-th quantile of the diseased population. Following the same reasoning as in Molanes-\enc{López}{Lopez} and \enc{Letón}{Leton} (2011), and considering that the value of \eqn{p(c_{GS})} is known in advance and the Generalized Symmetry point defines an operating point on the ROC curve fulfilling \eqn{1-x=p(c_{GS})}, the following adjusted empirical log-likelihood ratio function is derived to make inference on \eqn{c_{GS}}:
\deqn{\ell(c)=2n_0\hat{F}_{0,g_{0}}(c)\log\!\left(\frac{\hat{F}_{0,g_{0}}(c)}{p(c)}\right) +2n_0(1-\hat{F}_{0,g_{0}}(c))\log\left(\frac{1-\hat{F}_{0,g_0}(c)}{1-p(c)}\right)}
\deqn{+2n_1\hat{F}_{1,g_{1}}(c)\log\left(\frac{\hat{F}_{1,g_{1}}(c)}{\rho(1-p(c))}\right)
+2n_1(1-\hat{F}_{1,g_{1}}(c))\log\left(\frac{1-\hat{F}_{1,g_{1}}(c)}{1-\rho (1-p(c))}\right)\!,
}

where \eqn{\hat{F}_{i,g_{i}}(y)=\frac{1}{n_i}\sum_{k_i=1}^{n_i}K\left(\frac{y-Y_{ik_i}}{g_{i}}\right)} are kernel-type estimates of the cdfs \eqn{F_{i}}, of the two populations, \eqn{i=0,1}, with \eqn{K(y)=\int_{-\infty}^{y} K(z)\mathrm{d}z} a kernel function and \eqn{g_i} the smoothing parameter, for \eqn{i=0,1}.
}

\value{
Returns an object of class "gsym.point" with the following components: 

  \item{methods}{a character vector with the value of the \code{methods} argument used in the call.}    
  \item{levels.cat}{a character vector indicating the levels of the categorical covariate if the    \code{categorical.cov} argument in the \code{gsym.point} function is not NULL.}
  \item{call}{the matched call.}
  \item{data}{the data frame with the variables used in the call.}  
    
 For each of the methods used in the call, a list with the following components is obtained:  
                
       \item{"optimal.result"}{a list with the Generalized Symmetry point and its associated sensitivity and specificity accuracy measures with the corresponding confidence intervals.}
                     
        \item{"AUC"}{a list with the numerical value of the Area Under the ROC Curve.}
        
        \item{"rho"}{the numerical value of the cost ratio \eqn{\rho = \frac{C_{FP}}{C_{FN}}}.}				
	      
        \item{"pvalue.healthy"}{the numerical value of the p-value obtained by the Shapiro-Wilk normality test for checking the normality assumption of the marker in healthy population.}
        
        \item{"pvalue.diseased"}{the numerical value of the p-value obtained by the Shapiro-Wilk normality test for checking the normality assumption of the marker in diseased population.}
				
				In addition, if original data are not normally distributed the following components also appears:
				
				\item{"lambda"}{the estimated numerical value of the power in the Box-Cox transformation.}
				   
    		\item{"normality.transformed"}{a character string indicating if the transformed marker values by the Box-Cox transformation are normally distributed ("yes") or not ("no").}
    	       
        \item{"pvalue.healthy.transformed"}{the numerical value of the p-value obtained by the Shapiro-Wilk normality test for checking the normality assumption of the Box-Cox transformed marker in healthy population.}
        
        \item{"pvalue.diseased.transformed"}{the numerical value of the p-value obtained by the Shapiro-Wilk normality test for checking the normality assumption of the Box-Cox transformed marker in diseased population.}           
}

\references{
Gallop, R.J., Crits-Christoph, P., Muenz, L.R. and Tu, X.M. (2003). Determination and interpretation of the optimal operating point for ROC curves derived through generalized linear models. \emph{Understanding Statistics} \bold{2}, 219-242.

\enc{Jiménez}{Jimenez}-Valverde, A. (2012). Insights into the area under the receiver operating characteristic curve (AUC) as a discrimination measure in species distribution modelling. \emph{Global Ecology and Biogeography}  \bold{21}, 498-507.

\enc{Jiménez}{Jimenez}-Valverde, A. (2014). Threshold-dependence as a desirable attribute for discrimination assessment: implications for the evaluation of species distribution models. \emph{Biodiversity Conservation} \bold{23}, 369-385 

Lai, C.Y., Tian, L. and Schisterman, E.F. (2012). Exact confidence interval estimation for the Youden index and its corresponding optimal cut-point. \emph{Comput. Stat. Data Anal.} \bold{56}, 1103-1114.

\enc{López}{Lopez}-\enc{Ratón}{Raton}, M., Cadarso-\enc{Suárez}{Suarez}, C., Molanes-\enc{López}{Lopez}, E.M. and \enc{Letón}{Leton}, E. (2015). Confidence intervals for the Symmetry point: an optimal cutpoint in continuous diagnostic tests. (Submitted).

Metz, C.E. (1978). Basic Principles of ROC Analysis. \emph{Seminars in Nuclear Medicine} \bold{8}, 183-298.

Molanes-\enc{López}{Lopez}, E.M. and \enc{Letón}{Leton}, E. (2011). Inference of the Youden index and associated threshold using empirical likelihood for quantiles. \emph{Statistics in Medicine} \bold{30}, 2467-2480.

Molanes-\enc{López}{Lopez}, E.M., Van Keilegom, I. and Veraverbeke, N. (2009). Empirical likelihood for non-smooth criterion functions. \emph{Scandinavian Journal of Statistics} \bold{36}, 413-432.

Remaley, A.T., Sampson, M.L., DeLeo, J.M., Remaley, N.A., Farsi, B.D. and Zweig, M.H. (1999). Prevalence-value-accuracy plots: a new method for comparing diagnostic tests based on misclassification costs. \emph{Clinical Chemistry} \bold{45}, 934-941.

Riddle, D.L. and Stratford, P.W. (1999). Interpreting validity indexes for diagnostic tests: An illustration using the Berg Balance Test. \emph{Physical Therapy} \bold{79}, 939-948.

Rutter, C.M. and Miglioretti, D.L. (2003). Estimating the accuracy of psychological scales using longitudinal data. \emph{Biostatistics} \bold{4}, 97-107.

Thomas, D.R. and Grunkemeier, G.L. (1975). Confidence interval estimation of survival probabilities for censored data. \emph{Journal of the American Statistical Association} \bold{70}, 865-871.

Wand, M.P. and Jones, M.C. (1995). \emph{Kernel smoothing}. Chapman \& Hall, London.

Weerahandi, S. (1993). Generalized confidence intervals. \emph{Journal of the American Statistical Association} \bold{88}, 899-905. 

Weerahandi, S. (1995). \emph{Exact statistical methods for data analysis}. Springer-Verlag, New York.

Zhou, W. and Jing, B.Y. (2003). Adjusted empirical likelihood method for quantiles. \emph{Annals of the Institute of Statistical Mathematics} \bold{55}, 689-703.
}

\author{
	\enc{Mónica}{Monica} \enc{López}{Lopez}-\enc{Ratón}{Raton}, Carmen Cadarso-\enc{Suárez}{Suarez}, Elisa M. Molanes-\enc{López}{Lopez} and Emilio \enc{Letón}{Leton}
}

\seealso{
\code{\link{control.gsym.point}}, \code{\link{summary.gsym.point}}
}

\examples{
library(GsymPoint)

data(melanoma)

###########################################################
# marker: X
# status: group
###########################################################

###########################################################
# Generalized Pivotal Quantity Method ("GPQ"): 
# Original data normally distributed
###########################################################

gsym.point.GPQ.melanoma<-gsym.point(methods = "GPQ", data = melanoma,
marker = "X", status = "group", tag.healthy = 0, categorical.cov = NULL, 
CFN = 1, CFP = 1, control = control.gsym.point(),confidence.level = 0.95, 
trace = FALSE, seed = FALSE, value.seed = 3)

summary(gsym.point.GPQ.melanoma)

plot(gsym.point.GPQ.melanoma)


data(prostate)

###########################################################
# marker: marker
# status: status
###########################################################

###########################################################
# Generalized Pivotal Quantity Method ("GPQ"): 
# Box-Cox transformed data normally distributed
###########################################################

gsym.point.GPQ.prostate <- gsym.point (methods = "GPQ", data = prostate,
marker = "marker", status = "status", tag.healthy = 0, categorical.cov = NULL, 
CFN = 1, CFP = 1, control = control.gsym.point(), confidence.level = 0.95, 
trace = FALSE, seed = FALSE, value.seed = 3)

summary(gsym.point.GPQ.prostate)

plot(gsym.point.GPQ.prostate)


data(elastase)

###########################################################
# marker: elas
# status: status
###########################################################

###########################################################
# Generalized Pivotal Quantity Method ("GPQ"):
# Original data not normally distributed 
# Box-Cox transformed data not normally distributed
###########################################################

gsym.point.GPQ.elastase <- gsym.point(methods = "GPQ", data = elastase, 
marker = "elas", status = "status", tag.healthy = 0, categorical.cov = NULL, 
CFN = 1, CFP = 1, control = control.gsym.point(), confidence.level = 0.95, 
trace = FALSE, seed = FALSE, value.seed = 3) 

summary(gsym.point.GPQ.elastase)

plot(gsym.point.GPQ.elastase)

}