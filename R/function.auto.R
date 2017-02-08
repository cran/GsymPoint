function.auto <-
function(data, marker, status, tag.healthy = 0, CFN, CFP, control = control.gsym.point(), confidence.level, seed, value.seed)
{
	GPQ <- function.GPQ(data, marker, status, tag.healthy, CFN, CFP, control, confidence.level, seed, value.seed)
	
	# If original data are normally distributed:  
	if (!"lambda" %in% names(GPQ))
	{
	 	res <- list (optimal.result = GPQ$optimal.result, AUC = GPQ$AUC, rho = GPQ$rho, pvalue.healthy = GPQ$pvalue.healthy, pvalue.diseased = GPQ$pvalue.diseased)  
 	} 
	
	# If original data are not normally distributed: 
	if ("lambda" %in% names(GPQ))
	{
		# If transformed data are normally distributed: 

		if (GPQ$normality.transformed == "yes") 
		{
			res <- list (optimal.result = GPQ$optimal.result, AUC = GPQ$AUC, rho = GPQ$rho, lambda = GPQ$lambda, normality.transformed = GPQ$normality.transformed, pvalue.healthy = GPQ$pvalue.healthy, pvalue.diseased = GPQ$pvalue.diseased, pvalue.healthy.transformed = GPQ$pvalue.healthy.transformed, pvalue.diseased.transformed = GPQ$pvalue.diseased.transformed)     
   		} 
		
		# If transformed data are not normally distributed: 

		if (GPQ$normality.transformed == "no") 
		{
			EL <- function.EL(data, marker, status, tag.healthy, CFN, CFP, control, confidence.level, seed, value.seed)
			res <- list(optimal.result = EL$optimal.result, AUC = EL$AUC, rho = EL$rho, lambda = GPQ$lambda, normality.transformed= GPQ$normality.transformed, pvalue.healthy = GPQ$pvalue.healthy, pvalue.diseased = GPQ$pvalue.diseased, pvalue.healthy.transformed = GPQ$pvalue.healthy.transformed, pvalue.diseased.transformed = GPQ$pvalue.diseased.transformed)  
  		} 
     	}
	return(res) 
}
