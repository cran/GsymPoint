plot.gsym.point <-
function(x, xlab, ylab, main,...) {
  op <- par(pty = "s")
      methods <- x[x$methods]
      	
	n.levels.cat <- if(is.null(x$levels.cat)) {1} else {length(x$levels.cat)}
	levels.cat <- if(is.null(x$levels.cat)) {NULL} else {x$levels.cat}
      n.plots=0

	TPR <- matrix(NA,nrow=n.levels.cat,ncol=length(methods))
	FPR <- matrix(NA,nrow=n.levels.cat,ncol=length(methods))
	
	for (i in 1:n.levels.cat) 
	{
	  dev.new(noRStudioGD = TRUE)
	  dev.set(which = dev.cur())
	  
	  for(j in 1:length(methods)) 
		{
			if(length(methods[[j]][[i]][["optimal.result"]][[1]])== 0) 
			{
				if(is.null(x$levels.cat)) {
					cat(paste(names(methods)[j], ": There are no cutoff values that fulfill the criterion \n", sep = ""))
				} else {
					cat(paste(names(methods)[j], ": There are no cutoff values that fulfill the criterion for ", levels.cat[i], "\n", sep = ""))
				}
			} 	
			
     	     		m <- methods[[j]][[i]]

			if (n.levels.cat >1)
			{
				# Marker in the healthy population:
      				X0 <- x$data[x$data[,x$call$status] == x$call$tag.healthy & x$data[,x$call$categorical.cov] == levels.cat[i], x$call$marker]
      				# Marker in the diseased population:
      				X1 <- x$data[x$data[,x$call$status] != x$call$tag.healthy & x$data[,x$call$categorical.cov] == levels.cat[i], x$call$marker]
			}

			else
			{
				# Marker in the healthy population:
      				X0 <- x$data[x$data[,x$call$status] == x$call$tag.healthy, x$call$marker]
      				# Marker in the diseased population:
      				X1 <- x$data[x$data[,x$call$status] != x$call$tag.healthy, x$call$marker]
			}
				
      			x0 = relative_sample(X0,X1,0)
      			rho = m[["rho"]]			
			
			if (missing(xlab))
			{                 
				xlab <- paste("False Positive Rate")
     	     		}

			if (missing(ylab))
			{                 
				ylab <- paste("True Positive Rate")
     	     		}
	
      			parameters = c(rho,x0)
      			x_axis = seq(0,1,length.out=1000)
      		
			TPR[i,j]<-m[["optimal.result"]]$Sensitivity[1]
			FPR[i,j] <-1-m[["optimal.result"]]$Specificity[1]			

			X0 <- sort(X0)
			X1 <- sort(X1)

			if (n.levels.cat >1)
			{
				X <- sort(x$data[x$data[,x$call$categorical.cov] == levels.cat[i], x$call$marker])
			}

			else
			{
				X <- sort(x$data[, x$call$marker])
			}                                    
                  
                  F0 = cdf_empirical_dist(X0,X)                  
			F1 = cdf_empirical_dist(X1,X)			
			F0 = c(0,F0,1)			
			F1 = c(0,F1,1)			        
			          							
			if (missing(main))
			{				               
			  plot(1-F0,1-F1,type="l",xlim=c(0,1),ylim=c(0,1),xlab=xlab,ylab=ylab, main = paste("Empirical ROC Curve and line y = 1-",rho, "x", "\n", " ", ifelse(is.null(levels.cat), "", levels.cat[i]), sep = ""), cex.main=1,...)
			  }

			else
			{
			  plot(1-F0,1-F1,type="l",xlim=c(0,1),ylim=c(0,1),xlab=xlab,ylab=ylab, main = main, cex.main=1,...)
			  }

			lines(x_axis, 1-rho*x_axis, lty = 2)
						
			if (length(methods) == 1)
			{
				legend.coordinatesGPQ<-paste("GPQ:"," ", "(",round(FPR[i,],3), ", ", round(TPR[i,],3),")", sep = "")				
				legend.coordinatesEL<-paste("EL:"," ", "(",round(FPR[i,j],3), ", ", round(TPR[i,j],3),")", sep = "")
				legend.AUC <- paste("AUC: ",round(m[["AUC"]], 3))
				 
				if ((names(methods) == "GPQ") | (names(methods) == "auto" & !"normality.transformed" %in% names(m)))
				{
					points(FPR[i,j],TPR[i,j], pch = 21:00)						
					legend("bottom", c(legend.coordinatesGPQ,legend.AUC), pch = c(21,NA),bty = "n")		
				}

				else if (names(methods) == "EL") 
				{
					points(FPR[i,j],TPR[i,j], pch = 21:00, bg="black")
					legend("bottom", c(legend.coordinatesEL,legend.AUC), pch = c(19,NA),bty = "n")		
				}

				else if (names(methods) == "auto" & "normality.transformed" %in% names(m) & m$normality.transformed == "yes")
				{
					points(FPR[i,j],TPR[i,j], pch = 21:00)				
					legend("bottom", c(legend.coordinatesGPQ,legend.AUC), pch = c(21,NA),bty = "n")		
				}
				
				else if (names(methods) == "auto" & "normality.transformed" %in% names(m) & m$normality.transformed == "no")
				{
					points(FPR[i,j],TPR[i,j], pch = 21:00, bg="black")
					legend("bottom", c(legend.coordinatesEL,legend.AUC), pch = c(19,NA),bty = "n")					
				}
			}

			else
			{
				if ("GPQ" == names(methods)[1])    
				{
					points(FPR[i,1],TPR[i,1], pch = 21:00)
					points(FPR[i,2],TPR[i,2], pch = 21:00, bg="black")	
					legend.coordinatesGPQ<-paste("GPQ:"," ", "(",round(FPR[i,1],3), ", ", round(TPR[i,1],3),")", sep = "")
					legend.coordinatesEL<-paste("EL:"," ", "(",round(FPR[i,2],3), ", ", round(TPR[i,2],3),")", sep = "")     					
				}						

				else 
				{
					points(FPR[i,2],TPR[i,2], pch = 21:00)
					points(FPR[i,1],TPR[i,1], pch = 21:00, bg="black")	
					legend.coordinatesGPQ<-paste("GPQ:"," ", "(",round(FPR[i,2],3), ", ", round(TPR[i,2],3),")", sep = "")
					legend.coordinatesEL<-paste("EL:"," ", "(",round(FPR[i,1],3), ", ", round(TPR[i,1],3),")", sep = "")     					
				}

				legend.AUC <- paste("AUC: ",round(m[["AUC"]], 3))
      				legend("bottom", c(legend.coordinatesGPQ,legend.coordinatesEL,legend.AUC), pch = c(21,19,NA), bty = "n")					
			}					
			
              	n.plots = n.plots + 1
		}
	}
	par(op)
}
