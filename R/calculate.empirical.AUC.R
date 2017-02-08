calculate.empirical.AUC <-
function(data, marker, status, tag.healthy) {
      marker.diseased = data[data[,status] != tag.healthy, marker]
    	n.diseased = length (marker.diseased)

    	marker.healthy = data[data[,status] == tag.healthy, marker]
    	n.healthy = length(marker.healthy)

    	marker.diseasedmat <- matrix(rep(marker.diseased,n.healthy), nrow = n.healthy, byrow = T)
    	marker.healthymat <- matrix(rep(marker.healthy,n.diseased), nrow = n.healthy, byrow = F)
    	diffmat <- marker.healthymat-marker.diseasedmat
    	AUC <- (length(diffmat[diffmat < 0])+0.5*length(diffmat[diffmat == 0]))/(n.diseased*n.healthy)
    	
    	res <- AUC
    	return(res)
}
