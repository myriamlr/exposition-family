mpOutputHandler <-
function(res=NULL,mpPlotInfo=NULL){

	if(!is.null(res) && !is.null(mpPlotInfo)){
		final.output <- list(mexPosition.Data=res,Plotting.Data=mpPlotInfo)
		class(final.output) <- c("mexPosition.Output","list")	
		return(final.output)
	}else if(!is.null(res) && is.null(mpPlotInfo)){
		return(res)
	}else{
		print("Unknown inputs. mpOutputHandler must exit.")	
		return(0)
	}
	
	print("It is unknown how this was executed. mpOutputHandler must exit.")
	return(0)
}
