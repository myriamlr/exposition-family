continueResampling <- function(cycle.time,test.iters){
	iter.time <- (cycle.time[1] * test.iters)/60 #seconds for iters into minutes.	
	print(paste("It is estimated that your iterations will take",round(iter.time,digits=2),"minutes.",sep=" "))
	valueCaptured <- ""
	if(iter.time > 1){ #greater than 1 minute.
		while(tolower(valueCaptured) != "n" && tolower(valueCaptured) != "y"){
			print("Do you want to proceed: Y/N")
			valueCaptured <- readline()	
		}
		# if(tolower(valueCaptured)=="y"){
			# print("Proceeding with resample-based tests. Please take note of the progress bar.")
			# return(TRUE)
		# }
		if(tolower(valueCaptured)=="n"){
			print("Inference tests exiting. Output is strictly descriptive (fixed effects) results.")
			return(FALSE)
		}
	}
	return(TRUE)
}