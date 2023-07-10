args <- commandArgs(trailingOnly = TRUE)
print(args)
print(typeof(args))

if (length(args) == 3){
	beta1      <<- as.numeric(args[1])
	delta1     <<- as.numeric(args[2]) 
	epsilon1 <<- as.numeric(args[3]) 
} else {
	
	
	----------------------------------------------
	
	