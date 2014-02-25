r2t <- function(# Transform r values into t values
	### Function to transform r values into t values by use of raw r and degrees of freedom.
	r,
	### Raw correlation value
	df
	### Degrees of freedom (N-1)
	){
          r / (sqrt((1-r^2)/df))
        }