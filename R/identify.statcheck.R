identify.statcheck <- structure(function(# Identify specific points in a \code{statcheck} plot.
	### With this function you can simply point and click on the datapoints in the plot to see the corresponding statcheck details, such as the paper from which the data came and the exact statistical results.
	x,
	### a \code{statcheck} object.
	...
	### additional arguments to be passed on to the plot method.
	){
	
  ##seealso<<
  ## \code{\link{statcheck}}
  reported <- x$Reported.P.Value
  computed <- x$Computed
  
  plot(x,...) # makes use of the plot.statcheck() function
  ID <- identify(reported,computed)
  
  res <- x[ID,]
  class(res) <- c("statcheck","data.frame")
  
  return(res)
  ##value<<
  ## This function returns both a plot and a dataframe. For the contents of the dataframe see \code{\link{statcheck}}.
},ex=function(){
	# given that the articles of interest are saved in "DIR"
DIR <- "C:/mydocuments/articles"
stat_result <- checkdir(DIR)

identify(stat_result)

## Further instructions:
# click on one or multiple points of interest
# press Esc
# a dataframe with information on the selected points will appear
	} )