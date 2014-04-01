print.diagnose <- structure(function(# Print diagnose output.
	### Print the output of a \code{diagnose} object.
	x,
	## a \code{diagnose} object.
	...
	## further arguments passed to or from other methods.
	){
	##details<<
	## This dataframe contains the error diagnosis of the analyzed articles that contained an error and has the following components: 

	## This dataframe contains an overview of the detected copy-paste errors in all the articles. Its components are the same as the ones in ErrorDiagnosis. Note that a copy-paste error could still be congruent in terms of test statistic and p value, so it is possible that these errors do not show up in ErrorDiagnosis. 
	## This dataframe provides a summary of all detected errors. It summarizes the number of detected errors in each of the aforementioned categories per article. The copy-paste errors are counted in a way that two identical strings of results are counted as one copy-paste error.
	##seealso<<
	## \code{\link{diagnose}}, \code{\link{statcheck}}
  print(x[!(names(x)%in%"FullDiagnosis")])
  ##value<<
  ## a list that contains four dataframes: ErrorDiagnosis, CopyPaste, Summary, and FullDiagnosis. In the following sections the content of these dataframes will be described in detail. 
},ex=function(){
	# given that the articles of interest are saved in "DIR"
DIR <- "C:/mydocuments/articles"
stat_result <- checkdir(DIR)

diagnose(stat_result)
	})