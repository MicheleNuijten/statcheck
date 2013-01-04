print.diagnose <- function(x,...){
  print(x[!(names(x)%in%"FullDiagnosis")])
}