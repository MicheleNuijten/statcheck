calc_APA_factor <- function(pRes, Res){
  
  # select only the results of pRes that are from articles with at least 1 statcheck result
  pRes_selection <- pRes[pRes$Source %in% Res$Source, ]
  
  # Source should not be a factor. This would result in a bug down the road, if
  # one of the sources didn't have any APA results:
  # Error in by(Res_selection, Res_selection$Source, nrow)/by(pRes_selection,  : 
  # non-conformable arrays
  pRes_selection$Source <- as.vector(pRes_selection$Source)
  
  # select only the statcheck results that are from an article with at least one p value
  # this is relevant, because it sometimes happens that statcheck extracts less p values
  # p values than statcheck results. For instance in cases when a p value appears to be
  # greater than 1.
  
  Res_selection <-
    Res[Res$Source %in% pRes_selection$Source, ]
  APA <-
    by(Res_selection, Res_selection$Source, nrow) / by(pRes_selection, pRes_selection$Source, nrow)
  
  APAfactor <-
    round(as.numeric(apply(Res, 1, function(x)
      APA[names(APA) == x["Source"]])), 2)
  
  return(APAfactor)
  
}
