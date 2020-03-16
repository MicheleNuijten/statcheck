check_alpha_levels <- function(Res, pEqualAlphaSig, messages) {
  
  DecisionErrorAlphas <- logical()
  alphas <- c(.01, .1)
  
  for (a in alphas) {
    DecisionErrorAlphas <-
      c(DecisionErrorAlphas, DecisionErrorTest(Res, alpha = a, 
                                               pEqualAlphaSig = pEqualAlphaSig))
  }
  
  if(messages == TRUE & 
     any(DecisionErrorAlphas[!is.na(DecisionErrorAlphas) &
                             !is.nan(DecisionErrorAlphas)])) {
    message(
      "\n Check the significance level. \n \n Some of the p value incongruencies are decision errors if the significance level is .1 or .01 instead of the conventional .05. It is recommended to check the actual significance level in the paper or text. Check if the reported p values are a decision error at a different significance level by running statcheck again with 'alpha' set to .1 and/or .01. \n "
    )
  }
}