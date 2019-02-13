# Calculate PSI
#
# @param varA  
# @param varB 
# @param intv interval partition of variable, needing manual ajustment
# @param verbose  if TRUE, print frequency dataframe

psi.calc <- function(varA, varB, intv, verbose=FALSE) {
  library(dplyr)
  
  if(!(is.numeric(varA)|is.factor(varA)|is.character(varA))) {
    print("type error: it's not type numeric, factor, or character")
    break
  }
  if(is.numeric(varA)) {
    varA_intv <- cut(varA, intv)
    varB_intv <- cut(varB, intv)
  } 
  else {
    varA_intv <- varA
    varB_intv <- varB
  }
  
  varA_intv <- as.character(varA_intv)
  varB_intv <- as.character(varB_intv)
  varA_intv[is.na(varA_intv)] <- "Missing"
  varB_intv[is.na(varB_intv)] <- "Missing"
  varA_frame <- data.frame(table(varA_intv))
  varB_frame <- data.frame(table(varB_intv))
  varA_frame$varA_perc <- varA_frame$Freq/sum(varA_frame$Freq)
  varB_frame$varB_perc <- varB_frame$Freq/sum(varB_frame$Freq)
  var_frame <- merge(varA_frame, varB_frame, by.x="varA_intv", by.y="varB_intv")
  var_frame <- select(var_frame, var_intv=varA_intv, varA_freq=Freq.x, varB_freq=Freq.y, varA_perc, varB_perc)
  if(verbose==TRUE) print(var_frame)
  psi <- with(var_frame, sum((varA_perc - varB_perc)*log(varA_perc/varB_perc)))
  
  return(psi)
}



