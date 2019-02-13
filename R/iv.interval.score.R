# Calculate interval score of per variable after WOE transformation
#
# @param P0  score corresponding to Odds
# @param theta0  specified Odds
# @param PDO  score corresponding to double Odds
# @param iv  result of iv.mult(..., summary=FALSE)
# @param model_coefs  model coefficients vector of logistic regression
# @examples 
# coefs <- glm.fit$coefficients
# interval.score(600, 20, 1/60, iv, coefs)

iv.interval.score <- function(P0, PDO, theta0, iv, model_coefs) {
  # here according to definition: Score = A - B*log(Odds)
  B <- PDO/log(2)
  A <- P0 + PDO*log(theta0)/log(2)

  base_score <- round(A - B*as.numeric(model_coefs[1]), 0)

  for(i in seq_along(iv)) {
    iv_one <- iv[[i]]
    var_score <- iv_one[, c("variable", "class", "woe")]
    score <- round(-B*model_coefs[[i+1]]*var_score$woe, 0)
    iv[[i]]$score <- score
  }

  iv_list <- list("A"=A, "B"=B, "base_score"=base_score, "iv"=iv)

  return(iv_list)
}