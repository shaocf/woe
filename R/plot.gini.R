# Plot KS-Curve and calculate KS-value
# 
# @param actual_label  true class label, responsive variable in dataset
# @param pred_proba  predictive probability of dataset
# @example
# pred_proba <- predict(model_fit, data, type='response')
# plot.gini(data$calss_label, pred_proba)

plot.gini <- function(actual_label, pred_proba) {
  library(ineq)

  pred_result <- prediction(pred_proba, actual_label)
  fnr <- performance(pred_result, measure='fnr')@y.values[[1]] 
  tnr <- performance(pred_result, measure='tnr')@y.values[[1]] 
  plot(tnr, fnr, type='l', main='Gini', xlab='x-tnr', ylab='y-fnr', yaxs="i", xaxs="i", col=2)
  abline(a=0, b=1, col=1)
  kslable1 <- paste("Gini:", round(Gini(fnr), 3), sep="")
  legend(0.5, 0.5, c(kslable1), bty="n", ncol = 1)
  legend("bottomright", legend=c("RandomSelection", "ModelPick"), col=c("1", "2"), cex=0.75, lwd=2)
}