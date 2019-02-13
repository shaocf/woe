# Plot KS-Curve and calculate KS-value
# 
# @param actual_label  true class label, responsive variable in dataset
# @param pred_proba  predictive probability of dataset
# @example
# pred_proba <- predict(model_fit, data, type='response')
# plot.ks(data$calss_label, pred_proba)

plot.ks <- function(actual_label, pred_proba) {
  library(ROCR)
  
  pred_result <- prediction(pred_proba, actual_label)
  tpr <- performance(pred_result, measure='tpr')@y.values[[1]]
  fpr <- performance(pred_result, measure='fpr')@y.values[[1]]
  ks <- (tpr-fpr)
  depth <- performance(pred_result, measure='rpp')@y.values[[1]]
  plot(depth, ks, type='l', main='KS-Curve', ylab='ks', xlab='depth', ylim=c(0,1))
  lines(depth, tpr,col='2')
  lines(depth, fpr, col='3')
  kslabel <- c(paste("KS:", round(max(ks),3),sep=""), paste("tpr:", round(tpr[which(ks==max(ks))],3), sep=""), paste("fpr:", round(fpr[which(ks==max(ks))],3), sep=""))
  legend(0.0, 1.0, c(kslabel), bty="n", ncol = 1)
  legend(0.6, 1.0, legend=c("TPR", "FPR","TPR-FPR"), col=c("2", "3","1"), cex=0.6, lwd=2)
}