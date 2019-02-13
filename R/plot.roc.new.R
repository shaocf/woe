
# Plot ROC Curve and calculate AUC
# 
# @param actual_label  true class label, responsive variable in dataset
# @param pred_proba  predictive probability of dataset
# @example  
# predProba <- predict(model_fit, data, type='response')  
# roc.plot(data$class_label, pred_proba)

plot.roc.new <- function(actual_label, pred_proba) {
  library(pROC)
  
  # ROC of train set 
  model_roc <- roc(actual_label, pred_proba)
  plot(model_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.1), 
       auc.polygon.col='skyblue')
  title("ROC Curve")
}
  

