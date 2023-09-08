library(ROSE)

rebalance <- function(data, target, alpha, beta){
  oversampling(undersampling(data, target, beta), target, alpha)
}

undersampling <- function(data, target, beta){
  negatives = sum(target==0)
  positives = sum(target==1)
  new_negatives=negatives/beta
  N=positives+as.integer(new_negatives)
  data_balanced = ovun.sample(TenYearCHD~., data=data, method="under", N=N)$data
  data_balanced
}

oversampling <- function(data, target, alpha){
  positives = sum(target==1)
  new_positives=positives/alpha
  N=nrow(data)-positives+as.integer(new_positives)
  
  data_balanced = ovun.sample(TenYearCHD~., data=data, method="over", N=N)$data
  data_balanced
}


precision <- function(pred, outcome){
  true_positives <- sum(pred == 1 & outcome == 1)
  false_positives <-  sum(pred == 1 & outcome == 0)
  true_positives/(true_positives+false_positives)
}

true_precision <- function(prec, gamma){
  prec/(prec+gamma*(1-prec))
}

sensitivity <- function(pred, outcome){ #recall
  true_positives <- sum(pred == 1 & outcome == 1)
  positives <- sum(outcome == 1)
  true_positives/positives
}

accuracy <- function(pred, outcome){
  true_positives <- sum(pred == 1 & outcome == 1)
  true_negatives <- sum(pred == 0 & outcome ==0)
  positives <- sum(outcome == 1)
  negatives <- sum(outcome == 0)
  (true_positives+true_negatives)/(positives+negatives)
}

true_accuracy <- function(pred, outcome, gamma){
  true_positives <- sum(pred == 1 & outcome == 1)
  true_negatives <- sum(pred == 0 & outcome ==0)
  positives <- sum(outcome == 1)
  negatives <- sum(outcome == 0)
  (true_positives+gamma*true_negatives)/(positives+gamma*negatives)
}