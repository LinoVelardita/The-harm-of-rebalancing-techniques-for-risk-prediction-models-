results <- readRDS("cases_study_results.RDS")

#Barplot to visually compare performance measures among different classifiers

classifiers_names <- c("SLR", "Down", "Up", "Smote", "Ada",
                       "Ridge", "R-Down", "R-Up", "R-SMote", "R-Ada")

precision <- results[,4]
bar_heights <- precision

#barplot
bp <- barplot(bar_heights, names.arg = classifiers_names, col = "lightblue",
        main = "Classifiers Precision", xlab = "Classifiers", ylab = "Observed Precision",
        cex.names=1, ylim=c(0,0.9))


bar_width <- bp[2]-bp[1]
bar_colors <- c(rep("sienna", 5), rep("bisque3", 5))
for(i in 1:10){
  rect(bp[i]-bar_width/2, 0, bp[i]+bar_width/2, bar_heights[i], col=bar_colors[i])
}

text(x=bp, y=bar_heights, labels=round(bar_heights, 2), pos=3, cex=1.5, col="black")


#--------- warping posterior probability analysis-------#
library(ggplot2)
library(tidyverse) 
library(glmnet)
library(glmnetUtils)
library(caret) 
library(smotefamily) 
library(tidyverse) 
library(rjags)
#imbalance
library(caret) 
library(smotefamily) 

# Performance
library(xtable) 
library(CalibrationCurves)
library(rmda)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("correction_under_shift.R")

data = read.csv("df_8predictors.csv")

#dynamic selection of predictors
num_var=3 #CHANGE {3, 6, 8}
predictor_vars = c("age", "sysBP", "diaBP", "glucose", "totChol", "BMI", "cigsPerDay", "heartRate")
selected_vars = predictor_vars[1:num_var]

formula_str = paste("TenYearCHD ~", paste("rcs(", selected_vars, ", nk = 2)", collapse = " + "))
data = cbind(data["TenYearCHD"], data[,selected_vars])

# Sample indexes to select data for train set
train_index = sample(x = nrow(data), 
                     size = round(0.8 * length(data$TenYearCHD)), 
                     replace = FALSE)

# Split data into train and test set
train_set = data[train_index,]
test_set = data[-train_index,]
x = train_set[,2:ncol(train_set)] 
y = as.factor(train_set[,1]) 
y_test = test_set[,1]

library(ROSE)

table(train_set$TenYearCHD)


#Original Ratio
P_orig = sum(data$TenYearCHD==1)
N_orig = sum(data$TenYearCHD==0)
Orig_Ratio = N_orig/P_orig #5.5

alpha = seq(0.2, 1, 0.2) #oversampling
beta = seq(1, 5, 1) #undersampling
gamma <- list()

for (i in alpha) {
  for (j in beta) {
    gamma <- c(gamma, list(c(i, j)))
  }
}
print(gamma)
calculate_ratio <- function(pair) {
  return(pair[[2]] / pair[[1]])
}
ratios <- sapply(gamma, calculate_ratio)
print(ratios)
order_indices <- order(ratios)
gamma_ordered <- gamma[order_indices]
print(gamma_ordered)

n = length(gamma)

train_balanced = rebalance(train_set, train_set$TenYearCHD, 0.5, 2)
table(train_balanced$TenYearCHD)

models_prec <- list()
models_true_prec <- list()
models_sensitivity <- list()
models_acc <- list()
models_true_acc <- list()

for(i in 1:n){
  train_balanced = rebalance(train_set, train_set$TenYearCHD, gamma_ordered[[i]][1], gamma_ordered[[i]][2])
  table(train_balanced$TenYearCHD)
  log_model = glm(formula = formula_str, family = 'binomial', data = train_balanced)
  
  log_probs = predict(log_model, test_set, type = 'response')
  log_pred = rep(0, length(log_probs))
  log_pred[log_probs > .5] = 1
  
  prec = precision(log_pred, y_test)
  true_prec = true_precision(prec, gamma_ordered[[i]][2]/gamma_ordered[[i]][1]) #beta/alpha
  sensit = sensitivity(log_pred, y_test)
  accur = accuracy(log_pred, y_test)
  true_accur = true_accuracy(log_pred, y_test, gamma_ordered[[i]][2]/gamma_ordered[[i]][1])
  
  models_prec <- append(models_prec, prec)
  models_true_prec <- append(models_true_prec, true_prec)
  models_sensitivity <- append(models_sensitivity, sensit)
  models_acc <- append(models_acc, accur)
  models_true_acc <- append(models_true_acc, true_accur)
}

summary <- data.frame(gamma=ratios, prec=models_prec, true_prec=models_true_prec)
summary
print(models_prec)
print(models_true_prec)
print(models_sensitivity)
print(ratios)


data_df <- data.frame(ratios = unlist(ratios),
                      models_prec = unlist(models_prec),
                      models_true_prec = unlist(models_true_prec),
                      models_sensitivity = unlist(models_sensitivity),
                      models_acc = unlist(models_acc),
                      models_true_acc = unlist(models_true_acc))

plt_prec <- ggplot(data_df, aes(x = ratios)) +
  geom_point(aes(y=models_prec, color="Observed Precision"), show.legend=TRUE) +
  geom_point(aes(y=models_true_prec, color="True Precision"), show.legend=TRUE) +
  labs(x = "Gamma", y = "Models Precision", title = "") +
  theme(legend.position="bottom")+
  guides(color = guide_legend(title = ""))

plt_prec


plt_sensitivity <- ggplot(data_df, aes(x = ratios)) +
  geom_point(aes(y=models_sensitivity)) +
  labs(x = "Gamma", y = "Recall", title = "")

plt_sensitivity

plt_acc <- ggplot(data_df, aes(x = ratios)) +
  geom_point(aes(y=models_acc, color="Observed Accuracy"), show.legend=TRUE) +
  geom_point(aes(y=models_true_acc, color="True Accuracy"), show.legend=TRUE) +
  labs(x = "Gamma", y = "Models Accuracy", title = "") +
  theme(legend.position="bottom")+
  guides(color = guide_legend(title = ""))

plt_acc
