# ----Model Building - Model 3:- Random forest

#---------------------------------------------------------    

# Package required for randomForest algorithm is:
# install randomForest
library(randomForest)
library(ggplot2)
#---------------------------------------------------------    
bank <- bank_final
#---------------------------------------------------------

# Spliting the bank data in 70:30 ratio

set.seed(101)

bank$response <- as.factor(ifelse(bank$response==1,"yes","no"))
split_indices <- sample.split(bank$response, SplitRatio = 0.70)

train_rf <- bank[split_indices, ]

test_rf <- bank[!split_indices, ]

nrow(train_rf)/nrow(bank)

nrow(test_rf)/nrow(bank)

#---------------------------------------------------------    

# Building the model 

bank_rf <- randomForest(response ~., data = train_rf, proximity = F, do.trace = T, mtry = 5)

# Predict response for test data

rf_pred <- predict(bank_rf, test_rf[, -20], type = "prob")

#---------------------------------------------------------    

# Cutoff for randomforest to assign yes or no

perform_fn_rf <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(rf_pred[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_rf$response, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}

#---------------------------------------------------------    

# creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
s = seq(.01,.99,length=100)

OUT_rf = matrix(0,100,3)

# calculate the sens, spec and acc for different cutoff values

for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rf(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()

legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.01)]

# The plot shows that cutoff value of around 22% optimises sensitivity and accuracy

predicted_response_22 <- factor(ifelse(rf_pred[, 2] >= 0.21, "yes", "no"))

conf_forest <- confusionMatrix(predicted_response_22, test_rf[, 20], positive = "yes")

conf_forest

# Sensitivity
conf_forest$byClass[1]

# Specificity 
conf_forest$byClass[2]

# Accuracy 
conf_forest$overall[1]


# Final RF important variables
importance <- bank_rf$importance 

importance <- data.frame(importance)