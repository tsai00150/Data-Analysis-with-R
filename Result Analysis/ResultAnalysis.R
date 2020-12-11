# parameters: target badthre input output
# for input and output, enter absolute path, otherwise read.csv() and
# write.csv() will start from the folder where R is executed
target <- 'bad'
badthre <- 0.5
input <- c('C:/Users/Tsai/R projects/Data Analysis with R/Result Analysis/data/data1.csv',
           'C:/Users/Tsai/R projects/Data Analysis with R/Result Analysis/data/data2.csv',
           'C:/Users/Tsai/R projects/Data Analysis with R/Result Analysis/data/data3.csv')
output <- 'C:/Users/Tsai/R projects/Data Analysis with R/Result Analysis/result.csv'
#create the dataframe used for output
output_df <- data.frame()
#change the difference of the badthre
if(target=='good'){
  badthre <- (1-as.numeric(badthre))
}

#calculate each file one by one
for (i in c(1:length(input))){
  inputCsv <- read.csv(input[i])
  #if target=bad, continue
  if (target == 'bad'){
    rowNum <- nrow(inputCsv)
  }
  #if target=good, let 1 minus the pred score
  else if (target == 'good'){
    rowNum <- nrow(inputCsv)
    inputCsv$pred.score <- (1-inputCsv$pred.score)
  }else{print('Error')}
  
  #-----ACTUAL MODEL-----
  sum_likeli = 0 #set the sum number of the likelihood
  TP=0
  FP=0
  TN=0 
  FN=0
  #read line one by one,add prediction and calculate likelihood
  for (j in c(1:rowNum)){
    if (inputCsv$pred.score[j] > badthre){
      pred <- target
    }else {
      if (target == 'bad'){
        pred <- 'good'
      }else if (target == 'good'){
        pred <- 'bad'
      }
    }
    #calculate the matrix
    if (pred == inputCsv$reference[j]){
      if (target == pred){
        TP=TP+1
      }else if(target != pred){
        TN=TN+1
      }
    }else if(pred != inputCsv$reference[j]){
      if (target == pred){
        FP=FP+1
      }else if(target != pred){
        FN=FN+1
      }
    }
    # LOG LIKELIHOOD
    likeli_model <- ifelse(inputCsv$reference[j]==target, log(inputCsv$pred.score[j]), log(1-inputCsv$pred.score[j]))
    sum_likeli = sum_likeli + likeli_model
  }
  
  #-----NULL MODEL-----
  #calculate the proportion of "bad" loans
  null_bad_loans = 0
  for (j in c(1:rowNum)){
    if (inputCsv$reference[j]=='bad'){
      null_bad_loans=null_bad_loans+1
    }
  }
  null_pred.score <- null_bad_loans/rowNum
  #set the sum number of the likelihood
  null_sum_likeli = 0
  #read line one by one,calculate likelihood
  for (j in c(1:rowNum)){
    null_likeli_model <- ifelse(inputCsv$reference[j]=='bad', log(null_pred.score), log(1-null_pred.score))
    null_sum_likeli = null_sum_likeli + null_likeli_model
  }
  # calculate pseudo R-squared
  S <- 0
  pseudo_R_squared <- 1-((-2*(sum_likeli-S))/(-2*(null_sum_likeli-S)))
  
  #set up the parameters
  output_method <- gsub('.csv', '', basename(input[i]))
  output_sensitivity <- TP/(TP+FN)
  output_specificity <- TN/(TN+FP)
  precision <- TP/(TP+FP)
  recall <- TP/(TP+FN)
  output_F1 <- 2*precision*recall/(precision+recall)
  
  # Output the results of the method
  newrow <- data.frame(method=output_method,
                          sensitivity=output_sensitivity,
                          specificity=output_specificity,
                          F1=output_F1,
                          logLikelihood=sum_likeli,
                          pseudoRsquared=pseudo_R_squared)
  output_df <- rbind(output_df, newrow)
}
#calculate the max and output csv
findMax <- function (column_name){
  max_meth <- output_df$method[1]
  max_num <- as.numeric(column_name[1])

  for (k in 1:length(column_name)){
    if (max_num < as.numeric(column_name[k])){
      max_num <- as.numeric(column_name[k])
      max_meth <- output_df$method[k]
    }
  }
  return(max_meth)
}
maxrow <- data.frame(method='max',
                     sensitivity=findMax(output_df$sensitivity),
                     specificity=findMax(output_df$specificity),
                     F1=findMax(output_df$F1),
                     logLikelihood=findMax(output_df$logLikelihood),
                     pseudoRsquared=findMax(output_df$pseudoRsquared))
#round up all the numbers
output_df$sensitivity <- round(output_df$sensitivity, digits=2)
output_df$specificity <- round(output_df$specificity, digits=2)
output_df$F1 <- round(output_df$F1, digits=2)
output_df$logLikelihood <- round(output_df$logLikelihood, digits=2)
output_df$pseudoRsquared <- round(output_df$pseudoRsquared, digits=2)
#put the max row in
output_df <- rbind(output_df, maxrow)

write.csv(output_df,output, row.names=FALSE, quote=FALSE)
