# parameters: fold, train, test, report, predict
# for input and output, enter absolute path, otherwise read.csv() and
# write.csv() will start from the folder where R is executed

fold <- 6
train <- 'C:/Users/Tsai/R projects/Data-Analysis-with-R/Titanic - Machine Learning from Disaster/Titanic_Data/train.csv'
test <- 'C:/Users/Tsai/R projects/Data-Analysis-with-R/Titanic - Machine Learning from Disaster/Titanic_Data/test.csv'
report <- 'C:/Users/Tsai/R projects/Data-Analysis-with-R/Titanic - Machine Learning from Disaster/bestperformance.csv'
predict <- 'C:/Users/Tsai/R projects/Data-Analysis-with-R/Titanic - Machine Learning from Disaster/bestpredict.csv'

# predict the input data set, then evaluate and output the accuracy 
pred_and_acc <- function(model, target_set){
  #1 predict the target_set
  resultframe <- data.frame(truth=target_set$Survived,
                                  pred=predict(model, newdata=target_set, type="class"))
  #2 check if the data is out of bound
  out_of_bound <- c()
  for (i in 1:length(resultframe$truth)){
    if (is.na(resultframe$truth[i])==TRUE){
      out_of_bound <- c(out_of_bound, i)}
  }
  if (is.null(out_of_bound)==FALSE){
    resultframe <- resultframe[-out_of_bound,]}
  #3 calculate the accuracy
  accuracy_count <- 0
  for(model_row in 1:nrow(resultframe)){
    if (resultframe$truth[model_row]==resultframe$pred[model_row]){
      accuracy_count <- accuracy_count+1}
  }
  accuracy <- accuracy_count/nrow(resultframe)
  return (accuracy)
}

output_df <- data.frame()# the output of the accuracy, used in report csv
model_df <- data.frame()# dataframe for choosing the right model between folds

library('rpart')
# read input data
d <- read.csv(train, header = TRUE)


#START DATA PREPROCESSING: PREDICT THE AGE FOR NA ROWS
na_age_rownum <- c()
for (i in 1:nrow(d)){
  if(is.na(d$Age[i])==TRUE){
    na_age_rownum <- c(na_age_rownum, i)
  }
}
na_age <- d[c(na_age_rownum), ]
not_na_age <- d[-c(na_age_rownum), ]
# after the data is cleaned, train the model, predict the Age
model <- lm(formula=Age ~ Pclass+Survived+Sex+SibSp+Parch+Fare,
            data=not_na_age)
pred_age_df <- data.frame(PassengerId=na_age_rownum, 
                          pred=predict(model, newdata=na_age))
# put the predictions back to the data
for (i in 1:nrow(na_age)){
  na_age$Age[i] <- pred_age_df$pred[i]
}
d <- rbind(not_na_age, na_age)
#END DATA PREPROCESSING


# randomly swap the rows
set.seed(1)
d <- d[sample(nrow(d)),]

#seperate the file to k fold, return the starting index of every fold
fold <- as.numeric(fold)
set_size <- round(nrow(d)/fold, digits=0)
set_start <- seq(1, nrow(d), set_size)
if (length(set_start)>fold){
  set_start <- head(set_start, -1)}


#LEVEL: BETWEEN FOLDS, cut the data into different folds
for(fold_num in 1:length(set_start)){
  print(sprintf('%s%d%s', 'fold ', fold_num, ' running'))
  set_name <- sprintf('fold%d', fold_num)
  
  
  # ASSIGNING FOLDS TO DIFFERENT SETS
  #assign testing_set
  testing_set <- d[c(seq(set_start[fold_num], set_start[fold_num]+set_size-1)),]
  testing_set_head = c(seq(set_start[fold_num], set_start[fold_num]+set_size-1))
  #assign validation_set, 'if' prevents validation from out of bound
  if (fold_num+1>length(set_start)){
    validation_set <- d[c(seq(set_start[1], set_start[1]+set_size-1)),]
    validation_set_head = c(seq(set_start[1], set_start[1]+set_size-1))
  }else {validation_set = d[c(seq(set_start[fold_num+1], set_start[fold_num+1]+set_size-1)),]
  validation_set_head = c(seq(set_start[fold_num+1], set_start[fold_num+1]+set_size-1))}
  #assign training_set, removing testing_set and validation_set
  training_set <- d[-c(testing_set_head, validation_set_head),]
  #END ASSIGNING FOLDS TO DIFFERENT SETS
  
  
  # PREDEFINE FACTOR '' so it doesn't reply error
  level_embarked_train <- levels(factor(training_set$Embarked))
  if (!is.element('', level_embarked_train)){
    training_set$Embarked <- factor(training_set$Embarked, levels=c(level_embarked_train, ''))}
  
  level_embarked_valid <- levels(factor(validation_set$Embarked))
  if (!is.element('', level_embarked_valid)){
    validation_set$Embarked <- factor(validation_set$Embarked, levels=c(level_embarked_valid, ''))}
  
  level_embarked <- levels(factor(testing_set$Embarked))
  if (!is.element('', level_embarked)){
    testing_set$Embarked <- factor(testing_set$Embarked, levels=c(level_embarked, ''))}
  # END PREDEFINE FACTOR
  
  
  #LEVEL: BETWEEN MODELS WITHIN SAME FOLD, choose depth(calculate max depth, return depth_df)
  depth_df <- data.frame() # choose the right model for different parameters
  for (depth_num in 1:10){
    for (minbucket_num in 1:10){
      model <- rpart(Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,
                     data=training_set, control=rpart.control(maxdepth=depth_num, minsplit=2, minbucket=minbucket_num),
                     method="class")
      
      train_accuracy <- pred_and_acc(model, training_set)
      valid_accuracy <- pred_and_acc(model, validation_set)
      
      #write the result to depth_df
      newdepth_df <- data.frame(depth=depth_num,
                                minbucket=minbucket_num,
                                train_accuracy=train_accuracy, 
                                valid_accuracy=valid_accuracy)
      depth_df <- rbind(depth_df, newdepth_df)
    }
  }
  
  #choose the max accuracy of the model via valid accuracy
  best_line <- which.max(depth_df$valid_accuracy)
  best_depth <- depth_df$depth[best_line]
  best_minbucket <- depth_df$minbucket[best_line]
  
  training_result <- round(depth_df$train_accuracy[best_line], digits=2)
  validation_result <- round(max(depth_df$valid_accuracy), digits=2)
  
  
  model <- rpart(Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,
                 data=training_set, control=rpart.control(maxdepth=best_depth, minsplit=2, minbucket=best_minbucket),
                 method="class")

  test_accuracy <- pred_and_acc(model, testing_set)
  test_result <- round(test_accuracy, digits=2)
  
  # add the fold row to the final output
  newrow <- data.frame(set=set_name,
                       training=training_result,
                       validation=validation_result,
                       test=test_result)
  output_df <- rbind(output_df, newrow)

  # write the unrounded test score and corresponding variables for choosing the model later (between folds)
  newmodel_df <- data.frame(fold_num=fold_num, 
                            test_result=test_accuracy, 
                            depth=best_depth, 
                            minbucket=best_minbucket)
  model_df <- rbind(model_df, newmodel_df)
}

# calculate the mean for the report file
averow <- data.frame(set='ave.',
                     training=round(mean(as.numeric(output_df$training)), digits=2),
                     validation=round(mean(as.numeric(output_df$validation)), digits=2),
                     test=round(mean(as.numeric(output_df$test)), digits=2))
#put the ave row in, write it out to report csv
output_df <- rbind(output_df, averow)
output_df <- format.data.frame(output_df, digits=2)
write.csv(output_df, report, row.names=FALSE, quote=FALSE)


# predict the given test file
#choose the best model
best_model <- which.max(model_df$test_result)
best_model_depth <- model_df$depth[best_model]
best_model_minbucket <- model_df$minbucket[best_model]
fold_num <- best_model

# determine the choosen training_set via best_model(fold_num)
testing_set_head = c(seq(set_start[fold_num], set_start[fold_num]+set_size-1))
if (fold_num+1>length(set_start)){
  validation_set_head = c(seq(set_start[1], set_start[1]+set_size-1))
}else {validation_set_head = c(seq(set_start[fold_num+1], set_start[fold_num+1]+set_size-1))}
training_set <- d[-c(testing_set_head, validation_set_head),]

# predict the test.csv file
test_data <- read.csv(test, header = TRUE)
model <- rpart(Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,
               data=training_set, control=rpart.control(maxdepth=best_model_depth, minsplit=2, minbucket=best_model_minbucket),
               method="class")

predict_result <- data.frame(PassengerId=test_data$PassengerId,
                             Survived=predict(model, newdata=test_data, type="class"))
# write out the result of predict.csv
write.csv(predict_result, predict, row.names=FALSE, quote=FALSE)

