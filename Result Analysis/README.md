# Result Analysis
This project does several calculations on the result of the predictions. 

## "data" folder
 There are three files that each represent one method for evaluating and producing the prediction scores. Columns include ```persons``` (You can see it as the ID of the row), ```reference``` (the person is in fact bad/good) and ```pred.score``` (the predicted probability of the person being 'bad'). 
## ResultAnalysis.R
### Parameters
There are four parameters:\
```target``` Determine what the positve case is. Enter ```bad``` or ```good```. \
```badthre``` The threshold to evaluate if it is considered bad or good. For example, enter ```0.5``` with a target of 'bad' means the pred.scores above 0.5 are considered bad.\
```input``` The input data to be analyzed. Here, it is the files in data folder. \
```output``` The name of the output csv file, which contains the calculations on the result of the predictions. Here, it is named ```result.csv```.

### Calculations
Calculates true positive as ```TP``` / false positive as ```FP``` / true negative as ```TN``` / false negative as ```FN``` for future use. For the null model, it returns the proportion of 'bad'. 
## result.csv
The results show different attributes of different prediction methods (or input files). The attributes are as follows:\
```sensitivity``` = TP/(TP+FN)\
```specificity``` = TN/(TN+FP)\
```F1``` = 2(precision)(recall)/(precision+recall), when recall = sensitivity and precision = TP/(TP+FP)\
```logLikelihood``` Return log(pred.score) on positive cases or log(1-pred.score)on negative cases. The more difference between the prediction and the true classification, the more minus points it recieves. \
```pseudoRsquared``` = 1-((-2*(logLikelihood-S))/(-2*(null model logLikelihood-S))), while S = 0.
The final row returns the biggest number of the columns.
