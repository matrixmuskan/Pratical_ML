library(data.table)
library(caret)
library(ggplot2)
library(doParallel)
library(foreach)
registerDoParallel()
set.seed(100)
data = read.csv('pml-training.csv',header = TRUE)
data = data.frame(data, row.names = 1)
idx = which(data$new_window == 'yes') 
data = data[-idx, ];data = data[,-seq(1,6)]
data = data[, -nearZeroVar((data))]
data = data[,-findCorrelation(cor(as.matrix(data[,-ncol(data)])),cutoff = 0.95)]

# par(mfrow = c(7,7), mar = c(2,2,2,2))
# for(i in seq(1,(ncol(data) -1))){
#     hist(data[,i])
# }


inTrain = createDataPartition(y = data$classe, p = 0.75, list = FALSE) 
training = data[inTrain,]
validation = data[-inTrain,]

#filterCtrl <- sbfControl(functions = rfSBF,method = "repeatedcv", repeats = 5)

#featurePlot(x = training[,-ncol(training)],y = training[,ncol(training)],plot = 'pairs')

#rfWithFilter = sbf(x = training[,-ncol(training)],y = training[,ncol(training)],sbfControl = filterCtrl)

glm_model_pca = train(classe ~ . , data = training, method = 'lda',preProcess = c('BoxCox','center','scale','pca'))

rf_model = train(classe ~ . , data = training, method = 'rf')

rf_accuray = (confusionMatrix(predict(rf_model,newdata = validation), validation$classe)$overall)[1]
#save(rf_model,'rf_model.RData')
save(rf_model,file ='rf_model.RData')

rf_model$finalModel
plot(varImp(rf_model))

tree_model = train(classe ~ . , data = training, method = 'rpart')
tree_accuracy = confusionMatrix(predict(tree_model,newdata = validation), validation$classe)$overall[1]

tree_model$finalModel

glm_model = train(classe ~ . , data = training, method = 'lda')
lda_accuracy = confusionMatrix(predict(glm_model,newdata = validation), validation$classe)$overall[1]

test_data = read.csv('pml-testing.csv',header = TRUE)
test_data = data.frame(test_data, row.names = 1)
answers  = predict(rf_model,newdata = test_data)

pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}
pml_write_files(answers)
