final_data1 = read.csv("C:\\Users\\admin\\Documents\\dataframe.csv")

final_data = final_data1[,-1]
colnames(final_data)

##########################################################################

library(caret)
set.seed(2019)


intrain = createDataPartition(y=final_data$V1 , p=0.7 , list =F)
training = final_data[intrain,]
validation = final_data[-intrain,]

fit = glm(V1~. , data = training , family = binomial())
pred = predict(fit , newdata = validation , type = "response")


pred.res = factor(ifelse(validation$V1 == 1,"Y" ,"N"),levels = c("Y","N"))

pred.cat = factor(ifelse(pred == 1,"Y", "N"),levels = c("Y","N"))

confusionMatrix(pred.cat , pred.res,positive="N")

#############################################################

library(pROC)
plot.roc(validation$V1, pred, print.auc=TRUE ,
         col="magenta", main="Logistic Regression",
         legacy.axes=TRUE)


