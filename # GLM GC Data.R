# GLM GC Data # 28 DEC 15
library(Deducer)
library(ggplot2)
#
gcglm<- read.csv("C:/STAT/_Own_R/Credit/Credit-2/gc_names.csv");str(gcglm)
View(gcglm)
gcglm$default <-factor(gcglm$default) ;str(gcglm)
## Create Design.Matrix or MODEl.Matrix - factor variables, turned to indicator variables 
## first column of ones is omitted 
set.seed(123)
Xgcglm <- model.matrix(default~.,data=gcglm)[,-1] # Excluded the last "default" variable. 
str(Xgcglm) ; Xgcglm[1:10,] 
# Print first 10  Rows of MODEl.Matrix.
set.seed(123)
train <- sample(1:1000,900) 
# Train set size - 700 ROWS Error - 1.05 ,AIC: 720.27 ,
# Null deviance: 853.51  on 699  degrees of freedom , 
# Residual deviance: 622.27  on 651  degrees of freedom
# Train set size - 800 ROWS Error was - 0.72 ,AIC: 807.01 , 
# Train set size - 900 ROWS Error was -  ...AIC: 898.72
# Null deviance: 1094.42  on 899  degrees of freedom
# Residual deviance:  800.72  on 851  degrees of freedom
#
MM_train <- Xgcglm[train,] ; MM_test <- Xgcglm[-train,]
# Training and Testing Data sets from German Credit [MODEl.Matrix]
set.seed(123)
GC_train <- gcglm$default[train] ;GC_test <- gcglm$default[-train] 
# Training and Testing Data sets fom German Credit[Data]
# Create Model- GLM, use Train Data from both - GC[MODEl.Matrix] and GC
set.seed(123)
# Families -- binomial(link = "logit")
# quasibinomial(link = "logit")
# quasipoisson(link = "log")
GC_glm_binomial<-glm(default~.,family=binomial,data=data.frame(default=GC_train,MM_train)) 
#
pdf('GC_glm_binomial.pdf')
rocplot(GC_glm_binomial);# data visualization PDF 
dev.off()
#
GC_glm_quasibinomial<-glm(default~.,family=quasibinomial,data=data.frame(default=GC_train,MM_train)) 
pdf('GC_glm_binomial.pdf')
rocplot(GC_glm_quasibinomial);
dev.off()
#
# Family == Binomial , link is LOGIT .. 
summary(GC_glm_binomial)
# Family == Quasi Binomial - attempts to describe additional variance in the data that cannot be explained by a Binomial distribution alone.
summary(GC_glm_quasibinomial) # No AIC Value for family == Quasi witin GLM ..
## Model created now to Predict ...using Test Data. 
set.seed(123)
Pred_MM_test <- predict(GC_glm_binomial,newdata=data.frame(MM_test),type="response")
str(Pred_MM_test);head(Pred_MM_test,10)
str(MM_test)
# Here - Vector=="Pred_MM_test" shown under VALUES within Environment and the Matrix=="MM_test" shown under DATA within Environment.
# Now combine- Vector=="Pred_MM_test" and Matrix=="MM_test" into a DATA.FRAME
Df_GC_glm<-data.frame(MM_test,Pred_MM_test) 
# Predict using GLM, use Test Data from both -GC[MODEl.Matrix] and GC
str(Df_GC_glm);head(Df_GC_glm,3)
## We see in Df Print output - "default" has "probab" and not the earlier 1 OR 2 
# 
# As these are Randomly Sampled Observations from German Credit data we get Random Row Numbers
# Also the "Pred_MM_test" is the Probability - for example - ....
#
## Mis-classification rates - "GOODS- Will Pay Back" rated as "BADS- Will Default"...
## We use probability cutoff 1/6 or 16.66% , thus we code == Pred_fac<-floor(Pred_MM_test+(5/6))
# if we chose probability cutoff 1/4 or 25.00% ,we code == Pred_fac<-floor(Pred_MM_test+(3/4)).
#
set.seed(123)
Pred_fac<-floor(Pred_MM_test+(5/6))
Pred_fac[1:10]
#
t<-table(GC_test,Pred_fac)# Within GC_test , the 1 is a DEFAULTER - "0" and the 2 NOT DEFAULTER - "1"
t
# We want to now see the % of Misclassification by 
# creating a Confusion Matrix ...
# As seen - 
TN<-33
FN<-5
FP<-34
TP<-28
n_length<-length(Pred_MM_test)
# 
# Percentage of Misclassification = (FP+FN)/n_length
Mis.Class<-(FP+FN)/n_length
Mis.Class
## [1] 0.36 for Pred_MM_test and train <- sample(1:1000,900) 
# Percentage of Misclassification = 36%
#
# Sensitivity of Model = TP/(TP+FN)
Sentivity<-TP/(TP+FN)
Sentivity
## [1]  0.8548387  for Pred_MM_test and train <- sample(1:1000,900) 
## # Sensitivity of Model = 85.48%
#
# Specificity of Model = TN/(TN+FP)
Specificity<-TN/(TN+FP)
Specificity
## [1] 0.5434783 for Pred_MM_test and train <- sample(1:1000,900) 
## # Specificity of Model = 54.34% 
#
library(caret)
library(ipred)
library(plyr)
library(rpart)
gcBAG<- read.csv("C:/STAT/_Own_R/Credit/Credit-2/gc_names.csv");str(gcBAG)
gcBAG$default <- factor(gcBAG$default) ;str(gcBAG)
inTrain <- createDataPartition(y=gcBAG$default,p=0.7, list=FALSE)
trn <- gcBAG[inTrain,]
tst <- gcBAG[-inTrain,]
# dim(trn); dim(tst) # Optional
# str(trn);str(tst) # Optional
mFit <- train(default~ .,method="rpart",data=trn)
print(mFit$finalModel)
#
# OK --- library(rattle)
# OK -- fancyRpartPlot(mFit$finalModel)




# GC_bag<-train(default~.,method="treebag",data =gcBAG)
# # str(GC_bag) - DONT ...
# print(GC_bag)



#
# Another Option for Creating Train and Test ...
# library(caret)
# inTrain <- createDataPartition(y=credit$default,p=0.7, list=FALSE)
# trn <- credit[inTrain,]
# tst <- credit[-inTrain,]
# dim(trn); dim(tst)
# str(trn)
#
# Further Reads # 
# Quasi Binomial - http://stats.stackexchange.com/questions/91724/what-is-quasibinomial
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/family.html
# No AIC for Quasi Likelihood or Quasi Binomial 
# Akaike's An Information Criterion - https://stat.ethz.ch/R-manual/R-devel/library/stats/html/glm.html
# CRAN Resource - Quasi AIC -- https://cran.r-project.org/web/packages/bbmle/vignettes/quasi.pdf
# SO - http://stackoverflow.com/questions/17045915/using-rocr-package-difficulties
#
# Ignore Code below here .....
# nnn<-1/6
# nnn
# .83333+.16666
# 
# old_data <- read.csv("C:/STAT/_Own_R/Credit/Credit-2/d.csv")
# str(old_data)
# qplot(F1.R,F2.R,colour=d,data=trn)
