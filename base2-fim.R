library(dplyr)
library(MASS)
library(tidyverse)
library(DBI)
library(ROCR)
library(pROC)
library(ggpubr)
library(corrplot)
library(GGally)
library(car)
library(plotly)
library(gapminder)
library(TSA)
library(forecast)
library(xtable)
library(glmnet)
library(SMOTEWB)
library(ROSE)
library(caret)
library(DMwR2)
library(stats)
library(caTools)
library(UBL)

base2_treino <- read.csv("/Users/user/Desktop/Pessoal/UFSCar/TCC/dados/base_treino_Kaggle.csv")
base2_teste <- read.csv("/Users/user/Desktop/Pessoal/UFSCar/TCC/dados/base_teste_Kaggle.csv")

base2_treino$Resposta <- as.factor(base2_treino$Resposta)
base2_teste$Resposta <- as.factor(base2_teste$Resposta)

base2 <- rbind(base2_treino,base2_teste)

base2_amostra <- base2 %>%
  sample_n(size = round((0.2)*length(base2$Resposta)))

train_index <- createDataPartition(base2_amostra$Resposta, p = .8, list = FALSE)
b2_treino <- base2_amostra[ train_index,]
b2_teste <- base2_amostra[-train_index,]

# GERAL

b2_modelo <- glm(formula = Resposta~.,data=b2_treino, 
                 family = binomial(link = "logit"))

b2_modelo_lasso <- cv.glmnet(as.matrix(b2_treino[,1:6]), 
                             as.matrix(b2_treino[,7]), 
                             family="binomial", alpha=1)


# TOMEK

base2_treino_tomek <- TomekClassif(Resposta~., 
                                   b2_treino, 
                                   dist = "HVDM", 
                                   rem = "maj")

tomek2_treino <- base2_treino_tomek[[1]]

b2_modelo_tomek <- glm(Resposta~.,data=tomek2_treino, family = binomial(link="logit"))

b2_modelo_tomek_lasso <- cv.glmnet(as.matrix(tomek2_treino[,1:6]), 
                                   as.matrix(tomek2_treino[,7]), 
                                   family="binomial", alpha=1)

# SMOTE

base2_geral_smote <- SMOTE(b2_treino[,1:6],b2_treino[,7],k=5)

df2_smote <- data.frame(base2_geral_smote$x_new,base2_geral_smote$y_new)

colnames(df2_smote)[7] <- "Resposta"
df2_smote$Resposta <- as.factor(df2_smote$Resposta)

b2_modelo_smote <- glm(formula = Resposta~.,data=df2_smote, 
                       family = binomial(link = "logit"))

b2_modelo_smote_lasso <- cv.glmnet(as.matrix(df2_smote[,1:6]), 
                                   as.matrix(df2_smote[,7]), 
                                   family="binomial", alpha=1)

# TOMEK + SMOTE

treino2_tomek_smote <- TomekClassif(Resposta~., 
                                    df2_smote, 
                                    dist = "Euclidean", rem = "both")

treino2_tomek_smote_df <- treino2_tomek_smote[[1]]

b2_modelo_tomek_smote <- glm(formula = Resposta~.,data=treino2_tomek_smote_df, 
                       family = binomial(link = "logit"))

b2_modelo_tomek_smote_lasso <- cv.glmnet(as.matrix(treino2_tomek_smote_df[,1:6]), 
                                   as.matrix(treino2_tomek_smote_df[,7]), 
                                   family="binomial", alpha=1)
