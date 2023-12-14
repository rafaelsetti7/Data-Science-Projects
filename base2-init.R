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

base2_inadimp <- base2 %>% filter(Resposta==1)
base2_adimp <- base2 %>% filter(Resposta==0)

summary(base2_inadimp$Prop_limite_utilizado)
summary(base2_adimp$Prop_limite_utilizado)
summary(base2_inadimp$Idade)
summary(base2_adimp$Idade)
summary(base2_inadimp$Atraso30_59)
summary(base2_adimp$Atraso30_59)
summary(base2_inadimp$Qtd_emprestimos_imobiliario)
summary(base2_adimp$Qtd_emprestimos_imobiliario)
summary(base2_inadimp$Qtd_emprestimos_ativos)
summary(base2_adimp$Qtd_emprestimos_ativos)
summary(base2_inadimp$Prop_divida_renda)
summary(base2_adimp$Prop_divida_renda)


desbalanceamento <- base2 %>% 
  ggplot(aes(fill=Resposta, x=Resposta)) + 
  geom_bar(position="dodge", stat="count") + 
  ggtitle("Distribuição da variável resposta") + 
  xlab("Estado do pagamento") + 
  ylab("Contagem") +
  scale_x_discrete(labels = c("Andimplente", "Inadimplente")) +
  theme_bw() + 
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position = "none")

base2_bp_prop_limite <- base2 %>%
  ggplot( aes(x=Resposta, y=Prop_limite_utilizado, fill=Resposta)) +
  geom_boxplot() +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) + 
  ggtitle("BoxPlot da proporção do limite utilizado") +
  xlab("Inadimplência") + ylab("Proporção do limite utilizado") + 
  scale_fill_brewer(palette = "Dark2") +
  scale_x_discrete(labels = c("Adimplente", "Inadimplente"))

base2_bp_idade <- base2 %>%
  ggplot( aes(x=Resposta, y=Idade, fill=Resposta)) +
  geom_boxplot() +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) + 
  ggtitle("BoxPlot da Idade dos clientes de cartão de crédito") +
  xlab("Inadimplência") + ylab("Idade padronizada") + 
  scale_fill_brewer(palette = "Dark2") +
  scale_x_discrete(labels = c("Adimplente", "Inadimplente"))

base2_bp_atraso <- base2 %>%
  ggplot( aes(x=Resposta, y=Atraso30_59, fill=Resposta)) +
  geom_boxplot() +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) + 
  ggtitle("BoxPlot do número de vezes que o cliente atrasou de 30 a 59 dias") +
  xlab("Inadimplência") + ylab("Quantidade de atrasos de 30 a 59 dias") + 
  scale_fill_brewer(palette = "Dark2") +
  scale_x_discrete(labels = c("Adimplente", "Inadimplente"))

base2_bp_prop_divida_renda <- base2 %>%
  ggplot( aes(x=Resposta, y=Prop_divida_renda, fill=Resposta)) +
  geom_boxplot() +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) + 
  ggtitle("BoxPlot da proporção da dívida em relação a renda") +
  xlab("Inadimplência") + ylab("Proporção da dívida em relação a renda") + 
  scale_fill_brewer(palette = "Dark2") +
  scale_x_discrete(labels = c("Adimplente", "Inadimplente"))

base2_qnt_empr_ativos <- base2 %>%
  ggplot( aes(x=Resposta, y=Qtd_emprestimos_ativos, fill=Resposta)) +
  geom_boxplot() +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) + 
  ggtitle("BoxPlot da quantidade de empréstimos ativos") +
  xlab("Inadimplência") + ylab("Quantidade de empréstimos ativos") + 
  scale_fill_brewer(palette = "Dark2") +
  scale_x_discrete(labels = c("Adimplente", "Inadimplente"))

base2_qnt_empr_imob <- base2 %>%
  ggplot( aes(x=Resposta, y=Qtd_emprestimos_imobiliario, fill=Resposta)) +
  geom_boxplot() +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) + 
  ggtitle("BoxPlot da quantidade de empréstimos imobiliários") +
  xlab("Inadimplência") + 
  ylab("Quantidade de empréstimos imobiliários") + 
  scale_fill_brewer(palette = "Dark2") +
  scale_x_discrete(labels = c("Adimplente", "Inadimplente"))

ggarrange(base2_bp_atraso,base2_bp_prop_divida_renda,base2_bp_idade,base2_bp_prop_limite,
          base2_qnt_empr_imob, base2_qnt_empr_ativos)

corr <- cor(base2[,-7])

corrplot(corr, type = 'lower', order = 'hclust', tl.col = 'black',
         cl.ratio = 0.2, tl.srt = 45, col = COL2('PuOr', 10),tl.cex=0.7)

# MODELO GLM GERAL
b2_modelo <- glm(formula = Resposta~.,data=base2_treino, 
                 family = binomial(link = "logit"))

b2_modelo_lasso <- cv.glmnet(as.matrix(base2_treino[,1:6]), 
                             as.matrix(base2_treino[,7]), 
                             family="binomial", alpha=1)

summary(b2_modelo)
summary(b2_modelo_lasso)

vif(b2_modelo)

p_b2 <- predict(b2_modelo, 
                newdata=subset(base2_teste), 
                type="response")

p_b2_lasso <- predict(b2_modelo_lasso, 
                      newx = as.matrix(subset(base2_teste[,1:6])),
                      type = "response", s = 0.01)

## MODELO TOMEK LINK

tomek2_treino <- read.csv("/Users/user/Desktop/Pessoal/UFSCar/TCC/dados/portes/tomek_rafa.csv", sep = ";")
tomek2_teste <- read.csv("/Users/user/Desktop/Pessoal/UFSCar/TCC/dados/base_teste_Tomek.csv", sep = ";")

tomek2_treino$Resposta <- as.factor(tomek2_treino$Resposta)
tomek2_teste$Resposta <- as.factor(tomek2_teste$Resposta)

tomek2_teste <- tomek2_teste[,2:8]
tomek2_treino <- tomek2_treino[,2:8]

base2_treino <- read.csv("/Users/user/Desktop/Pessoal/UFSCar/TCC/dados/base_treino_Kaggle.csv")
base2_teste <- read.csv("/Users/user/Desktop/Pessoal/UFSCar/TCC/dados/base_teste_Kaggle.csv")

base2_treino_tomek <- TomekClassif(Resposta~., 
                                   b2_treino, 
                                   dist = "HVDM", 
                                   rem = "maj")


b2_modelo_tomek <- glm(Resposta~.,data=tomek2_treino, family = binomial(link="logit"))

b2_modelo_tomek_lasso <- cv.glmnet(as.matrix(tomek2_treino[,1:6]), 
                             as.matrix(tomek2_treino[,7]), 
                             family="binomial", alpha=1)

## MODELO SMOTE

base2_geral_smote <- SMOTE(base2_treino[,1:6],base2_treino[,7],k=2)

df2_smote <- data.frame(base2_geral_smote$x_new,base2_geral_smote$y_new)

colnames(df2_smote)[7] <- "Resposta"
df2_smote$Resposta <- as.factor(df2_smote$Resposta)

#frac2 <- sample.split(Y = df2_smote$Resposta,SplitRatio = 0.7)
#base2_treino_smote <- df2_smote[frac2,]
#base2_teste_smote <- df2_smote[!frac2,]

b2_modelo_smote <- glm(formula = Resposta~.,data=df2_smote, 
                 family = binomial(link = "logit"))

b2_modelo_smote_lasso <- cv.glmnet(as.matrix(df2_smote[,1:6]), 
                             as.matrix(df2_smote[,7]), 
                             family="binomial", alpha=1)

summary(b2_modelo_smote)

vif(b2_modelo_smote)

summary(b2_modelo_smote_lasso)

coefficients(b2_modelo_smote_lasso)
coefficients(b2_modelo_smote)

## SMOTE + TOMEK LINK

treino2_smote <- SMOTE(base2_treino[,1:6],base2_treino[,7],k=2)
treino_smote_fim2 <- data.frame(treino2_smote$x_new,treino2_smote$y_new)

colnames(treino_smote_fim2)[7] <- "Resposta"

treino2_tomek_smote <- TomekClassif(Resposta~., 
                                    treino_smote_fim2, 
                                    dist = "Euclidean", rem = "maj")
