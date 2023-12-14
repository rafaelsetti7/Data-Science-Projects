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
library(rpart)
library(ggcorrplot)
library(vip)

View(dados5)

## DESCRITIVA

summary(dados5)
hist(dados5)

## histograma classes
dados5 %>% 
  ggplot(aes(fill=fetal_health, x=fetal_health)) + 
  geom_bar(position="dodge", stat="count") + 
  ggtitle("Distribuição da variável resposta") + 
  xlab("Estado de saúde do feto") + 
  ylab("Contagem") +
  theme_bw() + 
  scale_fill_brewer(palette = "Dark2")

##histograma todos
dados5 %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

## Nova variavel Y e tratamento de dados

str(dados5)
#dados5 <- lapply(dados5, as.numeric)
dados5$fetal_health <- as.factor(dados5$fetal_health)
dados5$Y <- ifelse(dados5$fetal_health==1,0,1)
dados5$Y <- as.factor(dados5$Y)

dados5$fetal_health <- NULL
dados5$histogram_tendency <- as.factor(dados5$histogram_tendency)

## histograma novo Y
dados5 %>% 
  ggplot(aes(fill=Y, x=Y)) + 
  geom_bar(position="dodge", stat="count") + 
  ggtitle("Distribuição da nova variável resposta") + 
  xlab("Estado de saúde do feto") + 
  ylab("Contagem") +
  theme_bw() + 
  scale_fill_brewer(palette = "Dark2")

## DF de risco e saudaveis
risco <- dados5 %>% filter(Y==1)
saudaveis <- dados5 %>% filter(Y==0)

risco_treino <- df_treino %>% filter(Y==1)
saud_treino  <- df_treino %>% filter(Y==0)


## zoom dos grafs por classe

h1 <- dados5 %>% 
  ggplot(aes(x=accelerations)) + 
  geom_bar(position="dodge", stat="count") + 
  ggtitle("Distribuição da aceleração") + 
  xlab("Aceleração") + 
  ylab("Contagem") +
  theme_bw() + 
  scale_fill_brewer(palette = "Dark2")

h2 <- dados5 %>% 
  ggplot(aes(x=light_decelerations)) + 
  geom_bar(position="dodge", stat="count") + 
  ggtitle("Distribuição das desacelerações leves") + 
  xlab("Desacelerações leves") + 
  ylab("Contagem") +
  theme_bw() + 
  scale_fill_brewer(palette = "Dark2")

h3 <- dados5 %>% 
  ggplot(aes(x=severe_decelerations)) + 
  geom_bar(position="dodge", stat="count") + 
  ggtitle("Distribuição das desacelerações severas") + 
  xlab("Desacelerações severas") + 
  ylab("Contagem") +
  theme_bw() + 
  scale_fill_brewer(palette = "Dark2")

h4 <- dados5 %>% 
  ggplot(aes(x=prolongued_decelerations)) + 
  geom_bar(position="dodge", stat="count") + 
  ggtitle("Distribuição das desacelerações prolongadas") + 
  xlab("Desacelerações prolongadas") + 
  ylab("Contagem") +
  theme_bw() + 
  scale_fill_brewer(palette = "Dark2")

ggarrange(h1, h2, h3, h4, ncol = 2, nrow = 2)

## boxplots

b1 <- dados5 %>%
  ggplot( aes(x=Y, y=accelerations, fill=Y)) +
  geom_boxplot() +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) + 
  ggtitle("BoxPlot do número de acelerações de batimentos") +
  xlab("Estado de saúde") + ylab("Acelerações") + 
  scale_fill_brewer(palette = "Dark2")

b2 <- dados5 %>%
  ggplot( aes(x=Y, y=abnormal_short_term_variability, fill=Y)) +
  geom_boxplot() +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) + 
  ggtitle("BoxPlot da Variação anormais de curto período") +
  xlab("Estado de saúde") + ylab("Variação anormais de curto período") + 
  scale_fill_brewer(palette = "Dark2")

b3 <- dados5 %>%
  ggplot( aes(x=Y, y=baseline.value, fill=Y)) +
  geom_boxplot() +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) + 
  ggtitle("BoxPlot da Frequência cardíaca basal") +
  xlab("Estado de saúde") + ylab("Frequência cardíaca basal") + 
  scale_fill_brewer(palette = "Dark2")

b4 <- dados5 %>%
  ggplot( aes(x=Y, y=percentage_of_time_with_abnormal_long_term_variability, fill=Y)) +
  geom_boxplot() +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) + 
  ggtitle("BoxPlot do % de tempo com variações anormais longas") +
  xlab("Estado de saúde") + ylab("% Tempo de variações anormais") + 
  scale_fill_brewer(palette = "Dark2")

ggarrange(b1, b2, b3, b4, ncol = 2, nrow = 2)

dados5 %>%
  ggplot( aes(x=Y, y=uterine_contractions, fill=Y)) +
  geom_boxplot() +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) + ggtitle("BoxPlot do número de contrações uterinas") +
  xlab("Estado de saúde") + ylab("Número de contrações uterinas") + 
  scale_fill_brewer(palette = "Dark2")

# tabelas

# aceleracoes
summary(saudaveis$accelerations)
summary(risco$accelerations)

# FC basal
summary(saudaveis$baseline.value)
summary(risco$baseline.value)

# % tempo
summary(saudaveis$percentage_of_time_with_abnormal_long_term_variability)
summary(risco$percentage_of_time_with_abnormal_long_term_variability)

# var anormais
summary(saudaveis$abnormal_short_term_variability)
summary(risco$abnormal_short_term_variability)

#uterine contraction
summary(saudaveis$uterine_contractions)
summary(risco$uterine_contractions)


data.frame(summary(saudaveis$accelerations),summary(risco$accelerations))

## correlacao

quant <- data.frame(dados5$accelerations,
                    dados5$prolongued_decelerations,
                    dados5$severe_decelerations,
                    dados5$light_decelerations,
                    dados5$fetal_movement,
                    dados5$baseline.value,
                    dados5$uterine_contractions,
                    dados5$abnormal_short_term_variability,
                    dados5$mean_value_of_short_term_variability,
                    dados5$percentage_of_time_with_abnormal_long_term_variability,
                    dados5$mean_value_of_long_term_variability,
                    dados5$histogram_width,dados5$histogram_min,
                    dados5$histogram_max,
                    dados5$histogram_number_of_peaks,
                    dados5$histogram_number_of_zeroes,
                    dados5$histogram_mode,
                    dados5$histogram_mean,
                    dados5$histogram_median,
                    dados5$histogram_variance,
                    as.numeric(dados5$Y))

colnames(quant) <- c("accelerations","prolongued_decel","severe_decel","light_decel",
                     "fetal_mov","baseline_value", "uterine_contrac","anorm_STV",
                     "mean_STV","percent_of_time_STV","mean_LTV", "histogram_width",
                     "histogram_min","histogram_max","histogram_n_peaks","histogram_zeroes",
                     "histogram_mode","histogram_mean","histogram_med","histogram_var",
                     "Resultado do Exame")

cor <- cor(quant)

#ggpairs(dados5[,2:10])

ggcorrplot(cor, hc.order = TRUE, type = "lower", lab=TRUE,lab_size = 2.3,
           outline.col = "white",
           ggtheme = ggplot2::theme_bw,
           colors = c("#0a72ac", "white", "#ec6c04"))

cor_resp <- cor(quant$`Resultado do Exame`,quant)

## modelo


#treino e teste
prop = sample.split(Y = dados5$Y, SplitRatio = 0.7)
df_treino <- dados5[prop,]
treino_sat <- df_treino %>% filter(Y==0)
treino_insat <- df_treino %>% filter(Y==1)
#teste
df_teste = dados5[!prop,]

#arvore
arvore <- rpart(Y ~ ., data = df_treino, method = "class")
p_arvore <- predict(arvore, newdata=subset(df_teste), type="class")

## arvore 2

arvore2 <- rpart(Y ~ accelerations+
                   prolongued_decelerations+
                   severe_decelerations+
                   light_decelerations+
                   fetal_movement+
                   baseline.value+
                   uterine_contractions+
                   abnormal_short_term_variability+
                   mean_value_of_short_term_variability+
                   percentage_of_time_with_abnormal_long_term_variability+
                   mean_value_of_long_term_variability+
                   histogram_min+
                   histogram_number_of_peaks+
                   histogram_mode+
                   histogram_variance, data = df_treino, method = "class")
p_arvore2 <- predict(arvore2, newdata = subset(df_teste), type = "class")
table(p_arvore2,df_teste$Y)


#matriz de confusao
table(p_arvore,df_teste$Y)

#logistica

modelo <- glm(Y ~ .,family=binomial(link='logit'),data=df_treino)

modelo <- glm(Y ~ accelerations+
                prolongued_decelerations+
                severe_decelerations+
                light_decelerations+
                fetal_movement+
                baseline.value+
                uterine_contractions+
                abnormal_short_term_variability+
                mean_value_of_short_term_variability+
                percentage_of_time_with_abnormal_long_term_variability+
                mean_value_of_long_term_variability+
                histogram_width+histogram_min+histogram_max+
                histogram_number_of_peaks+histogram_number_of_zeroes+
                histogram_mode+histogram_mean+histogram_median+
                histogram_variance+histogram_tendency,
                family=binomial(link='logit'),data=df_treino)

modelo2 <- glm(Y ~ accelerations+
                prolongued_decelerations+
                severe_decelerations+
                light_decelerations+
                fetal_movement+
                baseline.value+
                uterine_contractions+
                abnormal_short_term_variability+
                mean_value_of_short_term_variability+
                percentage_of_time_with_abnormal_long_term_variability+
                mean_value_of_long_term_variability+
                histogram_min+
                histogram_number_of_peaks+
                histogram_mode+
                histogram_variance,
              family=binomial(link='logit'),data=df_treino)

modelo3 <- glm(Y ~ accelerations+
                 prolongued_decelerations+
                 #severe_decelerations+
                 #light_decelerations+
                 #fetal_movement+
                 baseline.value+
                 uterine_contractions+
                 abnormal_short_term_variability+
                 mean_value_of_short_term_variability+
                 percentage_of_time_with_abnormal_long_term_variability+
                 mean_value_of_long_term_variability+
                 histogram_min+
                 histogram_number_of_peaks+
                 histogram_mode+
                 histogram_variance,
               family=binomial(link='logit'),data=df_treino)

p3 <- predict(modelo3, newdata = subset(df_teste), type = "response")
valores_preditos_GERAL3 <- ifelse(p3 > 0.284,1,0)
table(df_teste$Y,valores_preditos_GERAL3)



p2 <- predict(modelo2, newdata = subset(df_teste),type="response")
valores_preditos_GERAL2 <- ifelse(p2 > 0.284,1,0)
table(df_teste$Y,valores_preditos_GERAL2)

roc_score1=roc(df_teste$Y~p2,plot=FALSE) 

plot(roc_score1, 
     main ="Curva ROC para o modelo geral ajustado",
     xlab="Especificidade",
     ylab="Sensitividade",
     col = "Blue")

auc(df_teste$Y,p2)

stepAIC(modelo)

p_geral <- predict(modelo, newdata=subset(df_teste), type="response")

valores_preditos_GERAL <- ifelse(p_geral > 0.284,1,0)

table(df_teste$Y,valores_preditos_GERAL)

modelo_step <- glm(Y ~ accelerations + 
                      prolongued_decelerations + 
                      severe_decelerations + 
                      uterine_contractions + 
                      abnormal_short_term_variability + 
                      percentage_of_time_with_abnormal_long_term_variability + 
                      histogram_width + histogram_min + 
                      histogram_number_of_peaks + histogram_mean + 
                      histogram_median + histogram_variance + 
                      histogram_tendency, 
                      family = binomial(link = "logit"), data = df_treino)

p_step <- predict(modelo_step, newdata=subset(df_teste), type="response")


## valor de corte p classificacao = proporcao de doentes
valores_preditos <- ifelse(p_step > 0.284,1,0)

## matriz de confusao

table(df_teste$Y,valores_preditos)

# auc

auc(df_teste$Y,p_step)

## SMOTE p oversampling

sample_SMOTE <- SMOTE(df_treino[,1:20],df_treino[,22],k=2)

df_SMOTE <- data.frame(sample_SMOTE$x_new,sample_SMOTE$y_new)


## modelo smote sem histogram tendency
modelo_SMOTE <- glm(sample_SMOTE$y_new ~ accelerations+
                      prolongued_decelerations+
                      severe_decelerations+
                      light_decelerations+
                      fetal_movement+
                      baseline.value+
                      uterine_contractions+
                      abnormal_short_term_variability+
                      mean_value_of_short_term_variability+
                      percentage_of_time_with_abnormal_long_term_variability+
                      mean_value_of_long_term_variability+
                      histogram_width+histogram_min+histogram_max+
                      histogram_number_of_peaks+histogram_number_of_zeroes+
                      histogram_mode+histogram_mean+histogram_median+
                      histogram_variance,
                      #+histogram_tendency,
                    family=binomial(link='logit'),
                    data=df_SMOTE)

## modelo smote 2

modelo_SMOTE2 <- glm(sample_SMOTE$y_new ~ accelerations+
                       prolongued_decelerations+
                       severe_decelerations+
                       light_decelerations+
                       fetal_movement+
                       baseline.value+
                       uterine_contractions+
                       abnormal_short_term_variability+
                       mean_value_of_short_term_variability+
                       percentage_of_time_with_abnormal_long_term_variability+
                       mean_value_of_long_term_variability+
                       histogram_min+
                       histogram_number_of_peaks+
                       histogram_mode+
                       histogram_variance,
                    #+histogram_tendency,
                    family=binomial(link='logit'),
                    data=df_SMOTE)


p_smote <- predict(modelo_SMOTE, newdata=subset(df_teste), type="response")
valores_preditos_SMOTE <- ifelse(p_smote > 0.5,1,0)

# matriz de confusao SMOTE
table(df_teste$Y,valores_preditos_SMOTE)
auc(df_teste$Y,p_smote)

# modelo LASSO

modelo_lasso <- glmnet(df_treino[,1:20], df_treino[,22], family="binomial", alpha=1)
p_lasso <- predict(modelo_lasso, newx = as.matrix(subset(df_teste[,1:20])),
                   type = "response", s = 0.01)

valores_preditos_LASSO <- ifelse(p_lasso > 0.284,1,0)
table(df_teste$Y,valores_preditos_LASSO)
auc(df_teste$Y,p_lasso)

# modelo LASSO SMOTE - MELHOR MODELO NO SENTIDO DE ERRO MENOS CUSTOSO

modelo_lasso_SMOTE <- glmnet(df_SMOTE[,1:20], df_SMOTE[,21], 
                             family="binomial", alpha=1)

p_lasso_SMOTE <- predict(modelo_lasso_SMOTE, 
                         newx = as.matrix(subset(df_teste[,1:20])),
                         type = "response", s = 0.01)

valores_preditos_LASSO_SMOTE <- ifelse(p_lasso_SMOTE > 0.5,1,0)
table(df_teste$Y,valores_preditos_LASSO_SMOTE)
auc(df_teste$Y,p_lasso_SMOTE)

## arvore SMOTE

arvore_SMOTE <- rpart(sample_SMOTE.y_new ~ ., 
                      data = df_SMOTE, method = "class")

p_arvore_SMOTE <- predict(arvore_SMOTE, newdata=subset(df_teste), type="class")

m_conf_ASMOTE <- table(df_teste$Y,p_arvore_SMOTE)
auc(df_teste$Y,as.numeric(p_arvore_SMOTE))

##PLOT MATRIZ CONFUSAO

ggplot(data =  as.data.frame(m_conf_ASMOTE), 
       mapping = aes(x = Var1, y = p_arvore_SMOTE)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_bw() + theme(legend.position = "none")


# Identify non-zero coefficients
nonzero_coefs <- which(coef(modelo_lasso_SMOTE) != 0)

# Extract non-zero coefficients
coef(modelo_lasso_SMOTE)[nonzero_coefs]

# matriz de confusao SMOTE
table(df_teste$Y,valores_preditos_LASSO_SMOTE)
auc(df_teste$Y,p_smote)

## diagnostico

residuos <- p_lasso_SMOTE-as.numeric(df_teste$Y)
plot(p_lasso_SMOTE,residuos)
qresid(modelo_lasso_SMOTE)
qqPlot(residuals(modelo2), envelope = 0.95)

vif(modelo_lasso_SMOTE)


fit <- rpart(Y ~ accelerations+
               prolongued_decelerations+
               severe_decelerations+
               light_decelerations+
               fetal_movement+
               baseline.value+
               uterine_contractions+
               abnormal_short_term_variability+
               mean_value_of_short_term_variability+
               percentage_of_time_with_abnormal_long_term_variability+
               mean_value_of_long_term_variability+
               histogram_min+
               histogram_number_of_peaks+
               histogram_mode+
               histogram_variance, method = "class", data = df_treino)
melhor_cp <- fit$cptable[which.min(fit$cptable[, "xerror"]),
                         "CP"]
pfit <- rpart::prune(fit, cp = melhor_cp)
rpart.plot::rpart.plot(pfit, type = 4, extra = 106)

p_arvoreFIT <- predict(fit, newdata = subset(df_teste), type = "class")
table(p_arvoreFIT,df_teste$Y)

vi(fit)

table(p_arvore,df_teste$Y)