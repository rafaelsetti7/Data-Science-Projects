library(factoextra)
library(FactoMineR)
library(ggplot2)
library(tidyverse)
library(corrplot)
library(readxl)
library(cowplot)


## ex 1

x_a = c(8209.5,538.16,13375.7)
x_b = c(4326.3,316.3,5837.1)
cp1 = c(0.617, 0.557, 0.556)
cp2 = c(-0.001,-0.706,0.708)

y_a = c(sum(x_a*cp1),sum(x_a*cp2))
y_b = c(sum(x_b*cp1),sum(x_b*cp2))


ggplot() + geom_point(y_a,y_b)

## ex 3


M = c("M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8")
X1 = c(2.75, 3.9, 3.12, 4.58, 3.97, 3.01, 4.19, 3.82)
X2 = c(4.03, 4.12, 3.97, 4.86, 4.34, 3.98, 4.65, 4.12)
X3 = c(2.8, 3.4, 3.62, 4.34, 4.28, 2.9, 4.52, 3.62)
X4 = c(2.62, 3.52, 3.05, 4.82, 4.98, 2.82, 4.77, 3.71)
d3 = data.frame(M, X1, X2, X3, X4)

cor(d3[,-1])

acp3 <- PCA(d3[,-1])

corrplot(cor(d3[,-1]), method = "color", col = col(200),
         type = "upper", order = "hclust",
         addCoef.col = "black", tl.col = "black", 
         tl.srt = 45, sig.level = 0.01, diag = FALSE, tl.cex = 0.75)

fviz_eig(acp3, addlabels = T, barfill = 'darkblue', 
         barcolor = 'black') + theme_bw() + labs(x = 'Componentes', y = '% da variância explicada') + 
  ylim(c(0, 100))

fviz_contrib(acp3, choice = 'var', axes = 1:2, fill = 'darkblue', 
             color = 'black', sort.val = 'none') +
  ylim(c(0, 40)) +
  labs(title = 'Contribuição das variáveis ao 1º plano', x = 'Variáveis', 
       y = '% contribuição') +
  theme_bw()

fviz_pca_biplot(acp3, pointshape = 21, pointsize = 2, 
                col.var = 'darkblue') +
  theme_bw() +
  labs(title = " Grafico do Primeiro Plano", x = 'CP1 (90.80%)', 
       y = 'CP2 (4.90%)') +
  xlim(c(-4, 4)) +
  ylim(c(-4, 4))

## ex 4

d4 <- read_excel("C:/Users/rafa_/Downloads/L24.xlsx")

d4$Agric <- as.numeric(d4$Agric)
d4$Miner <- as.numeric(d4$Miner)
d4$Manuf <- as.numeric(d4$Manuf)
d4$Supr <- as.numeric(d4$Supr)
d4$Const <- as.numeric(d4$Const)
d4$Serv <- as.numeric(d4$Serv)
d4$Finan <- as.numeric(d4$Finan)
d4$Soc_Pes <- as.numeric(d4$Soc_Pes)
d4$Tr_Com <- as.numeric(d4$Tr_Com)


#matriz de corr

CM <- cor(d4[,-1])

col <- colorRampPalette(c("#BB4444", "#EE9988", 
                          "#FFFFFF", "#77AADD", "#4477AA"))

g_c <- corrplot(CM, method = "color", col = col(200),
         type = "upper", order = "hclust",
         addCoef.col = "black", tl.col = "black", 
         tl.srt = 45, sig.level = 0.01, diag = FALSE, tl.cex = 0.75)

# ACP

acp <- PCA(d4[,-1])

## scree plot

fviz_eig(acp, addlabels = T, barfill = 'darkblue', 
         barcolor = 'black') + theme_bw() + labs(x = 'Componentes', y = '% da variância explicada') + 
  ylim(c(0, 100))

## contribuição no primeiro plano
fviz_contrib(acp, choice = 'var', axes = 1:2, fill = 'darkblue', 
             color = 'black', sort.val = 'none') +
  ylim(c(0, 40)) +
  labs(title = 'Contribuição das variáveis ao 1º plano', x = 'Variáveis', 
       y = '% contribuição') +
  theme_bw()


## gráfico do primeiro plano

fviz_pca_biplot(acp, axes = c(1,2), pointshape = 21, pointsize = 2, 
                col.var = 'darkblue') +
  theme_bw() +
  labs(title = " Grafico do Primeiro Plano", x = 'CP1 (40.6%)', 
       y = 'CP2 (23.8%)') +
  xlim(c(-4, 4)) +
  ylim(c(-4, 4))


#contribuição por dimensão

p1 = fviz_contrib(acp, choice="var", axes=1, fill = 'darkblue', 
             color = 'black', sort.val = 'none') + ylim(c(0, 40)) +
  labs(title = 'Contribuição das variáveis a 1ª dimensão', 
       x = 'Variáveis', 
       y = '% contribuição') +
  theme_bw()

p2 = fviz_contrib(acp, choice="var", axes=2,  fill = 'darkblue', 
             color = 'black', sort.val = 'none') + ylim(c(0, 40)) +
  labs(title = 'Contribuição das variáveis a 2ª dimensão', 
       x = 'Variáveis', 
       y = '% contribuição') +
  theme_bw()

p3 = fviz_contrib(acp, choice="var", axes=3, fill = 'darkblue', 
             color = 'black', sort.val = 'none') + ylim(c(0, 40)) +
  labs(title = 'Contribuição das variáveis a 3ª dimensão', 
       x = 'Variáveis', 
       y = '% contribuição') +
  theme_bw()

p4 = fviz_contrib(acp, choice="var", axes=4,  fill = 'darkblue', 
             color = 'black', sort.val = 'none') + ylim(c(0, 40)) +
  labs(title = 'Contribuição das variáveis a 4ª dimensão', 
       x = 'Variáveis', 
       y = '% contribuição') +
  theme_bw()

options(repr.plot.width=14, repr.plot.height=10)
plot_grid(p1,p2,p3,p4, ncol = 2, nrow = 2)

