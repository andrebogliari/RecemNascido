library(tidyverse)
library(ggplot2)
library(rpart)
library(rpart.plot)

#Puxando os dados

data <- read.csv("birthwt.csv")

#Transformando variáveis categóricas em fatores

cols <- c('low', 'race', 'smoke', 'ht', 'ui')
data[cols] <- lapply(data[cols], as.factor)

# Examinando estrutura do dataframe

str(data)

# Olhando o histograma

hist(data$bwt)

#Separando dados para treinamento e testes

set.seed(123)

train.index <- sample((nrow(data)),0.7*nrow(data))

train <- data[train.index,]
test <- data[-train.index,]


#Aplicando a árvore de decisão


  fit <- rpart(age ~ low + ftv + lwt + smoke + ptl + ht + race + ui, data = train, method = "class")
summary(fit)

# Testando modelo

prediction <- predict(fit, test, type = "class")
table(prediction, test$low)  


# Analisando em porcentagem

prop.table((table(prediction, test$low)))


