# Documentação Oficial
#https://developers.google.com/analytics/solutions/r-google-analytics

# Instalando e ativando o pacote do Google Analytics para o R

#Aplicando uma camada de Ciência no Marketing Digital

# Instalando o Pacote
library(needs)
needs(RGoogleAnalytics)

# Autorizar a conta do Google Analytics
# Isso não precisa ser executado em cada sessão uma vez que o objeto token criado
# é salvo

# Substitua as chaves pelas que você gerou no projeto da API
#https://console.developers.google.com
client.id <-"xxx.apps.googleusercontent.com"
client.secret <-"xxx"
token <- Auth(client.id,client.secret)

# Salvar o objeto token para sessões futuras
save(token,file="./token_file")

# Em sessões futuras, ele pode ser carregado 
load("./token_file")

ValidateToken(token)
GetProfiles(token)

# Criar uma lista de consulta com os par?metros da API
query.list <- Init(start.date = "2017-01-01",
                   end.date = "2017-08-31",
                   dimensions = "ga:date, ga:dayOfWeek, ga:source, ga:medium, ga:campaign, ga:deviceCategory",
                   metrics = "
                   ga:sessions,
                   ga:users,
                   ga:organicSearches,
                   ga:impressions,
                   ga:adClicks,
                   ga:CPC,
                   ga:adCost,
                   ga:transactions,
                   ga:costPerTransaction,
                   ga:totalValue",
                   max.results = 10000,
                   sort = "ga:date",
                   table.id = "ga:136578367")
                   


# Crie o objeto Query Builder para que os par?metros de consulta sejam validados
ga.query <- QueryBuilder(query.list)

# Extrair os dados e armazená-los em um data-frame
ga.data <- GetReportData(ga.query, token, paginate_query = F)
head(ga.data)

#Gravar os dados em outra variável é uma boa prática, pois em caso de neccessidade,
#os dados originais continuam armazenados e disponíveis.
analytics <- data.frame(ga.data)
head(analytics)

#explorando a estrutura dos dados
str(analytics)

#os dados virão brutos e em formatos que não permitem as análises, portanto é 
#preciso transformá-los!

#transformando a primeira coluna em Data
class(analytics$date)
#pode ser necessário instalar o pacote antes de utilizá-lo
needs(lubridate)
analytics$date <- ymd(analytics$date)
class(analytics$date)
str(analytics)
any(is.na(analytics))

# Criar um fator ordenado para o dia da semana
analytics$dayOfWeek <- as.numeric(analytics$dayOfWeek)
analytics$dayOfWeek <- factor(analytics$dayOfWeek, labels = c("Domingo","Segunda","Terça","Quarta","Quinta","Sexta","Sábado"))

#arredondar as colunas que representam valor monetário
analytics$CPC <- round(analytics$CPC,2)
analytics$adCost <- round(analytics$adCost,2)
analytics$costPerTransaction <- round(analytics$costPerTransaction,2)
analytics$totalValue <- round(analytics$totalValue,2)
str(analytics)

# Aqui começa a construção do Modelo Predittivo
# Análise exploratória dos dados

# Pequenas Análises
dispositivo <- table(analytics$deviceCategory)
(dispositivo <- round((prop.table(dispositivo) * 100)))
dia <-  table(analytics$dayOfWeek)
(dia <- round(prop.table(dia) * 100))

# Construção de alguns gráficos para análises visuais
# Transações x potenciais variáveis 
# Pode ser necessário instalar o pacote
needs(ggplot2)
labels <- list("Boxplots - Transações por Origem",
               "Boxplots - Transações por Mídia",
               "Boxplots - Transações por Dia Dispositivo",
               "Boxplots - Transações por Dia da Semana")

xAxis <- list("source", "medium", "deviceCategory", "dayOfWeek")

# Gráficos
plot.scatter <- function(X, label){ 
  ggplot(analytics, aes_string(x = X, y = "totalValue")) + 
    geom_point(aes_string(colour = "totalValue"), alpha = 0.1) + 
    scale_colour_gradient(low = "green", high = "blue") + 
    geom_smooth(method = "loess") + 
    ggtitle(label) +
    theme(text = element_text(size = 20)) }

Map(plot.scatter, xAxis, labels)

# Obtendo apenas as colunas num?ricas
# Pode ser necessário instalar o pacote
needs(dplyr)
analytics_numericas <- sapply(analytics,is.numeric)
analytics <- analytics[, analytics_numericas]

#Análise de Correlação

par(mfrow=c(1,1))
data_cor <- cor(analytics)
data_cor
# Pode ser necessário instalar o pacote
needs(corrplot)
# Pode ser necessário instalar o pacote
needs(corrgram)
corrplot(data_cor, method = 'circle')
# Alternativas de gráficos
#corrgram(analytics)
#corrgram(analytics, order = T, lower.panel = panel.shade, 
#         upper.panel = panel.pie, text.panel = panel.txt)
#pairs(data_cor, panel = panel.smooth)

# Alternativa de Regressão Simples

# Selecionando as váriaveis
a <- analytics$adClicks
b <- analytics$totalValue
c <- data.frame(a,b)

# Dividindo os dados para o modelo
# Pode ser necessário instalar o pacote
needs(caTools)
set.seed(100)
amostra <- sample.split(c$a, SplitRatio = 0.70)
treino = subset(c, amostra = T)
teste = subset(c, amostra = F)


# Construindo o modelo de regressão
modelo <- lm(b ~ a, treino)
summary(modelo)
#plot(modelo)

# Plotando o gráfico de dispersão com a reta de regressão
k <- coefficients(modelo)
plot(a,b)
abline(k)

#fazendo as predições
#modelo <- lm(b ~ a, treino)
prevendo_totalValue <- predict(modelo,teste)
(sum(prevendo_totalValue))
sum(analytics$totalValue)

# Alterando o Valor para responder a pergunta do problema formulado
#venda_previsto <- data.frame(x=20000)
#y = a + bx
(venda_previsto <- k[1] + k[2]*34000)
(sum(analytics$totalValue))
(sum(analytics$adCost))
#previsao_1 <- predict(modelo, venda_previsto)
#sum(previsao_1)

################################################################

#Bootstrap

attach(analytics)

##Fazer um Bootstrap para regressão
require(boot)
require(bootstrap)
index<- analytics[,1]

N<-10000 #número de repetições bootstrap
n<-500   #Tamanho da amostra

b0<- rep(NA,N)
b1<- rep(NA,N)

for(i in 1:N){
  xstar <- sample(index,n,TRUE)
  mod<- lm(analytics$totalValue[xstar]~analytics$impressions[xstar])
  b0[i]<- as.numeric(mod$coefficients[1])
  b1[i]<- as.numeric(mod$coefficients[2])
  
}

#Estimativas finais dos coeficientes de regressão

mean(b0)
mean(b1)

#Fazer o ajuste do modelo

###########################################################################################

# Caret
library(caret)


# Função do Caret para divisão dos dados
#?createDataPartition
split <- createDataPartition(y = analytics$totalValue , p = 0.7, list = FALSE)

# Criando dados de treino e de teste
dados_treino <- analytics[split,]
dados_teste <- analytics[-split,]

# Treinando o modelo
#?train
names(getModelInfo())

# Regressao linear
modelolm <- train(totalValue ~ ., data = dados_treino, method = "lm")

# Regressao logistica
modelolm2 <- train(totalValue ~ ., data = dados_treino, method = "glm")

# Random forest
modelolm3 <- train(totalValue ~ ., data = dados_treino, method = "rf")

# Monotone Multi-Layer Perceptron Neural Network
modelolm4 <- train(totalValue ~ ., data = dados_treino, method = "monmlp")

# Multi Layer Percepton
modelolm5 <- train(totalValue ~ ., data = dados_treino, method = "mlp")

# Neural Network
modelolm6 <- train(totalValue ~ ., data = dados_treino, method = "neuralnet")

# Neural Network
modelolm7 <- train(totalValue ~ ., data = dados_treino, method = "nnet")

# Deep Neural 
modelolm8 <- train(totalValue ~ ., data = dados_treino, method = "dnn")

# Stocastic Gradient Boosting
modelolm9 <- train(totalValue ~ ., data = dados_treino, method = "gbm")

# Suport Vector Machine
library(kernlab)
modelolm10 <- train(totalValue ~ ., data = dados_treino, method = "svmLinear")

# Resumo do modelo
summary(modelolm)   # Regressão linear
summary(modelolm2)  # Regressão logistica
summary(modelolm3)  # Random forest
summary(modelolm4)  # Monotone Multi-Layer Perceptron Neural Network
summary(modelolm5)  # Multi Layer Percepton
summary(modelolm6)  # Neural Network
summary(modelolm7)  # Neural Network
summary(modelolm8)  # Deep Neural 
summary(modelolm9)  # Stocastic Gradient Boosting
summary(modelolm10) # Suport Vector Machine

# Ajustando o modelo
#?expand.grid
#?trainControl
#controle1 <- trainControl(method = "cv", number = 100)

#modelolm_v2 <- train(totalValue ~ ., data = dados_treino, method = "lm", 
#                     trControl = controle1, 
#                     metric = "Rsquared")

# Resumo do modelo
#summary(modelolm_v2)

# Coletando os residuos
#(residuals <- resid(modelolm))

# Previsões
#?predict
#(predictedValues <- predict(modelolm))
#(predictedValues <- predict(modelolm7))
#plot(dados_treino$totalValue, predictedValues)

# Mostrando a importância das variáveis para a criação do modelo
#?varImp
library(caret)

varImp(modelolm)   # Regressão linear
varImp(modelolm2)  # Regressão logistica
varImp(modelolm3)  # Random forest
varImp(modelolm4)  # Monotone Multi-Layer Perceptron Neural Network
varImp(modelolm5)  # Multi Layer Percepton
varimp(modelolm6)  # Neural Network
varImp(modelolm7)  # Neural Network
varImp(modelolm8)  # Deep Neural 
varImp(modelolm9)  # Stocastic Gradient Boosting
varImp(modelolm10) # Suport Vector Machine

# Plot
plot(varImp(modelolm))   # Regressão linear
plot(varImp(modelolm2))  # Regressão logistica
plot(varImp(modelolm6))  # Neural Network
plot(varImp(modelolm7))  # Neural Network
plot(varImp(modelolm10)) # Suport Vector Machine

#fazendo as predições
modelo_ajustado <- lm(totalValue ~ transactions + sessions + users + costPerTransaction
                      + CPC, dados_treino)
(summary(modelo_ajustado))
prevendo_totalValue <- predict(modelo,dados_teste)
(sum(prevendo_totalValue))
(sum(analytics$totalValue))

#######################################################################################

# Carregando o pacote para Redes Neurais
#install.packages("neuralnet")
library(neuralnet)

# Como primeiro passo, vamos abordar o pre-processamento de dados. 
# Eh uma boa pratica normalizar seus dados antes de treinar uma rede neural. 
# Dependendo do seu conjunto de dados, evitando a normalizacao pode levar a 
# resultados inuteis ou a um processo de treinamento muito dificil 
# (na maioria das vezes o algoritmo não ira convergir antes do numero de iteracoes
# maximo permitido). Voce pode escolher diferentes metodos para dimensionar os 
# dados (normalizacao-z, escala min-max, etc ...). 
# Normalmente escala nos intervalos [0,1] ou [1,1] tende a dar melhores resultados. 

dados <- analytics

# Normalizacao 
maxs <- apply(dados, 2, max) 
mins <- apply(dados, 2, min)

# Imprimindo os valores
maxs
mins

# Normalizando
dados_normalizados <- as.data.frame(scale(dados, center = mins, scale = maxs - mins))
head(dados_normalizados)

# Criando os dados de treino e de teste
#install.packages("caTools")
library(caTools)
split = sample.split(dados_normalizados$totalValue, SplitRatio = 0.70)

treino = subset(dados_normalizados, split == TRUE)
teste = subset(dados_normalizados, split == FALSE)

# Obtendo o nome das colunas
(coluna_nomes <- names(treino))

# Agregando
(formula <- as.formula(paste("totalValue ~", paste(coluna_nomes[!coluna_nomes %in% "totalValue"], collapse = " + "))))


# Treinando o Modelo
rede_neural <- neuralnet(formula, data = treino, hidden = c(5,3), linear.output = TRUE)

# Plot

plot(rede_neural)

# Fazendo previsoes com os dados de teste
rede_neural_prev <- compute(rede_neural, teste[1:9])

# O retorno da previsao da Rede Neural eh uma lista
str(rede_neural_prev)

# Convertendo os dados de teste
previsoes <- rede_neural_prev$net.result * (max(dados$totalValue) - min(dados$totalValue)) + min(dados$totalValue)
teste_convert <- (teste$totalValue) * (max(dados$totalValue) - min(dados$totalValue)) + min(dados$totalValue)
sum(teste_convert)

# Calculando o Mean Squared Error
(MSE.nn <- sum((teste_convert - previsoes)^2)/nrow(teste))


# Obtendo os erros de previsao
error.df <- data.frame(teste_convert, previsoes)
head(error.df)

# Plot dos erros
library(ggplot2)
ggplot(error.df, aes(x = teste_convert,y = previsoes)) + 
  geom_point() + stat_smooth()

#################################################################################################

