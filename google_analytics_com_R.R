# Documentação Oficial
#https://developers.google.com/analytics/solutions/r-google-analytics

# Instalando e ativando o pacote do Google Analytics para o R

#Aplicando uma camada de Ciência no Marketing Digital

# Instalando o Pacote
needs(RGoogleAnalytics)

# Autorizar a conta do Google Analytics
# Isso não precisa ser executado em cada sessão uma vez que o objeto token criado
# é salvo

# Substitua as chaves pelas que você gerou no projeto da API
#https://console.developers.google.com
client.id <-"xxxxxxxxxxxx-xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.apps.googleusercontent.com"
client.secret <-"xxxxxxxxxxxxxxxxxxxxxxxx"
token <- Auth(client.id,client.secret)

# Salvar o objeto token para sessões futuras
save(token,file="./token_file")

# Em sessões futuras, ele pode ser carregado 
load("./token_file")

ValidateToken(token)

# Criar uma lista de consulta com os parâmetros da API
query.list <- Init(start.date = "2017-01-01",
                   end.date = "2016-01-31",
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
                   table.id = "ga:xxxxxxxx")
                   


# Crie o objeto Query Builder para que os parâmetros de consulta sejam validados
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

#os dados virão brutos e em formatos que não permitem as análises, portanto é preciso transformá-los!

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
# Transações x potenciais variaveis 
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

# Obtendo apenas as colunas numéricas
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
a <- analytics$impressions
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
sum(prevendo_totalValue)

# Alterando o Valor para responder a pergunta do problema formulado
venda_previsto <- data.frame(x=20000)
previsao_1 <- predict(modelo, venda_previsto)
sum(previsao_1)

# Este Modelo está sofrendo de overfiting e precisa ser ajustado