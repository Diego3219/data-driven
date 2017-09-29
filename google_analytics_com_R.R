# Documenta??o Oficial
#https://developers.google.com/analytics/solutions/r-google-analytics

# Instalando e ativando o pacote do Google Analytics para o R

#Aplicando uma camada de Ci?ncia no Marketing Digital

# Instalando o Pacote
library(needs)
needs(RGoogleAnalytics)

# Autorizar a conta do Google Analytics
# Isso n?o precisa ser executado em cada sess?o uma vez que o objeto token criado
# ? salvo

# Substitua as chaves pelas que voc? gerou no projeto da API
#https://console.developers.google.com
client.id <-"xx"
client.secret <-"xxx"
token <- Auth(client.id,client.secret)

# Salvar o objeto token para sess?es futuras
save(token,file="./token_file")

# Em sess?es futuras, ele pode ser carregado 
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
                   table.id = "ga:xxxxxxx")
                   


# Crie o objeto Query Builder para que os par?metros de consulta sejam validados
ga.query <- QueryBuilder(query.list)

# Extrair os dados e armazen?-los em um data-frame
ga.data <- GetReportData(ga.query, token, paginate_query = F)
head(ga.data)

#Gravar os dados em outra vari?vel ? uma boa pr?tica, pois em caso de neccessidade,
#os dados originais continuam armazenados e dispon?veis.
analytics <- data.frame(ga.data)
head(analytics)

#explorando a estrutura dos dados
str(analytics)

#os dados vir?o brutos e em formatos que n?o permitem as an?lises, portanto ? preciso transform?-los!

#transformando a primeira coluna em Data
class(analytics$date)
#pode ser necess?rio instalar o pacote antes de utiliz?-lo
needs(lubridate)
analytics$date <- ymd(analytics$date)
class(analytics$date)
str(analytics)
any(is.na(analytics))

# Criar um fator ordenado para o dia da semana
analytics$dayOfWeek <- as.numeric(analytics$dayOfWeek)
analytics$dayOfWeek <- factor(analytics$dayOfWeek, labels = c("Domingo","Segunda","Ter?a","Quarta","Quinta","Sexta","S?bado"))

#arredondar as colunas que representam valor monet?rio
analytics$CPC <- round(analytics$CPC,2)
analytics$adCost <- round(analytics$adCost,2)
analytics$costPerTransaction <- round(analytics$costPerTransaction,2)
analytics$totalValue <- round(analytics$totalValue,2)
str(analytics)

# Aqui come?a a constru??o do Modelo Predittivo
# An?lise explorat?ria dos dados

# Pequenas An?lises
dispositivo <- table(analytics$deviceCategory)
(dispositivo <- round((prop.table(dispositivo) * 100)))
dia <-  table(analytics$dayOfWeek)
(dia <- round(prop.table(dia) * 100))

# Constru??o de alguns gr?ficos para an?lises visuais
# Transa??es x potenciais variaveis 
# Pode ser necess?rio instalar o pacote
needs(ggplot2)
labels <- list("Boxplots - Transa??es por Origem",
               "Boxplots - Transa??es por M?dia",
               "Boxplots - Transa??es por Dia Dispositivo",
               "Boxplots - Transa??es por Dia da Semana")

xAxis <- list("source", "medium", "deviceCategory", "dayOfWeek")

# Gr?ficos
plot.scatter <- function(X, label){ 
  ggplot(analytics, aes_string(x = X, y = "totalValue")) + 
    geom_point(aes_string(colour = "totalValue"), alpha = 0.1) + 
    scale_colour_gradient(low = "green", high = "blue") + 
    geom_smooth(method = "loess") + 
    ggtitle(label) +
    theme(text = element_text(size = 20)) }

Map(plot.scatter, xAxis, labels)

# Obtendo apenas as colunas num?ricas
# Pode ser necess?rio instalar o pacote
needs(dplyr)
analytics_numericas <- sapply(analytics,is.numeric)
analytics <- analytics[, analytics_numericas]

#An?lise de Correla??o

par(mfrow=c(1,1))
data_cor <- cor(analytics)
data_cor
# Pode ser necess?rio instalar o pacote
needs(corrplot)
# Pode ser necess?rio instalar o pacote
needs(corrgram)
corrplot(data_cor, method = 'circle')
# Alternativas de gr?ficos
#corrgram(analytics)
#corrgram(analytics, order = T, lower.panel = panel.shade, 
#         upper.panel = panel.pie, text.panel = panel.txt)
#pairs(data_cor, panel = panel.smooth)

# Alternativa de Regress?o Simples

# Selecionando as v?riaveis
a <- analytics$impressions
b <- analytics$totalValue
c <- data.frame(a,b)

# Dividindo os dados para o modelo
# Pode ser necess?rio instalar o pacote
needs(caTools)
set.seed(100)
amostra <- sample.split(c$a, SplitRatio = 0.70)
treino = subset(c, amostra = T)
teste = subset(c, amostra = F)


# Construindo o modelo de regress?o
modelo <- lm(b ~ a, treino)
summary(modelo)
#plot(modelo)

# Plotando o gr?fico de dispers?o com a reta de regress?o
k <- coefficients(modelo)
plot(a,b)
abline(k)

#fazendo as predi??es
#modelo <- lm(b ~ a, treino)
prevendo_totalValue <- predict(modelo,teste)
sum(prevendo_totalValue)

# Alterando o Valor para responder a pergunta do problema formulado
#venda_previsto <- data.frame(x=20000)
#y = ax + b
(venda_previsto <- k[1]*35000 + k[2])
(sum(analytics$totalValue))
(sum(analytics$adCost))
#previsao_1 <- predict(modelo, venda_previsto)
#sum(previsao_1)



# Este Modelo est? sofrendo de overfiting e precisa ser ajustado