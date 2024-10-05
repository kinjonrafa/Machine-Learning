
#Fórum - 2 - Modelo de previsão de Aluguel de Bikes

#1) Qual o Problema quero resolver.
#Montar um modelo que preveja o aluguel de bikes para os dias finais de cada mês dos anos de 2011 e 2012

#2) Tenho Base de dados?
#Duas bases divididas entre treino e teste sendo que a base de teste não tem os valores de locação, que deveremos prever

#3) Como são os dados? Tem valores ausentes? Quais os tipos de de variáveis?

#4) Qual a frequência dos dados?

#5) Como os dados estão distribuídos?

#6) Resumo estatístico dos dados

#7) Transformações. Quais transformações podemos aplicar? Quais devemos aplicar? Por que aplicar?



# Limpando o Ambiente -----------------------------------------------------
rm(list = ls())
cat("\014")
while (dev.cur() > 1) dev.off()



# Importando Bibliotecas --------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(fastDummies)
library(tidyverse)
library(lubridate)
library(nnls)
library(rsample)
library(glmnet)
library(randomForest)
library(reshape2)

# Setando o seed desta sessão
set.seed(1)

# Importando os Dados -----------------------------------------------------

bike_train_raw <- read.csv("train.csv" ,
                           sep = "," ,
                           header = TRUE,
                           stringsAsFactors = TRUE)

bike_test_raw <- read.csv("test.csv" ,
                          sep = "," ,
                          header = TRUE,
                          stringsAsFactors = TRUE)


# Criando uma lista com as colunas do data-set
bike_colunas <- names(bike_train_raw )
print(bike_colunas)


#Plotando um Histograma e avaliando a distribuição das locações
hist(bike_train_raw$count)


# Verificando as estatísticas básicas do dataframe
summary(bike_train_raw[,c("temp","atemp","count")] )


# Convertendo a coluna datetime para data
bike_train_raw$datetime <- ymd_hms(bike_train_raw$datetime)


# Verificando os tipos de dados do dataframe
str(bike_train_raw)


# Criando colunas com os dados de dia, mês, ano, hora e dia da semana
bike_train_raw$hora <- hour(bike_train_raw$datetime)

bike_train_raw$dia <- day(bike_train_raw$datetime)

bike_train_raw$mes <- month(bike_train_raw$datetime)

bike_train_raw$ano <- year(bike_train_raw$datetime)

# Criando uma coluna com o dia da semana e convertendo em fatores
bike_train_raw$diasemana <- factor(wday(bike_train_raw$datetime),
                                   levels = 1:7,
                                   labels = c("Domingo", "Segunda", "Terça", "Quarta", "Quinta", "Sexta", "Sábado"))


# Convertendo a coluna weather em fatores
bike_train_raw$weather <- factor(bike_train_raw$weather, levels = 1:4,
                                 labels = c("Clear/Few clouds", 
                                            "Mist/Cloudy", 
                                            "Light Snow/Rain", 
                                            "Heavy Rain/Snow"))

# Convertendo a coluna season em fatores
bike_train_raw$season <- factor(bike_train_raw$season, levels = 1:4,
                                 labels = c("spring", 
                                            "summer", 
                                            "fall", 
                                            "winter"))




# Criando coluna contendo o período do dia - Manhã, tarde, noite e madrugada

bike_train_raw <- bike_train_raw %>% mutate(periodo = case_when(
  hora >= 6 & hora < 12 ~ "Manhã",
  hora >= 12 & hora < 18 ~ "Tarde",
  hora >= 18 & hora <= 23 ~ "Noite",
  hora >= 0 & hora < 6 ~ "Madrugada"
  ))




# Plotando uma matriz de correlação
plot(bike_train_raw[,!(names(bike_train_raw) %in% ("datetime"))])


# Explorando a base de treino, para identificação de possíveis relacionamentos

# Plotando um gráfico da dispersão entre a temperatura e a quantidade total de locações
ggplot(bike_train_raw, aes(x = temp, y = count)) +
  geom_point() +
  geom_smooth(method = "lm", col = "lightblue") + # Inclui uma linha de tendência
  labs(title = "Relação entre Temperatura e Locações de Bikes", x = "Temperatura (°C)", y = "Locações Totais")



# Plotando um gráfico boxplot entre a quantidade total de locações e o dia da semana
ggplot(bike_train_raw, aes(x = diasemana , y = count )) +
  geom_boxplot() +
  labs(title = "Distribuição de Locações por Dia da Semana", x = "Dia da Semana", y = "Locações Totais")



# Plotando um gráfico boxplot entre a quantidade total de locações e o clima
ggplot(bike_train_raw, aes(x = weather, y = count)) +
  geom_boxplot() +
  labs(title = "Locações de Bikes por Condição Climática", x = "Clima", y = "Locações Totais")


# Plotando um HeatMap - Locações por Hora x Dia da Semana
ggplot(bike_train_raw, aes(x = hora, y = diasemana, fill = count)) +
  geom_tile() +
  labs(title = "Demanda de Locações por Hora e Dia da Semana", x = "Hora do Dia", y = "Dia da Semana")

# Plotando um gráfico de barras - Locações x Mês
ggplot(bike_train_raw, aes(x = factor(mes), y = count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Locações Totais de Bikes por Mês", x = "Mês", y = "Locações Totais")

# Plotando um gráfico com a Relação entre Umidade, Clima e Locações
ggplot(bike_train_raw, aes(x = humidity, y = count, color = season)) +
  geom_point() +
  labs(title = "Relação entre Umidade, Clima e Locações de Bikes", x = "Umidade (%)", y = "Locações Totais")


# Plotando um gráfico boxplot entre a Estação e a Quantidade total de locações
ggplot(bike_train_raw, aes(x = season , y = count )) +
  geom_boxplot() +
  labs(title = "Distribuição de Locações por Estação", x = "Estação", y = "Locações Totais")



# Separando a base bike_train_raw entre treino e teste

  # Identificando o número de observações
  n <- nrow(bike_train_raw)

  # Separando a proporção dos dados de treino (70%)
  train_index <- sample(1:n, size = 0.7*n)

  # Criando dataset de treino
  bike_model_train <- bike_train_raw[train_index,]
  
  # Criando dataset de teste
  bike_model_test <- bike_train_raw[-train_index,]



# Construindo modelo de regressão considerando todos os campos contra a coluna count
modelo1 = lm(count ~ ., data = bike_model_train[, !(names(bike_model_train) %in% c("datetime",
                                                                                   "casual",
                                                                                   "registered",
                                                                                   "season",
                                                                                   "atemp"))])

  summary(modelo1)
  # Mostrando os primeiros registros do modelo ajustado
  head(modelo1$fitted.values , 10)



# Verificando os tipos de dados do dataframe
str(bike_model_train)



# Construindo um segundo modelo modelo de regressão considerando apenas as colunas relevantes
modelo2 = lm(count ~ workingday + holiday + season + weather + temp + 
             diasemana + periodo + hora ,
             data = bike_model_train)

summary(modelo2)


#Realizando as Predições

  #Predição Modelo 1

  predic_mdl_1 <- predict(modelo1, newdata = bike_model_test)
  result_predic_1 <- data.frame(real = bike_model_test$count,
                                previsto = predic_mdl_1)
  
  #Avaliando os resultados das previsões identifiquei que existem valores negativos
  #tratando valores negativos
  
  result_predic_1$previsto[result_predic_1$previsto < 0] <- 0
  
  
  # Plotando as previsões versus os valores reais
  ggplot(result_predic_1, aes(x = real, y = previsto)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, col = "red") +
    labs(title = "Previsão vs Real (Modelo 1)",
         x = "Valor Real",
         y = "Valor Previsto")


  #Predição Modelo 2
  
  predic_mdl_2 <- predict(modelo2, newdata = bike_model_test)
  result_predic_2 <- data.frame(real = bike_model_test$count,
                                previsto = predic_mdl_2)
  
  #Avaliando os resultados das previsões identifiquei que existem valores negativos
  #tratando valores negativos
  result_predic_2$previsto[result_predic_2$previsto < 0] <- 0
  
  
  # Plotando as previsões versus os valores reais
  ggplot(result_predic_2, aes(x = real, y = previsto)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, col = "blue") +
    labs(title = "Previsão vs Real (Modelo 2)",
         x = "Valor Real",
         y = "Valor Previsto")


  #Plotando a raíz da média dos erros logarítimicos quadrados dos modelos
  
  RMSLE_mdl_1 <- sqrt((1/n)*sum((log(result_predic_1$previsto + 1) - log(result_predic_1$real + 1))^2))
  RMSLE_mdl_2 <- sqrt((1/n)*sum((log(result_predic_2$previsto + 1) - log(result_predic_2$real + 1))^2))
  
  print(paste("RMSLE do modelo 1:", RMSLE_mdl_1))
  print(paste("RMSLE do modelo 2:", RMSLE_mdl_2))
  
  
  #Os modelos acima utilizaram uma regressao multipla para modelagem, 
  #e os resultados nao foram tao satisfatorios
  
  
  #Vamos estimar um modelo Ridge e um modelo Lasso e avaliar os resultados
  
  #Preparando os dados de treino para o modelo
    x_train <- model.matrix(count ~ . -1, data = bike_model_train)
    y_train <- bike_model_train$count
  
  #Preparando os dados de Teste para o modelo
    x_test <- model.matrix(count ~ . -1, data = bike_model_test)
  
  # Ajustando o Lambda da Regressão Lasso / Validação Cruzada ------------------
  lambdas_to_try <- 10^seq(-3, 4, length = 100)
 
     
  # Lasso Model
  #Lasso - Validacao Cruzada
  bike_lasso_model_cv <- cv.glmnet(x_train, y_train, 
                             alpha = 1,
                             lambda = lambdas_to_try,
                             standardize = FALSE,
                             nfolds = 10)
  
  #Lasso - Plot Cross Validation
  plot(bike_lasso_model_cv)
  
  #Identificando o melhor Lambda
  best_lasso_lambda <- bike_lasso_model_cv$lambda.min
  
  
  #Estimando o modelo Lasso utilizando o melhor lambda
  bike_lasso_model<- glmnet(x_train , y_train,
                            alpha = 1,
                            lambda = best_lasso_lambda,
                            standardize = FALSE)
  
  #Avaliando os coeficientes do modelo
  coef(bike_lasso_model)
  
  
  #Estimando previsoes
  
  lasso_predict <- predict(bike_lasso_model,
                           s = "lambda.min",
                           newx = x_test)
  
  
  
  #Ridge Model
  
  #Ridge - Validacao Cruzada
  bike_ridge_model_cv <- cv.glmnet(x_train, y_train, 
                                   alpha = 0,
                                   lambda = lambdas_to_try,
                                   standardize = FALSE,
                                   nfolds = 10)
  
  #Ridge - Plot Cross Validation
  plot(bike_ridge_model_cv)
  
  
  #Identificando o melhor Lambda
  best_ridge_lambda <- bike_ridge_model_cv$lambda.min

    
  #Estimando o modelo Ridge utilizando o melhor lambda
  bike_ridge_model<- glmnet(x_train , y_train,
                            alpha = 0,
                            lambda = best_ridge_lambda,
                            standardize = FALSE)
  
 
   #Avaliando os coeficientes do modelo
  coef(bike_ridge_model)
  
  
  #Estimando previsoes
  
  ridge_predict <- predict(bike_ridge_model,
                           s = "lambda.min",
                           newx = x_test)
  
  
  # Calculando RMSLE para o modelo Lasso
  lasso_predict[lasso_predict < 0] <- 0  # Ajuste para evitar logs de valores negativos
  RMSLE_lasso <- sqrt(mean((log(lasso_predict + 1) - log(bike_model_test$count + 1))^2))
  
  # Calculando RMSE para o modelo Lasso
  RMSE_lasso <- sqrt(mean((lasso_predict - bike_model_test$count)^2))
  
  # Calculando MAE para o modelo Lasso
  MAE_lasso <- mean(abs(lasso_predict - bike_model_test$count))
  
  # Calculando RMSLE para o modelo Ridge
  ridge_predict[ridge_predict < 0] <- 0  # Ajuste para evitar logs de valores negativos
  RMSLE_ridge <- sqrt(mean((log(ridge_predict + 1) - log(bike_model_test$count + 1))^2))
  
  # Calculando RMSE para o modelo Ridge
  RMSE_ridge <- sqrt(mean((ridge_predict - bike_model_test$count)^2))
  
  # Calculando MAE para o modelo Ridge
  MAE_ridge <- mean(abs(ridge_predict - bike_model_test$count))
  
  # Calculando métricas para os modelos de regressão anteriores
  # Para modelo1:
  RMSLE_mdl_1 <- sqrt(mean((log(result_predic_1$previsto + 1) - log(result_predic_1$real + 1))^2))
  RMSE_mdl_1 <- sqrt(mean((result_predic_1$previsto - result_predic_1$real)^2))
  MAE_mdl_1 <- mean(abs(result_predic_1$previsto - result_predic_1$real))
  
  # Para modelo2:
  RMSLE_mdl_2 <- sqrt(mean((log(result_predic_2$previsto + 1) - log(result_predic_2$real + 1))^2))
  RMSE_mdl_2 <- sqrt(mean((result_predic_2$previsto - result_predic_2$real)^2))
  MAE_mdl_2 <- mean(abs(result_predic_2$previsto - result_predic_2$real))
  
  
  
  # Criando uma tabela de comparação
  comparacao_modelos <- data.frame(
    Modelo = c("Regressão Múltipla (modelo 1)", "Regressão Múltipla (modelo 2)", "Lasso", "Ridge"),
    RMSLE = c(RMSLE_mdl_1, RMSLE_mdl_2, RMSLE_lasso, RMSLE_ridge),
    RMSE = c(RMSE_mdl_1, RMSE_mdl_2, RMSE_lasso, RMSE_ridge),
    MAE = c(MAE_mdl_1, MAE_mdl_2, MAE_lasso, MAE_ridge)
  )
  
  # Exibindo a tabela de comparação
  print(comparacao_modelos)
  
  
  # Criar um dataframe com os valores reais e previsões
  df_plot <- data.frame(
    Dia = 1:length(bike_model_test$count),  # Ou alguma outra variável temporal
    Real = bike_model_test$count,
    Previsto_Modelo_1 = result_predic_1$previsto,
    Previsto_Modelo_2 = result_predic_2$previsto,
    Previsto_Lasso = lasso_predict
  )
  
  # Reshape o dataframe para formato longo (necessário para ggplot)
  df_long <- reshape2::melt(df_plot, id.vars = "Dia", variable.name = "Modelo", value.name = "Valor")
  
  # Criar o gráfico com ggplot
  ggplot(df_long, aes(x = Dia, y = Valor, color = Modelo)) +
    geom_line(size = 1) +  # Gráfico de linha
    labs(title = "Comparação de Previsões de Diferentes Modelos",
         x = "Dia", y = "Número de Locações de Bikes") +
    theme_minimal()
  
  # Treine o modelo Random Forest
  set.seed(123)  # Definir uma semente para resultados reprodutíveis
  
  # Estimando o modelo de Random Forest
  rf_model <- randomForest(count ~ ., 
                           data = bike_model_train[, !(names(bike_model_train) %in% c("datetime", "casual", "registered"))], 
                           ntree = 500,  # número de árvores na floresta
                           importance = TRUE)  # importância das variáveis
  
  
  # Fazer as previsões na base de teste
  rf_predict <- predict(rf_model, newdata = bike_model_test[, !(names(bike_model_test) %in% c("datetime", "casual", "registered"))])
  
  # Calcular RMSLE
  RMSLE_rf <- sqrt(mean((log(rf_predict + 1) - log(bike_model_test$count + 1))^2))
  
  # Exibir o valor de RMSLE
  print(RMSLE_rf)
  
  

  # Visualizar a importância das variáveis
  importance(rf_model)
  varImpPlot(rf_model)
  
  
  # Adicionar previsões do modelo Random Forest ao gráfico
  df_plot$Previsto_RF <- rf_predict
  df_long <- melt(df_plot, id.vars = "Dia", variable.name = "Modelo", value.name = "Valor")
  
  ggplot(df_long, aes(x = Dia, y = Valor, color = Modelo)) +
    geom_line(size = 1) +
    labs(title = "Comparação de Previsões de Diferentes Modelos",
         x = "Dia", y = "Número de Locações de Bikes") +
    theme_minimal()
  
