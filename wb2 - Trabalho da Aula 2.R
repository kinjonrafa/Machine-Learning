
#Trabalho de Técnicas em Machine Learning

#Professor: BRUNO TEBALDI DE QUEIROZ BARBOSA

#Alunos:

#EDUARDO FLEISCHMANN
#LUCAS LUCENA FALBO
#MARIANE KIAM ITOCAZU
#RAFAEL DA SILVA
#ROBIN DANIEL HONSI


#Limpando o Ambiente
rm(list = ls())
cat("\014")


#Importando as Bibliotecas
library(dplyr)
library(readxl)
library(readr)
library(ggplot2)
library(forecast)
library(urca)
library(tsibble)
library(fpp3)



#Lendo os dados

df <- read_csv("WB2 - Carseats.csv")


#Explorando os dados


#Vamos criar um histograma das vendas e avaliar seu comportamento:
ggplot(df) +
  geom_histogram(aes(x = Sales, y = ..density..), color = "black", fill = "lightblue", alpha = 0.8) +
  labs(title = "Histograma das Vendas", x = NULL, y = "Densidade") +
  theme_classic() + 
  theme(
    plot.title = element_text(size = 16 , face = "bold" , hjust = 0.5),
    axis.title = element_text(size = 14 , face = "bold"),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12 , face = "bold")
    )


#Vamos criar um gráfico de dispersão entre as variáveis "Sales" e "Price"
ggplot(df , aes(x = Sales, y = Price)) + 
  geom_point(color = "black" , size = 2) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) + 
  labs(title = "Dispersão entre Sales e Price", x = "Sales", y = "Price") +
  theme_classic() + 
  theme(
    plot.title = element_text(size = 16 , face = "bold" , hjust = 0.5),
    axis.title = element_text(size = 14 , face = "bold"),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12 , face = "bold"),
    panel.background = element_rect(fill = "gray90", color = NA), 
    panel.grid.major = element_line(color = "white", size = 0.5),  
    panel.grid.minor = element_line(color = "white", size = 0.25)  
  )

#Conclusão, olhando a dispersão entre os dados de Preço e vendas, podemos evidênciar
#uma correlação negativa, o que faz sentido. Quando o Preço aumenta, as vendas tendem a diminuir.




#Vamos criar um gráfico de dispersão entre as variáveis "Sales" e "Age"
ggplot(df , aes(x = Age, y = Sales)) + 
  geom_point(color = "black" , size = 2) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) + 
  labs(title = "Dispersão entre Sales e Age", x = "Age", y = "Sales") +
  theme_classic() + 
  theme(
    plot.title = element_text(size = 16 , face = "bold" , hjust = 0.5),
    axis.title = element_text(size = 14 , face = "bold"),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12 , face = "bold"),
    panel.background = element_rect(fill = "gray90", color = NA), 
    panel.grid.major = element_line(color = "white", size = 0.5),  
    panel.grid.minor = element_line(color = "white", size = 0.25)  
  )

#Conclusão, olhando a dispersão entre os dados de Vendas contra a idade do comprador, podemos evidênciar
#que há uma  correlação negativa, porém, menor do que intuitivamente nós esperávamos.
#Olhando a linha de tendência, percebemos a diminuição das vendas para idades mais elevadas, porém,
#com um coeficiente de inclinação baixo.



#Plotando um gráfico Boxplot comparando as variáveis "Sales" contra a variável categórica "Urban"
ggplot(df , aes(x = Urban, y = Sales)) + 
  geom_boxplot(fill = "white" , color = "black" , size = 1) +
  labs(title = "BoxPlot entre Sales e Urban", x = "Urban", y = "Sales") +
  theme_classic() + 
  theme(
    plot.title = element_text(size = 16 , face = "bold" , hjust = 0.5),
    axis.title = element_text(size = 14 , face = "bold"),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12 , face = "bold"),
    panel.background = element_rect(fill = "gray90", color = NA), 
    panel.grid.major = element_line(color = "white", size = 0.5),  
    panel.grid.minor = element_line(color = "white", size = 0.25)  
  )



#Plotando um gráfico Boxplot comparando as variáveis "Sales" contra a variável categórica "US"
ggplot(df , aes(x = US, y = Sales)) + 
  geom_boxplot(fill = "white" , color = "black" , size = 1) +
  labs(title = "BoxPlot entre Sales e US", x = "US", y = "Sales") +
  theme_classic() + 
  theme(
    plot.title = element_text(size = 16 , face = "bold" , hjust = 0.5),
    axis.title = element_text(size = 14 , face = "bold"),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12 , face = "bold"),
    panel.background = element_rect(fill = "gray90", color = NA), 
    panel.grid.major = element_line(color = "white", size = 0.5),  
    panel.grid.minor = element_line(color = "white", size = 0.25)  
  )




#Próximos passos

#Montar uma dispersão com mais variáveis
ggplot(df , aes(x = CompPrice, y = Sales)) + 
  geom_point(color = "black" , size = 2) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) + 
  labs(title = "Dispersão entre Sales e CompPrice", x = "CompPrice", y = "Sales") +
  theme_classic() + 
  theme(
    plot.title = element_text(size = 16 , face = "bold" , hjust = 0.5),
    axis.title = element_text(size = 14 , face = "bold"),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12 , face = "bold"),
    panel.background = element_rect(fill = "gray90", color = NA), 
    panel.grid.major = element_line(color = "white", size = 0.5),  
    panel.grid.minor = element_line(color = "white", size = 0.25)  
  )

ggplot(df , aes(x = Income, y = Sales)) + 
  geom_point(color = "black" , size = 2) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) + 
  labs(title = "Dispersão entre Sales e Income", x = "Income", y = "Sales") +
  theme_classic() + 
  theme(
    plot.title = element_text(size = 16 , face = "bold" , hjust = 0.5),
    axis.title = element_text(size = 14 , face = "bold"),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12 , face = "bold"),
    panel.background = element_rect(fill = "gray90", color = NA), 
    panel.grid.major = element_line(color = "white", size = 0.5),  
    panel.grid.minor = element_line(color = "white", size = 0.25)  
  )

ggplot(df , aes(x = Advertising, y = Sales)) + 
  geom_point(color = "black" , size = 2) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) + 
  labs(title = "Dispersão entre Sales e Advertising", x = "Advertising", y = "Sales") +
  theme_classic() + 
  theme(
    plot.title = element_text(size = 16 , face = "bold" , hjust = 0.5),
    axis.title = element_text(size = 14 , face = "bold"),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12 , face = "bold"),
    panel.background = element_rect(fill = "gray90", color = NA), 
    panel.grid.major = element_line(color = "white", size = 0.5),  
    panel.grid.minor = element_line(color = "white", size = 0.25)  
  )

ggplot(df , aes(x = Population, y = Sales)) + 
  geom_point(color = "black" , size = 2) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) + 
  labs(title = "Dispersão entre Sales e Population", x = "Population", y = "Sales") +
  theme_classic() + 
  theme(
    plot.title = element_text(size = 16 , face = "bold" , hjust = 0.5),
    axis.title = element_text(size = 14 , face = "bold"),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12 , face = "bold"),
    panel.background = element_rect(fill = "gray90", color = NA), 
    panel.grid.major = element_line(color = "white", size = 0.5),  
    panel.grid.minor = element_line(color = "white", size = 0.25)  
  )

ggplot(df , aes(x = Education, y = Sales)) + 
  geom_point(color = "black" , size = 2) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) + 
  labs(title = "Dispersão entre Sales e Education", x = "Education", y = "Sales") +
  theme_classic() + 
  theme(
    plot.title = element_text(size = 16 , face = "bold" , hjust = 0.5),
    axis.title = element_text(size = 14 , face = "bold"),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12 , face = "bold"),
    panel.background = element_rect(fill = "gray90", color = NA), 
    panel.grid.major = element_line(color = "white", size = 0.5),  
    panel.grid.minor = element_line(color = "white", size = 0.25)  
  )

df$ShelveLocOrdered <- factor(df$ShelveLoc, levels = c("Bad", "Medium", "Good"))
ggplot(df , aes(x = ShelveLocOrdered, y = Sales)) + 
  geom_boxplot(fill = "white" , color = "black" , size = 1) +
  labs(title = "BoxPlot entre Sales e ShelveLoc", x = "ShelveLoc", y = "Sales") +
  theme_classic() + 
  theme(
    plot.title = element_text(size = 16 , face = "bold" , hjust = 0.5),
    axis.title = element_text(size = 14 , face = "bold"),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12 , face = "bold"),
    panel.background = element_rect(fill = "gray90", color = NA), 
    panel.grid.major = element_line(color = "white", size = 0.5),  
    panel.grid.minor = element_line(color = "white", size = 0.25)  
  )


#Elencar as variáveis relevantes para o modelo

#Criar variáveis dummy utilizando as variáveis US e Urban como referência
df$UrbanYes <- ifelse(df$Urban == "Yes", 1, 0)
df$USYes <- ifelse(df$US == "Yes", 1, 0)
df$ShelveLocBad <- ifelse(df$ShelveLoc == "Bad", 1, 0)
df$ShelveLocMedium <- ifelse(df$ShelveLoc == "Medium", 1, 0)
df$ShelveLocGood <- ifelse(df$ShelveLoc == "Good", 1, 0)

#Criar os modelos de regressão mútipla
  #1 - Modelo de regressão múltipla sugerida pelo professor
    reg1 <- lm(Sales ~ Price + UrbanYes + USYes, data = df)
    summary(reg1)
  
    #Inserindo os valores estimados considerando o modelo 1 - Variáveis sugeridas pelo Professor
    val_estimated_1 <- predict(reg1)
    df$reg1 <- val_estimated_1
  
   #plotando gráfico para avaliar o modelo 1
    ggplot(df , aes(x = Sales, y = reg1)) + 
      geom_point(size = 2, color = "blue") +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 1.5) + 
      labs(title = "Dispersão entre Sales e os Valores Estimados pelo Modelo 1", x = "Sales", y = "Valores Estimados") +
      theme_classic() + 
      theme(
       plot.title = element_text(size = 16 , face = "bold" , hjust = 0.5),
       axis.title = element_text(size = 14 , face = "bold"),
       axis.text = element_text(size = 12),
       strip.text = element_text(size = 12 , face = "bold"),
       panel.background = element_rect(fill = "gray90", color = NA), 
       panel.grid.major = element_line(color = "white", size = 0.5),  
        panel.grid.minor = element_line(color = "white", size = 0.25)  
    )
  
  
#2 - Modelo de regressão múltipla com as variáveis que nós identificamos como relevantes
  reg2 <- lm(Sales ~ Price + Advertising + ShelveLocBad + ShelveLocMedium, data = df)
  summary(reg2)
  
  
  
  #Inserindo os valores estimados considerando o modelo 2 - Variáveis escolhidas por nós
  val_estimated_2 <- predict(reg2)
  df$reg2 <- val_estimated_2
  
  #plotando gráfico para avaliar o modelo 2
  ggplot(df , aes(x = Sales, y = reg2)) + 
    geom_point(size = 1.5, color = "purple") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 1.5) + 
    labs(title = "Dispersão entre Sales e os Valores Estimados pelo Modelo 2", x = "Sales", y = "Valores Estimados") +
    theme_classic() + 
    theme(
      plot.title = element_text(size = 16 , face = "bold" , hjust = 0.5),
      axis.title = element_text(size = 14 , face = "bold"),
      axis.text = element_text(size = 12),
      strip.text = element_text(size = 12 , face = "bold"),
      panel.background = element_rect(fill = "gray90", color = NA), 
      panel.grid.major = element_line(color = "white", size = 0.5),  
      panel.grid.minor = element_line(color = "white", size = 0.25)  
    )

# Analisar os resultados dos modelos comparar e avaliar qual modelo tem melhor comportamento
  #Critérios:
    #Valores dos coeficientes e Pvalor
    #Residual Standard error: quanto menor, melhor
    #R^2
    #Estatística F

#Construir o relatório em Word













