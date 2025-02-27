#Estudando as vari?veis ambientais

#Selecionando o diretório
setwd("~/Documentos_pessoais/Estudos_abelha_solitaria")

#Pacotes necessários

library(ggplot2)

#Importando os dados
dados1=read.table("dados_1.txt",h=T)

#Identificando variáveis

TemperaturaExterna=dados1[,9]
UmidadeExterna=dados1[,10]
TemperaturaInterna=dados1[,11]
UmidadeInterna=dados1[,12]

### TEMPERATURA EXTERNA

# Medidas de tendencia central e dispersao da temperatura externa
resumoTemperaturaExterna <- c( media = mean(TemperaturaExterna), 
                 mediana = median(TemperaturaExterna), 
                 desvio_padrao = sd(TemperaturaExterna),
                 minimo = min(TemperaturaExterna),
                 maximo = max(TemperaturaExterna))
print(resumoTemperaturaExterna)

BoxPlot1<- boxplot(Temperatura_Externa, names="Temperatura externa", ylab="Temperatura Externa (ºC)", col=0)

# Histograma da temperatura externa
ggplot(dados1, aes(x = TemperaturaExterna)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma da Temperatura Externa",
       x = "Temperatura externa (?C)",
       y = "Frequ?ncia") +
  theme_minimal()

# Gráfico de densidade (distribuição suavizada)
ggplot(dados1, aes(x = TemperaturaExterna, fill = NULL)) +
  geom_density(alpha = 0.5) +
  labs(title = "Densidade do Peso",
       x = "Temperatura Externa (?C)",
       y = "Densidade") +
  theme_minimal()

### TEMPERATURA INTERNA

# Medidas de tendencia central e dispersao da temperatura interna
resumoTemperaturaInterna <- c( media = mean(TemperaturaInterna), 
                               mediana = median(TemperaturaInterna), 
                               desvio_padrao = sd(TemperaturaInterna),
                               minimo = min(TemperaturaInterna),
                               maximo = max(TemperaturaInterna))
print(resumoTemperaturaInterna)

BoxPlot2<- boxplot(TemperaturaInterna, names="Temperatura externa", ylab="Temperatura Interna (ºc)", col=0)

# Histograma da temperatura interna
ggplot(dados1, aes(x = TemperaturaInterna)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma da Temperatura Interna",
       x = "Temperatura interna (?C)",
       y = "Frequ?ncia") +
  theme_minimal()

# Gráfico de densidade (distribui??o suavizada)
ggplot(dados1, aes(x = TemperaturaInterna, fill = NULL)) +
  geom_density(alpha = 0.5) +
  labs(title = "Densidade da Temperatura Interna",
       x = "Temperatura Interna (?C)",
       y = "Densidade") +
  theme_minimal()


### UMIDADE INTERNA

# Medidas de tendencia central e dispersao da umidade interna
resumoUmidadeInterna <- c( media = mean(UmidadeInterna), 
                               mediana = median(UmidadeInterna), 
                               desvio_padrao = sd(UmidadeInterna),
                               minimo = min(UmidadeInterna),
                               maximo = max(UmidadeInterna))
print(resumoUmidadeInterna)

BoxPlot3<- boxplot(Umidade_Interna, names="Umidade Interna", ylab="Umidade Interna (%)", col=0)
            

### UMIDADE EXTERNA

# Medidas de tendencia central e dispersao da umidade interna
resumoUmidadeExterna <- c( media = mean(UmidadeExterna), 
                           mediana = median(UmidadeExterna), 
                           desvio_padrao = sd(UmidadeExterna),
                           minimo = min(UmidadeExterna),
                           maximo = max(UmidadeExterna))
print(resumoUmidadeExterna)

BoxPlot4<- boxplot(Umidade_Externa, names="Umidade Externa", ylab="Umidade Externa (%)", col=0)
