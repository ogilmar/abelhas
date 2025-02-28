#Selecionando o diretorio
setwd("~/Documentos_pessoais/Estudos_abelha_solitaria")


#Importando os dados
dados1=read.table("dados_1.txt",h=T)

#Indentificando as vari?veis
Repeticao=dados1[,1]
Abelhas=dados1[,2]
Sexo=dados1[,3]
Nascimento=dados1[,4]
Pluviosidade=dados1[,5]
Peso=dados1[,6]
Comprimento=dados1[,7]
MedidaItertecular=dados1[,8]
TemperaturaExterna=dados1[,9]
UmidadeExterna=dados1[,10]
TemperaturaInterna=dados1[,11]
UmidadeInterna=dados1[,12]

#Analisando as variaveis morfometricas

## Peso corporal

# Medidas de tendencia central e dispersao
resumoPeso <- c( media = mean(Peso), 
                mediana = median(Peso), 
                desvio_padrao = sd(Peso),
                minimo = min(Peso),
                maximo = max(Peso))
print(resumoPeso)

# Histograma dos pesos
ggplot(dados1, aes(x = Peso)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma do Peso das Abelhas",
       x = "Peso (gramas)",
       y = "Frequ?ncia") +
  theme_minimal()

modeloPeso<-(glm(Peso~Pluviosidade+Sexo*Pluviosidade+TemperaturaExterna+TemperaturaInterna+UmidadeExterna+UmidadeInterna))
summary(modeloPeso)

# Grafico de densidade (distribui??o suavizada)
ggplot(dados1, aes(x = Peso, fill = NULL)) +
  geom_density(alpha = 0.5) +
  labs(title = "Densidade do Peso",
       x = "Peso (gramas)",
       y = "Densidade") +
  theme_minimal()

#ANOVA do peso

modeloPeso=glm(Peso~Pluviosidade+Sexo*Pluviosidade+TemperaturaExterna+TemperaturaInterna+UmidadeExterna+UmidadeInterna)
anova(modeloPeso,test = "F")

modeloComprimento=glm(Comprimento~Pluviosidade+Sexo*Pluviosidade+TemperaturaExterna+TemperaturaInterna+UmidadeExterna+UmidadeInterna)
anova(modeloComprimento,test = "F")

modeloMedIntertecular=glm(MedidaItertecular~Pluviosidade+Sexo*Pluviosidade+TemperaturaExterna+TemperaturaInterna+UmidadeExterna+UmidadeInterna)
anova(modeloMedIntertecular,test="F")
