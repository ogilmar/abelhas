#Selecionando o diret?rio
setwd("~/Documentos_pessoais/Estudos_abelha_solitaria")

#Pacotes

require(ggplot2)
library(reshape2)

### Avaliando variaveis ambientais
dados1=read.table("dados_1.txt",h=T)

Repeticao=dados1[,1]
Abelhas=dados1[,2]
Sexo=dados1[,3]
Nascimento=dados1[,4]
Pluviosidade=dados1[,5]
Peso=dados1[,6]
Comprimento=dados1[,7]
Medida_Itertecular=dados1[,8]
Temperatura_Externa=dados1[,9]
Umidade_Externa=dados1[,10]
Temperatura_Interna=dados1[,11]
Umidade_Interna=dados1[,12]

#Criando nova base de dados

dados2 <- cbind(Peso, Comprimento, Medida_Itertecular, Temperatura_Externa, Temperatura_Interna, Umidade_Externa, Umidade_Interna)

# Criando matrix de correlação

cormat <- round(x = cor(dados2), digits = 2)
melted_cormat <- melt(cormat)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill = value)) +
  geom_tile()
