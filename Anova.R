### Avaliando vari?veis ambientais
dados1=read.table("dados.txt",h=T)
require(ggplot2)
require(nlme)

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

##Morfometria
modeloPeso=glm(Peso~Pluviosidade+Sexo*Pluviosidade+TemperaturaExterna+TemperaturaInterna+UmidadeExterna+UmidadeInterna)
anova(modeloPeso,test = "F")

modeloComprimento=glm(Comprimento~Pluviosidade+Sexo*Pluviosidade+TemperaturaExterna+TemperaturaInterna+UmidadeExterna+UmidadeInterna)
anova(modeloComprimento,test = "F")

modeloMedIntertecular=glm(MedidaItertecular~Pluviosidade+Sexo*Pluviosidade+TemperaturaExterna+TemperaturaInterna+UmidadeExterna+UmidadeInterna)
anova(modeloMedIntertecular,test="F")

##Correlações
Correlacao1=cor.test(Peso,Comprimento, method = "spearman")
Correlacao2=cor.test(Peso,MedidaItertecular, method = "spearman")
Correlacao3=cor.test(Peso,TemperaturaExterna, method = "spearman")
Correlacao4=cor.test(Peso,TemperaturaInterna, method = "spearman")
Correlacao5=cor.test(Peso,UmidadeExterna, method = "spearman")
Correlacao6=cor.test(Peso,UmidadeInterna, method = "spearman")
Correlacao7=cor.test(Comprimento,MedidaItertecular, method = "spearman")
Correlacao8=cor.test(Comprimento,TemperaturaExterna, method = "spearman")
Correlacao9=cor.test(Comprimento,TemperaturaInterna, method = "spearman")
Correlacao10=cor.test(Comprimento,UmidadeInterna, method = "spearman")
Correlacao11=cor.test(Comprimento,UmidadeExterna, method = "spearman")
Correlacao12=cor.test(MedidaItertecular,TemperaturaExterna, method = "spearman")
Correlacao13=cor.test(MedidaItertecular,TemperaturaInterna, method = "spearman")
Correlacao14=cor.test(MedidaItertecular,UmidadeInterna, method = "spearman")
Correlacao15=cor.test(MedidaItertecular,UmidadeExterna, method = "spearman")
Correlacao16=cor.test(TemperaturaExterna,TemperaturaInterna, method = "spearman")
Correlacao17=cor.test(TemperaturaExterna,UmidadeInterna, method = "spearman")
Correlacao18=cor.test(TemperaturaExterna,UmidadeExterna, method = "spearman")
Correlacao19=cor.test(TemperaturaInterna,UmidadeInterna, method = "spearman")
Correlacao20=cor.test(TemperaturaInterna,UmidadeExterna, method = "spearman")
Correlacao21=cor.test(UmidadeInterna,UmidadeExterna, method = "spearman")



####conjunto de dados2

dados2=read.table("dados2.txt",h=T)
require(ggplot2)
require(nlme)

modeloVivas=glm(Vivas~Estacao+Temp_Externa+Umid_Externa+Temp_Interna+Umid_Interna,data=dados2)
anova(modeloVivas, test="F")

modeloSobrevivencia=glm(Sobrevivencia~Estacao+Temp_Externa+Umid_Externa+Temp_Interna+Umid_Interna,data=dados2)
anova(modeloSobrevivencia, test="F")

modeloMortas=glm(Mortas~Estacao+Temp_Externa+Umid_Externa+Temp_Interna+Umid_Interna,data=dados2)
anova(modeloMortas, test="F")

modeloMortalidade=glm(Mortalidade~Estacao+Temp_Externa+Umid_Externa+Temp_Interna+Umid_Interna,data=dados2)
anova(modeloMortalidade, test="F")

modeloCelulas=glm(Celulas~Estacao+Temp_Externa+Umid_Externa+Temp_Interna+Umid_Interna,data=dados2)
anova(modeloCelulas, test="F")

###Correlações
Vivas=dados2[,6]
Mortas=dados2[,7]
Celulas=dados2[,8]
Temp_Externa=dados2[,2]
Umid_Externa=dados2[,3]
Temp_Interna=dados2[,4]
Umid_Interna=dados2[,5]

Correlacao22=cor.test(Vivas,Mortas, method = "spearman")
Correlacao23=cor.test(Vivas,Celulas, method = "spearman")
Correlacao24=cor.test(Vivas,Temp_Externa, method = "spearman")
Correlacao25=cor.test(Vivas,Temp_Interna, method = "spearman")
Correlacao26=cor.test(Vivas,Umid_Externa, method = "spearman")
Correlacao27=cor.test(Vivas,Umid_Interna, method = "spearman")
Correlacao28=cor.test(Mortas,Celulas, method = "spearman")
Correlacao29=cor.test(Mortas,Temp_Externa, method = "spearman")
Correlacao30=cor.test(Mortas,Temp_Interna, method = "spearman")
Correlacao31=cor.test(Mortas,Umid_Externa, method = "spearman")
Correlacao32=cor.test(Mortas,Umid_Interna, method = "spearman")
Correlacao33=cor.test(Celulas,Temp_Externa, method = "spearman")
Correlacao34=cor.test(Celulas,Temp_Interna, method = "spearman")
Correlacao35=cor.test(Celulas,Umid_Externa, method = "spearman")
Correlacao36=cor.test(Celulas,Umid_Interna, method = "spearman")
Correlacao37=cor.test(Temp_Externa,Temp_Interna, method = "spearman")
Correlacao38=cor.test(Temp_Externa,Umid_Externa, method = "spearman")
Correlacao39=cor.test(Temp_Externa,Umid_Interna, method = "spearman")
Correlacao40=cor.test(Temp_Interna,Umid_Externa, method = "spearman")
Correlacao41=cor.test(Temp_Interna,Umid_Interna, method = "spearman")
Correlacao42=cor.test(Umid_Externa,Umid_Interna, method = "spearman")

#Dias para nascer
dados3=read.table("dados3.txt",h=T)
require(ggplot2)
require(nlme)

modeloDias=glm(Dias_para_nascer~Estacao+Temp_Externa+Temp_Interna+Umid_Externa+Umid_Interna,data=dados3)

anova(modeloDias, test="F")
