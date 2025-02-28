#Gráfico Pizza 3D Sobrevivência

#Sobrevivência total

Observacao <- c("Abelhas vivas", "Abelhas mortas")
numeros <- c(50.59, 48.24)

pie(numeros, labels = Observacao, main = "Sobrevivência total de abelhas")

library(ggplot2)
df <- data.frame(Observacao, numeros)
ggplot(df, aes(x = "", y = numeros, fill = Observacao)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  labs(title = "Proporção total de abelhas vivas e mortas")+
  theme_void()


#Sobrevivência durante período chuvoso

Observacao1 <- c("Abelhas vivas", "Abelhas mortas")
numeros1 <- c(58.33, 38.89)

df1 <- data.frame(Observacao1, numeros1)
ggplot(df, aes(x = "", y = numeros1, fill = Observacao1)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  labs(title = "Proporção total de abelhas vivas e mortas durante temporada chuvosa")+
  theme_void()

#Sobrevivência durante período seco

Observacao2 <- c("Abelhas vivas", "Abelhas mortas")
numeros2 <- c(44.90, 55.10)

df2 <- data.frame(Observacao2, numeros2)
ggplot(df, aes(x = "", y = numeros2, fill = Observacao2)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  labs(title = "Proporção total de abelhas vivas e mortas durante temporada seca")+
  theme_void()