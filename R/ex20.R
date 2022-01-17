

x = c(2,7,9,1,5,12)
y = c(13,21,23,14,15,21)

dados = cbind(x, y)

dados= as.data.frame(dados)

# Gráfico de dispesão entre o número de semanas trabalhadas e
# o número de carros inspecionados

plot(x,y, main="Diagrama de Dispersao", ylab="Número de carros inspecionados", xlab="Número de semanas trabalhadas")

plot(dados$x,dados$y, main="Diagrama de Dispersao", ylab="Número de carros inspecionados", xlab="Número de semanas trabalhadas", col="red")

abline(lm(y~x),col="red")
