

x = c(2,7,9,1,5,12)
y = c(13,21,23,14,15,21)

dados = cbind(x, y)

dados= as.data.frame(dados)

# Gr�fico de dispes�o entre o n�mero de semanas trabalhadas e
# o n�mero de carros inspecionados

plot(x,y, main="Diagrama de Dispersao", ylab="N�mero de carros inspecionados", xlab="N�mero de semanas trabalhadas")

plot(dados$x,dados$y, main="Diagrama de Dispersao", ylab="N�mero de carros inspecionados", xlab="N�mero de semanas trabalhadas", col="red")

abline(lm(y~x),col="red")
