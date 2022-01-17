
filial = c(1,1,1,1,1,1,0,0,0,0,0,0,1,1,0,0,0,0,1,0)
faturacao = c(650.7,620,701.3,789.3,726,320,1676,1817.3,2234,2292.3,1572.7,1012.8,735.3,844.1,948.7,1178.2,1440.7,1482.6,614.3,1180.1)
despesa = c(45.3,43.8,45.8,43.8,48.2,51.1,84.7,113.5,134.2,149.8,102.4,67.3,28.3,46.8,62.4,71.8,86.2,71.8,37.6,125.3)




# Filial Distruibiçao discreta e binominal , grafico circular e o grafico de barras

#faturacao e despesa variaveis continuas histograma e diagrama de extremos e quartis



f_f<- table(filial)

fr_f<-prop.table(f_f)

f_fatura<- table(faturacao)

fr_fatura<-prop.table(f_fatura)

freq_rel=h$counts/20

text(locator(n=5),paste(round(freq_rel,2)))



nomes_c<-c("Sul","Norte")
cores<-c("Black","Blue")
rotulo<-paste(nomes_c,"(",paste(f_f),")",sep=" ")
pie(f_f, main="Numero de individuos por regiao",labels=rotulo,col=cores)


barplot(f_f,main="Numero por filial",xlab="Filiais",ylab="Numero",col="skyblue", ylim=c(0,20))

fa<-table(faturacao) #var continua

# Nï¿½o utilizar barras para var contï¿½nuas
barplot(fa,main="Numero por Faturacao",xlab="faturacao",ylab="n",col="skyblue", ylim=c(0,10))

#Histograma
h<-hist(faturacao,main="Faturacao",xlab="Faturas em m€",ylab="Numero de faturas", col="skyblue",xlim=c(0,2500),ylim=c(0,10))
h

#Diagrama de extremos e quartis
boxplot(faturacao)

boxplot(faturacao ~ filial, main = "Comparacao fatura fililal", ylab="fatura em euros", xlab="", names=c("Sul","Norte"),col=c("pink","blue"))
IQR(faturacao)
tapply(faturacao,filial,summary)# Para interpretar os valores do boxplot

# nuvem de pontos, comparacao entre duas variaveis continuas
plot (despesa, faturacao)

#correlacao
cor(faturacao, despesa)  # correlaçao forte ???


#Modelo regresao linear

#b0 e o interept b1 e a correlacao

model = lm(despesa ~ faturacao)

abline(model, col="red")

# intercept quando a variavel independente  e 0 a outra e o valor de intercept, neste caso quando a despesa e 0, a faturacao e 108

# correlacao, a cada unidade que se adiciona a variavel indepente aumenta o (valor da variavel) á variavel restante.

# r elevado a 2 ( determinacao ),  r2 ..... da variancia de y e explicada pela variancia de x
cor(faturacao, despesa)^2
