#************************************************************
#*Scripts do Trabalho
#*#************************************************************



#************************************************************
#*Variaveis
#*#************************************************************


filial = c(1,1,1,1,1,1,0,0,0,0,0,0,1,1,0,0,0,0,1,0)
faturacao = c(650.7,620,701.3,789.3,726,320,1676,1817.3,2234,2292.3,1572.7,1012.8,735.3,844.1,948.7,1178.2,1440.7,1482.6,614.3,1180.1)
despesa = c(45.3,43.8,45.8,43.8,48.2,51.1,84.7,113.5,134.2,149.8,102.4,67.3,28.3,46.8,62.4,71.8,86.2,71.8,37.6,125.3)
funcionarios = c(53,49,52,61,43,28,105,137,156,139,98,87,52,66,67,97,116,101,47,83)
avaliacao = c(2,1,1,2,3,1,3,4,4,3,3,4,3,2,1,2,3,4,1,2)


#Summary das variaveis
summary(despesa)
summary(faturacao)
summary(funcionarios)

#Utilizar as variaveis

f_f<- table(filial)
fr_f<-prop.table(f_f)
f_fatura<- table(faturacao)
f_despesa<- table(despesa)
f_funcionarios<- table(funcionarios)
f_avaliacao<- table(avaliacao)
fr_fatura<-prop.table(f_fatura)

#************************************************************
#*Graficos
#*#************************************************************

#Extras dos graficos

nomes_c<-c("Sul","Norte")
cores<-c("Black","Blue")
rotulo<-paste(nomes_c,"(",paste(f_f),")",sep=" ")
nomes_a<-c("Grande","Boa","Decente","Fraca")
cores_a<-c("Green","Blue","Orange","Red")
rotulo_ava<-paste(nomes_a,"(",paste(f_avaliacao),")",sep=" ")
rotulo_ava<-paste(nomes_a,"(",paste(f_avaliacao),")",sep=" ")
rotulo<-paste(nomes_c,"(",paste(round(freq_rel,3) ),")",sep=" ")

# pie charts
pie(f_f, main="Filiais",labels=rotulo,col=cores)
pie(f_avaliacao, main="Avaliacoes",labels=rotulo_ava,col=cores_a)




# Grafico de Barras
barplot(f_despesa,main="Numero por Faturacao",xlab="faturacao",ylab="n",col="skyblue", ylim=c(0,10))
barplot(f_funcionarios,main="Numero por filial",xlab="Filiais",ylab="Numero",col="skyblue", ylim=c(0,20))



# Histogramas
h<-hist(faturacao,main="Faturacao",xlab="Faturas em m€¬",ylab="Numero de faturas", col="skyblue",xlim=c(0,2500),ylim=c(0,10))
h
histograma<-hist(despesa,main="Despesa",xlab="Despesas em m€¬",ylab="Numero de Despesas", col="skyblue",xlim=c(0,160),ylim=c(0,8))
histograma
h_func<-hist(funcionarios,main="Funcionarios",xlab="nÂº de funcionarios",ylab="Numero de funcionarios", col="skyblue",xlim=c(0,200),ylim=c(0,10))



#Diagrama de extremos e quartis
boxplot(faturacao)

boxplot(faturacao ~ filial, main = "Comparacao fatura fililal", ylab="fatura em euros", xlab="", names=c("Sul","Norte"),col=c("pink","blue"))

boxplot(faturacao ~ avaliacao, main = "Comparacao faturaçao avaliaçao", ylab="fatura em euros", xlab="", names=c("Grande","Boa","Decente","Fraca"),col=c("Green","Blue","Orange","Red"))

boxplot(funcionarios ~ filial, main = "Comparacao funcionarios filial", ylab="numero de funcionarios", xlab="", names=c("Sul","Norte"),col=c("pink","blue"))

boxplot(funcionarios, main = "Comparacao funcionarios filial", ylab="numero de funcionarios", xlab="")


tapply(faturacao,filial,summary)# Para interpretar os valores do boxplot



#correlacao
cor(faturacao, despesa)


#**********************************************
#Modelos de Regressao
#***********************************************

model = lm(faturacao ~ despesa)

model
cor(faturacao, despesa)
summary(model)
cor(faturacao, despesa)^2
abline(model, col="green")
plot (despesa, faturacao)

model2 = lm(faturacao ~funcionarios)
cor(faturacao, funcionarios)
cor(faturacao, funcionarios)^2
model2
summary(model2)
abline(model2, col="red")
plot (funcionarios, faturacao)

model = lm(faturacao ~funcionarios)
cor(faturacao, funcionarios)
cor(faturacao, funcionarios)^2
model2
summary(model2)
abline(model2, col="red")
plot (funcionarios, faturacao)


# TESTES DE Media

t.test(faturacao,
       alternative="two.sided",
       mu=1000,
       conf.level = 0.90)

t.test(despesa,
       alternative="two.sided",
       mu=75,
       conf.level = 0.90)

t.test(funcionarios,
       alternative="two.sided",
       mu=75,
       conf.level = 0.95)


# TESTES DE GRUPO FILIAL


t.test(faturacao~filial,
       alternative="two.sided",
       conf.level = 0.95)

t.test(despesa~filial,
       alternative="two.sided",
       conf.level = 0.95)

t.test(funcionarios~filial,
       alternative="two.sided",
       conf.level = 0.95)



# TESTES DE GRUPO PoupanÃ§a

anova(lm(faturacao~avaliacao)) 


predict(model, antiguidade = 7) # 144.09303

#Distribuicao e Probabilidade de uma amostra ser realistica comparada com a populacao.
dbinom(9,20,0.21)

#teste de normalidade


shapiro.test(faturacao)
shapiro.test(despesa)
shapiro.test(funcionarios)


shapiro.test(model$residuals)

t.test(model$residuals,mu=0 ,alternative="two.sided", conf.level = 0.95)


# Contas utilizados na previsao de um valor pelo modelo de regressao linear

#lucro = 11.041 + (-0.234 * 17) = 7.063 


#faturacaao =  108.6 + (14.153 * 87.8)

#3000 = 108.6 + (14.153 * x)
#3000 = 108.6 + 14.153x
#3000 - 108.6 = 14.153x
#2891.4 = 14.153x
#2891.4/14.153 = x
#x= 204.295908995


#950 = 108.6 + (14.153 * y)
#841.4 = 14.153y
#y = 841.4/14.153
#y = 59.4502932241

#faturaÃ§ao =  108.6 + (14.153 * 120)


# Grafico naos usados
plot(model$residuals,xlab="Faturação",ylab="Residuos")
abline(model, col="green")
















