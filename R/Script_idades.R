
###########################
# Exemplo Aula 25/10/2021 #
###########################

idades = c(14,15,18,18,18,19,21,16,16,16,16,20,21,6,24,25,25,20) # criar vetor/variavel 

notas = c(11,6,12,14,14,15,16,14,12,10,12,14,15,16,16,12,1,20) # criar vetor/variavel 

#criar uma base de dados 18 pessoas e 2 variaveis 18x2

dados = cbind(idades,notas)

dados = as.data.frame(dados)

View(dados) # permite visualizar a base de dados

# calcular media das idades 

mean(dados$idades) # media das idades é de 18,22 anos
mean(dados$notas) #media das notas é de 12,77

median(dados$idades) # 50% dos indivíduos da amostra tem 18 ou mais anos

# sumário da base de dados

summary(dados)

boxplot(dados$idades, main="Boxplot das idades", ylab="idades", col="light blue")

hist(dados$idades, main="Histograma das idades", xlab="idades", ylab="Número de alunos" , col="light green")

