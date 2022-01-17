
vetor = c(29.9, 40.2, 37.8, 19.7, 30.0, 29.7, 19.4, 39.2, 24.7, 20.4,
          19.1, 34.7, 33.5, 18.3, 19.4, 27.3, 38.2, 16.2, 36.8, 33.1,
          41.4, 13.6, 32.2, 24.3, 19.1, 37.4, 23.8, 33.3, 31.6, 20.1,
          17.2, 13.3, 37.7, 12.6, 39.6, 24.6, 18.6, 18.0, 33.7, 38.2)

# c)
# média
mean(vetor)

# mediana

median(vetor)

summary(vetor)

# moda

hist(vetor) # a classe modal é de [15,20[ cm

# d)

summary(vetor) # quartis

# 1º quartil 19.32 cm - 25% da nossa amostra mede 19,32 cm ou menos
# 2º quartil 28.50 cm - 50% da nossa amostra mede 28.50 cm ou menos
# 3º quartil 35.23 cm - 75% da nossa amostra mede 35.23 cm ou menos


# percentil 90

quantile(vetor, 0.9)

# interpretação -  90% da nossa amostra tem 38.3 cm ou menos



# ------------------------------------------------------- # 

###################################
### Exercicio 15 Ficha 2 ##########
###################################

comprimento = c(29.9,40.2,37.8,19.7,30.0,29.7,19.4, 39.2, 24.7, 20.4,
                19.1, 34.7, 33.5, 18.3, 19.4, 27.3, 38.2, 16.2, 36.8, 33.1,
                41.4, 13.6, 32.2, 24.3, 19.1, 37.4, 23.8, 33.3, 31.6, 20.1,
                17.2, 13.3, 37.7, 12.6, 39.6, 24.6, 18.6, 18.0, 33.7, 38.2)

#c) Calcule e interprete a média, a moda e a mediana.

summary(comprimento)

# A média e de 27,45 cm a mediana é de 28,50 cm

table(comprimento) #não se consegue entender o valor da moda
hist(comprimento) # A classe modal é de [15,20[ cm.

# d) Calcule e interprete os quartis e o percentil 90.

#Interpretação: a função summary já nos devolveu os quartis:

#Q1 = 19,32cm Interpretação: 25% da nossa amostra mede 19,32 cm ou menos
#Q2 = 28,50cm Interpretação:50% da nossa amostra mede 28,50cm ou menos
#Q3 = 35.23cm Interpretação:50% da nossa amostra mede 35.23 cm ou menos

#Função para determinal o percentil 90 = quantil 0,90

quantile(comprimento, 0.90)

#Interpretação: 90% da nossa amostra mede 38,3cm ou menos.

