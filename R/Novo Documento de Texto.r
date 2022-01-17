#Slides: Exerc�cio 1 - Horas de sono vs rendimento escolar

  #rendimento escolar (depend) vs horas de sono (ind)
    x<- c(7.5,8.5,8.0,6.5,4.3,8.0,7.3,4.2,5.2,4.9)
    y<- c(14.1,14.4,15.2,13.5,10,16.3,14,11.6,12.3,11.2)
   
    
     # Representa��o gr�fica da reta de regress�o
    plot(x,y, main="Diagrama de dispers�o", xlab="horas de sono",
         ylab="rendimento escolar", col="blue")
    
  
  # reta de regress�o (estudo analitico)
    lm(y~x)
    rl<-lm(y~x)
    rl
        #(Intercept) =a            b (declive)x  
        #6.233                         1.091 

        # rendimento escolar Y = a + b*x = 6.233 + 1.091 *horas de sono
  
  #Previs�o  
    #predict(rl)
    abline(rl,col="red")

        # Com uma m�dia de seis horas de sono o rendimento estimado ser� de 12.8 valores
             6.233 + 1.091 * 6 #= 12.779
             6.233 + 1.091 * 14 #= 21.507, valor fora do intervalo!!!
        # Com uma m�dia de 13 valores, quantas horas deve dormir, em m�dia?
             #x = (y-6.233)/1.091
             (13-6.233)/1.091 #= 6.202566
  
  # Coeficientes de correla��o e de determina��o
    cor(x,y) # r= 0.9237318 o modelo adapta-se bem � nuvem de pontos e traduz uma rela��o forte e positiva
    cor(x,y)^2 #r^2 =0.8532804 isto �, 85.32% da variancia total do rendimento escolar � explicada pelo descanso m�dio noturno

    


    
#Slides: Exerc�cio 2 - consumo vs temperatura
    temp=c(15,14,12,14, 12, 11, 11,10,12,13)
    cons=c(4.3, 4.4, 5.3, 4.6, 5.5, 5.9, 5.7, 6.2, 5.2, 5.0)
   
  # reta de regress�o (estudo analitico)
    reta<-lm(cons~temp) # modelo de regress�o linear simples (reta)
    reta
    
    #Call:
    #  lm(formula = cons ~ temp)
    #Coefficients:
    #  (Intercept)         temp  
    #  10.1589      -0.3991
    
    # cons = a(intersept) + b*temp = 10.1589 - 0.3991 *temperatura
    
    
    
  #Previs�o
    predict(reta)# estimativas do modelo 
    
    # cons = 10.1589 - 0.3991 *temperatura
    10.1589 - 0.3991 *13.5 #4.77105 kwh
    10.1589 - 0.3991 * 42 #-6.6033 kwh
    10.1589 - 0.3991 * 16#= 3.7733 kwh se a temperatura for de 16 graus espera-se um consumo de 3.77
    
  
  #Representa��o gr�fica
    plot(temp,cons)
    abline(reta, col="red") 
    
  # coeficiente de correla��o e coeficiente de determina��o, para a qualidade do ajustamento
    cor(temp,cons) # r= -0.9834656 o modelo adapta-se bem � nuvem de pontos e traduz uma rela��o forte e negativa
    
    cor(temp,cons)^2  # r^2 =0.9672046 cerca de 97% da vari�ncia total do consumo � justificada pelo consumo
       


#Exerc�cio 3 - Ordenado inicial vs ordenado actual
    
    ord_inic =c(807.67, 1038.88, 2291.10, 1249.36, 1290.50, 2400.55, 1522.64,
                314.46,  719.20,  888.04, 2056.86, 1451.87, 1480.54, 1277.48,
                810.91, 2450.84, 1548.50, 1690.95,  869.05)
    ord_atual =c(1510.52, 1600.38, 2364.85, 1762.97, 1514.50, 2701.00, 2106.14,
                 650.63,  999.86, 1213.30, 2673.50, 1916.54, 1953.57, 1699.73,
                 1116.46, 3166.24, 2038.18, 2215.38, 1187.93)
    genero=c(0,1,1,1,1,1,1,0,0,0,1,1,1,0,0,1,0,1,0)#codifica��o

  
###### Estat�stica Descritiva   
  #criar tabela  
    tabela=cbind(ord_inic, ord_atual, genero)
    tabela
    summary(tabela)
  
  #Tabelas
    f_g<-table(genero) 
    f_g
    fr_g<-prop.table(f_g)
    fr_g 
    
  #Representa��es Gr�ficas
    
    nomes_c<-c("feminino","masculino")
    cores<-c("pink","skyblue")
    rotulo<-paste(nomes_c,"(",paste(f_g),")",sep=" ")
    pie(f_g, main="N�mero de Indiv�duos por g�nero",labels=rotulo,col=cores)
    
    #Gr�fico Barras (N�o serve para var. cont�nuas!!!!)
    barplot(fr_g,main="N�mero de indiv�duos por g�nero",xlab="g�nero",ylab="n� de indiv�duos",col="skyblue", ylim=c(0,1))
    
   
        f_o_i<-table(ord_inic) #var continua
        f_o_i
        
    # N�o utilizar barras para var cont�nuas
        barplot(f_o_i,main="N�mero de indiv�duos por ordenado",xlab="ordenado",ylab="n� de indiv�duos",col="skyblue", ylim=c(0,20))
    
    #Histograma
    h<-hist(ord_inic,main="Distribui��o dos ind por ordenado inicial",xlab="ordenado inicial",ylab="N�mero de indiv�duos", col="skyblue",xlim=c(0,2500),ylim=c(0,6))
    h
    
    
  # Compara��oBoxplot m�ltiplo (y num�rica ~ x nominal ou ordinal)
    
    boxplot(ord_atual ~ genero, main = "Compara��o do ordenado atual por genero", ylab="ordenado em euros", xlab="", names=c("Feminino","Masculino"),col=c("pink","blue"))
    IQR(ord_atual)
    tapply(ord_atual,genero,summary)# Para interpretar os valores do boxplot
    
###### Estat�stica INDUTIVA    
  # PREVIS�O: Regress�o linear entre ord inicial e atual
    plot(ord_inic, ord_atual, pch = 1, cex = 1.3, col = "blue", main = "actual vs inicial", xlab = "inicial", ylab = "atual")
    model = lm(ord_atual~ ord_inic)
    model #ord_atual=384.978+1.035*ord_inicial
    abline(model, col="red")
    #summary(model) # sum�rio com as estimativas dos coeficientes, p-value e r-quadrado
    
    #predict(model) #previs�o - valores estimados pela reta para cada valor de x dado
    # coeficiente de correla��o e coeficiente de determina��o, para a qualidade do ajustamento
    cor(ord_inic,ord_atual) # r= 0.9658736 
    cor(ord_inic,ord_atual)^2  # r^2 =0.9329119 
    
    384.978+1.035*2000 # Estima-se que um ind que tivesse ord_inic de 2000 receba atualmente 2454.978
    384.978+1.035*3000 # Estima-se que um ind que tivesse ord_inic de 3000 receba atualmente 3489.978
    (2000-384.978)/1.035# 1560.408 � o ord_inicial estimado para um indiv�duo que reveba atualmente 2000???
  
    
#Exerc�cio 4 - Peso Vs Altura
    Altura=c(175,145,195,162,155,167,188,171,168,177)
    Peso=c(83,35,92,59, 76, 74, 85, 75, 60, 94)
    genero=c(0,1,1,1,1,0,0,0,1,0)
    
    
    ###### Estat�stica Descritiva   
    #criar tabela  
    tabela=cbind(Altura, Peso, genero)
    tabela
    
    #Tabelas
    f_g<-table(genero) 
    f_g
    fr_g<-prop.table(f_g)
    fr_g 
    
    #Representa��es Gr�ficas
    
    nomes_c<-c("masculino", "feminino")
    cores<-c("skyblue","pink")
    rotulo<-paste(nomes_c,"(",paste(f_g),")",sep=" ")
    pie(f_g, main="N�mero de Indiv�duos por g�nero",labels=rotulo,col=cores)
    
    #Gr�fico Barras (N�o serve para var. cont�nuas!!!!)
    barplot(fr_g,main="N�mero de indiv�duos por g�nero",xlab="g�nero",ylab="n� de indiv�duos",col="skyblue", ylim=c(0,1))
    
    
    
       
    #Histograma
    h<-hist(Altura,main="Distribui��o dos ind por ordenado inicial",xlab="altura em cm",ylab="N�mero de indiv�duos", col="skyblue",xlim=c(140,200),ylim=c(0,5))
    h
    
    
    # Compara��oBoxplot m�ltiplo (y num�rica ~ x nominal ou ordinal)
    
    boxplot(Altura ~ genero, main = "Compara��o do ordenado atual por genero", ylab="ordenado em euros", xlab="", names=c("Masculino","Feminino"),col=c("blue","pink"))
    IQR(Altura)
    tapply(Altura,genero,summary)# Para interpretar os valores do boxplot
    
    ###### Estat�stica INDUTIVA    
    # PREVIS�O: Regress�o linear entre ord inicial e atual
    model = lm(Peso ~ Altura)
    model 
    #Peso= -95+0.9892*altura
    summary(model)
    plot(Altura,Peso)
    abline(model, col="red")
    # ou 
    abline(-95, 0.9892, col="blue")
    plot(altura, Peso, pch = 16, cex = 1.3, col = "blue", main = "Peso em fun��o da Altura", xlab = "Altura (cm)", ylab = "Peso (kg)")
    
    
    
    # coeficiente de correla��o e coeficiente de determina��o, para a qualidade do ajustamento
    cor(Altura,Peso) # r= -0.9834656 
    cor(Altura,Peso)^2  # r^2 =0.9672046 
    