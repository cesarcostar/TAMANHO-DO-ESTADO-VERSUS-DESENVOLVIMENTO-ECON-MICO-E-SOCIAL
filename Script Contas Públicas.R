library(readxl)
Gasto_Primario <- read_excel("Gasto Primario.xlsx", sheet = "Health Systems")
View(Gasto_Primario)


Gasto_Primario["gasto_adj"]<-Gasto_Primario$Gasto/1000

reg_pib<-lm(PIB~gasto_adj, data=Gasto_Primario)

summary(reg_pib)

grafico<-plot (PIB~gasto_adj, data=Gasto_Primario,
               ylim = c(-0, 70000), 
               main =    "PIB per capita (PPP) versus 
            Gasto Primario do Governo Geral (% do PIB)",
               ylab="PIB per capita, em US$ de 2017 PPP",
               xlab="Gasto Primário do governo geral, em % do PIB",
               pch=16,
               col = "blue")
grid()

abline(reg_pib, lwd=2)

text(PIB~gasto_adj, labels=Country,data=Gasto_Primario, cex=0.9, font=1)


cons<-data.frame(Gasto_Primario$PIB,Gasto_Primario$gasto_adj)

summary(cons)

cor(cons) # correlação entre as variáveis


## MODELO Semilog (LOG-LIN)

Gasto_Primario["gasto_adj"]<-Gasto_Primario$Gasto/1000

reg_logpib<-lm(log(PIB)~gasto_adj, data=Gasto_Primario)

summary(reg_logpib)

grafico<-plot (log(PIB)~gasto_adj, data=Gasto_Primario,
               ylim = c(5, 15), 
               main =    "Log PIB per capita (PPP) versus 
               Gasto Primario do Governo Geral (% do PIB)",
               ylab="Log PIB per capita, em US$ de 2017 PPP",
               xlab="Gasto Primário do governo geral, em % do PIB",
               pch=15,
               col = "red")
grid()

abline(reg_logpib, lwd=2)


text(log(PIB)~gasto_adj, labels=Country, data=Gasto_Primario, cex=0.9, font=1)


## Paises Americanos 

library(readxl)

Gasto_Primario <- read_excel("Gasto Primario.xlsx", sheet ="Health Systems America")
View(Gasto_Primario)

Gasto_Primario["gasto_adj"]<-Gasto_Primario$Gasto/1000

reg_pib<-lm(PIB~gasto_adj, data=Gasto_Primario)

summary(reg_pib)

grafico<-plot (PIB~gasto_adj, data=Gasto_Primario,
               ylim = c(0, 50000), 
               main =   "PIB per capita (PPP) versus 
               Gasto Primario do Governo Geral (% do PIB)",
               ylab="PIB per capita, em US$ de 2017 PPP",
               xlab="Gasto Primário do governo geral, em % do PIB",
               pch=16,
               col = "red")
grid()


abline(reg_pib, lwd=2)

text(PIB~gasto_adj, labels=Country,data=Gasto_Primario, cex=0.9, font=1)


cons<-data.frame(Gasto_Primario$PIB,Gasto_Primario$gasto_adj)

summary(cons)

cor(cons) # correlação entre as variáveis

