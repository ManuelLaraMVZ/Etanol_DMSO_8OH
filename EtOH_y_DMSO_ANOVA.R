# datos: https://raw.githubusercontent.com/ManuelLaraMVZ/Etanol_DMSO_8OH/main/Crudos_etanol.csv

library("pacman")
p_load("vroom", #llama datos
       "ggplot2",#gráfica
       "dplyr", #magriter
       "agricolae", #para poder agrupar el Tukey
       "ggpubr") #gráficas simplificadas


###########################
#datos

crudos8OH <- vroom(file ="https://raw.githubusercontent.com/ManuelLaraMVZ/Etanol_DMSO_8OH/main/Crudos_etanol.csv")
head(crudos8OH)

names(crudos8OH)

crudos2 <- crudos8OH
crudos2$Vehiculo<- factor(crudos2$Vehiculo, 
                       levels = c("DMSO", "EtOH"))
crudos2$Tratamiento <- factor(crudos2$Tratamiento, 
                               levels = c("Control","Vehicle", "62.5 µM", "125.0 µM"))
head(crudos2)



#obtenemos el resumen de la media y e.e.m.

#sacamos los datos reales

crudos3 <- crudos2 %>% 
  select(Vehiculo, Tratamiento, No_cel)

crudos3

resumen_azul <- crudos3 %>% 
  group_by(Vehiculo, Tratamiento) %>% #decimos cuales son nuestros factores
  get_summary_stats(type = "mean_sd") %>% #obtenemos la tabla
  select(Vehiculo, Tratamiento, n, mean, sd)
resumen_azul


#guardamos para la gráfica

write.csv(x=resumen_azul,
          file = "Resumen_para_gráfica_EtOH_DMSO.csv",
          row.names = F)

#####################################

#####################################
#Prueba ANOVA todos los datos

#Prueba ANOVA todos los datos

#visualizamos la distribución
caja1 <- crudos3 %>% 
  ggboxplot(x="Tratamiento",
            y="No_cel",
            color="Vehiculo",
            palette = "jco")
caja1

#supuestos

supuesto1 <- crudos3 %>% 
  group_by(Vehiculo, Tratamiento) %>% 
  identify_outliers(No_cel)  #buscamos outliers

supuesto1

modelo <- lm(No_cel~Vehiculo*Tratamiento, data = crudos3)
ggqqplot(residuals(modelo))

#prueba de normalidad de Shapiro

shapiro.test(residuals(modelo))

#No pasa la prueba de normalidad con p<0.05

######################################################
#Se decide hacer análisis de los tiempos por separados

#primero hay que extraer los datos por separados 

######################################################

######################################################
# para DMSO

crudoDMSO <- crudos3 %>% 
  filter(Vehiculo=="DMSO") %>% 
  select(Tratamiento,No_cel)

crudoDMSO

resumenDMSO <- crudoDMSO %>% 
  group_by(Tratamiento) %>% #decimos cuales son nuestros factores
  get_summary_stats(type = "mean_sd")#obtenemos la tabla
resumenDMSO

#gráfica
graficaDMSO <- resumenDMSO %>% 
  ggplot(mapping = aes(x=Tratamiento,
                       y=mean))+
  geom_col()

graficaDMSO

supuestoDMSO <- crudoDMSO %>% 
  group_by(Tratamiento) %>% 
  identify_outliers(No_cel)  #buscamos outliers

supuestoDMSO

modelo <- lm(No_cel~Tratamiento, data = crudoDMSO)

ggqqplot(residuals(modelo)) #graficamos para saber si se ajusta a un modelo normal

#realizamos prueba de normalidad si p>0.05, entonces la pasa

shapiro.test(crudoDMSO$No_cel)
ks.test(crudoDMSO$No_cel, pnorm)

# p=0.1269 (similar a SigmaPlot) Pasó la prueba de normalidad para Shapiro
# prueva de igualdad de varianzas
#Levene's Test for Homogeneity of Variance


levene_test(No_cel~Tratamiento, data = crudoDMSO)
fligner.test(No_cel~Tratamiento, data = crudoDMSO)
bartlett.test(No_cel~Tratamiento, data = crudoDMSO)

#Pasa prueba de normalidad para levene y fligner

#Podemos proceer con el ANOVa

#################################################
#ANOVA

ANOVADMSO <- aov( No_cel~Tratamiento,data = crudoDMSO) #Ponemos de donde se sacan los datos y luego la relación

#Visualizamos
ANOVADMSO
#obtenemos la tabla e anova
summary.aov(ANOVADMSO)

#Existe diferencia entre los Tratamiento

#Prueba estadística
posthocDMSO <- (ANOVADMSO)

posthocDMSO

#obtenemos las similitudes

agrupadosDMSO <- HSD.test(ANOVADMSO,"Tratamiento", group=T, console=T,
                          main = "células con tratamiento por DMSO con 8-OH")

agrupadosDMSO

#Hay que tomar los datos y copiarlos en un txt para poder realizar el análisis

similitud <- c("a","b","c","b") #se construye con los resultados de las interacciones

resumenDMSO_ANOVA <- resumenDMSO %>% 
  select(Tratamiento, n, mean, sd) %>% 
  mutate(similitud)

resumenDMSO_ANOVA

write.csv(resumenDMSO_ANOVA,
          file = "ANOVA_DMSO.csv",
          row.names = F)

################################################################################
################################################################################
# para EtOH

crudoEtOH <- crudos2 %>% 
  filter(Vehiculo=="EtOH") %>% 
  select(Tratamiento,No_cel)

crudoEtOH

resumenEtOH <- crudoEtOH %>% 
  group_by(Tratamiento) %>% #decimos cuales son nuestros factores
  get_summary_stats(type = "mean_sd")#obtenemos la tabla
resumenEtOH

#gráfica
graficaEtOH <- resumenEtOH %>% 
  ggplot(mapping = aes(x=Tratamiento,
                       y=mean))+
  geom_point()

graficaEtOH

supuestoEtOH <- crudoEtOH %>% 
  group_by(Tratamiento) %>% 
  identify_outliers(No_cel)  #buscamos outliers

supuestoEtOH

modelo <- lm(No_cel~Tratamiento, data = crudoEtOH)

ggqqplot(residuals(modelo)) #graficamos para saber si se ajusta a un modelo normal

#realizamos prueba de normalidad si p>0.05, entonces la pasa

shapiro.test(crudoEtOH$No_cel)
ks.test(crudoEtOH$No_cel, pnorm)

# p=0.1269 (similar a SigmaPlot) Pasó la prueba de normalidad para Shapiro
# prueva de igualdad de varianzas
#Levene's Test for Homogeneity of Variance


levene_test(No_cel~Tratamiento, data = crudoEtOH)
fligner.test(No_cel~Tratamiento, data = crudoEtOH)
bartlett.test(No_cel~Tratamiento, data = crudoEtOH)

#Pasa prueba de normalidad para levene y fligner

#Podemos proceer con el ANOVa

#################################################
#ANOVA

ANOVAEtOH <- aov( No_cel~Tratamiento,data = crudoEtOH) #Ponemos de donde se sacan los datos y luego la relación

#Visualizamos
ANOVAEtOH
#obtenemos la tabla e anova
summary.aov(ANOVAEtOH)

#Existe diferencia entre los Tratamiento

#Prueba estadística
posthocEtOH <- TukeyHSD(ANOVAEtOH)

posthocEtOH

#obtenemos las similitudes

agrupadosEtOH <- HSD.test(ANOVAEtOH,"Tratamiento", group=T, console=T,
                          main = "células con tratamiento por EtOH con 8-OH")

agrupadosEtOH

#Hay que tomar los datos y copiarlos en un txt para poder realizar el análisis

similitud <- c("a","b","c","d") #se construye con los resultados de las interacciones

resumenEtOH_ANOVA <- resumenEtOH %>% 
  select(Tratamiento, n, mean, sd) %>% 
  mutate(similitud)

resumenEtOH_ANOVA

write.csv(resumenEtOH_ANOVA,
          file = "ANOVA_EtOH.csv",
          row.names = F)
################################################################################
