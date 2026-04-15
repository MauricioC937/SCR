############################ Trabajo Grupal###############################################

library(readxl)
library(lifecontingencies)
library(dplyr)
library(lubridate)
library(writexl)  # Para guardar en Excel sin perder formato
library(tidyverse)
library(ggplot2)

options(scipen=999)

probs <- unname(unlist(read_excel("C:/Users/User/Documents/Matemática Actuarial/Probabilidades_Ecuador_2023_2060.xlsx",sheet=1)[,c(4)]))
probsM <- unname(unlist(read_excel("C:/Users/User/Documents/Matemática Actuarial/Probabilidades_Ecuador_2023_2060.xlsx",sheet=2)[,c(4)]))

# Tablas de mortalidad
TH <- probs2lifetable(probs,radix=100000, type="qx",name="Ecuador")
TM <- probs2lifetable(probsM,radix=100000, type="qx",name="Ecuador")

# Datos
polizas <- read_excel("C:/Users/User/Documents/Matemática Actuarial/Datos_SCR.xlsx")

# Tipo de interes
ti <- 0.05

edad <- as.numeric(floor(interval(polizas$'FECHA_NACIMIENTO',Sys.Date())/years(1)))
polizas$EDAD <- edad
sexo <- polizas$'SEXO'

# Crear lista vacía para las tablas de vida individuales
tablas <- vector("list", length(edad))

# Llenar la lista según el sexo
for (h in 1:length(tablas)) {
  tablas[[h]] <- if (sexo[h] == "H") TH else TM
}

n <- ifelse(is.na(polizas$DURACION), 100 - edad, as.numeric(polizas$DURACION))
ind <- as.numeric(polizas$'INDEMNIZACION_INICIAL')
producto <- polizas$'PRODUCTO'
tipo_prima <- polizas$'PRIMA'
n_flujos <- as.numeric(polizas$'FLUJOS_ANUALES')
alpha <- polizas$'TASA_DIFERENCIA_ANUAL'

# Determinación de la Prima Pura
VA <- numeric(length(edad))

# Seguros de vida vitalicios (solo hay geométrico)
for (j in 1:length(edad)){
  if(producto[j]=='SEGURO VIDA ENTERA'){
    VA[j] <- sum(sapply(0:(n[j]-1),function(k){ind[j]*(1+alpha[j])^(k)*
        Axn(tablas[[j]],x=edad[j],n=1,i=ti,m=k)}))
  }
}

# Seguros de vida temporal (solo hay aritmetica)
for (j in 1:length(edad)){
  if(producto[j]=='SEGURO DE VIDA TEMPORAL'){
    VA[j] <- sum(sapply(0:(n[j]-1),function(k){(ind[j]+k*alpha[j])*
        Axn(tablas[[j]],x=edad[j],n=1,i=ti,m=k)}))
  }
}

# Rentas Vitalicias (solo hay geometrica)
i_ <- ifelse(producto =='RENTA VITALICIA MENSUAL', {(ti-alpha)/(1+alpha)}, 0)

for (j in 1:length(edad)){
  if(producto[j]=='RENTA VITALICIA MENSUAL'){
    VA[j] <- ind[j]*axn(tablas[[j]],x=edad[j],n=n[j],i=i_[j],payment='immediate')/(1+alpha)
  }
}

# Fraccionamiento de las primas
Prima_Fraccionada <- numeric(length(edad))
for (l in 1:length(Prima_Fraccionada)){
  if (tipo_prima[l]=="FRACCIONADA"){
    Prima_Fraccionada[l] <- VA[l]/(axn(tablas[[l]],x=edad[l],n=n_flujos[l],i=ti,payment="due"))
  }
  else{
    Prima_Fraccionada[l] <- VA[l]
  } 
}

# Agregar al dataframe y guardar en Excel
polizas$VA <- VA
polizas$PRIMA_FRACCIONADA <- Prima_Fraccionada

############################################################################################################################################
# Cálculo del SCR de mortalidad (Seguro de vida entera, seguro de vida temporal)
# Shock de mortalidad: incremento del 15% en la tasa de mortalidad,
# Tablas recargadas
probs1 <- 1.15*probs
probs1 [probs1>1] <- 1

probsM1 <- 1.15*probsM
probsM1[probsM1>1] <- 1

TH1 <- probs2lifetable(probs1,radix=100000, type="qx",name="Ecuador")
TM1 <- probs2lifetable(probsM1,radix=100000, type="qx",name="Ecuador")

# tablas para el shock de mortalidad.
tabla_mort <- vector("list", length(edad))

# Llenar la lista según el sexo
for (h in 1:length(tablas)) {
  tabla_mort[[h]] <- if (sexo[h] == "H") TH1 else TM1
}

# Prima Pura para el shock de mortalidad
VA_mort <- numeric(length(edad))

# Seguros de vida vitalicios (solo hay geométrico)
for (j in 1:length(edad)){
  if(producto[j]=='SEGURO VIDA ENTERA'){
    VA_mort[j] <- sum(sapply(0:(n[j]-1),function(k){ind[j]*(1+alpha[j])^(k)*
        Axn(tabla_mort[[j]],x=edad[j],n=1,i=ti,m=k)}))
  }
}

# Seguros de vida temporal (solo hay aritmetica)
for (j in 1:length(edad)){
  if(producto[j]=='SEGURO DE VIDA TEMPORAL'){
    VA_mort[j] <- sum(sapply(0:(n[j]-1),function(k){(ind[j]+k*alpha[j])*
        Axn(tabla_mort[[j]],x=edad[j],n=1,i=ti,m=k)}))
  }
}

# Rentas Vitalicias (solo hay geometrica)
i_ <- ifelse(producto =='RENTA VITALICIA MENSUAL', {(ti-alpha)/(1+alpha)}, 0)

for (j in 1:length(edad)){
  if(producto[j]=='RENTA VITALICIA MENSUAL'){
    VA_mort[j] <- ind[j]*axn(tablas[[j]],x=edad[j],n=n[j],i=i_[j],payment='immediate')/(1+alpha)
  }
}

# Fraccionamiento de las primas del shock de mortalidad
Prima_Frac_mort <- numeric(length(edad))
for (l in 1:length(Prima_Frac_mort)) {
  if (tipo_prima[l] == 'FRACCIONADA') {
    if (producto[l] == 'RENTA VITALICIA MENSUAL') {
      # Caso especial para rentas
      Prima_Frac_mort[l] <- VA_mort[l] / axn(tablas[[l]], x = edad[l], n = n_flujos[l], i = ti, payment = "due")
    } else {
      # Caso general de fraccionamiento (usando tabla_mort)
      Prima_Frac_mort[l] <- VA_mort[l] / axn(tabla_mort[[l]], x = edad[l], n = n_flujos[l], i = ti, payment = "due")
    }
  } else {
    Prima_Frac_mort[l] <- VA_mort[l]
  }
}

# Agregar al dataframe y guardar en Excel
polizas$VA_mort <- VA_mort
polizas$PRIMA_FRAC_mort <- Prima_Frac_mort
polizas$SCR_mort <- VA_mort-VA

#SCR mortalidad
SCR_mort <- sum(VA_mort)-sum(VA)
SCR_mort

############################################################################################################################################
# Cálculo del SCR de longevidad (Rentas vitalicias)
# Shock de longevidad: decremento del 20% en la tasa de mortalidad,
# Tablas recargadas
probs2 <- 0.8*probs
probs2[length(probs2)] <- 1

probsM2 <- 0.80*probsM
probsM2[length(probsM2)] <- 1

TH2 <- probs2lifetable(probs2,radix=100000, type="qx",name="Ecuador")
TM2 <- probs2lifetable(probsM2,radix=100000, type="qx",name="Ecuador")


# tablas para el shock de mortalidad.
tabla_long <- vector("list", length(edad))

# Llenar la lista según el sexo
for (h in 1:length(tablas)) {
  tabla_long[[h]] <- if (sexo[h] == "H") TH2 else TM2
}

# Primas para el shock de longevidad
VA_long <- numeric(length(edad))

# Seguros de vida vitalicios (solo hay geometrico)
for (j in 1:length(edad)){
  if(producto[j]=='SEGURO VIDA ENTERA'){
    VA_long[j] <- sum(sapply(0:(n[j]-1),function(k){ind[j]*(1+alpha[j])^(k)*
        Axn(tablas[[j]],x=edad[j],n=1,i=ti,m=k)}))
  }
}

# Seguros de vida temporal (solo hay aritmetica)
for (j in 1:length(edad)){
  if(producto[j]=='SEGURO DE VIDA TEMPORAL'){
    VA_long[j] <- sum(sapply(0:(n[j]-1),function(k){(ind[j]+k*alpha[j])*
        Axn(tablas[[j]],x=edad[j],n=1,i=ti,m=k)}))
  }
}

# Rentas Vitalicias (solo hay geometrica)
i_ <- ifelse(producto =='RENTA VITALICIA MENSUAL', {(ti-alpha)/(1+alpha)}, 0)

for (j in 1:length(edad)){
  if(producto[j]=='RENTA VITALICIA MENSUAL'){
    VA_long[j] <- ind[j]*axn(tabla_long[[j]],x=edad[j],n=n[j],i=i_[j],payment='immediate')/(1+alpha)
  }
}

# Fraccionamiento de las primas del shock de longevidad
Prima_Frac_long <- numeric(length(edad))
for (l in 1:length(Prima_Frac_long)) {
  if (tipo_prima[l] == "FRACCIONADA" && 
      (producto[l] == "SEGURO VIDA ENTERA" || producto[l] == "SEGURO DE VIDA TEMPORAL")) {
    # Fraccionada + seguro vida entera o temporal
    Prima_Frac_long[l] <- VA_long[l] / axn(tablas[[l]], x = edad[l], n = n_flujos[l], i = ti, payment = "due")
  } else if (tipo_prima[l] == "FRACCIONADA") {
    # Fraccionada renta vitlicia (usando tabla_long)
    Prima_Frac_long[l] <- VA_long[l] / axn(tabla_long[[l]], x = edad[l], n = n_flujos[l], i = ti, payment = "due")
  } else {
    Prima_Frac_long[l] <- VA_long[l]
  }
}

# Agregar al dataframe y guardar en Excel
polizas$VA_long <- VA_long
polizas$PRIMA_FRAC_long <- Prima_Frac_long
polizas$SCR_long <- VA_long-VA

#SCR longevidad
SCR_long <- sum(VA_long)-sum(VA)
SCR_long
############################################################################################################################################
# Cálculo del SCR del seguro de vida
SCR_vida <- sqrt(SCR_mort^2+SCR_long^2 -0.5*SCR_long*SCR_mort)
SCR_vida

############################################################################################################################################
# Cálculo del SCR de tipo de interés 
# Shock de baja o aumento de interés
#tipo de interés
i_shock<-ti*(0.85) # interés reducido
# Determinación de la Prima Pura para el tipo de interés
VA_int <- numeric(length(edad))

# Seguros de vida vitalicios (solo hay geométrico)
for (j in 1:length(edad)){
  if(producto[j]=='SEGURO VIDA ENTERA'){
    VA_int[j] <- sum(sapply(0:(n[j]-1),function(k){ind[j]*(1+alpha[j])^(k)*
        Axn(tablas[[j]],x=edad[j],n=1,i=i_shock,m=k)}))
  }
}

# Seguros de vida temporal (solo hay aritmetica)
for (j in 1:length(edad)){
  if(producto[j]=='SEGURO DE VIDA TEMPORAL'){
    VA_int[j] <- sum(sapply(0:(n[j]-1),function(k){(ind[j]+k*alpha[j])*
        Axn(tablas[[j]],x=edad[j],n=1,i=i_shock,m=k)}))
  }
}

# Rentas Vitalicias (solo hay geometrica)
i_int <- ifelse(producto =='RENTA VITALICIA MENSUAL', {(i_shock-alpha)/(1+alpha)}, 0)

for (j in 1:length(edad)){
  if(producto[j]=='RENTA VITALICIA MENSUAL'){
    VA_int[j] <- ind[j]*axn(tablas[[j]],x=edad[j],n=n[j],i=i_int[j],payment='immediate')/(1+alpha)
  }
}

# Fraccionamiento de las primas para el tipo de interés
Prima_Frac_int <- numeric(length(edad))
for (l in 1:length(Prima_Frac_int)){
  if (tipo_prima[l]=="FRACCIONADA"){
    Prima_Frac_int[l] <- VA_int[l]/(axn(tablas[[l]],x=edad[l],n=n_flujos[l],i=i_shock,payment="due"))
  }
  else{
    Prima_Frac_int[l] <- VA_int[l]
  } 
}
# Agregar al dataframe y guardar en Excel
polizas$VA_int <- VA_int
polizas$PRIMA_FRAC_INTERES <- Prima_Frac_int
polizas$SCR_int <- VA_int-VA
#########################################################################################
#SCR del tipo de interés de bajada
SCR_int <- max(sum(VA_int)-sum(VA),0)
SCR_int

#########################################################################################
# Shock de subida de interés
#tipo de interés
is_shock<-ti*(1.10) # interés aumentado
# Determinación de la Prima Pura para el tipo de interés de subida
VAs_int <- numeric(length(edad))

# Seguros de vida vitalicios (solo hay geométrico)
for (j in 1:length(edad)){
  if(producto[j]=='SEGURO VIDA ENTERA'){
    VAs_int[j] <- sum(sapply(0:(n[j]-1),function(k){ind[j]*(1+alpha[j])^(k)*
        Axn(tablas[[j]],x=edad[j],n=1,i=is_shock,m=k)}))
  }
}

# Seguros de vida temporal (solo hay aritmetica)
for (j in 1:length(edad)){
  if(producto[j]=='SEGURO DE VIDA TEMPORAL'){
    VAs_int[j] <- sum(sapply(0:(n[j]-1),function(k){(ind[j]+k*alpha[j])*
        Axn(tablas[[j]],x=edad[j],n=1,i=is_shock,m=k)}))
  }
}

# Rentas Vitalicias (solo hay geometrica)
is_int <- ifelse(producto =='RENTA VITALICIA MENSUAL', {(is_shock-alpha)/(1+alpha)}, 0)

for (j in 1:length(edad)){
  if(producto[j]=='RENTA VITALICIA MENSUAL'){
    VAs_int[j] <- ind[j]*axn(tablas[[j]],x=edad[j],n=n[j],i=is_int[j],payment='immediate')/(1+alpha)
  }
}

# Fraccionamiento de las primas para el tipo de interés
Prima_Frac_ints <- numeric(length(edad))
for (l in 1:length(Prima_Frac_int)){
  if (tipo_prima[l]=="FRACCIONADA"){
    Prima_Frac_ints[l] <- VAs_int[l]/(axn(tablas[[l]],x=edad[l],n=n_flujos[l],i=is_shock,payment="due"))
  }
  else{
    Prima_Frac_ints[l] <- VAs_int[l]
  } 
}
# Agregar al dataframe y guardar en Excel
polizas$VAs_int <- VAs_int
polizas$PRIMA_FRAC_INT_SUBIDA <- Prima_Frac_ints
polizas$SCR_ints <- VAs_int-VA
write_xlsx(polizas, "C:/Users/User/Documents/Matemática Actuarial/Datos_SCR_Pmli.xlsx")

#SCR del tipo de interés de subida
SCR_ints <- max(sum(VAs_int)-sum(VA),0)
SCR_ints
######################################################################################################
#El SCR del riesgo de mercado está dado por:
SCR_int
############################################################################################################################################
# Determinación del SCR del riesgo de vida y tipo de mercado(SCR_interés)
SCR <- sqrt(SCR_vida^2+SCR_int^2+0.5*SCR_int*SCR_vida) 
SCR

#####################################################################################################

# GRAFICAS
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Agrupar por producto y calcular promedio de cada VA
va_promedios <- polizas %>%
  group_by(PRODUCTO) %>%
  summarise(
    VA_base = mean(VA, na.rm = TRUE),
    VA_mort = mean(VA_mort, na.rm = TRUE),
    VA_long = mean(VA_long, na.rm = TRUE),
    VA_int  = mean(VA_int, na.rm = TRUE)
  )

# Convertir a formato largo para ggplot
va_largo <- va_promedios %>%
  pivot_longer(
    cols = starts_with("VA_"),
    names_to = "Tipo",
    values_to = "VA"
  )

# Renombrar tipos para que se vean mejor en la leyenda
va_largo$Tipo <- recode(va_largo$Tipo,
                        "VA_base" = "VA Base",
                        "VA_mort" = "Shock Mortalidad",
                        "VA_long" = "Shock Longevidad",
                        "VA_int"  = "Shock Interés")

# Gráfico
ggplot(va_largo, aes(x = PRODUCTO, y = VA, fill = Tipo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparación de VA promedio por producto y riesgo",
       y = "Valor Actual Promedio",
       x = "Producto",
       fill = "Tipo de Cálculo") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Calcular componentes del SCR por producto
scr_comp <- polizas %>%
  group_by(PRODUCTO) %>%
  summarise(
    SCR_mort = mean(VA_mort - VA, na.rm = TRUE),
    SCR_long = mean(VA_long - VA, na.rm = TRUE),
    SCR_int  = mean(VA_int - VA, na.rm = TRUE)
  )

# Formato largo
scr_largo <- scr_comp %>%
  pivot_longer(cols = starts_with("SCR"), names_to = "Riesgo", values_to = "SCR") %>%
  mutate(Riesgo = recode(Riesgo,
                         "SCR_mort" = "Mortalidad",
                         "SCR_long" = "Longevidad",
                         "SCR_int" = "Tasa de Interés"))

# Gráfico apilado
ggplot(scr_largo, aes(x = PRODUCTO, y = SCR, fill = Riesgo)) +
  geom_bar(stat = "identity") +
  labs(title = "Descomposición del SCR por producto y riesgo",
       y = "SCR (diferencia de VA)",
       x = "Producto",
       fill = "Riesgo") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ggplot(polizas, aes(x = EDAD, y = VA, color = "Base")) +
  geom_line() +
  geom_line(aes(y = VA_mort, color = "Mortalidad")) +
  geom_line(aes(y = VA_long, color = "Longevidad")) +
  geom_line(aes(y = VA_int, color = "Interés")) +
  labs(title = "Valor Actual por edad bajo distintos escenarios de riesgo",
       x = "Edad",
       y = "Valor Actual") +
  scale_color_manual(values = c("Base" = "black", "Mortalidad" = "red", "Longevidad" = "blue", "Interés" = "green")) +
  theme_minimal()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Distribución Prima Pura base y shock mortalidad por producto
polizas %>%
  filter(PRODUCTO == "SEGURO VIDA ENTERA") %>%
  pivot_longer(cols = c(VA, VA_mort),
               names_to = "Escenario",
               values_to = "Prima") %>%
  mutate(Escenario = recode(Escenario,
                            "VA" = "Base",
                            "VA_mort" = "Shock Mortalidad")) %>%
  ggplot(aes(x = Prima, fill = Escenario)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribución de Prima Pura: SEGURO VIDA ENTERA",
       x = "Prima Pura",
       y = "Densidad",
       fill = "Escenario") +
  theme_minimal()

####################################################################

polizas %>%
  filter(PRODUCTO == "SEGURO DE VIDA TEMPORAL") %>%
  pivot_longer(cols = c(VA, VA_mort),
               names_to = "Escenario",
               values_to = "Prima") %>%
  mutate(Escenario = recode(Escenario,
                            "VA" = "Base",
                            "VA_mort" = "Shock Mortalidad")) %>%
  ggplot(aes(x = Prima, fill = Escenario)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribución de Prima Pura: SEGURO DE VIDA TEMPORAL",
       x = "Prima Pura",
       y = "Densidad",
       fill = "Escenario") +
  theme_minimal()


##########################################################################

polizas %>%
  filter(PRODUCTO == "RENTA VITALICIA MENSUAL") %>%
  pivot_longer(cols = c(VA, VA_long),
               names_to = "Escenario",
               values_to = "Prima") %>%
  mutate(Escenario = recode(Escenario,
                            "VA" = "Base",
                            "VA_long" = "Shock Longevidad")) %>%
  ggplot(aes(x = Prima, fill = Escenario)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribución de Prima Pura: RENTA VITALICIA MENSUAL",
       x = "Prima Pura",
       y = "Densidad",
       fill = "Escenario") +
  theme_minimal()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

polizas %>%
  mutate(SCR_total = SCR_mort + SCR_long + SCR_int) %>%
  group_by(PRODUCTO) %>%
  summarise(
    Prima_prom = mean(VA, na.rm = TRUE),
    SCR_prom = mean(SCR_total, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(Prima_prom, SCR_prom),
               names_to = "Variable",
               values_to = "Valor") %>%
  mutate(Variable = recode(Variable,
                           Prima_prom = "Prima Pura",
                           SCR_prom = "SCR Total")) %>%
  ggplot(aes(x = PRODUCTO, y = Valor, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparación Prima Pura vs SCR Total por Producto",
       x = "Producto",
       y = "Valor promedio",
       fill = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

