install.packages("spatialreg")
install.packages("spdep")
install.packages("rgdal")
install.packages("lmtest")
# Instalar paquetes principales para econometría espacial y mapas
install.packages(c("sf", "spData", "rgdal", "raster", "sp"))
library(spdep) # Econometria espacial
#library(maptools) # leer archivos shapfiles y elaborar mapas library(rgdal)
library(lmtest) 
library(tseries) 
library(Hmisc)
library(sf)
library(spatialreg)
packageVersion("sf")
#datos<-readShapePoly("D:/PP/BASE 9.0.shp")
datos <- st_read("D:/PP/BASE 11.shp")  # Reemplaza con tu ruta real
# Leer el archivo shapefile
#datos <- readShapePoly("D:/PP/BASE 9.0.shp")
View(datos)
names(datos)

# Obtener las coordenadas de los polígonos
#coords <- coordinates(datos)
# Calcular los centroides de los polígonos en el shapefile
coords <- st_centroid(datos)  
# Extraer las coordenadas en una matriz
coords_matrix <- st_coordinates(coords)
# Crear una matriz de pesos de vecinos más cercanos (k=3)
col.knn <- knearneigh(coords, k=3)
nb1 <- knn2nb(col.knn, row.names = NULL)
knear3 <- nb2listw(nb1, glist=NULL, style="W", zero.policy=T)
View(knear3)
# Ver la estructura de los datos
View(datos)
names(datos)
summary(datos)

install.packages("dplyr")
library(dplyr)

datos <- datos %>%
  rename(
    
    X_TTRATA22 = ttrata22,
    X_TTRATA20 = ttrata20,
    X_TTRATA18 = ttrata18,
    X_TTRATA16 = ttrata16,
    
    X_Tinscrsal = tinscrsalu, 
    X_PIBPKPT22 = pibpkpt22,
    X_PIBPKPT20 = pibpkpt20,
    X_PIBPKPT18 = pibpkpt18,
    X_PIBPKPT16 = pibpkpt16,
    X_DENSD22 =  densd24,
    X_DENSD20 =  densd20,
    X_DENSD18 =  densd18,
    X_DENSD16 =  densd16,
    X_gstosal_3 = gstosalu_3,
    X_gstosal_2 = gstosalu_2,
    X_gstosal_1 = gstosalu_1,
    X_gstosalup = gstosalupp,
    X_AK.1.22 = ak122,
    X_AK.1.20 = ak120,
    X_AK.1.18 = ak118,
    X_AK.1.16 = ak116,
    X_gstoseg22 = gstoseg22,
    X_gstoseg20 = gstoseg20,
    X_gstoseg18 = gstoseg18,
    X_gstoseg16 = gstoseg16
    
  )


# Modelo estimado con OLS y poblacion inscrita al sistema de salud
## se espera una relación positiva como lo referencia el comportamiento en el tiempo
modelo_X_Tinscrsal <- lm(X_TTRATA22 ~ X_Tinscrsal, data=datos)
summary(modelo_X_Tinscrsal)

# Modelo estimado con OLS
Modelo_OLS <- lm(X_TTRATA22~ X_Tinscrsal+X_PIBPKPT22+X_Gastsal_3+
                   X_AK.1.22 + X_gstoseg22 + X_DENSD22, data=datos)
summary(Modelo_OLS)


# Prueba de Breush-Pagan de homocedasticidad (H0)
#install.packages("lmtest")
#library(lmtest)

bptest(Modelo_OLS)

# Prueba de normalidad de los errores de Jarque-Bera
#install.packages("tseries")
#library(tseries)
jarque.bera.test(Modelo_OLS$residuals)

# Prueba de normalidad Shapiro-Wilk
shapiro.test(Modelo_OLS$residuals)

# Prueba de Moran a residuales del modelo OLS
#install.packages("spdep")
#library(spdep)
I_Moran <- lm.morantest(Modelo_OLS, knear3, zero.policy=T)
print(I_Moran)

# Pruebas de especificación de modelos espaciales
resultados_tests <- lm.RStests(Modelo_OLS, knear3, test = c("LMerr", "RLMerr", "LMlag", "RLMlag", "SARMA"), zero.policy = TRUE)
print(resultados_tests)

# Modelos Espaciales
#install.packages("spatialreg")
#library(spatialreg)

######
######
######

# Estimar el Modelo Rezago Espacial
Modelo_lag <- lagsarlm(X_TTRATA22 ~ X_Tinscrsal + X_PIBPKPT22 + X_DENSD22+
                         gstosalu_8 + X_AK.1.22 + X_gstoseg22, 
                       data = datos, listw = knear3, 
                       zero.policy = TRUE, tol.solve = 1e-20)
summary(Modelo_lag)

library(spdep)
ut_mod_lag <- residuals.lm(Modelo_lag)
moran_mod_lag <- moran.test(ut_mod_lag, knear3,randomisation=TRUE, alternative="two.sided", na.action=na.exclude, zero.policy = T)
print(moran_mod_lag)

# Estimar el modelo de Error Espacial 
Modelo_err <- errorsarlm(X_TTRATA22~ X_Tinscrsal+X_PIBPKPT22+gstosalu_8+X_DENSD22+
                           X_AK.1.22 + X_gstoseg22, tol.solve = 1e-20,
                         data=datos,knear3, zero.policy = T) 
summary(Modelo_err)

ut_mod_err <- residuals.lm(Modelo_err)
moran_mod_err <- moran.test(ut_mod_err, knear3,randomisation=TRUE, alternative="two.sided", na.action=na.exclude)
print(moran_mod_err)


#Estimar el modelo de Durbin Rezago Espacial 
Modelo_lag_durbin <- lagsarlm(X_TTRATA22 ~ X_Tinscrsal + X_PIBPKPT22 +X_DENSD22+ 
                                gstosalu_8 + X_AK.1.22 + X_gstoseg22, 
                              data = datos, listw = knear3, type = "durbin", 
                              zero.policy = TRUE, tol.solve = 1e-20)

summary(Modelo_lag_durbin)

coef(Modelo_lag)
coef(Modelo_lag_durbin)
summary(knear3)

###
###
###
ut_mod_durbin <- residuals.lm(Modelo_lag_durbin)
moran_mod_durbin <- moran.test(ut_mod_durbin, knear3,randomisation=TRUE, alternative="two.sided", na.action=na.exclude)

print(moran_mod_durbin)








#Pruebas de Multiplicadores de Lagranges lm.LMtests(columbus.lm,col.listw,test=c("LMerr","RLMerr","LMlag","RLMlag","SAR MA"))
tests <- c("LMerr", "RLMerr", "LMlag", "RLMlag", "SARMA")
results <- lm.RStests(model = Modelo_OLS, listw = knear3, zero.policy = TRUE, test = tests)
print(results)



library(sf)
#knear3 <- knearneigh(coords, k=3)
library(sf)
library(spdep)








install.packages("spdep")
library(spdep)
# Calcular los centroides de los polígonos en el shapefile
coords <- st_centroid(datos)  
# Extraer las coordenadas en una matriz
coords_matrix <- st_coordinates(coords)
# Crear una matriz de pesos de vecinos más cercanos (k=3)
col.knn <- knearneigh(coords, k=3)
nb1 <- knn2nb(col.knn, row.names = NULL)
knear3 <- nb2listw(nb1, glist=NULL, style="W", zero.policy=T)
#View(knear3)

#library(spatialreg)
#Matriz de pesos espaciales
#Modelo OLS
#knear3 <- knearneigh(coordinates(datos), k = 3)
Modelo_OLS <- lm(X_TTRATA22~ X_Tinscrsal+X_PIBPKPT22+gstosalu_8+X_DENSD22+
                   X_AK.1.22 + X_gstoseg22, data = datos)
summary(Modelo_OLS)
#knear3_listw <- nb2listw(knn2nb(knear3))
single_test_result <- lm.RStests(model = Modelo_OLS, listw = knear3, zero.policy = TRUE, test = "LMerr")
print(single_test_result)

#Esta prueba de lm.RStests  sirve para Evalúa si el modelo OLS presenta autocorrelación espacial en los residuos, lo que indicaría que la estructura espacial no se ha capturado correctamente y que deberías considerar un modelo de errores espaciales (SARerr o SEM, según la literatura).
#aplica pruebas de especificación espacial (Lagrange Multiplier Tests).

#test = "LMerr" indica que estás usando la prueba para errores espaciales, es decir, que verifica si los errores siguen una estructura espacial no capturada por el modelo OLS.

#Si el p-valor es pequeño (p < 0.05), se rechaza la hipótesis nula de que los errores son independientes → hay que usar un modelo espacial tipo SEM (Spatial Error Model).

#Si el p-valor es grande (p > 0.05), entonces OLS podría ser suficiente y no es necesario un modelo espacial en los errores.

# Run all tests if the single test is successful
tests <- c("LMerr", "RLMerr", "LMlag", "RLMlag", "SARMA")
results <- lm.RStests(model = Modelo_OLS, listw = knear3, zero.policy = TRUE, test = tests)

# Print all test results3
print(results)

moran.test(x = datos$X_TTRATA22, listw = knear3)
print(moran.test)

library(tmap)
library(classInt)

lisa <- localmoran(datos$X_TTRATA22, knear3, zero.policy = TRUE)
colnames(lisa)
# Agregar resultados al objeto 'datos'
datos$Ii       <- lisa[, "Ii"]              # índice local de Moran
lisa[, "Pr(z != E(Ii))"]                   # p-valor


install.packages("tmap")  # solo la primera vez
library(tmap)

tmap_mode("plot")  # para mapas estáticos


# --- Mapa LISA ---
head(datos$Ii)
head(datos$P_Ii)

# Crea variable 'quadrant' para clasificar clusters LISA según signo y p-valor
datos$quadrant <- "Not Significant"  # Inicializar

# Define los clusters LISA: asumiendo p-valor < 0.05 como significativo
datos$quadrant[datos$Ii > 0 & datos$P_Ii <= 0.05] <- "High-High"
datos$quadrant[datos$Ii < 0 & datos$P_Ii <= 0.05] <- "Low-Low"

# Si quieres también incluir High-Low y Low-High según vecinos (opcional)
# Aquí solo con dos clusters para simplificar

# Convertir en factor para ordenar la leyenda
datos$quadrant <- factor(datos$quadrant, levels = c("High-High", "Low-Low", "Not Significant"))

# Verifica que la variable está creada
table(datos$quadrant)

# Paquetes necesarios
# Asegúrate de que datos$Ii y datos$P_Ii existan
head(datos$Ii)
head(datos$P_Ii)

# Crea variable 'quadrant' para clasificar clusters LISA según signo y p-valor
datos$quadrant <- "Not Significant"  # Inicializar

# Define los clusters LISA: asumiendo p-valor < 0.05 como significativo
datos$quadrant[datos$Ii > 0 & datos$P_Ii <= 0.05] <- "High-High"
datos$quadrant[datos$Ii < 0 & datos$P_Ii <= 0.05] <- "Low-Low"

# Convertir en factor para ordenar la leyenda
datos$quadrant <- factor(datos$quadrant, levels = c("High-High", "Low-Low", "Not Significant"))

# Verifica que la variable está creada
table(datos$quadrant)

# Paquetes necesarios
head(datos$Ii)
head(datos$P_Ii)

library(sf)
library(spdep)
library(tmap)

# 1. Calcular matriz de pesos
coords <- st_centroid(datos)
coords_matrix <- st_coordinates(coords)
col.knn <- knearneigh(coords, k=3)
nb1 <- knn2nb(col.knn, row.names = NULL)
knear3 <- nb2listw(nb1, style="W", zero.policy=TRUE)

# 2. Calcular índice local de Moran para variable de interés (ejemplo X_TTRATA22)
lisa <- localmoran(datos$X_TTRATA22, knear3)

# 3. Guardar índice y p-valor en el sf object
datos$Ii <- lisa[, "Ii"]
datos$P_Ii <- lisa[, "Pr(z != E(Ii))"]

# 4. Crear clusters LISA
datos$quadrant <- "Not Significant"
datos$quadrant[datos$Ii > 0 & datos$P_Ii <= 0.05] <- "High-High"
datos$quadrant[datos$Ii < 0 & datos$P_Ii <= 0.05] <- "Low-Low"
datos$quadrant <- factor(datos$quadrant, levels = c("High-High", "Low-Low", "Not Significant"))


# 5. Graficar mapa
tmap_mode("plot")
pal <- c("red", "blue", "white")

tm_shape(datos) +
  tm_polygons(
    fill = "quadrant",
    palette = pal,
    title = "LISA Clusters",
    border.col = "gray30",
    border.alpha = 0.5
  ) +
  tm_layout(legend.position = c("left", "bottom"))









# 2. Calcular índice local de Moran para variable de interés (ejemplo gstosalu_8)
lisa1 <- localmoran(datos$gstosalu_8, knear3)

# 3. Guardar índice y p-valor en el sf object
datos$Ii <- lisa1[, "Ii"]
datos$P_Ii <- lisa1[, "Pr(z != E(Ii))"]

# 4. Crear clusters LISA
datos$quadrant <- "Not Significant"
datos$quadrant[datos$Ii > 0 & datos$P_Ii <= 0.05] <- "High-High"
datos$quadrant[datos$Ii < 0 & datos$P_Ii <= 0.05] <- "Low-Low"
datos$quadrant <- factor(datos$quadrant, levels = c("High-High", "Low-Low", "Not Significant"))




#Anlanisis bivariado
# 2. Calcular índice local bivariado de Moran
# spdep no tiene función nativa para local bivariado, así que calculamos manualmente:

# a) Escalar variables (opcional pero recomendado)
x <- scale(datos$X_TTRATA22)      # variable dependiente
y <- scale(datos$gstosalu_8)     # variable independiente

# b) Calcular el lag espacial de la variable independiente (gasto en salud)
lag_y <- lag.listw(knear3, y)

# c) Índice local bivariado: Ii = x_i * lag_y_i
Ii_biv <- x * lag_y

# d) Para p-valores y significancia, requerirías simulaciones Monte Carlo (más complejo, aquí solo indice)

# 3. Guardar en el objeto sf
datos$Ii_biv <- Ii_biv

# 4. Clasificar clusters bivariados simple (puedes definir umbrales arbitrarios o percentiles)
datos$cluster_biv <- "No significativo"
datos$cluster_biv[datos$Ii_biv > 0] <- "Alto-Vecinos Alto"    # x alto con vecinos alto (pos. correlación)
datos$cluster_biv[datos$Ii_biv < 0] <- "Bajo-Vecinos Bajo"    # x bajo con vecinos bajo (neg. correlación)
datos$cluster_biv <- factor(datos$cluster_biv, levels = c("Alto-Vecinos Alto", "Bajo-Vecinos Bajo", "No significativo"))

# 5. Graficar mapa bivariado
tmap_mode("plot")
pal <- c("red", "blue", "gray80")

tm_shape(datos) +
  tm_polygons(
    fill = "cluster_biv",
    palette = pal,
    title = "   Clusters
  Bivariados LISA",
    border.col = "black",
    border.alpha = 1
  ) +
  tm_layout(legend.position = c("left", "bottom"))



moran.test(resid(Modelo_OLS), listw = knear3)
print(moran.test)

names(datos)



# Modelos Espaciales
# Estimar el Modelo Rezago Espacial 
install.packages("spatialreg")
library(spatialreg)
library(spdep)

Modelo_lag <- lagsarlm(X_TTRATA22~ X_Tinscrsal+X_PIBPKPT22+gstosalu_8+X_DENSD22+
                         X_AK.1.22 + X_gstoseg22, tol.solve= 1e-20, data=datos,listw=knear3, zero.policy = T)


# Print the summary of the spatial lag model
summary(Modelo_lag)


#Estimar el modelo de Durbin Rezago Espacial 
Modelo_lag_durbin <- lagsarlm(X_TTRATA22 ~ X_Tinscrsal + X_PIBPKPT22 + 
                                X_DENSD22 + gstosalu_8 + X_AK.1.22 + X_gstoseg22, 
                              data = datos, listw = knear3, 
                              Durbin = TRUE, zero.policy = TRUE, tol.solve = 1e-20)
summary(Modelo_lag_durbin)
library(spdep)
class(Modelo_lag)
class(Modelo_lag_durbin)
library(spatialreg)
install.packages('spDataLarge', repos='https://nowosad.github.io/drat/',
                 type='source')`
exists("LR.sarlm", where = "package:spatialreg")

install.packages("spatialreg", dependencies = TRUE)
library(spData)
library(spatialreg)
exists("LR.sarlm", where = "package:spatialreg")
install.packages("spatialreg")     # Si no lo tienes instalado
library(spatialreg)  
exists("LR.sarlm", where = "package:spatialreg")
class(Modelo_lag)
class(Modelo_lag_durbin)

a<-LR.sarlm(Modelo_lag_durbin, Modelo_lag)
print(a)







# Estimar el modelo de Error Espacial 
# Spatial Error Model
Modelo_err <- errorsarlm(X_TTRATA22~ X_Tinscrsal+X_PIBPKPT22+gstosalu_8+X_DENSD22+
                           X_AK.1.22 + X_gstoseg22, tol.solve= 1e-20, data=datos, listw=knear3, zero.policy = T)


# Print the summary of the spatial error model
summary(Modelo_err)

ut_mod_err <- residuals(Modelo_err)
print(ut_mod_err)

hist(ut_mod_err, main="Histograma de residuos", xlab="Residuos")
qqnorm(ut_mod_err)
qqline(ut_mod_err)
shapiro.test(ut_mod_err)
##errores heterocedasticos
library(lmtest)
residuos <- residuals(Modelo_err)  # Extraer los residuos
ajuste <- lm(residuos^2 ~ fitted(Modelo_err))  # Regresión auxiliar
bptest(ajuste)  # Aplicar la prueba de Breusch-Pagan
(hetwhite.sarlm(Modelo_err))
bptest(Modelo_err)  # Modelo_err es tu modelo de regresión

# Perform Moran's I test using the spatial weights list
moran_mod_err <- moran.test(ut_mod_err, listw = knear3, randomisation = TRUE, alternative = "two.sided", na.action = na.exclude)

# Print the Moran's I test results
print(moran_mod_err)

# Calcular Moran Local (LISA)
moran_local_err <- localmoran(ut_mod_err, listw = knear3, zero.policy = TRUE)

# Agregar resultados al dataframe original
datos$LISA <- moran_local_err[, 1]  # Índice de Moran Local
datos$pvalor_LISA <- moran_local_err[, 5]  # p-valor

datos$cluster <- with(datos, ifelse(pvalor_LISA < 0.05 & LISA > 0, "Cluster Alto",
                                    ifelse(pvalor_LISA < 0.05 & LISA < 0, "Cluster Bajo",
                                           "No significativo")))
library(ggplot2)
library(sf)

# Convertir datos a objeto espacial (asegúrate de que 'datos' tenga geometría)
datos_sf <- st_as_sf(datos, coords = c("longitud", "latitud"), crs = 4326)

moran.test(ut_mod_err, listw = knear3, zero.policy = TRUE)
print(moran.test)


install.packages("ggplot2")
library(ggplot2)
library(spdep)
moran.test(ut_mod_err, listw = knear3, zero.policy = TRUE)
# Graficar el mapa de clusters






ggplot(datos_sf) +
  geom_sf(aes(fill = cluster), color = "black") +
  scale_fill_manual(values = c("Cluster Alto" = "red", "Cluster Bajo" = "blue", "No significativo" = "gray")) +
  theme_minimal() +
  labs(title = "Clusters espaciales de los residuos del Modelo SEM",
       fill = "Categoría de Cluster")







#Estimar el modelo SAR LAG
Modelo_SARLag <- lagsarlm(X_TTRATA22 ~ X_Tinscrsal + X_PIBPKPT22 + 
                            gstosalu_8 + X_DENSD22 + X_AK.1.22 + X_gstoseg22, 
                          data = datos, 
                          listw = knear3, 
                          zero.policy = TRUE, 
                          tol.solve = 1e-20)

print(Modelo_SARLag)
summary(Modelo_SARLag)


# Estimar modelo SARAR 
Modelo_sarar <- sacsarlm(X_TTRATA22~ X_Tinscrsal+X_PIBPKPT22+gstosalu_8+X_DENSD22+
                           X_AK.1.22 + X_gstoseg22,
                         data = datos,
                         listw = knear3,
                         tol.solve = 3.196e-50,
                         zero.policy = TRUE)

print(Modelo_sarar)
summary(Modelo_sarar)

#Matriz de pesos espaciales knear3_listw

ut_mod_sarar <- residuals.lm(Modelo_err)
moran_mod_sarar <- moran.test(ut_mod_sarar, knear3,randomisation=TRUE, alternative="two.sided", na.action=na.exclude)
print(moran_mod_sarar)

#library(spatialreg)
#library(spData)
##LR_test <- LR.sarlm(Modelo_OLS, Modelo_lag)

# Extraer la log-verosimilitud de ambos modelos: Mayor log-verosimilitud → mejor ajuste a los datos.
logLik_OLS <- logLik(Modelo_OLS)
logLik_lag <- logLik(Modelo_lag)

# Calcular la estadística de la prueba de razón de verosimilitud
#Si el modelo espacial tiene más parámetros que OLS, debe justificarlo con una mejora significativa.
LR_statistic <- -2 * (as.numeric(logLik_OLS) - as.numeric(logLik_lag))

# Grados de libertad (diferencia en el número de parámetros entre los dos modelos)
df <- length(coef(Modelo_lag)) - length(coef(Modelo_OLS))

# Calcular el p-valor
p_value <- pchisq(LR_statistic, df, lower.tail = FALSE)

# Mostrar los resultados
LR_test <- list(LR_statistic = LR_statistic, df = df, p_value = p_value)
print(LR_test)



##LR.sarlm(Modelo_lag_durbin, Modelo_lag)

# Realizar la prueba de razón de verosimilitud entre ambos modelos

logLik_lag_durbin <- logLik(Modelo_lag_durbin)
logLik_lag <- logLik(Modelo_lag)

# Calcular la estadística de la prueba de razón de verosimilitud
LR_statistic <- -2 * (as.numeric(logLik_lag) - as.numeric(logLik_lag_durbin))

# Grados de libertad (diferencia en el número de parámetros entre los dos modelos)
df <- length(coef(Modelo_lag_durbin)) - length(coef(Modelo_lag))

# Calcular el p-valor
p_value <- pchisq(LR_statistic, df, lower.tail = FALSE)

# Mostrar los resultados
LR_test <- list(LR_statistic = LR_statistic, df = df, p_value = p_value)
print(LR_test)








LR_stat <- -2 * (as.numeric(logLik(Modelo_lag)) - as.numeric(logLik(Modelo_lag_durbin)))
df_diff <- attr(logLik(Modelo_lag_durbin), "df") - attr(logLik(Modelo_lag), "df")
p_value <- pchisq(LR_stat, df = df_diff, lower.tail = FALSE)

print(LR_stat)
print(p_value)

AIC(Modelo_lag, Modelo_lag_durbin)



BIC(Modelo_lag, Modelo_lag_durbin)


########################################################
########################################################
##El modelo mas preciso es el modelo Spatial error model (SEM)

##mapas
library(sf)
library(ggplot2)
library(tmap)
install.packages("tmap")
install.packages("tmap", repos = "http://cran.us.r-project.org")
R.version.string
install.packages("remotes")
remotes::install_github("mtennekes/tmap")

# Cargar el shapefile de las entidades federativas (ajusta la ruta)
# Agregar los residuos al shapefile
datos$residuos <- ut_mod_err  


# Mapa de residuos con ggplot2
tmap_mode("plot")

# Crear los mapas individuales sin recuadro en la leyenda
library(tmap)
library(grid)
library(gridExtra)
library(sf)
library(ggplot2)
datos <- st_transform(datos, crs = 6372)
ggplot(datos) +
  geom_sf(aes(fill = residuos), color = "black") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Mapa de residuos del modelo espacial",
       fill = "Residuos") +
  theme_minimal()


##MAPAS DE GASTO EN SALUD POR PERSONA INSCRITA
# Crear los mapas individuales
map_2016 <- tm_shape(datos) +
  tm_polygons(
    col = "X_gstosalup",
    palette = "Oranges",
    style = "quantile",
    n = 5,
    title = ""
  ) +
  tm_layout(
    main.title = "2016",
    main.title.position = "center",
    main.title.size = 1.5,
    legend.position = c("right", "top"),
    legend.text.size = 0.4,
    legend.title.size = 0.8
  )

map_2018 <- tm_shape(datos) +
  tm_polygons(
    col = "X_gstosal_1",
    palette = "Oranges",
    style = "quantile",
    n = 5,
    title = ""
  ) +
  tm_layout(
    main.title = "2018",
    main.title.position = "center",
    main.title.size = 1.5,
    legend.position = c("right", "top"),
    legend.text.size = 0.4,
    legend.title.size = 0.8
  )

map_2020 <- tm_shape(datos) +
  tm_polygons(
    col = "X_gstosal_2",
    palette = "Oranges",
    style = "quantile",
    n = 5,
    title = ""
  ) +
  tm_layout(
    main.title = "2020",
    main.title.position = "center",
    main.title.size = 1.5,
    legend.position = c("right", "top"),
    legend.text.size = 0.4,
    legend.title.size = 0.8
  )

map_2022 <- tm_shape(datos) +
  tm_polygons(
    col = "gstosalu_8",
    palette = "Oranges",
    style = "quantile",
    n = 5,
    title = ""
  ) +
  tm_layout(
    main.title = "2022",
    main.title.position = "center",
    main.title.size = 1.5,
    legend.position = c("right", "top"),
    legend.text.size = 0.4,
    legend.title.size = 0.8
  )

library(tmap)
library(grid)
library(gridExtra)

tmap_mode("plot")

# Crear los mapas individuales sin recuadro en la leyenda
library(tmap)
library(grid)
library(gridExtra)
library(sf)

# Aplicar proyección adecuada para México (EPSG:6372 - Mexico ITRF92 / LCC)
datos <- st_transform(datos, crs = 6372)

# Establecer modo "plot"
tmap_mode("plot")

# Función para crear cada mapa
crear_mapa <- function(columna, anio) {
  tm_shape(datos) +
    tm_polygons(
      col = columna,
      palette = "Oranges",
      style = "quantile",
      n = 4,
      border.col = "white",
      border.alpha = 1,
      lwd = 0.5,
      title = ""
    ) +
    tm_layout(
      main.title = anio,
      main.title.position = "center",
      main.title.size = 0.5,
      legend.position = c("right", "top"),
      legend.text.size = 0.3,
      legend.title.size = 0.7,
      legend.bg.color = NA,
      legend.bg.alpha = 0,
      frame = FALSE
    )
}

names(datos)

# Crear los mapas por año
map_2016 <- crear_mapa("gstosalu11", "2016")
map_2018 <- crear_mapa("gstosalu10", "2018")
map_2020 <- crear_mapa("gstosalu_9", "2020")
map_2022 <- crear_mapa("gstosalu_8", "2022")

# Convertir a grobs
g1 <- tmap_grob(map_2016)
g2 <- tmap_grob(map_2018)
g3 <- tmap_grob(map_2020)
g4 <- tmap_grob(map_2022)

# Título general
titulo <- textGrob("Distribución del Gasto en Salud por persona inscrita
                    en el sistema de salud en México",
                   gp = gpar(fontsize = 12, fontface = "bold"))

# Mostrar el arreglo con el título
grid.newpage()
grid.arrange(titulo, arrangeGrob(g1, g2, g3, g4, ncol = 2), heights = c(0.7, 6))






###Mapas de trata de personas
# Crear los mapas individuales
map_2016 <- tm_shape(datos) +
  tm_polygons(
    col = "_ttrata16",
    palette = "Purples",
    style = "quantile",
    n = 5,
    title = ""
  ) +
  tm_layout(
    main.title = "2016",
    main.title.position = "center",
    main.title.size = 1.5,
    legend.position = c("right", "top"),
    legend.text.size = 0.4,
    legend.title.size = 0.8
  )

map_2018 <- tm_shape(datos) +
  tm_polygons(
    col = "_ttrata18",
    palette = "Purples",
    style = "quantile",
    n = 5,
    title = ""
  ) +
  tm_layout(
    main.title = "2018",
    main.title.position = "center",
    main.title.size = 1.5,
    legend.position = c("right", "top"),
    legend.text.size = 0.4,
    legend.title.size = 0.8
  )

map_2020 <- tm_shape(datos) +
  tm_polygons(
    col = "_ttrata20",
    palette = "Purples",
    style = "quantile",
    n = 5,
    title = ""
  ) +
  tm_layout(
    main.title = "2020",
    main.title.position = "center",
    main.title.size = 1.5,
    legend.position = c("right", "top"),
    legend.text.size = 0.4,
    legend.title.size = 0.8
  )

map_2022 <- tm_shape(datos) +
  tm_polygons(
    col = "_ttrata22",
    palette = "Purples",
    style = "quantile",
    n = 5,
    title = ""
  ) +
  tm_layout(
    main.title = "2022",
    main.title.position = "center",
    main.title.size = 1.5,
    legend.position = c("right", "top"),
    legend.text.size = 0.4,
    legend.title.size = 0.8
  )

library(tmap)
library(grid)
library(gridExtra)

tmap_mode("plot")

# Crear los mapas individuales sin recuadro en la leyenda
library(tmap)
library(grid)
library(gridExtra)
library(sf)

# Aplicar proyección adecuada para México (EPSG:6372 - Mexico ITRF92 / LCC)
datos <- st_transform(datos, crs = 6372)

# Establecer modo "plot"
tmap_mode("plot")

# Función para crear cada mapa
crear_mapa <- function(columna, anio) {
  tm_shape(datos) +
    tm_polygons(
      col = columna,
      palette = "Purples",
      style = "quantile",
      n = 4,
      border.col = "white",
      border.alpha = 1,
      lwd = 0.5,
      title = ""
    ) +
    tm_layout(
      main.title = anio,
      main.title.position = "center",
      main.title.size = 0.5,
      legend.position = c("right", "top"),
      legend.text.size = 0.3,
      legend.title.size = 0.7,
      legend.bg.color = NA,
      legend.bg.alpha = 0,
      frame = FALSE
    )
}

# Crear los mapas por año
map_2016 <- crear_mapa("gstosalu_11", "2016")
map_2018 <- crear_mapa("gstosalu_10", "2018")
map_2020 <- crear_mapa("gstosalu_9", "2020")
map_2022 <- crear_mapa("gstosalu_8", "2022")

# Convertir a grobs
g1 <- tmap_grob(map_2016)
g2 <- tmap_grob(map_2018)
g3 <- tmap_grob(map_2020)
g4 <- tmap_grob(map_2022)

# Título general
titulo <- textGrob("Tasa de trata de personas por cada 100 mil habitantes en México",
                   gp = gpar(fontsize = 16, fontface = "bold"))

# Mostrar el arreglo con el título
grid.newpage()
grid.arrange(titulo, arrangeGrob(g1, g2, g3, g4, ncol = 2), heights = c(0.3, 2))





##Mapas gasto en seguridad
# Crear los mapas individuales
map_2016 <- tm_shape(datos) +
  tm_polygons(
    col = "_ttrata16",
    palette = "RdPu",
    style = "quantile",
    n = 5,
    title = ""
  ) +
  tm_layout(
    main.title = "2016",
    main.title.position = "center",
    main.title.size = 1.5,
    legend.position = c("right", "top"),
    legend.text.size = 0.4,
    legend.title.size = 0.8
  )

map_2018 <- tm_shape(datos) +
  tm_polygons(
    col = "_ttrata18",
    palette = "RdPu",
    style = "quantile",
    n = 5,
    title = ""
  ) +
  tm_layout(
    main.title = "2018",
    main.title.position = "center",
    main.title.size = 1.5,
    legend.position = c("right", "top"),
    legend.text.size = 0.4,
    legend.title.size = 0.8
  )

map_2020 <- tm_shape(datos) +
  tm_polygons(
    col = "_ttrata20",
    palette = "RdPu",
    style = "quantile",
    n = 5,
    title = ""
  ) +
  tm_layout(
    main.title = "2020",
    main.title.position = "center",
    main.title.size = 1.5,
    legend.position = c("right", "top"),
    legend.text.size = 0.4,
    legend.title.size = 0.8
  )

map_2022 <- tm_shape(datos) +
  tm_polygons(
    col = "_ttrata22",
    palette = "RdPu",
    style = "quantile",
    n = 5,
    title = ""
  ) +
  tm_layout(
    main.title = "2022",
    main.title.position = "center",
    main.title.size = 1.5,
    legend.position = c("right", "top"),
    legend.text.size = 0.4,
    legend.title.size = 0.8
  )

library(tmap)
library(grid)
library(gridExtra)

tmap_mode("plot")

# Crear los mapas individuales sin recuadro en la leyenda
library(tmap)
library(grid)
library(gridExtra)
library(sf)

# Aplicar proyección adecuada para México (EPSG:6372 - Mexico ITRF92 / LCC)
datos <- st_transform(datos, crs = 6372)

# Establecer modo "plot"
tmap_mode("plot")

# Función para crear cada mapa
crear_mapa <- function(columna, anio) {
  tm_shape(datos) +
    tm_polygons(
      col = columna,
      palette = "RdPu",
      style = "quantile",
      n = 4,
      border.col = "white",
      border.alpha = 1,
      lwd = 0.5,
      title = ""
    ) +
    tm_layout(
      main.title = anio,
      main.title.position = "center",
      main.title.size = 0.5,
      legend.position = c("right", "top"),
      legend.text.size = 0.3,
      legend.title.size = 0.7,
      legend.bg.color = NA,
      legend.bg.alpha = 0,
      frame = FALSE
    )
}

# Crear los mapas por año
map_2016 <- crear_mapa("gstosalu_11", "2016")
map_2018 <- crear_mapa("gstosalu_10", "2018")
map_2020 <- crear_mapa("gstosalu_9", "2020")
map_2022 <- crear_mapa("gstosalu_8", "2022")

# Convertir a grobs
g1 <- tmap_grob(map_2016)
g2 <- tmap_grob(map_2018)
g3 <- tmap_grob(map_2020)
g4 <- tmap_grob(map_2022)

# Título general
titulo <- textGrob("Gasto en seguridad de los hogares en México",
                   gp = gpar(fontsize = 16, fontface = "bold"))

# Mostrar el arreglo con el título
grid.newpage()
grid.arrange(titulo, arrangeGrob(g1, g2, g3, g4, ncol = 2), heights = c(0.3, 2))







##Densidad poblacional
tm_shape(datos) +
  tm_polygons("X_DENSD24", 
              palette = "Reds", 
              title = "Densidad de población",
              style = "quantile",  # Permite definir el número exacto de intervalos
              n = 5) +  # Cinco niveles de color
  tm_layout(main.title = "Densidad de población en México (hab/km²)",
            legend.outside = TRUE,  
            legend.outside.position = "right",  
            legend.text.size = 0.5,  
            legend.title.size = 1)

##Gasto en seguridad
tm_shape(datos) +
  tm_polygons("X_gstoseg22", 
              palette = "Purples", 
              title = "Gasto en Seguridad", 
              n = 5, 
              style = "quantile") +  # Fuerza 5 categorías con cuantiles
  tm_layout(main.title = "Distribución del Gasto en Seguridad de los hogares en México 2022",
            main.title.size = 0.8,  # Ajusta el tamaño del título principal
            legend.position = c("right", "top"),
            legend.text.size = 0.5,
            legend.title.size = 0.5)


###Para ver si los residuos siguen n patrón espacial
datos$residuos_err <- residuals(Modelo_err)  # Reemplaza 'Modelo_err' si tu modelo tiene otro nombre

# Crear el mapa de residuos espaciales
tm_shape(datos) +
  tm_polygons("residuos_err", 
              palette = "RdBu",  # Rojo-Azul para mostrar positivos y negativos
              title = "Residuos del Modelo", 
              n = 5, 
              style = "quantile") +  # Divide en 5 categorías según cuantiles
  tm_layout(main.title = "Mapa de Residuos del Modelo de Error Espacial",
            main.title.size = 0.8,  
            legend.position = c("right", "top"),
            legend.text.size = 0.5,
            legend.title.size = 0.5)


library(spdep)

# Prueba de Moran sobre los residuos del modelo de error espacial usando knear3
moran_test_residuos <- moran.test(ut_mod_err, listw = knear3)

# Mostrar resultados
print(moran_test_residuos)

##Mapa de las predicciones
# Agregar las predicciones al conjunto de datos
# ###Calcular predicciones manualmente
datos$predicciones <- fitted(Modelo_err)

# Crear el mapa de predicciones

# Aplicar proyección adecuada para México (EPSG:6372 - Mexico ITRF92 / LCC)
datos <- st_transform(datos, crs = 6372)

tm_shape(datos) +
  tm_polygons("predicciones", 
              palette = "YlGnBu",  
              title = "Estimaciones del Modelo SEM", 
              n = 5,  
              style = "quantile") +  
  tm_borders(lwd = 0.2, col = "gray50") +
  tm_text("NOMGEO", size = 0.4) +
  tm_layout(main.title = "Estimaciones del Modelo de Error Espacial",
            main.title.size = 0.8, 
            legend.position = c("right", "top"), 
            legend.text.size = 0.5, 
            legend.title.size = 1)







###Mapa de residuos
# Calcular residuos del modelo SEM
datos <- st_transform(datos, crs = 6372)
datos$residuos_err <- residuals(Modelo_err)

# Crear una categoría interpretativa
datos$cat_residuos <- cut(
  datos$residuos_err,
  breaks = c(-Inf, -0.5, -0.1, 0.1, 0.5, Inf),
  labels = c("Sobreestimación alta", "Sobreestimación leve", 
             "Ajuste cercano", "Subestimación leve", "Subestimación alta")
)

library(tmap)

tmap_mode("plot")

tm_shape(datos) +
  tm_polygons(
    col = "cat_residuos",
    palette = c(
      "Sobreestimación alta" = "#2166ac",
      "Sobreestimación leve" = "#67a9cf",
      "Ajuste cercano"       = "#66bd63",
      "Subestimación leve"   = "#f4a582",
      "Subestimación alta"   = "#b2182b"
    ),
    title = ""
  ) +
  tm_borders(lwd = 1.3, col = "gray50") +
  tm_text("NOMGEO", size = 0.35) +
  tm_layout(
    title = "Clasificación de los
residuos del modelo SEM",
    legend.outside = TRUE,
    legend.outside.position = "right",
    legend.title.size = 3.5,     # tamaño del título de la leyenda
    legend.text.size = 0.6       # tamaño del texto de las categorías
  )









library(tmap)

tmap_mode("plot")

tm_shape(datos) +
  tm_polygons(
    col = "cat_residuos",
    palette = c(
      "Sobreestimación alta" = "#2166ac",
      "Sobreestimación leve" = "#67a9cf",
      "Ajuste cercano"       = "#66bd63",
      "Subestimación leve"   = "#f4a582",
      "Subestimación alta"   = "#b2182b"
    ),
    title = "Clasificación de residuos"
  ) +
  tm_borders(lwd = 1, col = "gray50") +
  tm_text("NOMGEO", size = 0.35) +
  tm_layout(
    title = "",
    legend.position = c("left", "bottom"),  # <--- dentro del mapa
    legend.title.size = 0.8,
    legend.text.size = 0.5
  )


library(sf)

# Quitar la geometría para guardar solo los atributos
datos_sin_geom <- st_drop_geometry(datos)

# Guardar como CSV
write.csv(datos_sin_geom, "D:/PP/BASE_11_V1.csv", row.names = FALSE)







