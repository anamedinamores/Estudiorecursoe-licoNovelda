rm(list = ls()) 
par(family = "serif")
paleta = c("steelblue", "springgreen3","pink", "red3", "yellow", "mediumpurple3")


archivo = "POWER_Point_Hourly_20210101_20251231_038d39N_000d76W_UTC.csv"
datos = read.csv2(archivo, na.strings = "-999")
names(datos)
nrow(datos)
table(datos$YEAR)
summary(datos)

#################################################################################3
# 1.2 Evaluar cuantitativamente la cantidad datos erróneos para cada año del periodo seleccionado
# 1.3 Discutir la calidad de los datos e indicar que periodos son relevantes para el estudio eólico de la ubicación. 
#     Seleccionar el año más reciente que tenga una buena calidad de datos.
#FILTRADO DE DATOS
colSums(is.na(datos)) #buscar columnas con celdas vacías -> 0
sum(datos$WS10M < 0, na.rm=TRUE) #buscar velocidades negativas -> 0
sum(datos$WD10M < 0 | datos$WD10M > 360, na.rm=TRUE) #buscar direcciones fuera de 360º -> 0
#filtrado por años
d2021 = subset(datos, YEAR == 2021)
sum(is.na(d2021$WS10M)) #num de vel ausentes
sum(d2021$WS10M < 0, na.rm=TRUE) #num de vel negativas
d2022 = subset(datos, YEAR == 2022)
sum(is.na(d2022$WS10M)) 
sum(d2022$WS10M < 0, na.rm=TRUE)
d2023 = subset(datos, YEAR == 2023)
sum(is.na(d2023$WS10M)) 
sum(d2023$WS10M < 0, na.rm=TRUE) 
d2024 = subset(datos, YEAR == 2024)
sum(is.na(d2024$WS10M))
sum(d2024$WS10M < 0, na.rm=TRUE)
d2025 = subset(datos, YEAR == 2025)
sum(is.na(d2025$WS10M))
sum(d2025$WS10M < 0, na.rm=TRUE) 

#búqueda de atípicos en velocidades por años
boxplot(d2025$WS10M,
        main="Boxplot de la velocidad del viento",
        ylab="Velocidad (m/s)")
length(boxplot.stats(d2025$WS10M)$out) # -> año seleccionado (2do con menos atipicos)
length(boxplot.stats(d2024$WS10M)$out)
length(boxplot.stats(d2023$WS10M)$out)
length(boxplot.stats(d2022$WS10M)$out)
length(boxplot.stats(d2021$WS10M)$out)

####################################################################################
# 2.1 Análisis estadístico anual de los datos recopilados en los últimos 4 años con momentos estadísticos (velocidad 
#     media y varianza). Para ello se evalúan los momentos estadísticos por años, y se representará en una gráfica 
#     la media y varianza en función del año. 
mean22 <- mean(d2022$WS10M, na.rm = TRUE)
var22 <- var(d2022$WS10M, na.rm = TRUE)
mean23 <- mean(d2023$WS10M, na.rm = TRUE)
var23 <- var(d2023$WS10M, na.rm = TRUE)
mean24 <- mean(d2024$WS10M, na.rm = TRUE)
var24 <- var(d2024$WS10M, na.rm = TRUE)
mean25 <- mean(d2025$WS10M, na.rm = TRUE)
var25 <- var(d2025$WS10M, na.rm = TRUE)

# Análisis estadístico anual del viento (2022-2025)
anios <- 2022:2025
medias <- c(mean22, mean23, mean24, mean25)
varianzas <- c(var22, var23, var24, var25)
par(mar = c(5, 4.5, 4, 4.5), family = "serif")
plot(anios, medias, type = "b", pch = 19, col = paleta[1], lwd = 2,
     xlab = "Año", ylab = "Velocidad media (m/s)",
     main = "Análisis estadístico anual del viento (2022-2025)",
     xaxt = "n", col.lab = paleta[1],
     ylim = c(min(medias)*0.95, max(medias)*1.05))
axis(1, at = anios, labels = anios)
grid()
par(new = TRUE) #superponer
plot(anios, varianzas, type = "b", pch = 17, col = paleta[4], lwd = 2,
     axes = FALSE, xlab = "", ylab = "",
     ylim = c(min(varianzas)*0.95, max(varianzas)*1.05))
axis(4, col = paleta[4], col.ticks = paleta[4])
mtext("Varianza (m²/s²)", side = 4, line = 3, col = paleta[4])
legend("topright", legend = c("Velocidad media (m/s)", "Varianza (m²/s²)"),
       col = c(paleta[1], paleta[4]), lty = 1, pch = c(19, 17), lwd = 2, bty = "o", bg = "white")
par(mar = c(5, 4, 4, 2) + 0.1) # Restaurar márgenes 

####################################################################################
# 2.2 Análisis estadístico estacional de los datos recopilados en los últimos 4 años con momentos estadísticos 
#     (velocidad media y varianza). Para ello se evalúan los momentos estadísticos por meses, y se representará en 
#     una gráfica la media y varianza en función de los meses. 
datos4a <- subset(datos, YEAR >= 2022)
m1  <- subset(datos4a, MO == 1)
m2  <- subset(datos4a, MO == 2)
m3  <- subset(datos4a, MO == 3)
m4  <- subset(datos4a, MO == 4)
m5  <- subset(datos4a, MO == 5)
m6  <- subset(datos4a, MO == 6)
m7  <- subset(datos4a, MO == 7)
m8  <- subset(datos4a, MO == 8)
m9  <- subset(datos4a, MO == 9)
m10 <- subset(datos4a, MO == 10)
m11 <- subset(datos4a, MO == 11)
m12 <- subset(datos4a, MO == 12)

meanm1  <- mean(m1$WS10M, na.rm = TRUE)
meanm2  <- mean(m2$WS10M, na.rm = TRUE)
meanm3  <- mean(m3$WS10M, na.rm = TRUE)
meanm4  <- mean(m4$WS10M, na.rm = TRUE)
meanm5  <- mean(m5$WS10M, na.rm = TRUE)
meanm6  <- mean(m6$WS10M, na.rm = TRUE)
meanm7  <- mean(m7$WS10M, na.rm = TRUE)
meanm8  <- mean(m8$WS10M, na.rm = TRUE)
meanm9  <- mean(m9$WS10M, na.rm = TRUE)
meanm10 <- mean(m10$WS10M, na.rm = TRUE)
meanm11 <- mean(m11$WS10M, na.rm = TRUE)
meanm12 <- mean(m12$WS10M, na.rm = TRUE)

varm1  <- var(m1$WS10M, na.rm = TRUE)
varm2  <- var(m2$WS10M, na.rm = TRUE)
varm3  <- var(m3$WS10M, na.rm = TRUE)
varm4  <- var(m4$WS10M, na.rm = TRUE)
varm5  <- var(m5$WS10M, na.rm = TRUE)
varm6  <- var(m6$WS10M, na.rm = TRUE)
varm7  <- var(m7$WS10M, na.rm = TRUE)
varm8  <- var(m8$WS10M, na.rm = TRUE)
varm9  <- var(m9$WS10M, na.rm = TRUE)
varm10 <- var(m10$WS10M, na.rm = TRUE)
varm11 <- var(m11$WS10M, na.rm = TRUE)
varm12 <- var(m12$WS10M, na.rm = TRUE)

medias.m = c(meanm1, meanm2, meanm3, meanm4, meanm5, meanm6, 
             meanm7, meanm8, meanm9, meanm10, meanm11, meanm12)
varianzas.m = c(varm1, varm2, varm3, varm4, varm5, varm6, 
                varm7, varm8, varm9, varm10, varm11, varm12)
meses = 1:12
nombres_meses = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                  "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")

# Análisis estadístico estacional del viento (2022-2025)
meses = 1:12
nombres_meses = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                  "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
medias = medias.m
varianzas = varianzas.m
par(mar = c(5, 4.5, 4, 4.5), family = "serif")
plot(meses, medias, type = "b", pch = 19, col = paleta[1], lwd = 2,
     xlab = "Mes", ylab = "Velocidad media (m/s)",
     main = "Análisis estadístico estacional del viento (2022-2025)",
     xaxt = "n", col.lab = paleta[1],
     ylim = c(min(medias)*0.95, max(medias)*1.05))
axis(1, at = meses, labels = nombres_meses)
grid()
par(new = TRUE) #superponer
plot(meses, varianzas, type = "b", pch = 17, col = paleta[4], lwd = 2,
     axes = FALSE, xlab = "", ylab = "",
     ylim = c(min(varianzas)*0.95, max(varianzas)*1.05))
axis(4, col = paleta[4], col.ticks = paleta[4])
mtext("Varianza (m²/s²)", side = 4, line = 3, col = paleta[4])
legend("topright", legend = c("Velocidad media (m/s)", "Varianza (m²/s²)"),
       col = c(paleta[1], paleta[4]), lty = 1, pch = c(19, 17), lwd = 2, bty = "o", bg = "white")
par(mar = c(5, 4, 4, 2) + 0.1) # Restaurar márgenes

####################################################################################
# 2.3 Análisis estadístico estacional distinguiendo periodo diurno y nocturno de los datos recopilados en los últimos 
#   4 años con momentos estadísticos (velocidad media y varianza). Para ello se evalúan los momentos 
#   estadísticos por meses, distinguiendo los periodos diurnos (10:00-18:00) y nocturnos (22:00-6:00), y se 
#   representará en una gráfica la media y varianza en función de los meses 

datos4a = subset(datos, YEAR >= 2022)
datosdia = subset(datos4a, HR >= 10 & HR <= 18)
datosnoche = subset(datos4a, HR >= 22 | HR <= 6)

meandia   = numeric(12)
vardia    = numeric(12)
meannoche = numeric(12)
varnoche  = numeric(12)

m1d <- subset(datosdia, MO == 1); meandia[1] <- mean(m1d$WS10M); vardia[1] <- var(m1d$WS10M)
m2d <- subset(datosdia, MO == 2); meandia[2] <- mean(m2d$WS10M); vardia[2] <- var(m2d$WS10M)
m3d <- subset(datosdia, MO == 3); meandia[3] <- mean(m3d$WS10M); vardia[3] <- var(m3d$WS10M)
m4d <- subset(datosdia, MO == 4); meandia[4] <- mean(m4d$WS10M); vardia[4] <- var(m4d$WS10M)
m5d <- subset(datosdia, MO == 5); meandia[5] <- mean(m5d$WS10M); vardia[5] <- var(m5d$WS10M)
m6d <- subset(datosdia, MO == 6); meandia[6] <- mean(m6d$WS10M); vardia[6] <- var(m6d$WS10M)
m7d <- subset(datosdia, MO == 7); meandia[7] <- mean(m7d$WS10M); vardia[7] <- var(m7d$WS10M)
m8d <- subset(datosdia, MO == 8); meandia[8] <- mean(m8d$WS10M); vardia[8] <- var(m8d$WS10M)
m9d <- subset(datosdia, MO == 9); meandia[9] <- mean(m9d$WS10M); vardia[9] <- var(m9d$WS10M)
m10d <- subset(datosdia, MO == 10); meandia[10] <- mean(m10d$WS10M); vardia[10] <- var(m10d$WS10M)
m11d <- subset(datosdia, MO == 11); meandia[11] <- mean(m11d$WS10M); vardia[11] <- var(m11d$WS10M)
m12d <- subset(datosdia, MO == 12); meandia[12] <- mean(m12d$WS10M); vardia[12] <- var(m12d$WS10M)

m1n <- subset(datosnoche, MO == 1); meannoche[1] <- mean(m1n$WS10M); varnoche[1] <- var(m1n$WS10M)
m2n <- subset(datosnoche, MO == 2); meannoche[2] <- mean(m2n$WS10M); varnoche[2] <- var(m2n$WS10M)
m3n <- subset(datosnoche, MO == 3); meannoche[3] <- mean(m3n$WS10M); varnoche[3] <- var(m3n$WS10M)
m4n <- subset(datosnoche, MO == 4); meannoche[4] <- mean(m4n$WS10M); varnoche[4] <- var(m4n$WS10M)
m5n <- subset(datosnoche, MO == 5); meannoche[5] <- mean(m5n$WS10M); varnoche[5] <- var(m5n$WS10M)
m6n <- subset(datosnoche, MO == 6); meannoche[6] <- mean(m6n$WS10M); varnoche[6] <- var(m6n$WS10M)
m7n <- subset(datosnoche, MO == 7); meannoche[7] <- mean(m7n$WS10M); varnoche[7] <- var(m7n$WS10M)
m8n <- subset(datosnoche, MO == 8); meannoche[8] <- mean(m8n$WS10M); varnoche[8] <- var(m8n$WS10M)
m9n <- subset(datosnoche, MO == 9); meannoche[9] <- mean(m9n$WS10M); varnoche[9] <- var(m9n$WS10M)
m10n <- subset(datosnoche, MO == 10); meannoche[10] <- mean(m10n$WS10M); varnoche[10] <- var(m10n$WS10M)
m11n <- subset(datosnoche, MO == 11); meannoche[11] <- mean(m11n$WS10M); varnoche[11] <- var(m11n$WS10M)
m12n <- subset(datosnoche, MO == 12); meannoche[12] <- mean(m12n$WS10M); varnoche[12] <- var(m12n$WS10M)

meses = 1:12
nombresmeses = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                 "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
par(mfrow = c(2, 1), mar = c(4, 4.5, 2.5, 1), family = "serif")

# Velocidad Media (Ampliamos techo a 8 para liberar la esquina superior derecha)
plot(meses, meandia, type = "b", pch = 19, col = paleta[1], lwd = 2,
     xlab = "", ylab = "Velocidad media (m/s)",
     main = "Velocidad media mensual: Diurno vs Nocturno (2022-2025)",
     xaxt = "n", ylim = c(2, 8))
lines(meses, meannoche, type = "b", pch = 17, col = paleta[4], lwd = 2, lty = 2)
axis(1, at = meses, labels = nombresmeses)
grid()
legend("topright", legend = c("Diurno (10:00-18:00)", "Nocturno (22:00-06:00)"),
       col = c(paleta[1], paleta[4]), lty = c(1, 2), pch = c(19, 17), lwd = 2, bty = "n")

# Varianza 
plot(meses, vardia, type = "b", pch = 19, col = paleta[1], lwd = 2,
     xlab = "Mes", ylab = "Varianza (m²/s²)",
     main = "Varianza mensual: Diurno vs Nocturno (2022-2025)",
     xaxt = "n", ylim = c(0, 9.5))
lines(meses, varnoche, type = "b", pch = 17, col = paleta[4], lwd = 2, lty = 2)
axis(1, at = meses, labels = nombresmeses)
grid()
legend("topright", legend = c("Diurno (10:00-18:00)", "Nocturno (22:00-06:00)"),
       col = c(paleta[1], paleta[4]), lty = c(1, 2), pch = c(19, 17), lwd = 2, bty = "n")
par(mfrow = c(1, 1))


####################################################################################
# 2.4 Obtener los histogramas de velocidades para cada año. Representar los histogramas en una sola figura 
#   identificando cada año en la leyenda. Discutir los resultados teniendo en cuenta la calidad de los datos. 
#   Seleccionar el año más reciente que tenga una buena calidad de datos.

par(mar = c(5, 4.5, 4, 2), family = "serif")
max_dens <- max(
  max(density(d2022$WS10M, na.rm = TRUE)$y),
  max(density(d2023$WS10M, na.rm = TRUE)$y),
  max(density(d2024$WS10M, na.rm = TRUE)$y),
  max(density(d2025$WS10M, na.rm = TRUE)$y)
)

plot(density(d2025$WS10M, na.rm = TRUE), type = "l", lwd = 2.5, col = paleta[1],
     main = "Distribución anual de velocidad del viento (2022-2025)",
     xlab = "Velocidad del viento (m/s)", ylab = "Densidade de probabilidad",
     ylim = c(0, max_dens * 1.1), xlim = c(0, 20))

lines(density(d2024$WS10M, na.rm = TRUE), lwd = 2, col = paleta[2], lty = 2)
lines(density(d2023$WS10M, na.rm = TRUE), lwd = 2, col = paleta[4], lty = 3)
lines(density(d2022$WS10M, na.rm = TRUE), lwd = 2, col = paleta[6], lty = 4)

grid()
legend("topright", 
       legend = c("2025 (Seleccionado - Alta Calidad)", "2024", "2023", "2022"),
       col = c(paleta[1], paleta[2], paleta[4], paleta[6]), 
       lwd = c(2.5, 2, 2, 2), lty = c(1, 2, 3, 4), bty = "n", cex = 0.9)

####################################################################################
# 2.5 Realizar la rosa de los vientos del año seleccionado anteriormente, se valorará que se combine con frecuencia 
#     de velocidades en cada dirección (tres tramos de velocidades).

install.packages("openair")
library(openair)

d2025$date = ISOdate(d2025$YEAR, d2025$MO, d2025$DY, d2025$HR)

datosrosa = data.frame(date = d2025$date,
                       ws = d2025$WS10M,
                       wd = d2025$WD10M )

windRose(datosrosa, 
         ws = "ws", 
         wd = "wd", 
         paddle = FALSE,             
         cols = c(paleta[1], paleta[2], paleta[4]),
         n.pleasing = 4,
         breaks = 3,                 
         main = "Rosa de los vientos y frecuencia por tramos - Año 2025",
         key.header = "Velocidad (m/s)")


####################################################################################
# 2.6 Calcular el perfil de velocidades con la altura hasta 200 m para la ubicación elegida teniendo en cuenta la 
#   oleografía del terreno. Usar la velocidad media del año seleccionado teniendo en cuenta la altura a la que se 
#   recogieron los datos. 

href = 10                                       
vref = mean(d2025$WS10M, na.rm = TRUE) 
alturas = seq(from = 10, to = 200, by = 5) 
z0 = 0.03  
vperfil = vref * (log(alturas / z0) / log(href / z0))

perfiltabla = data.frame(
  Alturam = c(10, 50, 95, 150, 200),
  Velocidadms = round(vref * (log(c(10, 50, 95, 150, 200) / z0) / log(href / z0)), 2)
)
print(perfiltabla)

par(mar = c(5, 4.5, 4, 2), family = "serif")

plot(vperfil, alturas, type = "l", col = paleta[1], lwd = 2.5,
     xlab = "Velocidad media del viento (m/s)", ylab = "Altura h (m)",
     main = "Perfil de velocidades según la rugosidad del terreno",
     xlim = c(min(vperfil) * 0.9, max(vperfil) * 1.1),
     ylim = c(0, 200))

points(vref, href, col = "red3", pch = 19, cex = 1.3)
abline(h = 95, lty = 2, col = "gray40")
grid()

legend("topleft",
       legend = c("Perfil logarítmico v(h)", paste0("Dato medido a ", href, " m"), "Altura buje (95 m)"),
       col = c(paleta[1], "red3", "gray40"),
       lty = c(1, NA, 2), pch = c(NA, 19, NA), lwd = c(2.5, NA, 1), bty = "o", bg = "white", cex = 0.8)


####################################################################################
# 3.1  Del año representativo, obtener el histograma de velocidades teniendo en cuenta la altura del buje del 
#      aerogenerador. Representar el histograma y la frecuencia acumulada. 


hbuje = 95
d2025$WS95M = d2025$WS10M * (log(hbuje / z0) / log(href / z0))

vmax = ceiling(max(d2025$WS95M, na.rm = TRUE))
breaksv = seq(0, vmax, by = 1)
histb = hist(d2025$WS95M, breaks = breaksv, plot = FALSE)

freqrel = histb$density
freqacum = cumsum(histb$counts) / sum(histb$counts)
midsv = histb$mids

par(mar = c(5, 4.5, 4, 4.5), family = "serif")
bp = barplot(freqrel, space = 0, col = paleta[1], border = "white",
             xlab = "Velocidad del viento a 95 m (m/s)",
             ylab = "Frecuencia relativa",
             main = "Histograma y Frecuencia Acumulada a la altura del buje (95 m)",
             ylim = c(0, max(freqrel) * 1.15))

axis(1, at = c(0, seq(5, length(midsv), by = 5)), 
     labels = c(0, seq(5, length(midsv), by = 5)))
grid()

par(new = TRUE)
plot(bp, freqacum, type = "b", pch = 19, col = paleta[4], lwd = 2,
     axes = FALSE, xlab = "", ylab = "", ylim = c(0, 1))

axis(4, col = paleta[4], col.ticks = paleta[4])
mtext("Frecuencia acumulada", side = 4, line = 3, col = paleta[4])

legend("right", 
       legend = c("Frecuencia relativa (Histograma)", "Frecuencia acumulada"),
       col = c(paleta[1], paleta[4]), lty = c(1, 1), pch = c(15, 19), 
       lwd = c(5, 2), bty = "o", bg = "white", cex = 0.7)

par(mar = c(5, 4, 4, 2) + 0.1)


####################################################################################
# 3.2  Ajustar la distribución de Weibull al histograma de velocidades discutiendo la bondad del ajuste. 

centros = midsv
acumulada = freqacum

idx = which(acumulada > 0 & acumulada < 1)
vreg = centros[idx]
freg = acumulada[idx]

yweibull = log(-log(1 - freg))
xweibull = log(vreg)

modeloweibull = lm(yweibull ~ xweibull)
kpar = coef(modeloweibull)[2]
apar = coef(modeloweibull)[1]
cpar = exp(-apar / kpar)

print(paste("Parámetro de forma k:", round(kpar, 3)))
print(paste("Parámetro de escala c:", round(cpar, 3), "m/s"))
print(summary(modeloweibull))

par(mar = c(5, 4.5, 4, 2), family = "serif")

bp = barplot(freqrel, space = 0, col = paleta[1], border = "white",
             xlab = "Velocidad del viento a 95 m (m/s)",
             ylab = "Densidad de probabilidad / Frecuencia relativa",
             main = "Ajuste de la distribución de Weibull",
             ylim = c(0, max(freqrel) * 1.2))

axis(1, at = c(0, seq(5, length(midsv), by = 5)), 
     labels = c(0, seq(5, length(midsv), by = 5)))
grid()

vcurva = seq(0.1, max(vmax), by = 0.1)
fweibull = (kpar / cpar) * (vcurva / cpar)^(kpar - 1) * exp(-(vcurva / cpar)^kpar)

xpuntos = vcurva * (length(bp) / vmax) + 0.5 
lines(xpuntos, fweibull, col = paleta[4], lwd = 2.5)

legend("topright", 
       legend = c("Histograma empírico (95 m)", paste0("Weibull (k=", round(kpar, 2), ", c=", round(cpar, 2), " m/s)")),
       col = c(paleta[1], paleta[4]), lty = c(1, 1), pch = c(15, NA), 
       lwd = c(4, 2.5), bty = "o", bg = "white", cex = 0.8)


####################################################################################
# 3.3  Calcular y representar la curva de densidad de Weibull a partir de los parámetros ajustados en el rango de 
#      velocidades adecuado. Comparar el resultado con el histograma de velocidades del año representativo.

vcurva = seq(0, vmax, by = 0.1)
fweibull = (kpar / cpar) * (vcurva / cpar)^(kpar - 1) * exp(-(vcurva / cpar)^kpar)

par(mar = c(5, 4.5, 4, 2), family = "serif")

bp = barplot(freqrel, space = 0, col = paleta[1], border = "white",
             xlab = "Velocidad del viento a 95 m (m/s)",
             ylab = "Densidad de probabilidad",
             main = "Comparación: Histograma empírico vs Curva de Weibull",
             ylim = c(0, max(max(freqrel), max(fweibull)) * 1.15))

axis(1, at = c(0, seq(5, length(midsv), by = 5)), 
     labels = c(0, seq(5, length(midsv), by = 5)))
grid()

xpuntos = vcurva * (length(bp) / vmax) + 0.5 
lines(xpuntos, fweibull, col = paleta[4], lwd = 2.5)

legend("topright", 
       legend = c("Histograma empírico (Datos)", paste0("Curva Weibull (k=", round(kpar, 2), ", c=", round(cpar, 2), " m/s)")),
       col = c(paleta[1], paleta[4]), lty = c(1, 1), pch = c(15, NA), 
       lwd = c(4, 2.5), bty = "o", bg = "white", cex = 0.8)

par(mar = c(5, 4, 4, 2) + 0.1)


########################################################################################
# 3.4 Calcular y representar la curva de energía proporcionada por el aerogenerador para cada intervalo de 
# velocidades, usando la función de densidad de Weibull del apartado anterior y la curva de potencia 
# proporcionada en la Tabla 2. Evaluar la energía anual producida, y el factor de carga. Discutir los valores. 

velocidadtabla = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 
                   16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30)

potenciatabla = c(0, 0, 0, 50, 197, 464, 778, 1109, 1408, 1547, 
                  1600, 1600, 1600, 1600, 1600, 1600, 1600, 1600, 1600, 1600, 
                  1600, 1600, 1600, 1600, 1600, 1600, 0, 0, 0, 0, 0)

vclases = 0:30
potenciaclases = approx(velocidadtabla, potenciatabla, xout = vclases, rule = 2)$y

probweibull = numeric(length(vclases))
for (i in 1:length(vclases)) {
  vinf = max(0, vclases[i] - 0.5)
  vsup = vclases[i] + 0.5
  probweibull[i] = exp(-(vinf / cpar)^kpar) - exp(-(vsup / cpar)^kpar)
}

horasanio = 8760
energiaintervalo = potenciaclases * probweibull * horasanio / 1000 # Convertir kW·h a MW·h

aeptotal = sum(energiaintervalo, na.rm = TRUE)
potencianominal = 1600 # kW (según la Tabla 2 del PDF)
energiamaximateorica = (potencianominal / 1000) * horasanio
factorcarga = (aeptotal / energiamaximateorica) * 100

print(paste("Energía Anual Producida (AEP):", round(aeptotal, 2), "MWh/año"))
print(paste("Factor de Carga:", round(factorcarga, 2), "%"))

par(mar = c(5, 4.5, 4, 2), family = "serif")

bpenergia = barplot(energiaintervalo, names.arg = vclases, col = paleta[1], border = "white",
                    xlab = "Velocidad del viento a la altura del buje (m/s)",
                    ylab = "Energía anual aportada (MWh/año)",
                    main = "Curva de energía proporcionada",
                    ylim = c(0, max(energiaintervalo) * 1.15))
grid()

legend("topright", 
       legend = c(paste0("AEP Total: ", round(aeptotal, 1), " MWh"), 
                  paste0("Factor de Carga: ", round(factorcarga, 1), "%")),
       col = c(paleta[1], paleta[4]), pch = c(15, NA), 
       bty = "o", bg = "white", cex = 0.8)

par(mar = c(5, 4, 4, 2) + 0.1)

