library(readr)
library(ggplot2)
library(xts)


calidad_de_aire_2017 <- read_delim("/Volumes/Disco_SD/Set de datos/calidad-de-aire-2017.csv",
";", escape_double = FALSE, locale = locale(decimal_mark = ",",
grouping_mark = "."), trim_ws = TRUE,na = c("S/D"))

calidad_de_aire_2017$FECHA <- as.Date(calidad_de_aire_2017$FECHA,format = '%d/%m/%Y')

calidad_de_aire_2017$fecha_hora <- paste(calidad_de_aire_2017$FECHA," ",calidad_de_aire_2017$HORA,":00:00",sep = "")

calidad_de_aire_2017$fecha_hora <- as.POSIXct(calidad_de_aire_2017$fecha_hora,format= "%Y-%m-%d %H:%M:%S")

calidad_de_aire_2017$CO_LA_BOCA <- as.numeric(gsub(",",".",(gsub("<","",calidad_de_aire_2017$CO_LA_BOCA))))


plot(calidad_de_aire_2017$fecha_hora,calidad_de_aire_2017$CO_CENTENARIO,type = 'l')
lines(calidad_de_aire_2017$fecha_hora,calidad_de_aire_2017$CO_CORDOBA,type = 'l',col='red')
lines(calidad_de_aire_2017$fecha_hora,calidad_de_aire_2017$CO_LA_BOCA,type = 'l',col='green')

#-----datos co centenario-----

co_xts_centenerio <- xts(calidad_de_aire_2017$CO_CENTENARIO,order.by = calidad_de_aire_2017$fecha_hora)

summary(co_xts_centenerio)

plot.xts(co_xts_centenerio)

#----datos co cordoba----

co_xts_cordoba <- xts(calidad_de_aire_2017$CO_CORDOBA,order.by = calidad_de_aire_2017$fecha_hora)

summary(co_xts_cordoba)

plot.xts(co_xts_cordoba)

#-----datos co boca-----

co_xts_boca <- xts(calidad_de_aire_2017$CO_LA_BOCA,order.by = calidad_de_aire_2017$fecha_hora)

summary(co_xts_boca)

plot.xts(co_xts_boca)

#-----datos no2 centenario-----

no_xts_centenerio <- xts(calidad_de_aire_2017$NO2_CENTENARIO,order.by = calidad_de_aire_2017$fecha_hora)

summary(no_xts_centenerio)

plot.xts(no_xts_centenerio)

#----datos no2 cordoba----

no_xts_cordoba <- xts(calidad_de_aire_2017$NO2_CORDOBA,order.by = calidad_de_aire_2017$fecha_hora)

summary(no_xts_cordoba)

plot.xts(no_xts_cordoba)

#-----datos no2 boca-----

no_xts_boca <- xts(calidad_de_aire_2017$NO2_LA_BOCA,order.by = calidad_de_aire_2017$fecha_hora)

summary(no_xts_boca)

plot.xts(no_xts_boca)

#-----datos pm10 centenario-----

pm_xts_centenerio <- xts(calidad_de_aire_2017$PM10_CENTENARIO,order.by = calidad_de_aire_2017$fecha_hora)

summary(pm_xts_centenerio)

plot.xts(pm_xts_centenerio)

#----datos pm10 cordoba----

pm_xts_cordoba <- xts(calidad_de_aire_2017$PM10_CORDOBA,order.by = calidad_de_aire_2017$fecha_hora)

summary(pm_xts_cordoba)

plot.xts(pm_xts_cordoba)

#-----datos pm10 boca-----

pm10_xts_boca <- xts(calidad_de_aire_2017$PM10_LA_BOCA,order.by = calidad_de_aire_2017$fecha_hora)

summary(pm10_xts_boca)

plot.xts(no_xts_boca)

