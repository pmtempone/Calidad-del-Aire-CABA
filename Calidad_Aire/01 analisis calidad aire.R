library(ggplot2)
library(sqldf)
library(Rcpp)
library(xlsx)

aire09 <- read.csv("calidad-aire-2009.csv",sep = ";")
aire10 <- read.csv("calidad-aire-2010.csv",sep = ";")
aire11 <- read.csv("calidad-aire-2011.csv",sep = ";")
aire12 <- read.csv("calidad-aire-2012.csv",sep = ";")
aire13 <- read.csv("calidad-aire-2013.csv",sep = ";")
aire14 <- read.csv("calidad-aire-2014.csv",sep = ";")
aire15 <- read.csv("calidad-aire-2015.csv",sep = ";")
contaminantes <- read.csv("contaminantes.csv",sep = ";")
estaciones <- read.csv("estaciones.csv",sep = ";")
niveles <- read.csv("niveles.csv",sep = ";")
observaciones <- read.csv("observaciones.csv",sep = ";")
aire14 <- read.csv("calidad-aire-2014.csv",sep = ";")
aire <- merge(aire09,aire10,aire11,aire12,aire13,aire14,aire15)
aire <- rbind(aire09,aire10,aire11,aire12,aire13,aire14,aire15)
aire14 <- read.csv("calidad-aire-2014.csv",sep = ";")
aire <- rbind(aire09,aire10,aire11,aire12,aire13,aire14,aire15)
aire[,"valor"] <- is.numeric(aire$valor)
head(aire)
aire <- rbind(aire09,aire10,aire11,aire12,aire13,aire14,aire15)
aire[,"valor"] <- as.numeric(aire$valor)
head(aire)
aire <- rbind(aire09,aire10,aire11,aire12,aire13,aire14,aire15)
aire[,"valor"] <- as.character(aire$valor)
aire[,"valor"] <- as.numeric(aire$valor)
hist(aire$valor)

qplot(aire$valor)
p <- ggplot(aire, aes(x=valor, y=value)) +
geom_histogram(stat="identity") +
facet_wrap(~contaminante, ncol=2)
p
print(p)
p <- ggplot(aire, aes(x=valor)) +
geom_histogram(stat="identity") +
facet_wrap(~contaminante, ncol=2)
p
p <- ggplot(aire, aes(x=valor)) +
geom_histogram(stat="identity") +
facet_wrap(~contaminante)

p
ggplot(data=aire,aes(aire$valor))+geom_histogram()
ggplot(data=aire,aes(x=aire$valor,fill=contaminete))+geom_histogram(binwidth=.5, alpha=.5, position="identity")
ggplot(data=aire,aes(x=aire$valor,fill=contaminante))+geom_histogram(binwidth=.5, alpha=.5, position="identity")
ggplot(data=aire,aes(x=aire$valor,fill=contaminante))+geom_histogram(alpha=.5, position="identity")


ggplot(data=aire,aes(x=aire$valor,fill=contaminante))+geom_histogram(binwidth=5, alpha=.5, position="identity")
ggplot(data=aire,aes(x=aire$valor,fill=contaminante))+geom_histogram(alpha=.5, position="identity")
p
ggplot(data=aire,aes(aire$valor))+geom_histogram()


ggplot(data=aire,aes(aire$valor))+geom_histogram()
ggplot(data=aire,aes(x=aire$valor,fill=contaminante))+geom_histogram(alpha=.5, position="identity")
ggplot(data=aire,aes(x=aire$valor,fill=contaminante))+geom_histogram(alpha=.5, position="identity")+facet_grid(estacion~.)
unique(toupper(aire$estacion)
)
aire$estacion <- toupper(aire$estacion)
ggplot(data=aire,aes(x=aire$valor,fill=contaminante))+geom_histogram(alpha=.5, position="identity")+facet_grid(estacion~.)
subset(aire,aire$fecha>"01/01/2014")
strtrim(c("abcdef", "abcdef", "abcdef"), c(1,5,10))
strtrim(aire$fecha,-4)
strtrim(aire$fecha,6,4)
substr(aire$estacion,6,4)
unique(substr(aire$fecha,6))
unique(substr(aire$fecha,6,10))
unique(substr(aire$fecha,7,10))
subset(aire,unique(substr(aire$fecha,7,10))="2014")
subset(aire,substr(aire$fecha,7,10)="2014")
subset(aire,substr(aire$fecha,7,10)=="2014")
ggplot(data=subset(aire,substr(aire$fecha,7,10)=="2014"),aes(x=aire$valor,fill=contaminante))+geom_histogram(alpha=.5, position="identity")+facet_grid(estacion~.)
ggplot(data=subset(aire,substr(aire$fecha,7,10)=="2014"),aes(x=valor,fill=contaminante))+geom_histogram(alpha=.5, position="identity")+facet_grid(estacion~.)
ggplot(data=aire15,aes(x=valor,fill=contaminante))+geom_histogram(alpha=.5, position="identity")+facet_grid(estacion~.)
aire15[,"valor"] <- as.numeric(aire15$valor)
ggplot(data=aire15,aes(x=valor,fill=contaminante))+geom_histogram(alpha=.5, position="identity")+facet_grid(estacion~.)

niveles
unique(niveles$efecto)
niveles_daninos <- subset(niveles,efecto=="Ninguno")


niveles_daninos
niveles
View(niveles_daninos)
View(niveles_daninos)

sqldf("select * from niveles where efecto not in ('Ninguna')")
View(niveles)
View(niveles)
sqldf("select * from niveles where efecto not in ('Ninguno')")
niveles_efectos <- sqldf("select * from niveles where efecto not in ('Ninguno')")
View(niveles_efectos)
niveles_efectos$efecto
View(aire15)
View(aire15)
View(aire)
library(sqldf)
sqldf("select 	*,
case when CONTAMINANTE='CO' AND (valor>=7 and valor<9) then 3
when  CONTAMINANTE='CO' AND (valor>=9 and valor<13) then 4
when  CONTAMINANTE='CO' AND (valor>=13 and valor<16) then 5
when  CONTAMINANTE='CO' AND (valor>=16 and valor<31) then 6
when  CONTAMINANTE='CO' AND (valor>=30 and valor<10000) then 7
when  CONTAMINANTE='NO2' AND (valor>=200 and valor<276) then 18
when  CONTAMINANTE='NO2' AND (valor>=276 and valor<650) then 19
when  CONTAMINANTE='NO2' AND (valor>=650 and valor<1240) then 22
when  CONTAMINANTE='NO2' AND (valor>=1240 and valor<9999) then 21
when  CONTAMINANTE='PM10' AND (valor>=151 and valor<255) then 26
when  CONTAMINANTE='PM10' AND (valor>=255 and valor<355) then 27
when  CONTAMINANTE='PM10' AND (valor>=355 and valor<425) then 28
when  CONTAMINANTE='PM10' AND (valor>=425 and valor<999) then 26
from aire
")
sqldf("select 	*,
case when CONTAMINANTE='CO' AND (valor>=7 and valor<9) then 3
when  CONTAMINANTE='CO' AND (valor>=9 and valor<13) then 4
when  CONTAMINANTE='CO' AND (valor>=13 and valor<16) then 5
when  CONTAMINANTE='CO' AND (valor>=16 and valor<31) then 6
when  CONTAMINANTE='CO' AND (valor>=30 and valor<10000) then 7
when  CONTAMINANTE='NO2' AND (valor>=200 and valor<276) then 18
when  CONTAMINANTE='NO2' AND (valor>=276 and valor<650) then 19
when  CONTAMINANTE='NO2' AND (valor>=650 and valor<1240) then 22
when  CONTAMINANTE='NO2' AND (valor>=1240 and valor<9999) then 21
when  CONTAMINANTE='PM10' AND (valor>=151 and valor<255) then 26
when  CONTAMINANTE='PM10' AND (valor>=255 and valor<355) then 27
when  CONTAMINANTE='PM10' AND (valor>=355 and valor<425) then 28
when  CONTAMINANTE='PM10' AND (valor>=425 and valor<999) then 26
from aire
")
sqldf("select 	*,
case when CONTAMINANTE='CO' AND (valor>=7 and valor<9) then 3
when  CONTAMINANTE='CO' AND (valor>=9 and valor<13) then 4
when  CONTAMINANTE='CO' AND (valor>=13 and valor<16) then 5
when  CONTAMINANTE='CO' AND (valor>=16 and valor<31) then 6
when  CONTAMINANTE='CO' AND (valor>=30 and valor<10000) then 7
when  CONTAMINANTE='NO2' AND (valor>=200 and valor<276) then 18
when  CONTAMINANTE='NO2' AND (valor>=276 and valor<650) then 19
when  CONTAMINANTE='NO2' AND (valor>=650 and valor<1240) then 22
when  CONTAMINANTE='NO2' AND (valor>=1240 and valor<9999) then 21
when  CONTAMINANTE='PM10' AND (valor>=151 and valor<255) then 26
when  CONTAMINANTE='PM10' AND (valor>=255 and valor<355) then 27
when  CONTAMINANTE='PM10' AND (valor>=355 and valor<425) then 28
when  CONTAMINANTE='PM10' AND (valor>=425 and valor<999) then 26
End from aire
")
aire_contaminado <- sqldf("select 	*,
case when CONTAMINANTE='CO' AND (valor>=7 and valor<9) then 3
when  CONTAMINANTE='CO' AND (valor>=9 and valor<13) then 4
when  CONTAMINANTE='CO' AND (valor>=13 and valor<16) then 5
when  CONTAMINANTE='CO' AND (valor>=16 and valor<31) then 6
when  CONTAMINANTE='CO' AND (valor>=30 and valor<10000) then 7
when  CONTAMINANTE='NO2' AND (valor>=200 and valor<276) then 18
when  CONTAMINANTE='NO2' AND (valor>=276 and valor<650) then 19
when  CONTAMINANTE='NO2' AND (valor>=650 and valor<1240) then 22
when  CONTAMINANTE='NO2' AND (valor>=1240 and valor<9999) then 21
when  CONTAMINANTE='PM10' AND (valor>=151 and valor<255) then 26
when  CONTAMINANTE='PM10' AND (valor>=255 and valor<355) then 27
when  CONTAMINANTE='PM10' AND (valor>=355 and valor<425) then 28
when  CONTAMINANTE='PM10' AND (valor>=425 and valor<999) then 26
End from aire
")
View(aire_contaminado)
View(aire_contaminado)
aire_contaminado <- sqldf("select 	*,
case when CONTAMINANTE='CO' AND (valor>=7 and valor<9) then 3
when  CONTAMINANTE='CO' AND (valor>=9 and valor<13) then 4
when  CONTAMINANTE='CO' AND (valor>=13 and valor<16) then 5
when  CONTAMINANTE='CO' AND (valor>=16 and valor<31) then 6
when  CONTAMINANTE='CO' AND (valor>=30 and valor<10000) then 7
when  CONTAMINANTE='NO2' AND (valor>=200 and valor<276) then 18
when  CONTAMINANTE='NO2' AND (valor>=276 and valor<650) then 19
when  CONTAMINANTE='NO2' AND (valor>=650 and valor<1240) then 22
when  CONTAMINANTE='NO2' AND (valor>=1240 and valor<9999) then 21
when  CONTAMINANTE='PM10' AND (valor>=151 and valor<255) then 26
when  CONTAMINANTE='PM10' AND (valor>=255 and valor<355) then 27
when  CONTAMINANTE='PM10' AND (valor>=355 and valor<425) then 28
when  CONTAMINANTE='PM10' AND (valor>=425 and valor<999) then 26
End from aire
")
aire_contaminado <- sqldf("select 	*,
case when CONTAMINANTE='CO' AND (valor>=7 and valor<9) then 3
when  CONTAMINANTE='CO' AND (valor>=9 and valor<13) then 4
when  CONTAMINANTE='CO' AND (valor>=13 and valor<16) then 5
when  CONTAMINANTE='CO' AND (valor>=16 and valor<31) then 6
when  CONTAMINANTE='CO' AND (valor>=30 and valor<10000) then 7
when  CONTAMINANTE='NO2' AND (valor>=200 and valor<276) then 18
when  CONTAMINANTE='NO2' AND (valor>=276 and valor<650) then 19
when  CONTAMINANTE='NO2' AND (valor>=650 and valor<1240) then 22
when  CONTAMINANTE='NO2' AND (valor>=1240 and valor<9999) then 21
when  CONTAMINANTE='PM10' AND (valor>=151 and valor<255) then 26
when  CONTAMINANTE='PM10' AND (valor>=255 and valor<355) then 27
when  CONTAMINANTE='PM10' AND (valor>=355 and valor<425) then 28
when  CONTAMINANTE='PM10' AND (valor>=425 and valor<999) then 26
else 0 End id_nivel from aire
")
aire_contaminado_filtrado <- sqldf("select * from aire_contaminado where id_nivel<>0")
aire_contaminado_filtrado
View(aire_contaminado_filtrado)
View(aire_contaminado_filtrado)
aire_contaminado15 <- sqldf("select 	*,
case when CONTAMINANTE='CO' AND (valor>=7 and valor<9) then 3
when  CONTAMINANTE='CO' AND (valor>=9 and valor<13) then 4
when  CONTAMINANTE='CO' AND (valor>=13 and valor<16) then 5
when  CONTAMINANTE='CO' AND (valor>=16 and valor<31) then 6
when  CONTAMINANTE='CO' AND (valor>=30 and valor<10000) then 7
when  CONTAMINANTE='NO2' AND (valor>=200 and valor<276) then 18
when  CONTAMINANTE='NO2' AND (valor>=276 and valor<650) then 19
when  CONTAMINANTE='NO2' AND (valor>=650 and valor<1240) then 22
when  CONTAMINANTE='NO2' AND (valor>=1240 and valor<9999) then 21
when  CONTAMINANTE='PM10' AND (valor>=151 and valor<255) then 26
when  CONTAMINANTE='PM10' AND (valor>=255 and valor<355) then 27
when  CONTAMINANTE='PM10' AND (valor>=355 and valor<425) then 28
when  CONTAMINANTE='PM10' AND (valor>=425 and valor<999) then 26
else 0 End id_nivel from aire15
")

aire_contaminado_filtrado15 <- sqldf("select * from aire_contaminado15 where id_nivel<>0")
View(aire_contaminado_filtrado15)
View(aire_contaminado_filtrado15)
ggplot(data=aire_contaminado_filtrado15,aes(x=aire_contaminado_filtrado15$valor,fill=contaminante))+geom_histogram(alpha=.5, position="identity")+facet_grid(estacion~.)

ggplot(data=aire_contaminado_filtrado15,aes(x=aire_contaminado_filtrado15$valor,fill=contaminante))+geom_histogram(alpha=.5, position="identity")+facet_grid(estacion~.)
ggplot(data=aire_contaminado_filtrado15,aes(x=aire_contaminado_filtrado15$valor,fill=contaminante))+geom_histogram(alpha=.5, position="identity")+facet_grid(estacion+id_nivel~.)
ggplot(data=aire_contaminado_filtrado15,aes(x=aire_contaminado_filtrado15$valor,fill=id_nivel))+geom_histogram(alpha=.5, position="identity")+facet_grid(estacion~.)
ggplot(data=aire_contaminado_filtrado15,aes(x=aire_contaminado_filtrado15$valor,fill=id_nivel))+geom_histogram(alpha=.5, position="identity")+facet_grid(estacion~.)
ggplot(data=aire_contaminado_filtrado15,aes(x=aire_contaminado_filtrado15$valor,fill=aire_contaminado_filtrado15$id_nivel))+geom_histogram(alpha=.5, position="identity")+facet_grid(estacion~.)
ggplot(data=aire_contaminado_filtrado15,aes(x=aire_contaminado_filtrado15$valor,fill=contaminante))+geom_histogram(alpha=.5, position="identity")+facet_grid(estacion~.)
niveles_efectos
niveles_efectos[1,4,]

niveles_efectos[1-5,]
niveles_efectos[,1]
niveles_efectos[,1-5]
niveles_efectos[,c(1,5)]

boxplot(aire_contaminado_filtrado15$valor,aire_contaminado_filtrado15$estacion)
ggplot(aire_contaminado_filtrado15, aes(x=estacion, y=valor, col=estacion)) +
geom_boxplot()
ggplot(aire_contaminado_filtrado15, aes(x=aire_contaminado_filtrado15$estacion, y=valor, col=estacion)) +
geom_boxplot()
ggplot(aire_contaminado_filtrado15, aes(x=contaminante, y=valor, col=contaminante)) +
geom_boxplot()


ggplot(data=aire_contaminado_filtrado15,aes(x=aire_contaminado_filtrado15$valor,fill=contaminante))+geom_histogram(alpha=.5, position="identity")+facet_grid(estacion~.)

ggplot(data=aire_contaminado_filtrado15,aes(x=aire_contaminado_filtrado15$valor,fill=contaminante))+geom_histogram(alpha=.5, position="identity")+facet_grid(estacion~.)
