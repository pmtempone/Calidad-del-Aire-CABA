library(readr)
library(ggplot2)

calidad_aire_2017 <- read_delim("/Volumes/Disco_SD/Set de datos/calidad-aire-2017.csv",
";", escape_double = FALSE, locale = locale(decimal_mark = ",",
grouping_mark = "."), trim_ws = TRUE)


