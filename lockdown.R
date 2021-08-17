#Actualización 20/08/2021

#Análisis de Cuarentenas por comuna en base a datos del Ministerio de Ciencia (@MinCiencia)

library(readr)
library(dplyr)
library(scales)
library(ggplot2)
library(reshape2)
library(chilemapas)
library(leaflet)
library(ggplot2)
library(data.table)
library(tidyr)
library(stringr)
library(lubridate)

## Descarga de datos ---------
# Url para descarga directa de datos desde el Github del Ministerio de Ciencia
url <- "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output"

## Cuarentenas desde 2020/07/28 al día actualizado.
df_pasos <- read_csv(paste(url,"producto74","paso_a_paso_std.csv", sep="/"))
df_pasos$Fecha<-as.Date(df_pasos$Fecha)
df_pasos$dia <- weekdays(as.Date(df_pasos$Fecha))
#new variable dias_cuarentena
df_pasos$cuarentena <- case_when(df_pasos$Paso==1 ~ 1,
                                 df_pasos$Paso==2 & df_pasos$dia=="Saturday" ~ 1,
                                 df_pasos$Paso==2 & df_pasos$dia=="Sunday" ~ 1,
                                 TRUE ~ 0)
#mantener urbana y rural
df_pasos2 <- df_pasos %>% 
  unite(codigo_comuna, zona, col=codigo_zona, sep="-")

post_27jul <- df_pasos2 %>% 
  group_by(codigo_zona) %>% 
  summarise(cuarentena_post_37jul = sum(cuarentena))

df_pasos3 <- df_pasos2 %>%
  select(codigo_region, comuna_residencia, 
         region_residencia, codigo_zona)  %>%
  group_by(codigo_zona) %>%
  arrange(codigo_region) %>%
  filter(row_number()==1)

post_27jul <- merge(post_27jul, df_pasos3, by="codigo_zona")
post_27jul <- post_27jul %>% 
  separate(codigo_zona, into = c("codigo_comuna", "zona"), sep = "-") 

## Cuarentenas desde 2021/03 hasta 2020/07/28 
pre_27jul <- read_csv(paste(url,"producto29","Cuarentenas-Totales.csv", sep="/"))
names(pre_27jul) <- names(pre_27jul) %>% str_to_lower() %>% str_replace_all(" ","_")
pre_27jul$fecha_de_inicio <- as.Date(pre_27jul$fecha_de_inicio)
pre_27jul$fecha_de_término <- as.Date(pre_27jul$fecha_de_término)

# cortar en fecha 2020/07/27 
pre_27jul$fecha_de_termino2 <- ifelse(pre_27jul$fecha_de_término>"2020-07-27",
                                      date("2020-07-27"),
                                      pre_27jul$fecha_de_término)

pre_27jul$fecha_termino <- as.Date(pre_27jul$fecha_de_termino2, 
                                   origin = "1970-01-01")
#filtrar fecha_inicio > 2020/07/27 
pre_27jul <- pre_27jul %>% filter(fecha_de_inicio<"2020/07/27")
#calcular dias en cuarentena
pre_27jul$dias <- as.Date(pre_27jul$fecha_termino) - 
  as.Date(pre_27jul$fecha_de_inicio)

pre_27jul$dias <- as.numeric(pre_27jul$dias, units="days")

## Unir fechas
pre_27jul <- pre_27jul %>% 
  group_by(código_cut_comuna) %>% 
  summarise(cuarentena_pre_37jul = sum(dias))

df_cuarentena <- merge(post_27jul, pre_27jul,
                       by.x="codigo_comuna",
                       by.y= "código_cut_comuna", 
                       all = T)

df_cuarentena$cuarentena_pre_37jul[is.na(df_cuarentena$cuarentena_pre_37jul)] = 0
#crear variable de cuarentena agregada
df_cuarentena$cuarentena_agg <- df_cuarentena$cuarentena_pre_37jul + 
  df_cuarentena$cuarentena_post_37jul

rm(url)

##Exportar a .dta
library(foreign)
write.dta(df_cuarentena, "df_cuarentena.dta")

