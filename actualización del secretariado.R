library(dplyr)      # Manipulación de datos o funciones anidadas
library(tidyverse)  # Paqueterías de tidy
library(ggplot2)    # Generación de gráficos 
library(esquisse)   # Interfaz para generar gráfico de manera muy sencilla
library(readxl)     # Cargar archivos excel
library(mxmaps)     # Generación de mapas hexbin y coropletas
library(tidyr)      # Manejo de datos, en este caso usamos gather
library(plotly)     # Vuelve dinámico a los gráficos
library(ggthemes)   # Añade colores chidos del theme_
library(janitor)    # Transformar las variables a minusculas
library(DT)         # Hacer tablitas sencillas y bonitas
library(scales)     # Permite añadir formato a los numeros como porcentaje, o separación de coma
library(viridis)    # Paleta de colores amigables
library(RColorBrewer) # Paleta de colores

carpetas <- read.csv("IDEFC_NM_sep22.csv", encoding="LATIN1")
carpetas_mun <- read.csv("IDM_NM_sep22.csv", encoding="LATIN1")

victimas <- read.csv("IDVFC_NM_sep22.csv", encoding="LATIN1")



#--------------------------------------------------------------------#

distrito1<- c("Guadalajara", "Zapopan", "Tlaquepaque", "Tonalá", 
              "Tlajomulco de Zúñiga", "Zapotlanejo")

# Limpieza tidy

carpetas_mun%>% 
  filter(Entidad=="Jalisco") %>% 
  gather(Mes, Carpetas, Enero:Diciembre) %>% 
  group_by(Año, Mes, Entidad, Municipio, Subtipo.de.delito) %>% 
  summarise(Carpetas=sum(Carpetas, na.rm = T)) -> carpetas_mun_2


#--------------------------------------------------------------------#
# Carpetas: Violencia familiar

carpetas_mun_2 %>% 
  filter(Entidad=="Jalisco",
         Subtipo.de.delito=="Violencia familiar",
         Año==2022) %>% 
  group_by(Mes, Subtipo.de.delito) %>% 
  summarise(Carpetas=sum(Carpetas, na.rm = T))
            

#distrito 1
carpetas_mun_2 %>% 
  filter(Entidad=="Jalisco",
         Subtipo.de.delito=="Violencia familiar",
         Año==2022,
         Municipio %in% distrito1) %>% 
  group_by(Mes, Subtipo.de.delito) %>% 
  summarise(Carpetas=sum(Carpetas, na.rm = T))


#--------------------------------------------------------------------#
# Carpetas: Abuso sexual infantil

carpetas_mun_2 %>% 
  filter(Entidad=="Jalisco",
         Subtipo.de.delito=="Abuso sexual",
         Año==2022) %>% 
  group_by(Mes, Subtipo.de.delito) %>% 
  summarise(Carpetas=sum(Carpetas, na.rm = T))


#distrito 1
carpetas_mun_2 %>% 
  filter(Entidad=="Jalisco",
         Subtipo.de.delito=="Abuso sexual",
         Año==2022,
         Municipio %in% distrito1) %>% 
  group_by(Mes, Subtipo.de.delito) %>% 
  summarise(Carpetas=sum(Carpetas, na.rm = T))




#--------------------------------------------------------------------#
# Carpetas: violación

carpetas_mun_2 %>% 
  filter(Entidad=="Jalisco",
         Subtipo.de.delito %in% c("Violación simple", "Violación equiparada"),
         Año==2022) %>% 
  group_by(Año) %>% 
  summarise(Carpetas=sum(Carpetas, na.rm = T))


#distrito 1
carpetas_mun_2 %>% 
  filter(Entidad=="Jalisco",
         Subtipo.de.delito %in% c("Violación simple", "Violación equiparada"),
         Año==2022,
         Municipio %in% distrito1) %>% 
  group_by(Mes) %>% 
  summarise(Carpetas=sum(Carpetas, na.rm = T))



#--------------------------------------------------------------------#
# Carpetas: Feminicidio

carpetas_mun_2 %>% 
  filter(Entidad=="Jalisco",
         Subtipo.de.delito=="Feminicidio",
         Año==2022) %>% 
  group_by(Año, Subtipo.de.delito) %>% 
  summarise(Carpetas=sum(Carpetas, na.rm = T))




#--------------------------------------------------------------------#
# Carpetas: Homicidio doloso


victimas %>% 
  filter(Sexo=="Mujer",
         Entidad=="Jalisco",
         Subtipo.de.delito =="Homicidio doloso",
         Año==2022) %>% 
  gather(Mes, Victimas, Enero:Diciembre) %>% 
  group_by(Mes, Subtipo.de.delito) %>% 
  summarise(value=sum(Victimas, na.rm = T)) 

  summarise(Carpetas=sum(Carpetas, na.rm = T))

#distrito 1
carpetas_mun_2 %>% 
  filter(Entidad=="Jalisco",
         Subtipo.de.delito =="Homicidio doloso",
         Año==2022,
         Municipio %in% distrito1) %>% 
  group_by(Mes) %>% 
  summarise(Carpetas=sum(Carpetas, na.rm = T))





####################################################################################################
####################################################################################################
####################################################################################################


#--------------------------------------------------------------------#
# Carpetas: Feminicidio

carpetas_mun_2 %>% 
  filter(Entidad=="Jalisco",
         Subtipo.de.delito=="Feminicidio",
         Año==2022) %>% 
  group_by(Mes, Subtipo.de.delito) %>% 
  summarise(Carpetas=sum(Carpetas, na.rm = T))




# Mapa municipal de feminicidio
carpetas_mun_2 %>% 
  filter(Entidad=="Jalisco",
         Subtipo.de.delito=="Feminicidio",
         Año==2022) %>% 
  group_by(Subtipo.de.delito, Municipio) %>% 
  summarise(value=sum(Carpetas, na.rm = T))->mun_femi


data("df_mxmunicipio_2020")

df_mapa <- merge(df_mxmunicipio_2020, mun_femi, 
                 by.x = "municipio_name", by.y = "Municipio", all.x = TRUE)


df_mapa %>% filter(state_name=="Jalisco")->df_mapa

mxmunicipio_choropleth(df_mapa, num_colors = 1,
                       zoom = subset(df_mapa,state_name %in% 
                                       c("Jalisco"))$region) +
  scale_fill_gradient(
    low = "#e5cbf5", 
    high = "#7f10c2",
    guide = "colourbar")+
  theme_minimal()+
  theme(legend.position = "right",
        text=element_text(size=12),
        plot.title = element_text(size = 18L, hjust = 0), 
        plot.caption = element_text(size = 12L, hjust = 0),
        panel.background = element_rect(fill="white", colour = "white"))



write.csv(df_mapa, "df_mapa.csv")

# Mapa hexagonal nacional: víctimas

victimas%>% 
  filter(Año==2022,
         Entidad=="Jalisco",
         Subtipo.de.delito=="Feminicidio",
         Sexo=="Mujer") %>% 
  gather(Mes, Victimas, Enero:Diciembre) %>% 
  group_by(Año, Rango.de.edad) %>% 
  summarise(value=sum(Victimas, na.rm = T)) 


victimas%>% 
  filter(Año==2022, 
         Subtipo.de.delito=="Feminicidio") %>% 
  gather(Mes, Victimas, Enero:Diciembre) %>% 
  group_by(Año, Entidad, Subtipo.de.delito) %>% 
    summarise(value=sum(Victimas, na.rm = T)) -> victimas_2



data("df_mxstate_2020")

merge(df_mxstate_2020, victimas_2,
      by.x="state_name_official", by.y="Entidad")->vic_femi

mxhexbin_choropleth(vic_femi, num_colors = 1) +  
  scale_fill_gradient(
    low = "#e5cbf5", 
    high = "#7f10c2",
    guide = "colourbar")+
  theme_minimal()

write.csv(vic_femi, "vic_femi.csv")


# Mapa tasa 

vic_femi %>% 
  mutate(value=(value/pop)*100000)->vic_femi



mxhexbin_choropleth(vic_femi, num_colors = 1) +  
  scale_fill_gradient(
    low = "#e5cbf5", 
    high = "#7f10c2",
    guide = "colourbar")+
  theme_minimal()

write.csv(vic_femi, "vic_femi_tasa.csv")


################################################################################
################################################################################
################################################################################

#Víctimas feminicidio

victimas %>% 
  filter(Año==2022,
         Entidad=="Jalisco",
         Subtipo.de.delito=="Feminicidio") %>% 
  gather(Mes, Carpetas, Enero:Diciembre) %>% 
  group_by(Año, Mes, Entidad, Subtipo.de.delito) %>% 
  summarise(Carpetas=sum(Carpetas, na.rm = T)) 
