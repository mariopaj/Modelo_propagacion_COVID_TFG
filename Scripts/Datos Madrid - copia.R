library(readr)
library(tidyverse)
library(tseries)
library(extrafont)
library(forecast)
library(lubridate)

####### esta es la base de datos española
#sacada de https://github.com/datadista/datasets/tree/master/COVID%2019
ccaa_casos<-as.data.frame(read_csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_casos.csv"))
ccaa_casos_long<-as.data.frame(read_csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_casos_long.csv"))
ccaa_death<-as.data.frame(read_csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_fallecidos.csv"))
ccaa_death_long<-as.data.frame(read_csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_fallecidos_long.csv"))
ccaa_ucis<-as.data.frame(read_csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_uci.csv"))
ccaa_ucis_long<-as.data.frame(read_csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_uci_long.csv"))
ccaa_altas<-as.data.frame(read_csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_altas.csv"))
ccaa_altas_long<-as.data.frame(read_csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_altas_long.csv"))
ccaa_hospit<-as.data.frame(read_csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_hospitalizados.csv"))
ccaa_hospit_long<-as.data.frame(read_csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_hospitalizados_long.csv"))
ccaa_mascar<-as.data.frame(read_csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_mascarillas.csv"))
nacio_todo<-as.data.frame(read_csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/nacional_covid19.csv"))
nacio_edades<-as.data.frame(read_csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/nacional_covid19_rango_edad.csv"))
ccaa_camas<-as.data.frame(read_csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_camas_uci_2017.csv"))
muni_madrid<-as.data.frame(read_csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/municipios_distritos_madrid_casos.csv"))

kaka<-as.data.frame(read_csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_datos_sanidad_nueva_serie.csv"))
tests <- as.data.frame(read.csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_test_realizados.csv", encoding = "UTF-8"))
datos_madrid <- kaka %>% filter(CCAA == "Madrid")
tail(datos_madrid)

datos_madrid_tests <- tests %>% filter(CCAA == "Madrid")
tail(datos_madrid_tests)

max(datos_madrid$Casos) #7738
max(datos_madrid$Hospitalizados) #1930

### Datos test semanales en la C.Madrid

colnames(datos_madrid_tests)[1] <- "Fecha"
datos_madrid_tests <- datos_madrid_tests %>% filter(Fecha > "2020-07-05") %>% select(Fecha, Total_Pruebas_x_1000hab.)
datos_madrid_tests$Nº_semana <- 1:18
class(datos_madrid_tests$Total_Pruebas_x_1000hab.)
datos_madrid_tests$Total_Pruebas_x_1000hab. <- sub(",",".",datos_madrid_tests$Total_Pruebas_x_1000hab.)
datos_madrid_tests <- transform(datos_madrid_tests,Total_Pruebas_x_1000hab.= as.numeric(Total_Pruebas_x_1000hab.))

### Representación nº de test

ggplot(datos_madrid_tests,aes(x = Nº_semana, y = Total_Pruebas_x_1000hab.)) + 
  geom_line() + theme_elegante() + 
  theme (plot.title = element_text (face = "bold")) +
  labs(x = NULL, y = NULL,
       title = "Total Pruebas x 1000 habitantes",
       subtitle = "Datos semanales de la Comunidad de Madrid (Jul 2020 - Nov 2020)",
       caption = "\n Fuente: Datos recopilados de Datadista a través de Github")
 

### Datos semanales en la C.Madrid
datos_madrid_2020 <- datos_madrid %>% filter(Fecha > "2020-09-06" & Fecha < "2021-04-05")
tail(datos_madrid_2020)


datos_madrid_sem_2020 <- datos_madrid_2020 %>% mutate(year = format(Fecha, "%Y"),
                                            week_num = format(Fecha,"%W"),
                                            mes = format(Fecha, "%B")) 

#sustituimos los valores de la columna week  
datos_madrid_sem_2020$week_num[datos_madrid_sem_2020$week_num == "00"]<- "52"       
datos_madrid_sem_2020$week_num[datos_madrid_sem_2020$week_num == "01"]<- "53"       
datos_madrid_sem_2020$week_num[datos_madrid_sem_2020$week_num == "02"]<- "54"       
datos_madrid_sem_2020$week_num[datos_madrid_sem_2020$week_num == "03"]<- "55"       
datos_madrid_sem_2020$week_num[datos_madrid_sem_2020$week_num == "04"]<- "56" 
datos_madrid_sem_2020$week_num[datos_madrid_sem_2020$week_num == "05"]<- "57"       
datos_madrid_sem_2020$week_num[datos_madrid_sem_2020$week_num == "06"]<- "58"       
datos_madrid_sem_2020$week_num[datos_madrid_sem_2020$week_num == "07"]<- "59"       
datos_madrid_sem_2020$week_num[datos_madrid_sem_2020$week_num == "08"]<- "60"       
datos_madrid_sem_2020$week_num[datos_madrid_sem_2020$week_num == "09"]<- "61"       
datos_madrid_sem_2020$week_num[datos_madrid_sem_2020$week_num == "10"]<- "62"       
datos_madrid_sem_2020$week_num[datos_madrid_sem_2020$week_num == "11"]<- "63"       
datos_madrid_sem_2020$week_num[datos_madrid_sem_2020$week_num == "12"]<- "64"       
datos_madrid_sem_2020$week_num[datos_madrid_sem_2020$week_num == "13"]<- "65"       

datos_madrid_sem_2020 <- datos_madrid_sem_2020 %>% group_by(as.double(week_num)) %>%
                                                  summarise(Casos = sum(Casos), 
                                                  Fallecidos = sum(Fallecidos),
                                                  Hospitalizados = sum(Hospitalizados))


datos_madrid_sem_2020 <- datos_madrid_sem_2020 %>% mutate(prop_hosp = Hospitalizados/Casos,
                                            prop_def = Fallecidos/Casos,
                                            prop_rec = 1 - (Fallecidos/Hospitalizados))


datos_madrid_sem_2020 <- datos_madrid_sem_2020 %>% mutate( mean_prop_hosp = mean(prop_hosp),
                                                           mean_prop_def = mean(prop_def),
                                                           mean_prop_rec = mean(prop_rec))

colnames(datos_madrid_sem_2020)[1] <- "week"

datos_madrid_sem_2020$Nº_semana <- 1:30

# Elaboramos un modelo para ver como explica el número de test la variación de las proporciones

datos_madrid_sem_2020_2 <- datos_madrid_sem_2020 %>% filter(Nº_semana < 19)

prop_hosp <- datos_madrid_sem_2020_2[[5]]
prop_def <- datos_madrid_sem_2020_2[[6]]
prop_rec <- datos_madrid_sem_2020_2[[7]]
test <- datos_madrid_tests[[2]]

lm

# Proporción diaria de infectados que son hospitalizados semanales

ggplot(datos_madrid_sem_2020, aes(x = Nº_semana, y = prop_hosp)) + 
  geom_path() + theme_elegante() +
  geom_line(aes(y = mean_prop_hosp), col = "red", size = 0.8, linetype = 2) + 
  theme (plot.title = element_text (face = "bold")) +
  labs(x = NULL,y = NULL, 
       title = "Proporción de infectados que son hospitalizados",
       subtitle = "Datos semanales de la Comunidad de Madrid (Sept 2020- Mar 2021)",
       caption = "\n Fuente: Datos recopilados de Datadista a través de Github")

prop_hosp <- pull(datos_madrid_sem_2020, prop_hosp)
class(prop_hosp)

adf.test(prop_hosp, alternative="stationary", k=0) # No se rechaza la hipótesis nula de no estacionariedad en media


# Proporción diaria de infectados que mueren

ggplot(datos_madrid_sem_2020, aes(x = Nº_semana, y = prop_def)) + 
  geom_line() + theme_elegante() +
  geom_line(aes(y = mean_prop_def), col = "red", size = 0.8, linetype = 2) +
  theme (plot.title = element_text (face = "bold")) +
  labs(x = NULL,y = NULL, 
       title = "Proporción de infectados que fallecen",
       subtitle = "Datos semanales de la Comunidad de Madrid (Sept 2020- Mar 2021)",
       caption = "\n Fuente: Datos recopilados de Datadista a través de Github")

prop_def <- pull(datos_madrid_sem_2020, prop_def)
class(prop_def)

adf.test(prop_def, alternative="stationary", k=0) # No se rechaza la hipótesis nula de no estacionariedad en media

# Proporción diaria de infectados que se recuperan

ggplot(datos_madrid_sem_2020, aes(x = Nº_semana, y = prop_rec)) + 
   geom_line() + theme_elegante() +
  geom_line(aes(y = mean_prop_rec), col = "red", size = 0.8, linetype = 2) + 
  theme (plot.title = element_text (face = "bold")) +
  labs(x = NULL,y = NULL, 
       title = "Proporción de hospitalizados que se recuperan",
       subtitle = "Datos semanales de la Comunidad de Madrid (Sept 2020- Mar 2021)",
       caption = "\n Fuente: Datos recopilados de Datadista a través de Github") 
  

prop_rec <- pull(datos_madrid_sem_2020, prop_rec)
class(prop_rec)

adf.test(prop_rec, alternative="stationary", k=0) # No se rechaza la hipótesis nula de no estacionariedad en media

#### Porcentaje de muertos por Covid en cada Comunidad autónoma 

kaka_2 <- kaka %>% group_by(CCAA) %>%
          summarise(Casos = sum(Casos),
                    Fallecidos = sum(Fallecidos),
                    Fallecidos_percent = (Fallecidos/Casos)*100)


kaka_2 <- kaka_2[c(-19, -11, -16, -4, -6),]
kaka_2$num <- 1:15

ggplot(kaka_2, aes(x = num, y = Fallecidos_percent)) + 
  geom_bar(stat = "identity", aes(fill= CCAA)) + 
  theme_elegante() + xlab("CCAA") + ylab("(%) Fallecidos por Covid-19")

kaka_3 <- kaka %>% filter(Fecha > "2020-07-05") %>% 
          group_by(CCAA) %>%
          summarise(Casos = sum(Casos),
                    Fallecidos = sum(Fallecidos),
                    Fallecidos_percent = (Fallecidos/Casos)*100)

kaka_3 <- kaka_3[c(-19, -11, -16, -4, -6),]

#### Función para hacer gráficos más refinados

theme_elegante <- function(base_size = 10,
                           base_family = "Raleway"
)
{
  color.background = "#FFFFFF" # Chart Background
  color.grid.major = "#D9D9D9" # Chart Gridlines
  color.axis.text = "#666666" # 
  color.axis.title = "#666666" # 
  color.title = "#666666"
  color.subtitle = "#666666"
  strip.background.color = '#9999CC'
  ret <-
    theme_bw(base_size=base_size) +
    
    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major,size=.55, linetype="dotted")) +
    theme(panel.grid.minor=element_line(color=color.grid.major,size=.55, linetype="dotted")) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=base_size-3,color=color.axis.title, family = base_family)) +
    
    theme(strip.text.x = element_text(size=base_size,color=color.background, family = base_family)) +
    theme(strip.text.y = element_text(size=base_size,color=color.background, family = base_family)) +
    #theme(strip.background = element_rect(fill=strip.background.color, linetype="blank")) +
    theme(strip.background = element_rect(fill = "grey70", colour = NA)) +
    # theme(panel.border= element_rect(fill = NA, colour = "grey70", size = rel(1)))+
    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, 
                                  size=20, 
                                  vjust=1.25, 
                                  family=base_family, 
                                  hjust = 0.5
    )) +
    
    theme(plot.subtitle=element_text(color=color.subtitle, size=base_size+2, family = base_family,  hjust = 0.5))  +
    
    theme(axis.text.x=element_text(size=base_size,color=color.axis.text, family = base_family)) +
    theme(axis.text.y=element_text(size=base_size,color=color.axis.text, family = base_family)) +
    theme(text=element_text(size=base_size, color=color.axis.text, family = base_family)) +
    
    theme(axis.title.x=element_text(size=base_size+2,color=color.axis.title, vjust=0, family = base_family)) +
    theme(axis.title.y=element_text(size=base_size+2,color=color.axis.title, vjust=1.25, family = base_family)) +
    theme(plot.caption=element_text(size=base_size-2,color=color.axis.title, vjust=1.25, family = base_family)) +
    
    # Legend  
    theme(legend.text=element_text(size=base_size,color=color.axis.text, family = base_family)) +
    theme(legend.title=element_text(size=base_size,color=color.axis.text, family = base_family)) +
    theme(legend.key=element_rect(colour = color.background, fill = color.background)) +
    theme(legend.position="bottom", 
          legend.box = "horizontal", 
          legend.title = element_blank(),
          legend.key.width = unit(.75, "cm"),
          legend.key.height = unit(.75, "cm"),
          legend.spacing.x = unit(.25, 'cm'),
          legend.spacing.y = unit(.25, 'cm'),
          legend.margin = margin(t=0, r=0, b=0, l=0, unit="cm")) +
    
    # Plot margins
    theme(plot.margin = unit(c(.5, .5, .5, .5), "cm"))
  
  ret
}