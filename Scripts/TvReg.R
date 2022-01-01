library(tidyverse)
library(tvReg)
library(forecast)
library(dynlm)
library(EpiEstim)
library(lubridate)
library(readr)
library(readxl)
library(openxlsx)

kaka <- as.data.frame(read_csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_datos_sanidad_nueva_serie.csv"))

### Datos diarios

mad_data <- kaka %>% filter(CCAA == "Madrid") %>% select(Fecha, Casos, Fallecidos, Hospitalizados)
mad_infects <- mad_data %>% select(Fecha, Casos) %>% filter(Fecha > " 2020-02-24")
names(mad_infects)
mad_infects$Fecha <- format(mad_infects$Fecha, "%d/%m/%Y")

write_csv(mad_infects, "infects.csv")

plot(mad_infects)
Mad_infects <- ts(mad_infects$Casos, start = c(2020, 1), frequency = 365)
plot(Mad_infects, main = "Infectados por Covid-19 Madrid", col = "blue")

### Datos semanales

mad_data_sem <- mad_data %>% filter(Fecha > "2020-04-05" & Fecha < "2021-04-05") %>% 
                             mutate(year = format(Fecha, "%Y"),
                                    week_num = format(Fecha,"%W"),
                                    mes = format(Fecha, "%B")) 

mad_data_sem$week_num[mad_data_sem$week_num == "00"]<- "52"       
mad_data_sem$week_num[mad_data_sem$week_num == "01"]<- "53"
mad_data_sem$week_num[mad_data_sem$week_num == "02"]<- "54"
mad_data_sem$week_num[mad_data_sem$week_num == "03"]<- "55"
mad_data_sem$week_num[mad_data_sem$week_num == "04"]<- "56"
mad_data_sem$week_num[mad_data_sem$week_num == "05"]<- "57"
mad_data_sem$week_num[mad_data_sem$week_num == "06"]<- "58"
mad_data_sem$week_num[mad_data_sem$week_num == "07"]<- "59"
mad_data_sem$week_num[mad_data_sem$week_num == "08"]<- "60"
mad_data_sem$week_num[mad_data_sem$week_num == "09"]<- "61"
mad_data_sem$week_num[mad_data_sem$week_num == "10"]<- "62"
mad_data_sem$week_num[mad_data_sem$week_num == "11"]<- "63"
mad_data_sem$week_num[mad_data_sem$week_num == "12"]<- "64"
mad_data_sem$week_num[mad_data_sem$week_num == "13"]<- "65"

mad_data_sem <- mad_data_sem %>% group_by(as.double(week_num)) %>%
                                 summarise(Casos = sum(Casos),
                                           Fallecidos = sum(Fallecidos),
                                           Hospitalizados = sum(Hospitalizados)) 
colnames(mad_data_sem)[1] <- "week_num"

mad_infects_sem <- mad_data_sem %>% select(week_num, Casos)

mad_data_sem ## Datos semanales desde el 5 de abril de 2020 hasta el 5 de abril de 2021


mad_infects_sem <- mad_data_sem %>% select(Fecha, casos)

Mad_infects_sem <- ts(mad_infects_sem$Casos)

plot(mad_infects_sem)

## Modelo autorregresivo de orden 1 (datos diarios)

modelo1 <- dynlm(Mad_infects ~ L(Mad_infects), data = mad_infects)
summary(modelo1)

## Modelo autorregresivo de orden 3 (datos diarios)

modelo2 <- dynlm(Mad_infects ~ L(Mad_infects, 1:3), data = mad_infects)
summary(modelo2)

## Variable tendencia

tend <- seq_along(Mad_infects)
tend

## Metemos la tendencia al modleo anterior (datos diarios)

modelo3 <- dynlm(Mad_infects ~ L(Mad_infects, 1:3) + tend, data = mad_infects)
summary(modelo3)

### Estimación de coeficientes método tvAR

modeloAR1 <- tvAR(Mad_infects, p = 1, type = "const", est = "lc") # 1 retardo
summary(modeloAR1)
plot(modeloAR1$coefficients)

modeloAR2 <- tvAR(Mad_infects_sem, p = 1, type = "const", est = "lc")
summary(modeloAR2)
plot(modeloAR2$coefficients)

### Estimación con EpiEstim según los datos que nos proporciona la OMS sobre el covid mean = 4.8 y std = 2,3

covid_mad_parametric <- estimate_R(mad_infects$Casos, method = "parametric_si", 
                                  config = make_config(list(
                                    mean_si = 4.8, 
                                    std_si = 2.3)))

summary(covid_mad_parametric)
plot(covid_mad_parametric) # Muestra la serie de tiempo de incidencia, el número de reproducción estimado (media posterior e intervalo creíble del 95%, con estimaciones para una ventana de tiempo trazada al final de la ventana de tiempo) y la distribución discreta del intervalo de la serie.

Ro <- covid_mad_parametric$R

mean_Ro <- as.data.frame(Ro$`Mean(R)`)
mean_Ro$Time <- 1:400
mean_Ro$normal_Ro <- 1
colnames(mean_Ro)[1] <- "mean_Ro"

## Representamos la evolución del parámetro Ro

ggplot(mean_Ro, aes(x = Time, y = mean_Ro)) + 
  geom_path() + theme_elegante() +
  geom_line(aes(y = normal_Ro), col = "red", size = 0.8, linetype = 2) + 
  theme(plot.title = element_text (face = "bold")) +
  labs(x = "Tiempo", y = "mean(Ro)", 
       title = "Estimación del parámetro Ro durante el tiempo",
       subtitle = "Datos de la Comunidad de Madrid (Feb 2020 - Marz 2021)",
       caption = "\n Fuente: Elaboración propia")



      




