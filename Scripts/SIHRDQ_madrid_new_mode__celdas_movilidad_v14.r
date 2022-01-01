#####
library(igraph)
library(stringdist)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(ggplot2)
library(grid)
library(gridExtra)
library(readr)
library(haven)
library(stringr)
library(readxl)
library(stringi)
library(readr)
library(SimInf)
library(geosphere)
library(raster)
library(sp)
library(sf)
library(lattice)
library(tmap)
library(rgeos)
library(rgdal)
library(Rfast)
library(animation)
library(tidyverse)
library(openxlsx)
library(xlsx)


mapa<-readOGR("C:/Users/mario/Dropbox/Mario/movilidad/shapefiles_celdas_marzo2020", "celdas_marzo_2020",use_iconv=TRUE,encoding="UTF-8")
CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
mapa_df<-as.data.frame(mapa)

datos<-read_excel("C:/Users/mario/Dropbox/Mario/movilidad/areas_de_movilidad_y_poblacion_a_1_ene_2019.xlsx")
class(datos)
nmad<-grep("Madrid",as.data.frame(datos)$NPRO)
datos_madr<-as.data.frame(datos[nmad,])
class(datos_madr)

cels_madr<-unique(datos_madr$ID_GRUPO)
ind_mad<-match(cels_madr,mapa$ID_GRUPO)
mapa_madr<-mapa[ind_mad,]
mapa_madr_df<-as.data.frame(mapa_madr)
tm_shape(mapa_madr) + tm_borders(alpha=.4) + tm_fill("SUM_POB_AS",style="cont",palette="Blues")

mapa_madr_df[51,] ### Alcal· meco
mapa_madr_df[269,] ### Vic·lvaro
mapa_madr_df[100,] ### Coslada (distrito 3)
mapa_madr_df$NOMBRE_CEL[130]
mapa_madr_df$SUM_POB_AS[130]
mapa_madr_df$SUM_POB_AS

### mapa_madr
### datos_madr
###################################### movilidad de las personas (x moviles)
################################# datos reales de la cuarentena
######### origen-destino
ficheros<-dir("C:/Users/mario/Dropbox/Mario/movilidad/datos_disponibles",pattern="csv")
tmp<-grep("FlujosOrigen",ficheros)
fluxorig<-ficheros[tmp]
tmp<-substr(fluxorig,21,25)
tmp<-str_replace_all(tmp,"MAR","_03")
tmp<-str_replace_all(tmp,"ABR","_04")
tmp<-str_replace_all(tmp,"MAY","_05")
tmp<-str_replace_all(tmp,"JUN","_06")
tmp<-paste(substr(tmp,4,5),"_",substr(tmp,1,2),sep="")
tmp<-tmp[1:(length(tmp)-1)]
ind<-sort(tmp,index.return=TRUE)$ix
fluxori_ord<-fluxorig[ind]
fechas<-paste(substr(fluxori_ord,21,22),substr(fluxori_ord,23,25))

#localeToCharset(locale = Sys.getlocale("LC_CTYPE")) # determina el encoding correcto
#iconv(AP_Direcs,"UTF-8","ISO8859-1")
tt_cua<-length(fluxori_ord)
general_dest_porc=general_dest=general_ind_dest<-list()
for (i in 1:tt_cua) {
# ojo con el encoding
dia<-read.csv(paste("C:/Users/mario/Dropbox/Mario/movilidad/datos_disponibles","/",fluxori_ord[i],sep=""),sep=";",fileEncoding ="ISO8859-1")
flux_porc_list<-list()
flux_dest_list<-list()
flux_dest_ind_list<-list()
for (j in 1:length(cels_madr))  {
ind<-grep(cels_madr[j],trimws(as.character(dia$CELDA_ORIGEN)))
if(length(ind) > 0) {
pobla<-datos_madr[which(datos_madr$ID_GRUPO==cels_madr[j]),10][1]
flux_num<-dia$FLUJO[ind]
flux_dest<-trimws(as.character(dia$CELDA_DESTINO[ind]))
porc<-round(flux_num/pobla,4)
flux_porc_list[[j]]<-porc
flux_dest_list[[j]]<-flux_dest
flux_dest_ind_list[[j]]<-match(flux_dest,cels_madr)
}}
general_dest_porc[[i]]<-flux_porc_list
general_dest[[i]]<-flux_dest_list
general_ind_dest[[i]]<-flux_dest_ind_list
}
cuarentena_dest_porc<-general_dest_porc
cuarentena_dest<-general_dest
cuarentena_ind_dest<-general_ind_dest


######## destino-origen
ficheros<-dir("C:/Users/mario/Dropbox/Mario/movilidad/datos_disponibles",pattern="csv")
tmp<-grep("FlujosDestino",ficheros)
fluxdest<-ficheros[tmp]
tmp<-substr(fluxdest,22,26)
tmp<-str_replace_all(tmp,"MAR","_03")
tmp<-str_replace_all(tmp,"ABR","_04")
tmp<-str_replace_all(tmp,"MAY","_05")
tmp<-str_replace_all(tmp,"JUN","_06")
tmp<-paste(substr(tmp,4,5),"_",substr(tmp,1,2),sep="")
tmp<-tmp[1:(length(tmp)-1)]
ind<-sort(tmp,index.return=TRUE)$ix
fluxdest_ord<-fluxorig[ind]
fechas<-paste(substr(fluxdest_ord,21,22),substr(fluxdest_ord,23,26))

#localeToCharset(locale = Sys.getlocale("LC_CTYPE")) # determina el encoding correcto
#iconv(AP_Direcs,"UTF-8","ISO8859-1")
general_orig_porc=general_orig=general_ind_orig<-list()
for (i in 1:tt_cua) {
# ojo con el encoding
dia<-read.csv(paste("C:/Users/mario/Dropbox/Mario/movilidad/datos_disponibles","/",fluxdest_ord[i],sep=""),sep=";",fileEncoding ="ISO8859-1")
flux_porc_list<-list()
flux_orig_list<-list()
flux_orig_ind_list<-list()
for (j in 1:length(cels_madr))  {
ind<-grep(cels_madr[j],trimws(as.character(dia$CELDA_DESTINO)))
if(length(ind) > 0) {
pobla<-datos_madr[which(datos_madr$ID_GRUPO==cels_madr[j]),10][1]
flux_num<-dia$FLUJO[ind]
flux_orig<-trimws(as.character(dia$CELDA_ORIGEN[ind]))
porc<-round(flux_num/pobla,4)
flux_porc_list[[j]]<-porc
flux_orig_list[[j]]<-flux_orig
flux_orig_ind_list[[j]]<-match(flux_orig,cels_madr)
}}
general_orig_porc[[i]]<-flux_porc_list
general_orig[[i]]<-flux_orig_list
general_ind_orig[[i]]<-flux_orig_ind_list
}
cuarentena_orig_porc<-general_orig_porc
cuarentena_orig<-general_orig
cuarentena_ind_orig<-general_ind_orig

######################### grafico
#######  grafica para la cuarentena
cent<-51    # Pozuelo de Alarc√≥n
cent2<-269
cent3<-100
mapa_madr_porc<-mapa_madr
mapa_madr_porc$PORC<-0

#tmap_mode("view")
Pr<-list()
for (j in 1:tt_cua) {        
dest<-data.frame(general_dest[[j]][[cent]],general_dest_porc[[j]][[cent]])
dest2<-data.frame(general_dest[[j]][[cent2]],general_dest_porc[[j]][[cent2]])
dest3<-data.frame(general_dest[[j]][[cent3]],general_dest_porc[[j]][[cent3]])
if( (length(dest) > 0) & (length(dest2)) > 0 & (length(dest3) > 0)) {
colnames(dest)=colnames(dest2)=colnames(dest3)<-c("ID_GRUPO","PORC")
dest$ID_GRUPO<-trimws(as.character(dest$ID_GRUPO),"right")
dest_flux<-merge(mapa_madr,dest,by.x="ID_GRUPO",by.y="ID_GRUPO")
dest_flux<-dest_flux[which(!is.na(dest_flux$PORC)),]
dest_flux$PORC<-round(dest_flux$PORC*100,0)

dest2$ID_GRUPO<-trimws(as.character(dest2$ID_GRUPO),"right")
dest_flux2<-merge(mapa_madr,dest2,by.x="ID_GRUPO",by.y="ID_GRUPO")
dest_flux2<-dest_flux2[which(!is.na(dest_flux2$PORC)),]
dest_flux2$PORC<-round(dest_flux2$PORC*100,0)

dest3$ID_GRUPO<-trimws(as.character(dest3$ID_GRUPO),"right")
dest_flux3<-merge(mapa_madr,dest3,by.x="ID_GRUPO",by.y="ID_GRUPO")
dest_flux3<-dest_flux3[which(!is.na(dest_flux3$PORC)),]
dest_flux3$PORC<-round(dest_flux3$PORC*100,0)

origen<-mapa_madr[which(mapa_madr$ID_GRUPO==cels_madr[[cent]]),]
origen_nombre<-as.character(as.data.frame(mapa_madr[which(mapa_madr$ID_GRUPO==cels_madr[[cent]]),])$NOMBRE_CEL)
origen2<-mapa_madr[which(mapa_madr$ID_GRUPO==cels_madr[[cent2]]),]
origen2_nombre<-as.character(as.data.frame(mapa_madr[which(mapa_madr$ID_GRUPO==cels_madr[[cent2]]),])$NOMBRE_CEL)
origen3<-mapa_madr[which(mapa_madr$ID_GRUPO==cels_madr[[cent3]]),]
origen3_nombre<-as.character(as.data.frame(mapa_madr[which(mapa_madr$ID_GRUPO==cels_madr[[cent3]]),])$NOMBRE_CEL)
titulo=paste("DÌa",j,"-",fechas[j])
Pr[[j]]<-tm_shape(mapa_madr) + tm_borders(alpha=.4) + tm_shape(dest_flux) + tm_fill("PORC",stype="cont",breaks=seq(0,5,1),palette="Blues") + tm_shape(origen) + tm_fill(col="black") + tm_shape(dest_flux2) + tm_fill("PORC",stype="cont",breaks=seq(0,5,1),palette="Greens") + tm_shape(origen2) + tm_fill(col="black") + tm_shape(dest_flux3) + tm_fill("PORC",stype="cont",breaks=seq(0,5,1),palette="Reds") + tm_shape(origen3) + tm_fill(col="black") + tm_layout(title = titulo, title.position=c("right","bottom"))

}}
saveGIF({  for (i in 1:tt_cua) {print(Pr[[i]])}}, movie.name="Madrid_celdas.gif")


#############################  surrogados para dias normales fuera confinamiento
############## referencia de novimbre de 2019

########## origen-destino
nov<-read.csv(paste("C:/Users/mario/Dropbox/Mario/movilidad/datos_disponibles","/","FlujosOrigen100+_M1_NOV.csv",sep=""),sep=";",fileEncoding ="ISO8859-1")
class(nov)
# hago los surrogados del movimineto de personas

tt_sur=800
std=0.1
nov_dest_porc=nov_dest=nov_ind_dest<-list()
for (kk in 1:tt_sur) {
nov_porc_list<-list()
nov_dest_list<-list()
nov_dest_ind_list<-list()
for (j in 1:length(cels_madr))  {
ind<-grep(cels_madr[j],trimws(as.character(nov$CELDA_ORIGEN)))
if(length(ind) > 0) {
pobla<-datos_madr[which(datos_madr$ID_GRUPO==cels_madr[j]),10][1]
#flux_num<-nov$FLUJO[ind]
flux_num<-round(rnorm(length(nov$FLUJO[ind]),nov$FLUJO[ind],nov$FLUJO[ind]*std))
flux_dest<-trimws(as.character(nov$CELDA_DESTINO[ind]))
porc<-round(flux_num/pobla,4)
nov_porc_list[[j]]<-porc
nov_dest_list[[j]]<-flux_dest
nov_dest_ind_list[[j]]<-match(flux_dest,cels_madr)
}}
nov_dest_porc[[kk]]<-nov_porc_list
nov_dest[[kk]]<-nov_dest_list
nov_ind_dest[[kk]]<-nov_dest_ind_list
}
surrogados_dest_porc<-general_dest_porc<-nov_dest_porc
surrogados_dest<-general_dest<-nov_dest
surrogados_ind_dest<-general_ind_dest<-nov_ind_dest

########## destino-origen
nov<-read.csv(paste("C:/Users/mario/Dropbox/Mario/movilidad/datos_disponibles","/","Flujosdestino100+_M1_NOV.csv",sep=""),sep=";",fileEncoding ="ISO8859-1")
# hago los surrogados del movimineto de personas

nov_orig_porc=nov_orig=nov_ind_orig<-list()
for (kk in 1:tt_sur) {
nov_porc_list<-list()
nov_orig_list<-list()
nov_orig_ind_list<-list()
for (j in 1:length(cels_madr))  {
ind<-grep(cels_madr[j],trimws(as.character(nov$CELDA_DESTINO)))
if(length(ind) > 0) {
pobla<-datos_madr[which(datos_madr$ID_GRUPO==cels_madr[j]),10][1]
flux_num<-round(rnorm(length(nov$FLUJO[ind]),nov$FLUJO[ind],nov$FLUJO[ind]*std))
flux_orig<-trimws(as.character(nov$CELDA_ORIGEN[ind]))
porc<-round(flux_num/pobla,4)
nov_porc_list[[j]]<-porc
nov_orig_list[[j]]<-flux_orig
nov_orig_ind_list[[j]]<-match(flux_orig,cels_madr)
}}
nov_orig_porc[[kk]]<-nov_porc_list
nov_orig[[kk]]<-nov_orig_list
nov_ind_orig[[kk]]<-nov_orig_ind_list
}
surrogados_orig_porc<-general_orig_porc<-nov_orig_porc
surrogados_orig<<-general_orig<-nov_orig
surrogados_ind_orig<-general_ind_orig<-nov_ind_orig


#################  grafica para un dia normal de noviembre de 2019
cent<-51    # Alcal· meco
cent2<-  269  # Vic·lvaro
cent3<-100   # Coslada (distrito 1)
mapa_madr_porc<-mapa_madr
mapa_madr_porc$PORC<-0

#tmap_mode("view")
Pr<-list()
for (j in 1:tt_sur) {        
dest<-data.frame(general_dest[[j]][[cent]],general_dest_porc[[j]][[cent]])
dest2<-data.frame(general_dest[[j]][[cent2]],general_dest_porc[[j]][[cent2]])
dest3<-data.frame(general_dest[[j]][[cent3]],general_dest_porc[[j]][[cent3]])
if( (length(dest) > 0) & (length(dest2)) > 0 & (length(dest3) > 0)) {
colnames(dest)=colnames(dest2)=colnames(dest3)<-c("ID_GRUPO","PORC")
dest$ID_GRUPO<-trimws(as.character(dest$ID_GRUPO),"right")
dest_flux<-merge(mapa_madr,dest,by.x="ID_GRUPO",by.y="ID_GRUPO")
dest_flux<-dest_flux[which(!is.na(dest_flux$PORC)),]
dest_flux$PORC<-round(dest_flux$PORC*100,0)

dest2$ID_GRUPO<-trimws(as.character(dest2$ID_GRUPO),"right")
dest_flux2<-merge(mapa_madr,dest2,by.x="ID_GRUPO",by.y="ID_GRUPO")
dest_flux2<-dest_flux2[which(!is.na(dest_flux2$PORC)),]
dest_flux2$PORC<-round(dest_flux2$PORC*100,0)

dest3$ID_GRUPO<-trimws(as.character(dest3$ID_GRUPO),"right")
dest_flux3<-merge(mapa_madr,dest3,by.x="ID_GRUPO",by.y="ID_GRUPO")
dest_flux3<-dest_flux3[which(!is.na(dest_flux3$PORC)),]
dest_flux3$PORC<-round(dest_flux3$PORC*100,0)

origen<-mapa_madr[which(mapa_madr$ID_GRUPO==cels_madr[[cent]]),]
origen_nombre<-as.character(as.data.frame(mapa_madr[which(mapa_madr$ID_GRUPO==cels_madr[[cent]]),])$NOMBRE_CEL)
origen2<-mapa_madr[which(mapa_madr$ID_GRUPO==cels_madr[[cent2]]),]
origen2_nombre<-as.character(as.data.frame(mapa_madr[which(mapa_madr$ID_GRUPO==cels_madr[[cent2]]),])$NOMBRE_CEL)
origen3<-mapa_madr[which(mapa_madr$ID_GRUPO==cels_madr[[cent3]]),]
origen3_nombre<-as.character(as.data.frame(mapa_madr[which(mapa_madr$ID_GRUPO==cels_madr[[cent3]]),])$NOMBRE_CEL)
titulo=paste("D?a",j)
Pr[[j]]<-tm_shape(mapa_madr) + tm_borders(alpha=.4) + tm_shape(dest_flux) + tm_fill("PORC",stype="cont",breaks=seq(0,5,1),palette="Blues") + tm_shape(origen) + tm_fill(col="black") + tm_shape(dest_flux2) + tm_fill("PORC",stype="cont",breaks=seq(0,5,1),palette="Greens") + tm_shape(origen2) + tm_fill(col="black") + tm_shape(dest_flux3) + tm_fill("PORC",stype="cont",breaks=seq(0,5,1),palette="Reds") + tm_shape(origen3) + tm_fill(col="black") + tm_layout(title = titulo, title.position=c("right","bottom"))

}}



##No correrlo 
saveGIF({  for (i in 1:tt_sur) {print(Pr[[i]])}}, movie.name="Madrid_celdas_surr.gif")


############################################################################################


#   tt=1:tt_sur
#   general_dest_porc<-surrogados_dest_porc
#   general_dest<-surrogados_dest
#   general_ind_dest<-surrogados_ind_dest
#   general_orig_porc<-surrogados_orig_porc
#   general_orig<-surrogados_orig
#   general_ind_orig<-surrogados_ind_orig

#   tt=1:tt_cua
#   general_dest_porc<-cuarentena_dest_porc
#   general_dest<-cuarentena_dest
#   general_ind_dest<-cuarentena_ind_dest
#   general_orig_porc<-cuarentena_orig_porc
#   general_orig<-cuarentena_orig
#   general_ind_orig<-cuarentena_ind_orig

################################################# modelo
##################################################

tt <- 1:300
exists("tt")         # es el tiempo total de la simulacion, debe estar definida antes
print(tt)



centro=130    # Origen
ncities<-dim(mapa_madr)[1]
tmp<-mapa_madr$SUM_POB_AS# 293 celdas
n <- sum(tmp)
##                     condiciones iniciales
ndat<-length(tmp)

u0<-cbind(tmp,0,0,0,0,0,0,0)
u0[centro,]<-c(tmp[centro]-5,5,0,0,0,0,0,0)             # 5 infectados
#u0<-cbind(tmp-50,50,0,0,0,0)                          # infectado en cada pueblo                  

E<-array(0,dim=c(8,8))
diag(E)<-1                                          # esto se selecciona en la columna "select" de eventos
E<-cbind(E,rep(1,8))                                # cada fila es el comprtimiento de donde se van a sacar
colnames(E)<-c("1","2","3","4","5","6","7","8","9")         # en cada movimiento, por ejemplo la columna 3 me dice que
rownames(E)<-c("S","I","H","R","D","QI","QS","QR")             # se sacan de "H"
##################### para las conexiones entre ciudades
#################### para esto necesito cambiar el E agregar que se sacan de S,I y R
###### no necesito cambiar N porque no hay cambio de compratimiento
E1 <- matrix(c(1, 1, 0, 1,0,0,0,0), nrow = 8, ncol = 1,   # una sola columna 
dimnames = list(c("S","I","H","R","D","QI","QS","QR"), "10"))
E<-cbind(E,E1)
                                                                # esta columna se selecciona en la columna "shift" de eventos
N <- matrix(c(0, 0, 2, 0,0,0,0,0), nrow = 8, ncol = 1,          # La primera columna que dice que los de "H" "pasan" dos filas hacia abajo
dimnames = list(c("S","I","H","R","D","QI","QS","QR"), "1"))    # ,sea a "D" (muertes en el hospital)

######### a partir de aquÌ es para las cuarentenas
N1 <- matrix(c(6, 0, 0, 0,0,0,0,0), nrow = 8, ncol = 1,         # Esto es para los I -> QI 
dimnames = list(c("S","I","H","R","D","QI","QS","QR"), "2"))    # 
N<-cbind(N,N1)  
N2 <- matrix(c(0, 0, 0, 0,0,0,-6,0), nrow = 8, ncol = 1,        # Esto es para los QI -> I 
dimnames = list(c("S","I","H","R","D","QI","QS","QR"), "3"))    # 
N<-cbind(N,N2)       
N3 <- matrix(c(0, 4, 0, 0,0,0,0,0), nrow = 8, ncol = 1,         # Esto es para los S -> QS 
dimnames = list(c("S","I","H","R","D","QI","QS","QR"), "4"))    # 
N<-cbind(N,N3)  
N4 <- matrix(c(0, 0, 0, 0,0,-4,0,0), nrow = 8, ncol = 1,        # Estp es para los QS -> S 
dimnames = list(c("S","I","H","R","D","QI","QS","QR"), "5"))    # 
N<-cbind(N,N4)       
N5 <- matrix(c(0, 0, 0, 4,0,0,0,0), nrow = 8, ncol = 1,         # Esto es para los R -> QR 
dimnames = list(c("S","I","H","R","D","QI","QS","QR"), "6"))    # 
N<-cbind(N,N5)  
N6 <- matrix(c(0, 0, 0, 0,0,0,0,-4), nrow = 8, ncol = 1,        # Esto para los QR -> R
dimnames = list(c("S","I","H","R","D","QI","QS","QR"), "7"))    # 
N<-cbind(N,N6)       
N7 <- matrix(c(3, 0, 0, 0,0,0,0,0), nrow = 8, ncol = 1,        # Esto para los QR -> R
             dimnames = list(c("S","I","H","R","D","QI","QS","QR"), "8"))
N <- cbind(N, N7)





############ ecuaciones de transiciones
colnames(u0)<-c("S","I","H","R","D","QI","QS","QR")
transitions <- c("S -> b*S*I/(S+I+R+H+QI+QS+QR) -> I", "I -> g*I -> R", "I -> a*I -> H","H -> d*H -> R","QI->a*QI->H","QI->g*QI->R", "QI -> g*QI -> QR", "QI -> a*QI -> H" )
compartments <- c("S","I","H","R","D","QI","QS","QR")
#g_data<-c(b = 0.16, g = 0.072, a=0.00252, d=0.2)
g_data<-c(b = 0.39, g = 0.2, a=0.096, d=0.89)

#https://www.cdc.gov/coronavirus/2019-ncov/cases-updates/index.html
#g_data<-c(b = 0.14, g = 0.05, a=0, d=0     #desacoplado, solo SIR

print(g_data[1]/g_data[2]) #Valor par·metro Ro


#################  events ------->  event time node dest n proportion select shift
####### parametros generales de los eventos:
ini_V <-  120
ini_Q <- 90      #inicio Cuarentena
fin_Q <- 240  #fin Cuarentena
prop_pob_q <- 0    #proporcion de poblacion de este compartimiento que pasa a Q
prop_pob_v <- 0.1  #proporciÛn de poblaciÛn que se vacuna diariamente

tiempo<-rep(seq(1,tail(tt,1)), each=ndat)
####### esto hace un 11,1% pase de compartimento H -> D  (muertes en el hospital)
HtoD <- data.frame(event = "intTrans", time = tiempo,
node = seq(1,ndat), dest = 0, n = 0, proportion = 0.11, select = 3, shift = 1)

########## Susceptibles
############ saca un % de la poblacion de susceptibles y los pasa a quarentine a t = ini_Q
#tiempo1<-rep(20,tail(tt,1))
StoQS <- data.frame(event = "intTrans", time = rep(ini_Q,each=ndat),
node = seq(1,ndat), dest = 0, n = 0, proportion = prop_pob_q, select = 1, shift = 2)

############ saca % de cuarentenados susceptibles y los vuelve susceptibles
#tiempo1<-rep(20,tail(tt,1))
QStoS <- data.frame(event = "intTrans", time = rep(fin_Q,each=ndat),
node = seq(1,ndat), dest = 0, n = 0, proportion = 1, select = 7, shift = 3)


###### Infectados
############ saca un % de la poblacion de infectados y los pasa a quarentine a t=50
#tiempo1<-rep(20,tail(tt,1))
ItoQI <- data.frame(event = "intTrans", time = rep(ini_Q,each=ndat),
node = seq(1,ndat), dest = 0, n = 0, proportion = prop_pob_q, select = 2, shift = 4)

############ saca % de cuarentenados susceptibles y los vuelve susceptibles
#tiempo1<-rep(20,tail(tt,1))
QItoI <- data.frame(event = "intTrans", time = rep(fin_Q,each=ndat),
node = seq(1,ndat), dest = 0, n = 0, proportion = 1, select = 6, shift = 5)

##### no hace falta "sacarlos" de la cuarentena porque se recuperan o van al hospital


###### Recuperados
############ saca un % de la poblacion de recuperados y los pasa a quarentine a t=50
#tiempo1<-rep(20,tail(tt,1))
RtoQR <- data.frame(event = "intTrans", time = rep(ini_Q,each=ndat),
node = seq(1,ndat), dest = 0, n = 0, proportion = prop_pob_q, select = 4, shift = 6)

############ saca % de cuarentenados susceptibles y los vuelve susceptibles
#tiempo1<-rep(20,tail(tt,1))
QRtoR <- data.frame(event = "intTrans", time = rep(fin_Q,each=ndat),
node = seq(1,ndat), dest = 0, n = 0, proportion = 1, select = 8, shift = 7)

### Evento de vacunaci?n

StoR <- data.frame(event = "intTrans", time = rep(ini_V,each=ndat),
                   node = seq(1,ndat), dest = 0, n = 0, proportion = prop_pob_v, select = 1, shift = 8)


#### alternativo en prueba
tmp<-NULL
transf_ext_out<-NULL
for (kk in tt) {
nn<-lengths(general_ind_dest[[kk]])
tmp_dest_list<-general_ind_dest[[kk]][-which(nn==0)]
tmp_porc_list<-general_dest_porc[[kk]][-which(nn==0)]
nn1<-nn[-which(nn==0)]
ncities_p<-(1:ncities)[-which(nn==0)]
tmp <- data.frame(event = "extTrans", time = kk, node = rep(ncities_p,nn1), dest = unlist(tmp_dest_list), n=0, proportion =unlist(tmp_porc_list)/3, select = 1, shift = 0)
tmp1 <- data.frame(event = "extTrans", time = kk, node = rep(ncities_p,nn1), dest = unlist(tmp_dest_list), n=0, proportion =unlist(tmp_porc_list)/3, select = 2, shift = 0)
tmp2 <- data.frame(event = "extTrans", time = kk, node = rep(ncities_p,nn1), dest = unlist(tmp_dest_list), n=0, proportion =unlist(tmp_porc_list)/3, select = 4, shift = 0)
transf_ext_out<-rbind(transf_ext_out,tmp,tmp1,tmp2)
}
tmp<-NULL
transf_ext_in<-NULL
for (kk in tt) {
nn<-lengths(general_ind_orig[[kk]])
tmp_orig_list<-general_ind_orig[[kk]][-which(nn==0)]
tmp_porc_list<-general_orig_porc[[kk]][-which(nn==0)]
nn1<-nn[-which(nn==0)]
ncities_p<-(1:ncities)[-which(nn==0)]
tmp <- data.frame(event = "extTrans", time = kk,
node = rep(ncities_p,nn1), dest = unlist(tmp_orig_list), n=0, proportion =unlist(tmp_porc_list), select = 1, shift = 0)
tmp1 <- data.frame(event = "extTrans", time = kk,
node = rep(ncities_p,nn1), dest = unlist(tmp_orig_list), n=0, proportion =unlist(tmp_porc_list), select = 2, shift = 0)
tmp2 <- data.frame(event = "extTrans", time = kk,
node = rep(ncities_p,nn1), dest = unlist(tmp_orig_list), n=0, proportion =unlist(tmp_porc_list), select = 4, shift = 0)
transf_ext_in<-rbind(transf_ext_in,tmp,tmp1,tmp2)
}




eventos<-rbind(transf_ext_in,transf_ext_out,HtoD)
eventos_Q <-rbind(transf_ext_in,transf_ext_out,HtoD,StoQS,ItoQI,RtoQR,QStoS,QRtoR,QItoI)
eventos_V <- rbind(transf_ext_in,transf_ext_out,HtoD,StoQS,ItoQI,RtoQR,QStoS,QRtoR,QItoI, StoR)
eventos_Q <- na.omit(eventos_Q)
eventos_V <- na.omit(eventos_V)

#eventos<-rbind(transf_ext_rand,StoQ,QtoS_10p)
#eventos<-rbind(transf_ext_rand)
#eventos<-rbind(transf_ext_rand,HtoD)                            # sin cuarentena
#eventos<-rbind(transf_ext_rand,StoQ,QtoS,HtoD)    # dos cuarentenas
#eventos<-rbind(transf_ext_rand,StoQ,QtoS,HtoD,StoQ_2,QtoS_2)    # dos cuarentenas



SIHRDQ <- mparse(transitions = transitions, events=eventos_V, E=E, N=N, compartments = compartments, gdata = g_data, u0 = u0, tspan = tt)
set.seed(123)
result <- run(model = SIHRDQ, threads = NULL)              #threads=NULL usar todos los procesadores
dat <- trajectory(result)

View(dat)
plot(result)

prevalence_I <- prevalence(result, I ~ .)## Calcula la proporciÛn de infectados en el conjunto de la poblaciÛn para cada periodo. ## Si queremos ver la prevalencia d eun nodo en concreto (por ejemplo el 10) se hara asÌ : prevalence(result, I ~ ., index = 10)
plot(prevalence_I)
max_p_I <- max(prevalence_I$prevalence)


prevalence_H <- prevalence(result, H ~ .) ## Calcula la proporciÛn de hospitalizados en el conjunto de la poblaciÛn para cada periodo
plot(prevalence_H)
max_p_H <- max(prevalence_H$prevalence)

prevalence_H_vector <- prevalence_H[,2]
prevalence_I_H <- cbind(prevalence_I, prevalence_H = prevalence_H_vector)
write.xlsx(prevalence_I_H, "prevalenceIH Ro = 1.96 con vacunaciÛn.xlsx")

prevalence_D <- prevalence(result, D ~ .) ## Calcula la proporciÛn de defunciones en el conjunto de la poblaciÛn para cada periodo
plot(prevalence_D)
max_p_D <- max(prevalence_D$prevalence)

###########
########### Representamos los resultados

result_gbyt <- dat %>% group_by(time) %>% summarise(
  S = sum(S),
  I = sum(I),
  H = sum(H),
  R = sum(R),
  D = sum(D))

## Sacamos las proporciones

result_gbyt <- result_gbyt %>% summarise(
  time = time,
  S = S/n,
  I = I/n,
  H = H/n,
  R = R/n,
  D = D/n)

#Representamos para el conjunto de todos los nodos

ggplot(result_gbyt, aes(x = time)) + 
  geom_line(aes(y = S, colour = "S"), se = F) +
  geom_line(aes(y = I, colour = "I"), se = F) + 
  geom_line(aes(y = H, colour = "H"), se = F) + 
  geom_line(aes(y = R, colour = "R"), se = F) + 
  geom_line(aes(y = D, colour = "D"), se = F) + 
  scale_colour_manual("", breaks = c("S", "I", "H", "R", "D"),
                      values = c("blue", "red", "yellow", "green", "black")) +
  xlab("Time") + ylab("Proportion Poblation") + 
  labs(title = "Evoluci?n SIHRD con vacunaci?n, beta = 0.2")

# Representamos La curva de infectados vs hospitalizados

ggplot(prevalence_I_H, aes(x = time)) + 
  geom_point(aes(y = prevalence, col = "I"), size = 1.5, alpha = 0.7) + 
  geom_point(aes(y = prevalence_H, col = "H"), size = 1.5, alpha = 0.7) + 
  scale_colour_manual("", breaks = c("I", "H"), 
                      values = c("red","lightblue")) + 
  theme_elegante()


# Representamos curva de infectados sin vacunaciÛn vs con vacunaciÛn

prevalenceIH_normal <- read_excel("prevalenceIH Ro = 1.96 sin eventos.xlsx")

prevalenceI_NormalvsV <- cbind(time = 1:300, prevalence_1 = prevalence_I_H$prevalence, prevalence_2 = prevalenceIH_normal$prevalence)
prevalenceI_NormalvsV <- as.data.frame(prevalenceI_NormalvsV)
max(prevalenceI_NormalvsV$prevalence_1)
max(prevalenceI_NormalvsV$prevalence_2)

prevalenceH_NormalvsV <- cbind(time = 1:300, prevalence_1 = prevalence_I_H$prevalence_H, prevalence_2 = prevalenceIH_normal$prevalence_H)
prevalenceH_NormalvsV <- as.data.frame(prevalenceH_NormalvsV)
max(prevalenceH_NormalvsV$prevalence_1)
max(prevalenceH_NormalvsV$prevalence_2)

# Infectados

ggplot(prevalenceI_NormalvsV) + 
  geom_point(aes(x = time, y = prevalence_1, col = "I Vacu"), size = 1.5, alpha = 0.5) + 
  geom_point(aes(x = time, y = prevalence_2, col = "I No vacu"), size = 1.5, alpha = 0.5) +
  scale_colour_manual("", breaks = c("I Vacu", "I No vacu"), 
                      values = c("red","lightblue")) + 
  geom_hline(yintercept = max(prevalenceI_NormalvsV$prevalence_1), linetype = 2) +
  geom_hline(yintercept = max(prevalenceI_NormalvsV$prevalence_2), linetype = 2) +
  theme_elegante() + ylab("prevalence I")

#Hospitalizados

ggplot(prevalenceH_NormalvsV) + 
  geom_point(aes(x = time, y = prevalence_1, col = "H Vacu"), size = 1.5, alpha = 0.5) + 
  geom_point(aes(x = time, y = prevalence_2, col = "H No vacu"), size = 1.5, alpha = 0.5) +
  scale_colour_manual("", breaks = c("H Vacu", "H No vacu"), 
                      values = c("red","lightblue")) + 
  geom_hline(yintercept = max(prevalenceH_NormalvsV$prevalence_1), linetype = 2) +
  geom_hline(yintercept = max(prevalenceH_NormalvsV$prevalence_2), linetype = 2) +
  theme_elegante() + ylab("prevalence H")

#Fuente: https://www.iteramos.com/pregunta/37062/-anade-la-leyenda-al-grafico-de-lineas-de-ggplot2-

####### Elaboramos las proporciones de los nodos a representar

# proporci√≥n nodo 130

n_node130 <- tmp[130]

i <- dat %>% filter(node == 130) %>%
  transmute( node = node,
             time = time,
             S = S/n_node130,
             I = I/n_node130,
             H = H/n_node130,
             R = R/n_node130,
             D = D/n_node130)

# proporci√≥n nodo 38

n_node38 <- tmp[38]

i2 <- dat %>% filter(node == 38) %>%
  transmute( node = node,
             time = time,
             S = S/n_node38,
             I = I/n_node38,
             H = H/n_node38,
             R = R/n_node38,
             D = D/n_node38)

#Poblaci√≥n nodo 230

n_node230 <- tmp[230]

i3 <- dat %>% filter(node == 230) %>%
  transmute( node = node,
             time = time,
             S = S/n_node230,
             I = I/n_node230,
             H = H/n_node230,
             R = R/n_node230,
             D = D/n_node230)

# Data frame con la proporci√≥n de los nodos escogidos

prop_nodes <- rbind(i, i2, i3)
head(prop_nodes)
tail(prop_nodes)

##### Representamos para los nodos 130 (Pozuelo de Alarc√≥n), 38 (Colmenarejo), 230 (Madrid Aplomeras sureste-1)

ggplot(prop_nodes, aes(x = time)) + 
  geom_line(aes(y = S, colour = "S"), se = F) +
  geom_line(aes(y = I, colour = "I"), se = F) + 
  geom_line(aes(y = H, colour = "H"), se = F) + 
  geom_line(aes(y = R, colour = "R"), se = F) + 
  geom_line(aes(y = D, colour = "D"), se = F) + 
  scale_colour_manual("", breaks = c("S", "I", "H", "R", "D"),
                      values = c("blue", "red", "yellow", "green", "black")) + 
  facet_grid(~node) + 
  xlab("Time") + ylab("Proportion Poblation") + 
  labs(title = "Evoluci?n SIHRD con vacunaci?n nodos 38, 130 y 230, beta = 0.2") +
  geom_vline(xintercept = 100, size = 0.5)

##### Representamos los hospitalizados`

ggplot(dat, aes(x = time)) + 
  geom_line(aes(y = H)) + geom_vline(xintercept=c(90,95,145,150), size = 1, col = "red") 

# Representamos la prevalencia

ggplot(prevalence_I, aes(x = time)) + 
  geom_point(aes(y = prevalence, col = prevalence)) 
  

#trajectory(result, node = centro)
#plot(rowSums(trajectory(result, node = 150)))
save(result,mapa_madr,file="trajectory.dat")

########## trajectory muestra la evolucion de los nodos que se quieran ver
as.data.frame(mapa_madr[130,c(2,17)])

big_cities<-c(1,10,30,50,100,130,150,200,250)
gra<-list()
ii=0
for (i in seq(1,9)) {
ii=ii+1
trajecto_df<-as.data.frame(trajectory(result,node=big_cities[i]))
gra[[ii]]<-xyplot( S+R+H+I ~ time, data=trajecto_df, type="l",xlim=c(30,300), lwd=5,main=as.character(mapa_madr_df$NOMBRE_CEL[big_cities[i]]),xlab="Dias",ylab="Poblaci√≥n")
#x11()
#inmunidad de reba√±o
#plot(trajecto_df$R/(trajecto_df$S+trajecto_df$R+trajecto_df$D))
}
grid.arrange(gra[[1]],gra[[2]],gra[[3]],gra[[4]],gra[[5]],gra[[6]],gra[[7]],gra[[8]],gra[[9]])
auto.key=list(space="right",lines=TRUE,lwd=3)


grid.arrange(gra[[4]],gra[[5]],gra[[6]],gra[[7]])
auto.key=list(space="right",lines=TRUE,lwd=3)



################## cuentas
## nro celdas= ndat
## tiempo= length(tt)
dat<-trajectory(result,node=1:ndat)

muerts=infecs<-array(0,dim=c(length(tt),ndat))
for (i in 1:ndat) {
tmp<-which(dat$node==i)
infecs[,i]<-dat$I[tmp]
muerts[,i]<-dat$D[tmp]
}

colnames(infecs)=colnames(muerts)<-mapa_madr_df$NOMBRE_CEL

levelplot(infecs[1:50,],aspect="fill")
levelplot(muerts[1:50,],aspect="fill")


as.character(mapa_madr$ID_GRUPO[1])
as.character(mapa_madr$NOMBRE_CEL[1])


PP<-list()
for (jj in 1:100) {
icels<-which(infecs[jj,]!=0)
ncels<-infecs[jj,which(infecs[jj,]!=0)]
kaka<-cbind(mapa_madr[icels,],ncels)
names(kaka)<-c(names(mapa_madr),"ncels")
PP[[jj]]<-tm_shape(mapa_madr) + tm_borders(alpha=.4) + tm_shape(kaka) + tm_fill("ncels",stype="cont",breaks=seq(0,20,1),palette="Reds") + tm_shape(mapa_madr[centro,]) + tm_borders(lwd=3,col="green") 
}

saveGIF({  for (i in 1:100) {print(PP[[i]])}}, movie.name="Comienzo.gif")


plot(dat[which(dat$node==centro),]$I)




