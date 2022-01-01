library(DescTools)

library(devtools)
install_github('mjfii/Local-Extrema')
library('local.extrema')
library(reshape)
library(ggplot2)


tt <- 1:500
tiempo<-rep(seq(1,tail(tt,1)), each=ndat)

secuen_prop_pob = seq(0.05,0.45, by= 0.05)
secuen_ini_Q <- seq(20,200, by = 10) 
secuen_fin_Q <- seq(20,200, by = 10)
secuen_ini_vac <- seq(20, 200, by = 10)


u0[centro,]<-c(tmp-5,5,0,0,0,0,0,0)
####### parametros generales de los eventos:
ini_Q=10      #inicio Cuarentena
fin_Q=220     #fin Cuarentena
prop_pob<-0
g_data<-c(b = 0.16, g = 0.05, a=0.001, d=0.2)


guarda<-array(0,dim=c(9,19))
i=0
for (prop_pob_v in secuen_prop_pob) {
  i=i+1
  j=0
  for (ini_vac in secuen_ini_vac) {
    #for (fin_Q in secuen_fin_Q) {
    j=j+1
    
    
    tiempo<-rep(seq(1,tail(tt,1)), each=ndat)
    ####### esto hace un 5% pase de compartimento H -> D  (muertes en el hospital)
    HtoD <- data.frame(event = "intTrans", time = tiempo,
                       node = seq(1,ndat), dest = 0, n = 0, proportion = 0.05, select = 3, shift = 1)
    ### 20% de hispitalizados mueren, de acuerdo a 
    #Presenting Characteristics, Comorbidities, and Outcomes Among 5700
    #Patients Hospitalized With COVID-19 in the NewYork City Area
    
    ########## Susceptibles
    ############ saca un % de la poblacion de susceptibles y los pasa a quarentine a t = ini_Q
    #tiempo1<-rep(20,tail(tt,1))
    StoQS <- data.frame(event = "intTrans", time = rep(ini_Q,each=ndat),
                        node = seq(1,ndat), dest = 0, n = 0, proportion = prop_pob, select = 1, shift = 2)
    
    ############ saca % de cuarentenados susceptibles y los vuelve susceptibles
    #tiempo1<-rep(20,tail(tt,1))
    QStoS <- data.frame(event = "intTrans", time = rep(fin_Q,each=ndat),
                        node = seq(1,ndat), dest = 0, n = 0, proportion = 1, select = 7, shift = 3)
    
    
    ###### Infectados
    ############ saca un % de la poblacion de infectados y los pasa a quarentine a t=50
    #tiempo1<-rep(20,tail(tt,1))
    ItoQI <- data.frame(event = "intTrans", time = rep(ini_Q,each=ndat),
                        node = seq(1,ndat), dest = 0, n = 0, proportion = prop_pob, select = 2, shift = 4)
    
    ############ saca % de cuarentenados susceptibles y los vuelve susceptibles
    #tiempo1<-rep(20,tail(tt,1))
    QItoI <- data.frame(event = "intTrans", time = rep(fin_Q,each=ndat),
                        node = seq(1,ndat), dest = 0, n = 0, proportion = 1, select = 6, shift = 5)
    
    ##### no hace falta "sacarlos" de la cuarentena porque se recuperan o van al hospital
    
    
    ###### Recuperados
    ############ saca un % de la poblacion de recuperados y los pasa a quarentine
    #tiempo1<-rep(20,tail(tt,1))
    RtoQR <- data.frame(event = "intTrans", time = rep(ini_Q,each=ndat),
                        node = seq(1,ndat), dest = 0, n = 0, proportion = prop_pob, select = 4, shift = 6)
    
    ############ saca % de cuarentenados susceptibles y los vuelve susceptibles
    #tiempo1<-rep(20,tail(tt,1))
    QRtoR <- data.frame(event = "intTrans", time = rep(fin_Q,each=ndat),
                        node = seq(1,ndat), dest = 0, n = 0, proportion = 1, select = 8, shift = 7)
    
    
    tiempo_vac<-rep(seq(ini_vac,tail(tt,1)), each=ndat)
    ####### esto hace un 1% pase de compartimento S -> R  (vacunacion)
    vacu <- data.frame(event = "intTrans", time = tiempo_vac,
                       node = seq(1,ndat), dest = 0, n = 0, proportion = prop_pob_v, select = 1, shift = 8)
    
    #eventos<-rbind(transf_ext_in,transf_ext_out,HtoD,StoQS,ItoQI,RtoQR,QStoS,QRtoR,QItoI,vacu)
    eventos<-rbind(transf_ext_in,transf_ext_out,HtoD,StoQS,ItoQI,RtoQR,QStoS,QRtoR,QItoI,vacu)
    eventos<-na.omit(eventos)
    
    SIHRDQ <- mparse(transitions = transitions, events=eventos, E=E, N=N, compartments = compartments, gdata = g_data, u0 = u0, tspan = tt)
    set.seed(123)
    result <- run(model = SIHRDQ, threads = NULL)              #threads=NULL usar todos los procesadores
    
    
    globa<-prevalence(result, I~S+I+R+QI+QR+QS+H) 
    
    #x11()
    #print(ggplot(preva, aes(x=time, y=prevalence)) +  geom_point(aes(color=prevalence)))
    #x11()
    
    #print(ggplot(todos, aes(x = time, y = node, fill = prevalence)) + geom_tile() + scale_fill_gradient(low = "white", high = "red"))
    
    #hosps<-trajectory(result)[,c(1,2,5)]
    #x11()
    #print(ggplot(hosps, aes(x = time, y = node, fill = H)) + geom_tile() + scale_fill_gradient(low = "white", high = "blue"))
    
    #x11()
    #print(ggplot(globa, aes(x = time, y = prevalence)) + geom_point(aes(color=prevalence)))
    
    #print(c(ini_Q,fin_Q))
    
   
    tmp1<-max(globa$prevalence)
    tmp1<-round(tmp1*100,0)
    print(guarda[i,j]<-tmp1)
    
  }}

rownames(guarda) <- secuen_prop_pob
colnames(guarda) <- secuen_ini_vac

write.table(guarda, "prop_ini_vacu.csv", sep = ",")

guarda_m <- melt(guarda) ## Con la función melt lo que hacemos es agrupar todos los valores de la matriz en forma de dataframe para poder representar
guarda_m <- as.data.frame(guarda_m)
colnames(guarda_m)[1] <- "prop_vacu"
colnames(guarda_m)[2] <- "ini_vacu"

##### Representamos

ggplot(data = guarda_m, aes(x = prop_vacu, y = ini_vacu)) + 
  geom_tile(aes(fill = value), col = "white") +
  scale_fill_gradient(low = "red", high = "lightgreen")

