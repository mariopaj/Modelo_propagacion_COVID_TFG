library(DescTools)

library(devtools)
install_github('mjfii/Local-Extrema')
library('local.extrema')
library(reshape)
library(ggplot2)

tt <- 1:500
tiempo<-rep(seq(1,tail(tt,1)), each=ndat)
secuen_prop_pob = seq(0.1,0.9, by= 0.1)
secuen_ini_Q <- seq(20,200, by = 10) 
secuen_fin_Q <- seq(20,200, by = 10)
ini_Q <- 10
fin_Q <- 220


guarda1=guarda=guarda3=guarda4<-array(0,dim=c(9,19))

i=0

for (prop_pob in secuen_prop_pob) {
  
  i=i+1
  j=0
  
  for (fin_Q in secuen_fin_Q) {
  
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
  
  #eventos<-rbind(transf_ext_in,transf_ext_out,HtoD,StoQS,ItoQI,RtoQR,QStoS,QRtoR,QItoI,vacu)
  eventos<-rbind(transf_ext_in,transf_ext_out,HtoD,StoQS,ItoQI,RtoQR,QStoS,QRtoR,QItoI)
  eventos<-na.omit(eventos)
  
  SIHRDQ <- mparse(transitions = transitions, events=eventos, E=E, N=N, compartments = compartments, gdata = g_data, u0 = u0, tspan = tt)
  set.seed(123)
  result <- run(model = SIHRDQ, threads = NULL)
  
  ind_ord<-sort.int(u0[,1], index.return=TRUE)$ix
  ind<-which(prevalence(result, I~S+I+R+QI+QR+QS+H)$node==20)
  preva<-prevalence(result, I~S+I+R)[ind,]
  todos<-prevalence(result, I~S+I+R+QI+QR+QS+H, index = 1:293)   #en cada nodo
  globa<-prevalence(result, I~S+I+R+QI+QR+QS+H) 
  
  
  tmp2<-globa$time[which.max(globa$prevalence)]
  tmp1<-max(globa$prevalence)
  tmp1<-round(tmp1*100,0)
  print(c(tmp1,tmp2,c(ini_Q,fin_Q)))
  guarda[i,j]<-tmp1
  guarda1[i,j]<-tmp2
  ind<-which(todos$time==tmp2)
  guarda3[i,j]<-sd(todos$prevalence[ind])
  guarda4[i,j]<-Entropy(todos$prevalence[ind])
  
  }
}
rownames(guarda)<-secuen_prop_pob
colnames(guarda)<-secuen_fin_Q
guarda <- as.data.frame(guarda)

write.table(guarda, "prop_ini_Q.csv", sep = ",")
write.table(guarda, "prop_fin_Q.csv", sep = ",")

guarda_m <- melt(guarda) ## Con la función melt lo que hacemos es agrupar todos los valores de la matriz en forma de dataframe para poder representar
guarda_m <- as.data.frame(guarda_m)
guarda_m$proportion_pob <- seq(0.1,0.9, by = 0.1)

colnames(guarda_m)[1] <- "Ini_Q"
colnames(guarda_m)[1] <- "Fin_Q"

### Representamos los picos de los parámetros para ver cual puede ser la mejor estrategia para las cuarentenas

ggplot(data = guarda_m, aes(x = proportion_pob, y = Fin_Q)) + 
  geom_tile(aes(fill = value), col = "white") +
  scale_fill_gradient(low = "red", high = "lightgreen")  




