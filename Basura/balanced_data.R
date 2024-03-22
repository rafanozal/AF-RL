# script para balancear los datos
sensor <- read_csv("C:/Users/Usuario/Downloads/sensor.csv")
xdatos<-as.matrix(sensor[,-c(1,2,55)])
xdatos<-xdatos[,-16]
xdatos<-xdatos[,-50]


Y<-as.factor(sensor$machine_status)

Y<-as.numeric(Y)
Y[Y==3]=0
Y[Y==1]=0
Y[Y==2]=1



p<-ncol(xdatos)
n<-nrow(xdatos)
ina<-c()
x0<-1:n

for(i in 1:p)
{
  ii<-which(is.na(xdatos[,i]))
  if(length(ii)>0){
    z<-xdatos[-ii,i];x<-x0[-ii]
    xdatos[ii,i]<-approx(x,z,xout=x0[ii],rule=2)$y}  ### imputamos los datos faltantes interpolando
  
}

xdatos0<-scale(xdatos) #################!!!!!!!!!

### mis datos:
misdatos<-data.frame(cbind(xdatos0,Y))


#Hasta aqui lo que ya estaba pero con todos los datos
library("UBL")
newdatos=SmoteClassif(misdatos$Y~., misdatos, C.perc = list("0"=5,"1"=1))
pnewdatos=TomekClassif(misdatos$Y~.,misdatos,dist="Euclidean", Cl="1",rem="both")
