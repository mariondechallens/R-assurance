# Library:
library(verification)
library(ROCR)
library(ggplot2)
library(RColorBrewer)
library(ggthemes)
library(gridExtra)
library(rgdal)
library(maptools)
library(dplyr)
library(png)
library(rpart)
library(rpart.plot)
library(rattle)
library(gbm)
library(ineq)

##### Questions -----
data = read.csv2("DATABASE.csv",sep = ";", dec=",")

##frequence annuelle du nombre d'evt
nb_evt = aggregate(EVT~surva, data = data, n_distinct) #nb d'evt par annee
mean(nb_evt$EVT)
var(nb_evt$EVT)
plot(nb_evt$surva,nb_evt$EVT)

## nb de sinistres par evenement
nb_sin = aggregate(X~EVT, data = data, n_distinct) # nb de sinistres par evt
mean(nb_sin$X)
var(nb_sin$X)
plot(nb_sin$EVT,nb_sin$X)

## 1. ----
dept=readShapeSpatial("DEPARTEMENT",proj4string=CRS("+init=epsg:2154"))
points=SpatialPoints(na.omit(data[,c("X","Y")]),proj4string=CRS("+init=epsg:7421"))
points_L93=spTransform(points,CRS ("+init=epsg:2154"))
png('plot_1.png')
plot(dept,main="Inondations en France entre 1999 et 2012")
points(points_L93,pch=4,col="red")
dev.off()

summary(dept)

## 2.----
# Plot des sinistres:
data=data[which(data[,"CHARGE"]>0),]
data=data[which(data[,"engt_axa"]>0),]
data=data[which(data[,"EVT"]!=66),] #Without Xynthia.
png('plot_2.png')
ggplot(data,aes(engt_axa,CHARGE,color=factor(CATEG)))+xlim(0,10000000)+ylim(0,500000)+geom_point()+labs(x="Comitment AXA",y="Incurred",title="Incurred % Comitment")+theme_economist()+scale_colour_economist()+theme(legend.title=element_blank())
dev.off()
summary(data)

# moyenne par lob
mean_lob=data.frame(tapply(data$CHARGE,data$CATEG,mean),names(tapply(data$CHARGE,data$CATEG,mean)))
colnames(mean_lob)=c("mean","lob")
mean_lob=mean_lob[which(mean_lob$mean!="NA"),]
ggplot(mean_lob,aes(x=lob,y=mean))+geom_bar(aes(fill=lob), stat="identity")+labs(x="LOB",y="Amount in ???",title="Mean Incurred per lob")+theme_economist()+scale_colour_economist()+theme(legend.title=element_blank())

# nombre de sinistres par lob
n_lob=data.frame(tapply(data$CHARGE,data$CATEG,n_distinct),names(tapply(data$CHARGE,data$CATEG,n_distinct)))
colnames(n_lob)=c("n","lob")
n_lob=n_lob[which(n_lob$n!="NA"),]
ggplot(n_lob,aes(x=lob,y=n))+geom_bar(aes(fill=lob), stat="identity")+labs(x="LOB",y="Nombre",title="Number of claims per lob")+theme_economist()+scale_colour_economist()+theme(legend.title=element_blank())

# nombre de sinistres par année
n_year=data.frame(tapply(data$CHARGE,data$surva,n_distinct),names(tapply(data$CHARGE,data$surva,n_distinct)))
colnames(n_year)=c("n","year")
n_year=n_year[which(n_year$n!="NA"),]
ggplot(n_year,aes(x=year,y=n))+geom_bar(aes(fill=year), stat="identity")+labs(x="YEAR",y="Nombre",title="Number of claims per year")+theme_economist()+scale_colour_economist()+theme(legend.title=element_blank())

# taux de destruction
ggplot(data,aes(x=engt_axa,y=CHARGE))+geom_point()+xlim(0,10000000)+ylim(0,1000000)

## 3. ----
domp =subset(data,data$CATEG == "DOMP")
# nombre de sinistres par propriétaire ou locataire
n_qual=data.frame(tapply(domp$CHARGE,domp$QUAL,n_distinct),names(tapply(domp$CHARGE,domp$QUAL,n_distinct)))
colnames(n_qual)=c("n","qual")
n_qual=n_qual[which(n_qual$n!="NA"),]
ggplot(n_qual,aes(x=qual,y=n))+geom_bar(aes(fill=qual), stat="identity")+labs(x="QUAL",y="Nombre",title="Number of claims per qual")+theme_economist()+scale_colour_economist()+theme(legend.title=element_blank())

# nombre de sinistres (=claims) par étage
n_etage=data.frame(tapply(domp$CHARGE,domp$ETAGE,n_distinct),names(tapply(domp$CHARGE,domp$ETAGE,n_distinct)))
colnames(n_etage)=c("n","etage")
n_etage=n_etage[which(n_etage$n!="NA"),]
ggplot(n_etage,aes(x=etage,y=n))+geom_bar(aes(fill=etage), stat="identity")+labs(x="etage",y="Nombre",title="Number of claims per etage")+theme_economist()+scale_colour_economist()+theme(legend.title=element_blank())

# nombre de sinistres par gamme
n_gam=data.frame(tapply(domp$CHARGE,domp$GAMME,n_distinct),names(tapply(domp$CHARGE,domp$GAMME,n_distinct)))
colnames(n_gam)=c("n","gam")
n_gam=n_gam[which(n_gam$n!="NA"),]
ggplot(n_gam,aes(x=gam,y=n))+geom_bar(aes(fill=gam), stat="identity")+labs(x="gam",y="Nombre",title="Number of claims per gamme")+theme_economist()+scale_colour_economist()+theme(legend.title=element_blank())

# nombre de sinistres par région
n_region=data.frame(tapply(domp$CHARGE,domp$REGROUPEMENT_GEOGRAPHIQUE,n_distinct),names(tapply(domp$CHARGE,domp$REGROUPEMENT_GEOGRAPHIQUE,n_distinct)))
colnames(n_region)=c("n","region")
n_region=n_region[which(n_region$n!="NA"),]
ggplot(n_region,aes(x=region,y=n))+geom_bar(aes(fill=region), stat="identity")+labs(x="region",y="Nombre",title="Number of claims per region")+theme_economist()+scale_colour_economist()+theme(legend.title=element_blank())

## Relation charge-engagement
plot(domp$engt_axa,domp$CHARGE)
domp$DR = domp$CHARGE/domp$engt_axa
summary(domp$DR)

## arbre statistique 
f = as.formula(CHARGE ~ engt_axa)
cart<-rpart(formula=f,data=domp,minsplit=50)
fancyRpartPlot(cart)
summary(cart)

# >> Split base de training et base de test: 2/3, 1/3
t=round(2/3*nrow(domp))
train=domp[1:t,]
test=domp[(t+1):nrow(domp),]

## modèle linéaire
lm1 = lm(CHARGE ~ engt_axa, data = train)
summary(lm1)
plot(y=train$CHARGE,x=train$engt_axa,xlim=c(0,1000000))
abline(lm1,col='red')

pred = predict(lm1,test,type="response")
plot(test$engt_axa,test$CHARGE, type = "l")
lines(test$engt_axa,pred, col='blue')
RMSE_lm = sqrt(sum((pred-test$CHARGE)^2)/length(pred)) #bof

ineq(pred,type='Gini')
plot(Lc(pred))

## GLM
glm1 =  glm(CHARGE~engt_axa, family=gaussian("identity"), data=train)
summary(glm1)
pred2 =  predict(glm1,test, type="response")
plot(test$engt_axa,test$CHARGE, type = "l")
lines(test$engt_axa,pred2, col='blue')
RMSE_glm = sqrt(sum((pred2-test$CHARGE)^2)/length(pred2))

# Indice de Gini et Lorenz curve:  
# statistique permettant de rendre compte de la répartition d'une variable (=charge des sinistres)
# entre 0 et 1 : 0 égalité parfaite, 1: inégalité parfaite
ineq(pred2,type='Gini')
plot(Lc(pred2))

## Gradient boosting
gbm1 = gbm(CHARGE~engt_axa,data=train,distribution="gaussian",interaction.depth=20, n.trees=500,train.fraction=1, verbose=T)
summary(gbm1)

param=plot.gbm(gbm1, i.var = 1, lwd = 2, col = "blue", main = "Perte",xlim=range(1:600000))
param

# Pour mesurer la performance de l'algorithme d'apprentissage:
gbm.perf(gbm1)
pred3 =  predict(gbm1,test,type="response",n.trees=500)
plot(test$engt_axa,test$CHARGE, type = "l") # meilleur
lines(test$engt_axa,pred3, col='blue')
RMSE_gbm = sqrt(sum((pred3-test$CHARGE)^2)/length(pred3)) 

# Lorenz curve : #représente aussi les inégalités d'une grandeur (=charge des sinistres)
# égalité parfaite : courbe x=y dessinée
ineq(pred3,type='Gini')  # plus inégalitaire que les deux autres
plot(Lc(pred3)) # plus inégalitaire que les deux autres

### 4.----
# nombre de sinistres (=claims) par evt sur les branches

ana_branche = function(data,branche)
{
  n_evt=data.frame(tapply(data$CHARGE,data$EVT,n_distinct),names(tapply(data$CHARGE,data$EVT,n_distinct)))
  colnames(n_evt)=c("n","EVT")
  n_etage=n_evt[which(n_evt$n!="NA"),]
  print(ggplot(n_evt,aes(x=EVT,y=n))
        +geom_bar(aes(fill=EVT), stat="identity")
        +labs(x="EVT",y="Nombre",title=paste0("Number of claims per event for ",branche))
        +theme_economist()+scale_colour_economist()+theme(legend.title=element_blank()))
  
  
  m = mean(n_evt$n)
  v = var(n_evt$n)
  print('var > mean ?')
  print(v>m)
  # BNEG car var >> mean
  p<-m/v
  n<-m^2/(v-m)
  print('True mean')
  print(m)
  print('BNEG mean')
  print(mean(rnbinom(nrow(data),size=n,prob=p)))
  return(list('p' = p,'n'=n))
}

for (branche in c("AGRIC", "COLL" ,  "DOMP" , "IMM"  , "MRP"  , "RI" ))
{ # bug avec les autres branches
  print(branche)
  ana_branche(subset(data, data$CATEG == branche),branche)
}

###5.----
ri = subset(data, data$CATEG == "RI")
#Pareto:
pareto<-function(x,u,alpha)
{
  f<-1-(u/x)^alpha
  return(f)
}
#Moment estimation
u<-1000000
alpha<-mean(ri$CHARGE)/(mean(ri$CHARGE)-u)

qpareto<-function(p,u,alpha)
{
  f<-u*(1-p)^(-1/alpha)
  return(f)
}

#QQplot:
q<-seq(0,100000,by=1/31)
qqplot(ri$CHARGE,qpareto(q,1000000,alpha))
abline(0,1)

## 6.----
data2012 = subset(data, data$surva == 2012)
mean(data2012$CHARGE)

# >> Split base de training et base de test: 2/3, 1/3
t=round(2/3*nrow(data2012))
train=data2012[1:t,]
test=data2012[(t+1):nrow(data2012),]

## modèle linéaire
lm1 = lm(CHARGE ~ engt_axa, data = train)
summary(lm1)
plot(y=train$CHARGE,x=train$engt_axa,xlim=c(0,1000000))
abline(lm1,col='red')

pred = predict(lm1,test,type="response")
plot(test$engt_axa,test$CHARGE, type = "l")
lines(test$engt_axa,pred, col='blue')
RMSE_lm = sqrt(sum((pred-test$CHARGE)^2)/length(pred)) #bof

ineq(pred,type='Gini')
plot(Lc(pred)) # égalité parfaite

quantile(pred,0.995)

##7.----
## loi de sévérité

#Moment estimation
u<-1000000
alpha<-mean(data$CHARGE)/(mean(data$CHARGE)-u)
#QQplot:
q<-seq(0,100000,by=1/31)
qqplot(data2012$CHARGE,qpareto(q,1000000,alpha))
abline(0,1)

#Loi de fréquence
param = ana_branche(data,'toutes branches')

#OEP estimation:
simu<-rnbinom(1000,size=param$n,prob=param$p)
year<-rep(1,simu[1])
for(i in 2:1000){year<-c(year,rep(i,simu[i]))}  #Monte-Carlo: simuler un nb de sinistres à chaque fois
simu_claim<-cbind(year,qpareto(runif(sum(simu)),1000000,alpha)) #simuler la sévérité 

#Combien le reassureur va payer pour le sinistre x avec le contrat aXSb (la récupération)
aXSb<-function(a,b,x)
{
  return(min(max(0,x-a),b))
}
simu_claim = as.data.frame(simu_claim)
colnames(simu_claim) = c('Year','Cost')

# simu_claim$recup = rep(0,nrow(simu_claim))
# for (i in 1:nrow(simu_claim))
#   simu_claim$recup[i] = aXSb(7500000,7500000,simu_claim$Cost[i])
# 
# plot(simu_claim$Cost,simu_claim$recup,type='l')
# mean(simu_claim$recup)
