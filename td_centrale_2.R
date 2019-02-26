# Work database AXA France claims:

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
library(ggthemes)
library(gridExtra)
library(png)
library(rpart)
library(rpart.plot)
library(rattle)
install.packages("ineq")
library(ineq)

# Access database:
setwd("C:/Users/b011vim/Desktop/Cours/TD Centrale/")
db=read.csv2("DATABASE.csv",sep=";",dec=",")

##### Question 0: fréquence annuelle du nombre d'évènement:
db$surva<-as.Date(db$surva,"%m/%d/%y")

nb_evt <- aggregate(EVT~surva,data=db,n_distinct)
  
mean(nb_evt$EVT)
var(nb_evt$EVT)
# Choix d'une loi de Poisson

##### Question 0': nombre de sinistres par évènement:
nb_sin <- db %>% count(EVT) 
mean(nb_sin$n)
var(nb_sin$n)

##### Question 1: Affichage carte:
library(maptools)
library(png)
dept<-readShapeSpatial("DEPARTEMENT",proj4string=CRS("+init=epsg:2154"))
points=SpatialPoints(na.omit(db[,c("X","Y")]),proj4string=CRS("+init=epsg:7421"))
points_L93=spTransform(points,CRS ("+init=epsg:2154"))
png('plot_1.png')
plot(dept,main="Inondations France 1999-2012")
points(points_L93,pch=4,col="red")
dev.off()

summary(dept)

##### Question 2: réaliser des stats descriptives - ggplot:
# Plot des sinistres:
db=db[which(db[,"CHARGE"]>0),]
db=db[which(db[,"engt_axa"]>0),]
db=db[which(db[,"EVT"]!=66),] #Without Xynthia.
png('plot_2.png')
ggplot(db,aes(engt_axa,CHARGE,color=factor(CATEG)))+xlim(0,10000000)+ylim(0,500000)+geom_point()+labs(x="Comitment AXA",y="Incurred",title="Incurred % Comitment")+theme_economist()+scale_colour_economist()+theme(legend.title=element_blank())
dev.off()
summary(db)
# Moyenne par lob:
mean_lob=data.frame(tapply(db$CHARGE,db$CATEG,mean),names(tapply(db$CHARGE,db$CATEG,mean)))
colnames(mean_lob)=c("mean","lob")
mean_lob=mean_lob[which(mean_lob$mean!="NA"),]
ggplot(mean_lob,aes(x=lob,y=mean))+geom_bar(aes(fill=lob), stat="identity")+labs(x="LOB",y="Amount in ???",title="Mean Incurred per lob")+theme_economist()+scale_colour_economist()+theme(legend.title=element_blank())

# Nombre de sinistres par lob:
nb_lob <- db %>% count(CATEG)
colnames(nb_lob)
ggplot(nb_lob,aes(x=CATEG,y=n))+geom_bar(aes(fill=CATEG), stat="identity")+labs(x="LOB",y="Number",title="Number of claims per lob")+theme_economist()+scale_colour_economist()+theme(legend.title=element_blank())

ggplot(db,aes(x=engt_axa,y=CHARGE))+geom_point()+xlim(0,10000000)+ylim(0,1000000)

##### Question 3: analyse ds :
db_DOMP <- db %>% filter(CATEG == 'DOMP')

# Factor parameters:
db_DOMP =select(db_DOMP,DISTANCE,engt_axa,CHARGE,ETAGE,DEPT,QUAL,GAMME,REGROUPEMENT_GEOGRAPHIQUE,
                CODE_PROD,
                surva,
                NB_PIECES,content,
                EVT,
                NB_ARRETE_CATNAT_JO,
                DEPT,
                CODE_PROD,
                CODE_POLE,
                HAUTEUR_EAU)

db_DOMP %>% mutate_at(.vars=vars(ETAGE,DEPT,QUAL,GAMME,REGROUPEMENT_GEOGRAPHIQUE,
                                 CODE_PROD,
                                 surva,
                                 NB_PIECES,
                                 EVT,
                                 NB_ARRETE_CATNAT_JO,
                                 DEPT,
                                 CODE_PROD,
                                 CODE_POLE),.funs=funs(as.factor))
db_DOMP$DISTANCE=as.numeric((db_DOMP$DISTANCE))

# 3a: analyse taux de destruction et charge sinistre
# Taux de destruction:
db_DOMP$DR=db_DOMP$CHARGE/db_DOMP$engt_axa

summary(db_DOMP)

# 3a: analyse par arbre statistique:
# Arbre sans apprentissage:
f = as.formula(DR ~ QUAL +ETAGE +NB_PIECES+GAMME+REGROUPEMENT_GEOGRAPHIQUE+HAUTEUR_EAU+NB_PIECES)
cart<-rpart(formula=f,data=db_DOMP,minsplit=50)
fancyRpartPlot(cart)
summary(cart)

# 3b: analyse par algorithme ds:
# Analyse via GBM/GLM/Random Forest:

# >> Split base de training et base de test:
t=sample(nrow(db_DOMP),nrow(db_DOMP)*0.9)
t_=(1:nrow(db_DOMP))[-t]
db_train=db_DOMP[t,]
db_test=db_DOMP[t_,]

# Linear model:
reg<-lm(CHARGE~engt_axa,data=db_train)
lm_reg<-predict(reg,db_test,type="response")
plot(y=db_train$CHARGE,x=db_train$engt_axa,xlim=c(0,1000000))
abline(reg,col='red')
# Polynomial regression:
poly<-lm(CHARGE~poly(engt_axa,5), data=db_train)
plot(y=db_train$CHARGE,x=db_train$engt_axa,xlim=c(0,1000000))

# GLM:

  # GLM test des différents cas possibles:
  db<???data.frame(x=c ( 1 , 2 , 3 , 4 , 5 ) , y=c ( 1 , 2 , 4 , 2 , 6 ) )
  model <- glm(y~x, family=poisson(link="identity"), data=db)
  fit <- predict(model, type="response", se.fit=TRUE)


db_train$DR=pmin(1,db_train$DR)

# Drop 
f_init=as.formula(DR~.)
model_glm<-glm(formula=f_init, data = db_train, family=gaussian("identity"))


# Test de la significativé des variables:
summary(model_glm)
# Amélioration de la formule GLM:
f=as.formula(DR~QUAL+ETAGE+content+NB_ARRETE_CATNAT_JO+HAUTEUR_EAU)
predict_glm <- predict(model_glm,db_train,type="response")
# Indice de Gini et Lorenz curve:
ineq(predict_glm,type='Gini')

# GBM:
library(gbm)

summary(db_train)
f_=as.formula(CHARGE_NETTE_AS.IF~engt_axa+DISTANCE_HANI+HAUTEUR_EAU+QUAL+ETAGE+NB_PIECES+REGROUPEMENT_GEOGRAPHIQUE)
gbm_1 = gbm(formula=f_,data=db_train,distribution="gaussian",interaction.depth=20, n.trees=500,train.fraction=1, verbose=T)
summary(gbm_1)

param=plot.gbm(gbm_1, i.var = 1, lwd = 2, col = "blue", main = "Perte",xlim=range(1:600000))

# Travaux sur les taux de destruction:
f=as.formula(DR~HAUTEUR_EAU+QUAL+ETAGE+NB_PIECES+REGROUPEMENT_GEOGRAPHIQUE+GAMME+EVT+NB_ARRETE_CATNAT_JO)
gbm = gbm(formula=f,data=db_train,distribution="gaussian",interaction.depth=20, n.trees=1000,train.fraction=1, verbose=T)
  
  # Influence relative de chaque variable:
  summary(gbm)
  param=plot.gbm(gbm, i.var = 5, lwd = 2, col = "blue", main = "Perte")
  
  
  # Pour mesurer la performance de l'algorithme d'apprentissage:
  gbm.perf(gbm)

  # Prediction du modèle:
  predict_gbm <- predict(gbm,db_test,type="response",n.trees=500)
  
  # Lorenz curve :
  ineq(predict_gbm,type='Gini')
  plot(Lc(predict_gbm))
  
  # >> manuellement:
  lorenz_gbm=data.frame(db_test$DR,predict_gbm)
  lorenz_gbm=lorenz_gbm[order(db_test$DR,decreasing=T),]
  lorenz_gbm$sum=cumsum(x = lorenz_gbm[,2])
  lorenz_gbm$sum=lorenz_gbm$sum/lorenz_gbm$sum[nrow(lorenz_gbm)]
  lorenz_gbm$num=1:nrow(lorenz_gbm)/nrow(lorenz_gbm)
  ggplot(data=lorenz_gbm,aes(x=num))+geom_line(aes(y=sum, colour = "var0"))+geom_line(data=data.frame(x=c(0,1),y=c(0,1)),aes(x=x,y=y))
  

# Question 4: en déduire le meilleur algorithme
  
# Question 5: analyse de la volatilité des taux de destruction