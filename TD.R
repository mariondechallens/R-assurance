#Packages:
#install.packages("plyr")
library(plyr)
#TD:
claim<-read.csv2("exemple.csv",sep=";",dec=",")
claim = claim[,1:2]
#Inflation: actauliser l'historique des sinistres
#3% inflation/year:
infla<-(1+0.03)^(2016-claim$Year+1)
claim$cost_Adjust<-claim$Cost*infla

#---------------------
#mean excess funcion
c = seq(50000,6050000,50000)
claim$u = c
e_u = rep(0,nrow(claim))
for (i in 1:nrow(claim))
{
  sup = subset(claim, claim$Cost > claim$u[i])
  e_u[i] = sum(sup$Cost - claim$u[i])/nrow(sup)
}
claim$e_u = e_u
plot(claim$u,claim$e_u,type='l')
# chsi plus grand que 1
coef_dir = (claim$e_u[121] - claim$e_u[1])/(claim$u[121] - claim$u[1]) # chsi/(1-chsi)
chsi = coef_dir/(1+coef_dir)

#----------------------
#Frequency law:
head(claim)
number_year<-count(claim,c('Year')) #frequence de chaque année 
mean_freq<-mean(number_year$freq)
var_freq<-var(number_year$freq)
var_freq>mean_freq
#We can choose a negative binomial for frequency estimation because var > mean
#pas Poisson car var pas egal à moy
p<-mean_freq/var_freq
n<-mean_freq^2/(var_freq-mean_freq)
mean(rnbinom(100,size=n,prob=p))


#------------------------------
# Severity distribution:
#3 laws studied: Pareto, Lognormal, generalized pareto:
#Pareto:
pareto<-function(x,u,alpha)
{
  f<-1-(u/x)^alpha
  return(f)
}
  #Moment estimation
  u<-1000000
  alpha<-mean(claim$cost_Adjust)/(mean(claim$cost_Adjust)-u)

qpareto<-function(p,u,alpha)
{
  f<-u*(1-p)^(-1/alpha)
  return(f)
}
  #QQplot:
  q<-seq(0,1,by=1/31)
  qqplot(claim$cost_Adjust,qpareto(q,1000000,1.16))
  abline(0,1)

  #OEP estimation:
  simu<-rnbinom(10000,size=n,prob=p)
  year<-rep(1,simu[1])
  for(i in 2:10000){year<-c(year,rep(i,simu[i]))}  #Monte-Carlo: simuler un nb de sinistres à chaque fois
  simu_claim<-cbind(year,qpareto(runif(sum(simu)),1000000,1.16)) #simuler la sévérité 

#-------------------------
#Combien le reassureur va payer pour le sinistre x avec le contrat aXSb (la récupération)
aXSb<-function(a,b,x)
{
  return(min(max(0,x-a),b))
}
simu_claim = as.data.frame(simu_claim)
colnames(simu_claim) = c('Year','Cost')

simu_claim$recup = rep(0,nrow(simu_claim))
for (i in 1:nrow(simu_claim))
  simu_claim$recup[i] = aXSb(7500000,7500000,simu_claim$Cost[i])

#recup  
plot(simu_claim$Cost,simu_claim$recup,type='l')
mean(simu_claim$recup)
var(simu_claim$recup)
bx = boxplot(simu_claim$recup)
bx$stats
quantile(simu_claim$recup,c(0.25,0.5,0.75))
  
#depend bcp de la loi ajustée
  
  
