#### Chargement des donnees ----  

dir = "C:/Users/Admin/Documents/Centrale Paris/3A/OMA/Reassurance/Examen_2019/Sujet 1/"

#dir = /Users/guillaumeshi/Desktop/OMA/RÃ©assurance/Examen_2019/Sujet 1"
setwd(dir)
loss = read.csv2('Loss.csv')
prof = read.csv2('Profile.csv')
endo = read.csv2('Endorsement.csv')

#### Question 2 ----
# fusion des dataframes sur les pertes (loss) et expositions (endo) sur le numero de police
loss_endo = merge(endo, loss, by="UsualEndorsementId")

# a. moy et med
mean(loss_endo$LossTotal / loss_endo$Exposure)*100  # moyenne à 0,658%
median(loss_endo$LossTotal / loss_endo$Exposure)*100  # mediane À 0,032%

# b. fit
library(mbbefd)
loss_endo$DR = loss_endo$LossTotal / loss_endo$Exposure
loss_endo2 = subset(loss_endo, loss_endo$DR <=1) #la fonction de veut pas >= 1
# il y a une ligne où le taux dépasse 1
f1 =  fitDR(loss_endo2$DR, "mbbefd", method="mle")
summary(f1)

b1 = bootDR(f1, niter=20)
summary(b1)

#quantiles et densite
denscomp(f1, demp=TRUE) # on en voit pas grand chose
plot(b1, enhance=TRUE)
cdfcomp(f1) #bon

#courbe d'exposition
eccomp(f1, do.points=FALSE) # pas mal

#### Question 3 ----
# a. Simulation
N = 10000
n_bande = nrow(prof)
# loi uniforme des polices sur les bandes [1-21]
# loi de DR
esti = f1$estimate
police_bande = sample(1:n_bande,N,rep(1/n_bande,n_bande),replace = TRUE)
simu = data.frame(bande = police_bande, DR = rmbbefd(N, a = esti[1], esti[2]))
plot(ecdf(simu$DR))

# b. Fit loi de sévérité #pas sure 
seuil = 5e6
prof$id_bd = rep(1:21) #indice des bandes
bd_seuil = subset(prof,prof$MinExpo >=seuil)$id_bd
DR_seuil = subset(simu,simu$bande %in% bd_seuil)$DR

pareto<-function(x,u,alpha)
{
  f=1-(u/x)^alpha
  return(f)
}

#Moment estimation
u=1000000
alpha=mean(DR_seuil)/(mean(DR_seuil)-u)

qpareto<-function(p,u,alpha)
{
  f=u*(1-p)^(-1/alpha)
  return(f)
}
#QQplot:
q=seq(0,1,by=1/31)
qqplot(DR_seuil,qpareto(q,u,alpha))
abline(0,1)

#c. Loss Ratio
LR = 0.7
DR_mean = mean(simu$DR)
prof$exp_nb_loss = LR*prof$EarnedPremium/DR_mean
total_exp_nb = mean(prof$exp_nb_loss)



#d. cotation, prime pure et prime tech
traiteXS = function(data, franchise, portee, nb_reco, taux_reco){
  data$recov = pmin(pmax(data$LossTotal-franchise, 0), portee)
  data_ag = aggregate(data[,c("LossTotal", "recov")], by=list(data$UsualEndorsementId), FUN=sum)
  data_ag$recov_net = pmin(pmax(data_ag$recov,0), portee*(1+nb_reco)) #AAL = (n+1)*b
  prime_pure = mean(data_ag$recov_net)/(1+mean(data_ag$recov_net)/portee*taux_reco)
  prime_tech = (prime_pure + 0.2*sd(data_ag$recov_net))/(1-0.15)
  
  data_ag$net_loss = data_ag$loss - data_ag$recov_net
  cout_reass = (1-0.33)*(prime_tech - prime_pure)
  capital_gain = (quantile(data_ag$loss, 0.995)-mean(data_ag$loss)) - (quantile(data_ag$net_loss, 0.995)-mean(data_ag$net_loss))
  val = 0.06*capital_gain-cout_reass 
  return(list('val' = val,'PP' = prime_pure, 'PT' = prime_tech))
}

franchise = c(5e6,10e6,30e6,50e6,100e6)
portee = c(5e6,20e6,20e6,50e6,50e6)
reco = c(2,2,2,1,1)

for (i in 1:5)
{
  print(paste0('Traité ',portee[i],'XS',franchise[i],' ',reco[i],'@0'))
  print(traiteXS(loss_endo,franchise[i],portee[i],reco[i],0))
}