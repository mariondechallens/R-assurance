#### Chargement des donnees ----  

dir = "C:/Users/Admin/Documents/Centrale Paris/3A/OMA/Reassurance/Examen_2019/Sujet 1/"

#dir = "/Users/guillaumeshi/Desktop/OMA/Réassurance/Examen_2019/Sujet 1"
setwd(dir)
loss = read.csv2('Loss.csv')
prof = read.csv2('Profile.csv')
endo = read.csv2('Endorsement.csv')

#### Question 2 ----
# fusion des dataframes sur les pertes (loss) et expositions (endo) sur le numero de police
loss_endo = merge(endo, loss, by="UsualEndorsementId")

# a. moyenne et médiane sous l'outlier ou LossTotal > Exposure
m = mean(loss_endo$LossTotal[loss_endo$LossTotal <= loss_endo$Exposure] /
       loss_endo$Exposure[loss_endo$LossTotal <= loss_endo$Exposure])  # moyenne de 0,621%
med = median(loss_endo$LossTotal[loss_endo$LossTotal <= loss_endo$Exposure] /
         loss_endo$Exposure[loss_endo$LossTotal <= loss_endo$Exposure])  # mediane de 0,032%

# b. fit
library(mbbefd)
loss_endo$DR = loss_endo$LossTotal / loss_endo$Exposure
loss_endo2 = subset(loss_endo, loss_endo$DR <=1) #la fonction ne veut pas >= 1
# il y a une ligne o? le taux depasse 1
f1 =  fitDR(loss_endo2$DR, "mbbefd", method="mle")
summary(f1)

b1 = bootDR(f1, niter=20)
summary(b1)

#quantiles et densite
denscomp(f1, demp=TRUE) # on ne voit pas grand chose
plot(b1, enhance=TRUE)
cdfcomp(f1) #bon

#courbe d'exposition
eccomp(f1, do.points=FALSE) # pas mal

#### Question 3 ----
# a. Simulation
set.seed(123456) #toujours la m�me simu
N = 10000
n_bande = nrow(prof)
# loi uniforme des polices sur les bandes [1-21]
# loi de DR
esti = f1$estimate
police_bande = sample(1:n_bande,N,rep(1/n_bande,n_bande),replace = TRUE)
simu = data.frame(bande = police_bande, DR = rmbbefd(N, a = esti[1], b = esti[2]))
plot(ecdf(simu$DR),main = 'ECDF simulation')


### PARTIE 3B AVEC PERTES BRUTES ###

#simu$perte = simu$DR * max(prof$MaxExpo)  # pertes brutes (dénormalisées)
prof$id_bd = rep(1:21) #
#simu$perte2 = rep(0,nrow(simu))
simu$perte3 = rep(0, nrow(simu))

for (i in 1:N)
{
  #simu$perte2[i] = simu$DR[i]*prof$MaxExpo[simu$bande[i]]
  simu$perte3[i] = simu$DR[i] * (prof$MaxExpo[simu$bande[i]] - prof$MinExpo[simu$bande[i]]) +
    prof$MinExpo[simu$bande[i]]
}

# b. Fit loi de sévérité
seuil = 5e6 #ou 5e6 selon ce qu'on comprend par "5m"...

# loss_insured = simu$perte[simu$perte >= seuil]  # pertes brutes dépassant le seuil
# loss_insured2 = simu$perte2[simu$perte2 >= seuil]

loss_insured3 = simu$perte3[simu$perte3 >= seuil]

library(eva)

# mle_fit = gpdFit(loss_insured, threshold = seuil, method = "mle") # estimation des coeffs du GPD
# q = seq(1e-10, 1 - 1e-10, by=1/length(loss_insured))  #0 et 1 n'existent pas pour le quantile !
# qqplot(simu$perte, qgpd(q, loc=seuil, scale=mle_fit$par.ests[1], shape=mle_fit$par.ests[2]))
# 
# mle_fit2 = gpdFit(loss_insured2, threshold = seuil, method = "mle") # estimation des coeffs du GPD
# q = seq(1e-10, 1 - 1e-10, by=1/length(loss_insured2))  #0 et 1 n'existent pas pour le quantile !
# qqplot(simu$perte, qgpd(q, loc=seuil, scale=mle_fit2$par.ests[1], shape=mle_fit2$par.ests[2]))

mle_fit3 = gpdFit(loss_insured3, threshold = seuil, method = "mle") # estimation des coeffs du GPD
q = seq(1e-10, 1 - 1e-10, by=1/length(loss_insured3))  #0 et 1 n'existent pas pour le quantile !
qqplot(simu$perte, qgpd(q, loc=seuil, scale=mle_fit3$par.ests[1], shape=mle_fit3$par.ests[2]))

### FIN PARTIE 3B AVEC PERTES BRUTES ###

#c. Loss Ratio
LR = 0.7
DR_mean = mean(simu$DR)
DR_med = median(simu$DR)
# prof$exp_nb_loss = LR*prof$EarnedPremium/DR_mean
# total_exp_nb = mean(prof$exp_nb_loss)

total_exp_nb2 = (LR*sum(prof$EarnedPremium) / DR_mean) / max(prof$MaxExpo) # = 146

## sinistres > 5 000 000 euros
total_exp_seuil = (LR*sum(prof$EarnedPremium) / mean(simu$DR[simu$perte > 5e6])) /
  max(prof$MaxExpo)  # = 4 sinistres au-delà de 5 000 000 euros
##

# DR_mean_seuil = mean(subset(simu,simu$perte >=seuil)$DR)
# prof$exp_nb_loss_seuil = LR*prof$EarnedPremium/DR_mean_seuil

#d. cotation, prime pure et prime tech
traiteXS = function(data, franchise, portee, nb_reco, taux_reco){
  data$recov = pmin(pmax(data$perte3-franchise, 0), portee)
  data_ag = aggregate(data[,c("perte3", "recov")], by=list(data$bande), FUN=sum)
  data_ag$recov_net = pmin(pmax(data_ag$recov,0), portee*(1+nb_reco)) #AAL = (n+1)*b
  
  #cotation th�orique
  prime_pure = mean(data_ag$recov_net)/(1+mean(data_ag$recov_net)/portee*taux_reco)
  prime_tech = (prime_pure + 0.2*sd(data_ag$recov_net))/(1-0.15) #chargement

  return(list('PP' = prime_pure, 'PT' = prime_tech))
}

franchise = c(5e6,10e6,30e6,50e6,100e6)
franchise2 = c(5,10,30,50,100)

portee = c(5e6,20e6,20e6,50e6,50e6)
portee2 = c(5,20,20,50,50)

reco = c(2,2,2,1,1)

PP = rep(0,5)
PT = rep(0,5)
traite = rep(0,5)
for (i in 1:5)
{
  titre = paste0(portee2[i],'XS',franchise2[i],' ',reco[i],'@0')
  print(titre)
  traite[i] = titre
  
  t = traiteXS(simu,franchise[i],portee[i],reco[i],0)
  print(t)
  PP[i] = t$PP
  PT[i] = t$PT
  
}

barplot(PP,names.arg = traite,main = 'Prime pure par trait�',col='blue')
barplot(PT,names.arg = traite,main = 'Prime technique par trait�',col='red')
