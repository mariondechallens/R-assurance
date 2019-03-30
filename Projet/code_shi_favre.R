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

# a. Moyenne et mediane (sans l'outlier pour qui LossTotal > Exposure)
m = mean(loss_endo$LossTotal[loss_endo$LossTotal <= loss_endo$Exposure] /
           loss_endo$Exposure[loss_endo$LossTotal <= loss_endo$Exposure]) 
med = median(loss_endo$LossTotal[loss_endo$LossTotal <= loss_endo$Exposure] /
               loss_endo$Exposure[loss_endo$LossTotal <= loss_endo$Exposure])  
print('Moyenne en %')
m*100
print('Mediane en %')
med*100

# b. Fit d'un modele
library(mbbefd)
loss_endo$DR = loss_endo$LossTotal / loss_endo$Exposure
loss_endo2 = subset(loss_endo, loss_endo$DR <=1) #on enleve l'outlier

f1 =  fitDR(loss_endo2$DR, "mbbefd", method="mle")
summary(f1)

# verification avec l'ECDF
cdfcomp(f1) 

#courbe d'exposition
eccomp(f1, do.points=FALSE) 

#### Question 3 ----
# a. Simulation
set.seed(123456) #toujours la meme simulation
N = 10000
n_bande = nrow(prof)
# loi uniforme des polices sur les bandes [1-21]
# loi de DR
esti = f1$estimate
police_bande = sample(1:n_bande,N,rep(1/n_bande,n_bande),replace = TRUE)
simu = data.frame(bande = police_bande, DR = rmbbefd(N, a = esti[1], b = esti[2]))
plot(ecdf(simu$DR),main = 'ECDF simulation')

# b. Loi de severite
prof$id_bd = rep(1:21)  #indice des bandes
simu$perte = rep(0, nrow(simu))

#calcul des pertes avec le DR simule
for (i in 1:N)
{
  simu$perte[i] = simu$DR[i] * (prof$MaxExpo[simu$bande[i]] - prof$MinExpo[simu$bande[i]]) +
    prof$MinExpo[simu$bande[i]]
}

seuil = 5e6

#pertes au-dessus du seuil
loss_insured = simu$perte[simu$perte >= seuil]

library(eva)

mle_fit = gpdFit(loss_insured, threshold = seuil, method = "mle") # estimation des coeffs du GPD
q = seq(1e-10, 1 - 1e-10, by=1/length(loss_insured))  #0 et 1 n'existent pas pour le quantile !
qqplot(simu$perte, qgpd(q, loc=seuil, scale=mle_fit$par.ests[1], shape=mle_fit$par.ests[2]),main = 'QQplot loi de Pareto generalisee',ylab = '')
abline(0,1)

#c. Loss Ratio
LR = 0.7
DR_mean = aggregate(DR~bande,data=simu,mean) #DR moyen par bande
prof2 = merge(DR_mean, prof, by.x = "bande", by.y = "id_bd")

#calcul du nombre de sinitres par bande
prof2$exp_nb_loss =(LR*prof2$EarnedPremium/prof2$DR)/prof2$MaxExpo

#on somme sur toutes les bandes pour le nombre total de sinistres
total_exp_nb = sum(prof2$exp_nb_loss)
print('Nb total de sinistres')
print(round(total_exp_nb))

# sinistres > 5 000 000 euros
loss_mean = aggregate(perte~bande,data=simu,mean) # perte moyenne par bande
prof3 = merge(loss_mean, prof2, by = "bande")
prof4 = subset(prof3, prof3$perte >= seuil)

#calcul du nombre de sinitres > seuil par bande
prof4$exp_nb_loss =(LR*prof4$EarnedPremium/prof4$DR)/prof4$MaxExpo

#on somme sur toutes les bandes pour le nombre total de sinistres > seuil
total_exp_nb2 = sum(prof4$exp_nb_loss)
print('Nb total de sinistres > seuil')
print(round(total_exp_nb2))

#d. cotation, prime pure et prime technique

#Fonction de calcul de la cotation theorique d'un traite
traiteXS = function(data, franchise, portee, nb_reco, taux_reco){
  data$recov = pmin(pmax(data$perte-franchise, 0), portee)
  data_ag = aggregate(data[,c("perte", "recov")], by=list(data$bande), FUN=sum)
  data_ag$recov_net = pmin(pmax(data_ag$recov,0), portee*(1+nb_reco)) #AAL = (n+1)*b
  
  #cotation theorique
  prime_pure = mean(data_ag$recov_net)/(1+mean(data_ag$recov_net)/portee*taux_reco)
  prime_tech = (prime_pure + 0.2*sd(data_ag$recov_net))/(1-0.15) #chargement
  
  return(list('PP' = prime_pure, 'PT' = prime_tech))
}

# Application aux donnees de l enonce
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

# Diagrammes en barre
barplot(PP,names.arg = traite,main = 'Prime pure par traite',col='blue')
barplot(PT,names.arg = traite,main = 'Prime technique par traite',col='red')
