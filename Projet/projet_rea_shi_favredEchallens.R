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
mean(loss_endo$LossTotal / loss_endo$Exposure)  # moyenne à 0,658%
median(loss_endo$LossTotal / loss_endo$Exposure)  # mediane À 0,032%

# b. fit
library(mbbefd)
loss_endo$DR = loss_endo$LossTotal / loss_endo$Exposure
loss_endo2 = subset(loss_endo, loss_endo$DR <=1) #la fonction de veut pas >= 1
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

# b. Fit loi de sévérité
seuil = 5e3
loss_insured = prof$ExposureAxaShare[prof$ExposureAxaShare >= seuil]  #?

pareto<-function(x,u,alpha)
{
  f=1-(u/x)^alpha
  return(f)
}

#Moment estimation
u=1000000
alpha=mean(loss_insured)/(mean(loss_insured)-u)

qpareto<-function(p,u,alpha)
{
  f=u*(1-p)^(-1/alpha)
  return(f)
}
#QQplot:
q=seq(0,1,by=1/31)
qqplot(loss_insured,qpareto(q,u,alpha))
abline(0,1)

#c. Loss Ratio
LR = 0.7
nb_tot = 




#d. cotation, prime pure et prime tech
