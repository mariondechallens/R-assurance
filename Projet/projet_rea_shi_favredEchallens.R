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
denscomp(f1, demp=TRUE)
plot(b1, enhance=TRUE)
cdfcomp(f1)

#courbe d'exposition
eccomp(f1, do.points=FALSE)

#### Question 3 ----
# a. 
