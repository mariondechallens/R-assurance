#### Chargement des donn?es ----  

dir = "C:/Users/Admin/Documents/Centrale Paris/3A/OMA/R?assurance/Examen_2019/Sujet 1/"

#dir = /Users/guillaumeshi/Desktop/OMA/Réassurance/Examen_2019/Sujet 1"
setwd(dir)
loss = read.csv2('Loss.csv')
prof = read.csv2('Profile.csv')
endo = read.csv2('Endorsement.csv')

# fusion des dataframes sur les pertes (loss) et expositions (endo) sur le numéro de police
loss_endo = merge(endo, loss, by="UsualEndorsementId")

mean(loss_endo$LossTotal / loss_endo$Exposure)  # moyenne à 0,658%
median(loss_endo$LossTotal / loss_endo$Exposure)  # médiane à 0,032%