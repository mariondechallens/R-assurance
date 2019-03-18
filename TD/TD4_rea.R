dir = "C:/Users/Admin/Documents/Centrale Paris/3A/OMA/Réassurance/Cours 8 - calibration d'un traité/"

library(rpart)
library(rpart.plot)
library(fitdistrplus)
library(lattice)
## surplus : traité proportionnel
# calcul de taux de cession à appliquer à tous les sinistres : calibrer le seuil
# limiter la volatilité du portefeuille, céder les gros sinistres

## 1. chargement des données ----
data = read.csv2(paste0(dir,"Auto_Insurance_Claims_Sample.csv"))

data2 = subset(data, select = -c(1,4,8))

## 2. Arbre de décision pour segmentation du surplus ----
# surplus sur la sinistralité : total claimed amount

cart = rpart(formula=Total.Claim.Amount ~.,data=data2,minsplit=50)
rpart.plot(cart)
plotcp(cart)
pruning = prune(cart,cp = 0.028)

rpart.plot(pruning)
plotcp(pruning) # cede au surplus : suburban, prime > 93 (>5000)

## 3. Calibration du surplus ----
cedes = subset(data2, data2$Total.Claim.Amount >=5000)
pct_num = nrow(cedes)/nrow(data2)*100
pct_val = sum(cedes$Total.Claim.Amount)/sum(data2$Total.Claim.Amount)*100

surplus = subset(data2, data2$Location.Code == 'Suburban' & data2$Monthly.Premium.Auto>=93)
data2$surplus = rep(0,nrow(data))
for (i_sini in 1:nrow(data2))
{
  if(data2$Location.Code[i_sini] == 'Suburban' & data2$Monthly.Premium.Auto[i_sini]>=93)
    data2$surplus[i_sini] = 1
}

rate = function(seuil)
{
  print(seuil)
  #taux de retention site par site
  retention = rep(1,nrow(data2))
  prime_r = data2$Monthly.Premium.Auto
  sinis_r = data2$Total.Claim.Amount
  for (i_sini in 1:nrow(data2))
  {
    if (data2$surplus[i_sini] == 1)
    {
      retention[i_sini] = min(1,seuil/data2$Sum.insured.aged[i_sini])
      prime_r[i_sini] = retention[i_sini]*data2$Monthly.Premium.Auto[i_sini]
      sinis_r[i_sini] = retention[i_sini]*data2$Total.Claim.Amount[i_sini]
    }
  }
  
  #gain de vol
  gain = sd(sinis_r)/sd(data2$Total.Claim.Amount)*100
  #taux de retention de prime
  trp = sum(prime_r)/sum(data2$Monthly.Premium.Auto)*100
  
  return (list('gain' =gain, 'taux' =trp))
}

seuil = seq(10000,30000,500)
res = mapply(rate,seuil)

Fg = function(t,g)
{
  x = t-g/2
  return(x)
}

FF = rep(0,length(seuil))
for (i in 1:length(seuil))
{
  gt = res[,i]
  FF[i] = Fg(gt$taux,gt$gain)
}

plot(FF)
m= max(FF)

seuil[which(FF==m)]

## 4. Application du surplus
seuil = 16500
retention = rep(1,nrow(data2))
prime_r = data2$Monthly.Premium.Auto
sinis_r = data2$Total.Claim.Amount
for (i_sini in 1:nrow(data2))
{
  if (data2$surplus[i_sini] == 1)
  {
    retention[i_sini] = min(1,seuil/data2$Sum.insured.aged[i_sini])
    prime_r[i_sini] = retention[i_sini]*data2$Monthly.Premium.Auto[i_sini]
    sinis_r[i_sini] = retention[i_sini]*data2$Total.Claim.Amount[i_sini]
  }
}

prime_nette = sum(prime_r)
perte_nette = sum(sinis_r)
perte_brute = sum(data2$Total.Claim.Amount)

plot(ecdf(prime_r),col='red')
lines(ecdf(data2$Monthly.Premium.Auto),col='blue')
data2$prime_ret = prime_r
data2$sinis_ret = sinis_r

## 5. Loi fréquence - coût
evt = aggregate(sinis_ret~Event, data = data2, sum)
summary(fitdist(evt$sinis_ret,distr="norm"))
summary(fitdist(evt$sinis_ret,distr="lnorm"))
summary(fitdist(evt$sinis_ret,distr="weibull"))


#weibull
wb = fitdist(evt$sinis_ret,distr="weibull")
esti = wb$estimate

q = seq(0,1,by=1/31)
qqplot(evt$sinis_ret,qweibull(q,shape = esti[1],scale = esti[2]))
abline(0,1)

n_evt = nrow(evt)

#Estimation:
year=seq(1:5000)
nombre=rpois(5000,lambda = n_evt)
seve = rweibull(5000,shape = esti[1],scale = esti[2])


