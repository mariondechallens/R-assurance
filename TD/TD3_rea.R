library(maptools)
library(raster)
library(dplyr)

### 1. Importation des donnees ----

data = read.csv2("C:/Users/Admin/Documents/Centrale Paris/3A/OMA/Réassurance/Réassurance 7 - Modélisation cat/data.csv")
clients = read.csv2("C:/Users/Admin/Documents/Centrale Paris/3A/OMA/Réassurance/Réassurance 7 - Modélisation cat/Clients Portfolio.csv")

data$latitude = as.numeric(as.character(data$latitude))
data$longitude = as.numeric(as.character(data$longitude))
data$mag = as.numeric(as.character(data$mag))
clients$Longitude = as.numeric(as.character(clients$Longitude))
clients$Latitude = as.numeric(as.character(clients$Latitude))
clients$SumInsured = as.numeric(as.character(clients$SumInsured))

# Chargement des frontières de la Turquie
map = readShapeSpatial("C:/Users/Admin/Documents/Centrale Paris/3A/OMA/Réassurance/Réassurance 7 - Modélisation cat/Borders/borders", proj4string=CRS("+proj=longlat"))

# Affichage de la carte de la Turquie

points=SpatialPoints(na.omit(data[,c("longitude","latitude")]))


plot(map,main= 'Zones à risque')
points(points,pch=4,col="red")
symbols(x = data$longitude, y= data$latitude, squares = data$mag*10^(-100),add = TRUE,fg = 'black',bg = 'red',inches = FALSE)


### 2. Modélisation ----
# maillage 2D

r = raster(extent(map),nrow = 100,ncol=100)
mat = rasterize(points,r,fun='count')
seis_cell = mat@data@values
seis_cell[is.na(seis_cell)] = 0
sum(seis_cell)
plot(mat,main = 'Nombre de seismes par cellule')

# distribution de la magnitude
mag = data$mag
#nb_seisme = aggregate(mag~year, data = data, n_distinct)
summary(mag)
h = seq(3,7.7,by=0.1)

guten_r = function(h)
{
  s = sum(mag > h)/length(mag)  #normaliser par nombre total de séismes
  return(log(s))
}

loi = mapply(guten_r,h)
plot(h,loi,main = 'Loi de X',type = 'l')
lm1 = lm(loi~h)
abline(lm1,col='red')
c =lm1$coefficients  #a  = 5,33 et b = 1,76, log(s) = a-b*h
a = c[1]
b = -c[2]
### 3. Simuler les épicentres et les magnitudes ----
# tirer magnitudes et épicentres
# magnitudes
unif = runif(1000)
h = -(log(unif) - a)/b

#epicentres
epi = sample(1:length(seis_cell),1000,prob = seis_cell,replace=TRUE)
epi_coor = xyFromCell(mat,epi)

#Pour chaque événement, calculer la distance des sites à l'épicentre
points_c=SpatialPoints(na.omit(clients[1:5000,c("Longitude","Latitude")]))
epi_pt = SpatialPoints(epi_coor)
dist = spDists(epi_pt,points_c,longlat=TRUE)

#affectation 
sum(dist <= 30)


#vulnérabilité
vuln = function(mag)  # ou fonction approx
{
  if (mag<4)
    vuln = 0
  if (mag>9)
    vuln = 1
  if(mag >=4 && mag <=9)
  {
    a = (1-0)/(9-4)
    b = -4/5
    vuln = a*mag + b
  }
  return(vuln)
}
v = mapply(vuln,mag)
plot(mag,v)

vulnerabilite = dist
for (i in 1:1000)
{
  for (j in 1:5000)
  {
    if (dist[i,j] > 30)
      vulnerabilite[i,j] = 0
    else
      vulnerabilite[i,j] = vuln(h[i])
      
    
  }
}
sum(vulnerabilite >0)

# perte maximale possible associee au risque de seisme
assu = clients$SumInsured[1:5000]
perte = vulnerabilite
for(i in 1:1000)
{
  for (j in 1:5000)
  {
    perte[i,j] = vulnerabilite[i,j]*assu[j]
  }
}

perte_sinis = apply(perte,1,sum) #perte par seisme
perte_site = apply(perte,2,sum) #perte par site

plot(h,perte_sinis,main='Perte par seisme',xlab = 'magnitude')
plot(perte_site)
