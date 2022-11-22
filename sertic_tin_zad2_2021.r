#Podatke sam importao sa import dataset
ukupno_ljudi <- length(mojatabela2[,1])
ukupno_zena <- length(which(mojatabela2[,1] == "Z"))
ukupno_muskih <- length(which(mojatabela2[,1] == "M"))
postotak_zena <- ukupno_zena/ukupno_ljudi
#b podzatak
#Promatrano obiljezje bi moglo pripadati bernoullijevoj
#distribuciji s parametrom postotak_zena.
prvi_stupac <- mojatabela2[,1]
prvi_stupac[which(prvi_stupac[]=="M")] = 0
prvi_stupac[which(prvi_stupac[] == "Z")] = 1
prvi_stupac <- as.numeric(prvi_stupac)
sn <- var(prvi_stupac)
mean(prvi_stupac)
alfa <- 0.05
donja_granica <- postotak_zena + qt(p=alfa/2, df=ukupno_ljudi-1)*sn/(sqrt(ukupno_ljudi))
gornja_granica <- postotak_zena - qt(p=alfa/2, df=ukupno_ljudi-1)*sn/(sqrt(ukupno_ljudi))
#Na temelju toga ne moemo zakljuèiti da u poduzeæu rade veæinom muškarci
hist(mojatabela2[,2], main = "histogram2c")
#Promatrajuæi dobiveni histogram, ne mogu baš biti siguran da podaci dolaze iz normalne distribucije,
#no ne mogu to ni odbaciti
place <- mojatabela2[,2]
place_poredane <- sort(place)
place_poredane
u1 <- mean(place_poredane)
std1 <- sqrt(var(place_poredane))
place_poredane_standardizirane <- (place_poredane - u1)/std1
moj_max <- 0
n <- length(place_poredane_standardizirane)
for(i in 1:n)
{
  if(moj_max < max(abs((i-1)/n-pnorm(q = place_poredane_standardizirane[i])), abs(i/n-pnorm(q = place_poredane_standardizirane[i]))))
  {
    moj_max = max(abs((i-1)/n-pnorm(q = place_poredane_standardizirane[i])), abs(i/n-pnorm(q = place_poredane_standardizirane[i])))
  }
}
#Realizacija testne statistike
moj_max
#Promatrajuæi tablicu KS testa za n=45 vidimo da je p_vrijednost negdje oko 0.17,
#pa ne odbacujemo nultu hipotezu da su podaci normalno distribuirani 
#ni za jednu razinu znacajnosti (0.01,0.05,0.10)
hist(mojatabela2[,2], main = "histogram2c", probability = TRUE)
curve(dnorm(x,mean = u1, sd = std1), add = TRUE)
#Crtam ovo samo radi sebe
std2 <- 2000
alfa_moj <- 0.01
#H0: u = 7000
#H1: u > 7000
#Testna statistika je Z
Z <- (u1-7000)/2000*sqrt(45)
#Kriticno podrucje
lijevi_rub <- qnorm(p = alfa_moj, lower.tail = FALSE)
#kriticno podrucje je [2,326348,besk]
#A kako je Z element C, odbacujemo H0
#na razini znacajnosti 0.01
p_vrijednost <- pnorm(q = Z, lower.tail = FALSE)
#Kako je p_vrijednost manja od razine znacajnosti alfa,
#odbacujemo nultu hipotezu u korist alternativne na razini znacajnosti alfa
jakost_testa <- function(ni){
  pnorm(q = lijevi_rub, mean = ni, sd = std2)
}
curve(jakost_testa, from = -10000, to=10000)
