#Proba na pocetku uz prezentaciju
a <- c(1,2,3,4,5)
b <- c(2,15,12,13,16)
lines(a,b)
abline(1,10)
plot(a,b)
par(mfrow = c(2,2))
plot(a,b)
par(mfrow = c(1,1))
barplot(a)
hist(a)
c <- c(1,1,1,5,11,11,13,17)
barplot(c)
hist(c, probability = TRUE)
dpois(x=3, lambda =1.2)
rgamma(n = 100, shape = 2, rate = 3)
phyper(q = 4, m=10, n=8, k=6)
#Ovo gore su parametri, i dobivamo vjerojatnost (funkcija distribucije) da izvucemo manje ili jednako četiri bijele kuglice
?phyper()
qbeta(p=1/2, shape1 = 2, shape2=2)
?qbeta

#Asistent je pisao
x <- rnorm(100)
y <- c(2,6,7,8)
barplot(y, names.arg = 1:4)
barplot(x)
model <- hist(x, breaks = c(-3,-2,-1,-0.5,0,0.5,1,2,3))
model
#breaks - prekidi
#counts koliko je opservacija u kojem razredu
#density - visina
curve(dnorm(x), add=TRUE)
hist(y)
#Važno razlikovati za kolokvije
#U barplot idu frekvencije, a u histogram uzorak

#Zadaci
#Učitavanje podataka
#Učitamo sa import datasetom portiri.dat
nrow(podaci)
ncol(podaci)
x <- podaci$V1
x <- podaci[,1]
x <- t(x)
x
frekvencije <- podaci$V2
frekvencije
frekvencije <- t(frekvencije)
frekvencije
?order()
#Generalna proba
c <- c(2,3,5,1)
order(c)
podaci
poredak <- order(frekvencije, decreasing = TRUE)
poredak
sortirano <- podaci[poredak,]
podaci
#Grafički prikaz podataka
#Preskačemo prva dva
?plot()
plot(x,frekvencije, xlab = "Broj usnulih portira", ylab = "frekvencije", main = "SP1")
lines(x,frekvencije)
?barplot()
#po indexima
barplot(frekvencije, names.arg = 1:nrow(podaci))
#ili
barplot(frekvencije, names.arg = 1:length())
#po vrijednostima
barplot(frekvencije, names.arg = x)
#barplot - stupčasti dijagram
#Zadatak 2.6
barplot(frekvencije/1000, names.arg = x)
poredak1 = order(frekvencije/1000, decreasing = 
                   TRUE)
#poredak1 daje indexe poslagane po veličini
#Proba napravljenog:
barplot(frekvencije[poredak1]/1000,names.arg = x[poredak1])
#Moramo popravit
poredak2 = poredak1[1:5]
barplot(frekvencije[poredak2]/1000,names.arg = x[poredak2])

#Vjerojatnosne funkcije u R-u
hist(runif(100))
normalne <- rnorm(1000)
hist(normalne)
curve(dnorm,add=TRUE)
hist(normalne, probability = TRUE)
curve(dnorm,add=TRUE)
hist(pnorm(normalne))
#Transformirani podaci dolaze iz uniformne distribucije
#3.7
pbinom(2, size=10, prob=1/6)
?Hypergeometric()
#Hipergeomtrijska - biramo bijele loptice iz crno/bijele kutije i ne vraćamo loptice nazad
#Cure su bijele loptice
#zanima nas d, jer je to vjerojatnost (u diskretnom slucaju) da je x jednak nekoj vrijednosti
dhyper(x = 2, m = 9, n = 11, k = 5)

#Funckije u R-u
#Za domaću zadaću
#Zadatak 4.1
generator <- function(lambda, n){
  my_sample <- pexp(runif(n),lambda)
  return(list(my_sample, 1/mean(my_sample)))
}
#Testiramo što smo dobili
my_sample <- generator(4,10000)
#Prikaz uzorka iz liste
hist(unlist(my_sample[1]), main = "Histogram generiranog uzorka", xlab = "X vrijednosti")
#Vrijednost drugog povratnog argumenta
recip_prosjeka <- unlist(my_sample[2])
recip_prosjeka
#Reciprocna vrijednost prosjeka uzorka iznosi otprilike lambda/(lambda-1)
