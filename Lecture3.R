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
#Ovo gore su parametri, i dobivamo vjerojatnost (funkcija distribucije) da izvucemo manje ili jednako �etiri bijele kuglice
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
#Va�no razlikovati za kolokvije
#U barplot idu frekvencije, a u histogram uzorak

#Zadaci
#U�itavanje podataka
#U�itamo sa import datasetom portiri.dat
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
#Grafi�ki prikaz podataka
#Preska�emo prva dva
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
#barplot - stup�asti dijagram
#Zadatak 2.6
barplot(frekvencije/1000, names.arg = x)
poredak1 = order(frekvencije/1000, decreasing = 
                   TRUE)
#poredak1 daje indexe poslagane po veli�ini
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
#Hipergeomtrijska - biramo bijele loptice iz crno/bijele kutije i ne vra�amo loptice nazad
#Cure su bijele loptice
#zanima nas d, jer je to vjerojatnost (u diskretnom slucaju) da je x jednak nekoj vrijednosti
dhyper(x = 2, m = 9, n = 11, k = 5)

#Funckije u R-u
#Za doma�u zada�u