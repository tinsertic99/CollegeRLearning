getwd()
setwd("C:/Users/Tin/Desktop/CollegeRLearning")
getwd()
#1.1. & 1.2.
mojatablica <- read.table("tlak.dat")
colnames(mojatablica) <- c("Sistolicki", "Dijastolicki")
mojatablica
par(mfrow=c(1,2))
plot(x = mojatablica$Sistolicki, y = mojatablica$Dijastolicki, xlab = "Sistolicki", ylab = "Dijastolicki")
plot(x = mojatablica$Dijastolicki, y = mojatablica$Sistolicki, xlab = "Dijastolicki", ylab = "Sistolicki")
par(mfrow=c(1,1))
#1.3.
X <- mojatablica$Sistolicki
Y <- mojatablica$Dijastolicki
meanx <- mean(X)
meany <- mean(Y)
ox <- sd(X)
oy <- sd(Y)
sxy <- 0
n <- length(X)
for(i in 1:n)
{
  sxy = sxy + (X[i]-meanx)*(Y[i]-meany)
}
R <- sxy/(sqrt((n-1)*ox^2)*sqrt((n-1)*oy^2))
R
#Mogli smo i ovako izracunati Pearsonov koeficijent korelacije
R_uzfunkciju <- cor(X,Y, method = "pearson")
R_uzfunkciju

#1.4.
#H0: p = 0
#Ha: p != 0
?cor.test
test1 <- cor.test(X,Y,method = "pearson", alternative = "two.sided")
test1
test1$conf.int
#Buduæi da je p-vrijednost 0.006855, odbacujemo H0 na svim razinama znacajnosti
test2 <- cor.test(X,Y,method = "pearson", alternative = "greater")
test2
test2$conf.int
#Buduæi da je p-vrijednost 0.003427, odbacujemo H0 na svim razinama znacajnosti

#1.5.
test1$conf.int

#1.6.
kovarijacijska <- as.matrix(cov(mojatablica))
kovarijacijska
sqrt(kovarijacijska[1,1])
sqrt(kovarijacijska[2,2])
#Mozemo i ovako, fora je koristiti kovarijacijsku matricu
#Na taj nacin dobijemo sdx i sdy
meanx <- mean(X)
meany <- mean(Y)
sdx <- sd(X)
sdy <- sd(Y)
rho <- R_uzfunkciju
#1.7.
fjagustoce <- function(x,y){
  xpom <- (x-meanx)/sdx
  ypom <- (y-meany)/sdy
  vrijednost <- 1/(2*pi*sdx*sdy*sqrt(1-rho^2))*exp(-1/(2*(1-rho^2))*
                (xpom^2 - 2*rho*xpom*ypom + ypom^2))
  return(vrijednost)
}
?outer
?persp
meanx
x <- seq(120,230)
meany
y <- seq(50,160)
z <- outer(x,y,FUN=fjagustoce)
#z je 111x111 matrica vrijednosti fje gustoce
#persp crta u 3D, pogled pod kutevima theta i phi
persp(x,y,z,theta=30,phi=25,col="gold")
?persp
#1.8.
#1.8.1.
?qchisq
r_2 <- qchisq(seq(from=0.1,by=0.1,to=0.9),df=2)
#Mali komentar od Tina, chisq(df = 2), predstavlja X^2+Y^2, za
#X i Y standardne normalne sluèajne varijable, a ako nas zanima
#X^2+Y^2 <= r^2, trazimo kvantile sa qchisq(0.1,...,0.9) i dobijemo radijuse,
#koje je potrebno korijenovati zbog formule kruznice
r <- sqrt(r_2)
r

#1.8.2.
kovarijacijska
library(expm)
help(sqrtm)
A <- sqrtm(kovarijacijska)
A #ekvivalent stadnardne devijacije

#1.8.3.
A%*%t(A) #dobro je
kovarijacijska

#1.8.4.
mean_ukupno <- c(meanx,meany)
mean_ukupno

#1.8.5.
#fja elipsa za dani kut i radijus racuna tocke
elipsa <- function(r, kut){
  pocetna <- c(r*cos(kut), r*sin(kut))
  izlaz <- A%*%pocetna+mean_ukupno
  return(izlaz)
}
elipsa(1, pi/3)
phi <- seq(from=0,to=2*pi,by=0.1)
phi

install.packages("rgl")
library("rgl")
#ide pravo crtanje, ide gas
matrica_tocaka <- matrix(data = 0, nrow = length(x)*length(y), ncol = 3)
for(i in 1:length(x)){
  for(j in 1:length(y)){
    matrica_tocaka[(i-1)*length(x) + j, ] <- c(x[i],y[j], fjagustoce(x[i],y[j]))
  }
}
plot3d(matrica_tocaka, col = "blue", size = 1.0, xlab = "X", ylab = "Y", zlab = "Z")
for(j in r){
  tocke <- matrix(0,nrow=length(phi),ncol=3)
  for(i in 1:length(phi)){
    tocka <- elipsa(j,phi[i])
    tocke[i,]=c(tocka,fjagustoce(tocka[1],tocka[2]))
  }
  lines3d(tocke, col = "red")
}
?lines3d


