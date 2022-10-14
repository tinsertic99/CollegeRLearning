#Lecture1
#Vektori
x1 <- c(1:10)^2
x1
x2 <- seq(from = 2,by = 2, length.out = 5)^3
x2
x3 <- seq(from = 1, by = 3, length.out = 4)
x3
xnew <- c(x1,x2,x3)
xnew
xnewy <- append(x3, 0, 0)
xnewy
x3 <- c(x3,0)
x3
x1[-c(1,2)]
x1 <- x1[-c(2)]
x1
x1 <- x1[-(2:4)]
x1
x1 <- c(1:10)^2
x1
x1 <- x1[-((length(x1)-1):length(x1))]
x1
x1 <- append(x1, 3, 1)
x1
x1[x1>=49]
which(x2 %% 2 == 0)

#RaÄŤunske operacije s vektorima
x4 <- c(1,5,8)
sum(x4)
prod(x4)
x4^2
x5 <- sqrt(x4)
x <- c(1:5)
x
pi
y <- sin(x*pi/2)
y
sum(x*y)
x * 0.1
#Polako gradimo rjesenje zadatka 10
seq(from=3, by = 3.5, length.out = 7)
which(seq(from=3, by = 3.5, length.out = 7)>10)
prod(which(seq(from=3, by = 3.5, length.out = 7)>10))

#Liste i faktori
w <- list(x1, voce="ananas", sinusi = y)
w
prvi <- w[[1]]
prvi
w$voce = "banana"
w
w$sinusi=w$sinusi^2
w
w[[length(w)+1]] = c("Ivica", "Marica")
w
names(w)[length(w)] <- "bajka"
w
#Alternativno w$bajka <- c("Ivica", "Marica")
w$voce = NULL
w
#Ĺ to se dogodilo?
sudionici = factor(c("Slovenija", "Austrija", "Austrija", "Slovenija", "Slovenija", "Hrvatska"))
sudionici
levels(sudionici) <- c(levels(sudionici), "MaÄ‘arska")
sudionici
sudionici[length(sudionici)+1] = "MaÄ‘arska"
sudionici
levels(sudionici)
levels(sudionici)<-c("BeÄŤ", "Zagreb", "Ljubljana", "BudimpeĹˇta")
sudionici

#Lecture2
#Intro
M <- matrix(data = c(1,2,3,4,5,6,7,8,9), nrow = 3, byrow = TRUE)
M
cbind(M, c(0,0,0))
rbind(M, c(0,0,0))
#Matrices
a <- c(1,2,3,4,5)
b <- c(1,2,3,4,5)
m <- a%*%t(b)
m
x
t(x)%*%m
r <- rep(-0.1, 5)
r
t <- c(1:5)
t
mm <- t%*%t(r)
mm
#Po defaultu su svi stupac
t(mm)
mn = m + mm
mn
t(x)%*%mn
x
mn%*%t(x)
mn%*%m
m%*%mn
install.packages("expm")
library(expm)
m
m%^%3
a <- c(1:3)
jedin <- rep(1,3)
jedin
prvi_clan_kvad = (a*a)%*%t(jedin)  
prvi_clan_kvad
drugi_clan_kvad = jedin%*%t(a*a)
drugi_clan_kvad
mjesoviti = 2*a%*%t(a)
mjesoviti
m <- prvi_clan_kvad + mjesoviti + drugi_clan_kvad
m
det(m)
i <- diag(1,3)
i
#Ax = i
mi <- solve(m,i)
mi
m%*%mi
mi
mf <- as.vector(mi)
#Proba
a = matrix(c(1,2,3,4), nrow = 2, byrow = TRUE)
a
as.vector(a)
#As vector ide po stupcima
mf <- t(mf)
mf
matrix(mf, nrow =3, byrow= FALSE)

#Data frameovi
studenti <- read.table("studenti.txt", header = TRUE, sep = " ", stringsAsFactors = FALSE)
studenti
colnames(studenti)[3] <- "Diplomski_studij"
colnames(studenti)
studenti
studenti <- rbind(studenti, c("Tin", "Sertic", "FM", 1191243844, 100))
studenti
studenti$Postotak = as.numeric(studenti$Postotak)
položio = c(studenti$Postotak > 60)
položio
položio <- replace(položio, which(položio == TRUE),"DA" )
položio <- replace(položio, which(položio == FALSE), "NE")
položio
studenti$Položio = položio
studenti
studenti$Postotak = NULL
studenti
#varijanta studenti <- subset(studenti, Ime != "Ivan" & Prezime != "Horvat")
studenti <- studenti[!(studenti$Ime == "Ivan" & studenti$Prezime == "Horvat"),]

#Funkcije
f <- function(x){
  y = 1/sqrt(2*pi) * exp(-x^2/2)
  return(y)
}
f(0)
dnorm(0,0,1)
curve(f, from = -4, to = 4)
curve(dnorm(x,0,1), from = -4, to = 4)
