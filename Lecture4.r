#Prilagodba modela podacima
podaci <- matrix (c(1:6,71,28,5,2,2,1), nrow = 2, byrow = TRUE)
podaci
n <- sum(podaci[2,])
n
#Stupcasti dijagram
#Zapamti: barplot - stupčasti dijagram
#U barplot idu frekvencije, a u histogram 
#Napomena - relativne frekvencije se traže
barplot(height=podaci[2,]/n, names=podaci[1,])
moja_funkcija <- function(p, freq){
  privremeni_vektor <- dgeom(0:(length(freq)-1), prob = p)^freq
  return(prod(privremeni_vektor))
}
#Pomoc u razmisljanju za zadatak prije
ae <- c(1,2,3,4)
be <- c(2,2,2,2)
length(be)
de <- ae^be
de
prod(de)
#Treba nam vjerojatnost u točki, zato koristimo dgeom, a ne treba nam funkcija distribucije, što bi bilo pgeom
dgeom(1, prob = 1/2)
dgeom(0, prob = 1/2)
dgeom(1:6, prob=1/2)
#Zadatak 1.5
?optimize()
p <- optimize(moja_funkcija, freq = podaci[2,], lower = 0.01, upper = 0.99, maximum = TRUE)
p <- unname(unlist(p[1]))
p
barplot(dgeom(0:5, p), names=podaci[1,])
barplot(matrix(c(podaci[2,]/n, dgeom(0:5,p)), nrow = 2, byrow = TRUE), names = podaci[1,], beside = TRUE)

#Pearsonov hi kvadrat test
ocek_frekv <- n*dgeom(0:5,p)
ocek_frekv[6] <- n*pgeom(4,prob=p, lower.tail = FALSE)
frekv <- podaci[2,]
ocek_frekv
frekv
ocek_frekv_novo = c(ocek_frekv[1:2],sum(ocek_frekv[3:6]))
frekv_novo = c(frekv[1:2], sum(frekv[3:6]))
sum(ocek_frekv_novo)
sum(frekv_novo)
frekv_novo
ocek_frekv_novo
#2.3 nije
#2.4
H =  sum((frekv_novo-ocek_frekv_novo)^2/ocek_frekv_novo)
H
df = 3 - 1 - 1
df
1 - pchisq(q = H, df = df)
#Ne odbacujemo nultu hipotezu da uzorak dolazi iz geometrijske razdiobe
uzorak <- rnorm(n=50, mean = 5, sd = 2)
#oprez, piše N(5,4), ali sd = 2
uzorak <- sort(uzorak)
uzorak
#Računamo qi sa slajda
n=50
qi = qnorm(((1:n)-3/8)/(n+1/4))
qi
plot(x=qi, y = uzorak)
abline(a=5, b=2)
#KS test i Lillie KS test
#KS test hipoteze
#H0: X dolazi iz N(5,4) (jednostavna hipoteza)
#H1: ne H0
test1 <- ks.test(x=uzorak, "pnorm", mean = 5, sd=2)
help(ks.test)
test1
#pogledamo p-value, ne odbacujemo nultu hipotezu
#Lillie KS test
#H0: x dolazi iz N(NI, sigmakvad) (oprez ona radi samo za normalnu razdiobu)
#H1: ne H0
install.packages("nortest")
library("nortest")
lillie.test(x=uzorak)
#Ne odbacujemo nultu hipotezu na svim razinama znacajnosti
#(Veliki p)
