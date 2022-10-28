#Prilagodba modela podacima
podaci <- matrix (c(1:6,71,28,5,2,2,1), nrow = 2, byrow = TRUE)
podaci
n <- sum(podaci[2,])
n
#Stupcasti dijagram
#Zapamti: barplot - stupèasti dijagram
#U barplot idu frekvencije, a u histogram 
#Napomena - relativne frekvencije se traže
barplot(podaci[2,]/n, names=podaci[1,])
moja_funkcija <- function(p, f = podaci[2,]){
  privremeni_vektor <- dgeom(0:(length(f)-1), prob = p)^f
  return(prod(privremeni_vektor))
}
#Pomoc u razmisljanju za zadatak prije
ae <- c(1,2,3,4)
be <- c(2,2,2,2)
length(be)
de <- ae^be
de
prod(de)
#Treba nam vjerojatnost u toèki, zato koristimo dgeom, a ne treba nam funkcija distribucije, što bi bilo pgeom
dgeom(1, prob = 1/2)
dgeom(0, prob = 1/2)
dgeom(1:6, prob=1/2)
#Zadatak 1.5
?optimize()
p <- optimize(moja_funkcija, lower = 0, upper = 1, maximum = TRUE)
p <- unname(unlist(p[1]))
p
barplot(dgeom(0:5, p), names=podaci[1,])
barplot(matrix(c(podaci[2,]/n, dgeom(0:5,p)), nrow = 2, byrow = TRUE), names = podaci[1,], beside = TRUE)

#Pearsonov hi kvadrat test
ocek_frekv <- n*dgeom(0:5,p)
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
df = length(ocek_frekv_novo)-1
df
#df je broj razreda-1
pchisq(q = H, df = df, lower.tail = FALSE)
#Ne odbacujemo nultu hipotezu da uzorak dolazi iz geometrijske razdiobe

