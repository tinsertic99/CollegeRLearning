#Importao sam podatke sa import dataset
aparat1 <- mojatablica[which(mojatablica$V2 == 1),]
aparat2 <- mojatablica[which(mojatablica$V2 == 2),]
aparat3 <- mojatablica[which(mojatablica$V2 == 3),]
48 + 55 + 52 == 155
max1 <- max(aparat1[,1])
max2 <- max(aparat2[,1])
max3 <- max(aparat3[,1])
aps1 <- c()
aps2 <- c()
aps3 <- c()
for(i in 0:max1)
{
  aps1[i+1] = length(which(aparat1[,1] == i))
}
for(i in 0:max2)
{
  aps2[i+1] = length(which(aparat2[,1] == i))
}
for(i in 0:max3)
{
  aps3[i+1] = length(which(aparat3[,1] == i))
}
sum(aps1)
sum(aps2)
sum(aps3)
rel1 <- aps1/sum(aps1)
rel2 <- aps2/sum(aps2)
rel3 <- aps3/sum(aps3)
sum(rel1)
sum(rel2)
sum(rel3)
tam <- replicate(10, 0)
rel1 <- c(rel1, replicate(max(max1,max2,max3) + 1 - length(rel1),as.numeric(0)))
rel2 <- c(rel2, replicate(max(max1,max2,max3) + 1 - length(rel2),as.numeric(0)))
barplot(height = matrix(c(rel1,rel2,rel3), nrow = 3, byrow = TRUE), beside = TRUE)
u1 <- mean(aparat1[,1])
u2 <- mean(aparat2[,1])
u3 <- mean(aparat3[,1])
var1 <- var(aparat1[,1])
var2 <- var(aparat2[,1])
var3 <- var(aparat3[,1])
min1 <- min(aparat1[,1])
min2 <- min(aparat2[,1])
min3 <- min(aparat3[,1])
max1 <- max(aparat1[,1])
max2 <- max(aparat2[,1])
max3 <- max(aparat3[,1])
#Promatrajući raspon uzorka te veliku razliku u aritmetičkoj sredini između
#prvog i trećeg uzorka, zaključujemo da podaci ne sugeriraju da dolaze iz iste 
#distribucije
#Isto tako, možemo zaključiti da se na prvom aparatu prodaje dominantno manje kava,
#dok na trećem aparatu ima mjerenja (i vremena) gdje se prodaje znatno vise kava
#Provodimo Pearsonov hi kvadrat test
#MLE za Poissonov daje da lambdu je lambda jednaka aritmetickoj sredini
lambda <- mean(aparat3[,1])
#Sprovodimo test
#Opažene frekvencije
opazene_frekv <- aps3
teoretske_frekv <- c(replicate(length(aps3),0))
for(i in 1:length(teoretske_frekv))
{
  teoretske_frekv[i] = dpois(x = i-1, lambda = lambda)
}
teoretske_frekv[11] = ppois(q = 9, lambda = lambda, lower.tail = FALSE)
sum(teoretske_frekv)
teoretske_frekv_aps <- 52 * teoretske_frekv
sum(teoretske_frekv_aps)
sum(opazene_frekv)
a <- teoretske_frekv_aps
teoretske_frekv_aps <- c(a[1] + a[2] + a[3],a[4:7], a[8] + a[9] + a[10] + a[11])
teoretske_frekv_aps
b <- opazene_frekv
opazene_frekv <- c(b[1] + b[2] + b[3],b[4:7], b[8] + b[9] + b[10] + b[11])
opazene_frekv
H = sum((opazene_frekv-teoretske_frekv_aps)^2/(teoretske_frekv_aps))
p_vrijednost = pchisq(q = H, df = 6-1-1, lower.tail = FALSE)
p_vrijednost
#Kako p_vrijednost nije manja ni od jedne razine znacajnost(0.01,0.05,0.10)
#ne odbacujemo nultu hipotezu da naš uzorak dolazi iz poissonove distribucije
#s parametrom lambda
lambda2 <- u2
lambda2
n <- 55
alfa = 0.01
laU = (2*n*lambda2 + qnorm(p = alfa/2)^2 + sqrt(4*n*lambda2*qnorm(p=alfa/2)^2+qnorm(p=alfa/2)^4))/(2*n)
laL = (2*n*lambda2 + qnorm(p = alfa/2)^2 - sqrt(4*n*lambda2*qnorm(p=alfa/2)^2+qnorm(p=alfa/2)^4))/(2*n)
#Pouzdani interval je [2.603242, 3.84466433]
#E Sad ide pravi nacin kao sa vjezbi
t <- sum(aparat2[,1])
n <- length(aparat2[,1])
n
F <- function(l){
  ppois(t, lambda = n*l)
}

G <- function(l){
  1 - ppois(t - 1, lambda = n*l)
}
curve(F, from = 0, to = 10)
curve(G, from = 0, to = 10, add = T)
#Ocito je da su injekcije. 

Fpom <- function(l){
  F(l) - alpha/2
}

Gpom <- function(l){
  G(l) - alpha/2
}
curve(Fpom, from = 0, to = 10)
curve(Gpom, from = 0, to = 10, add = T)
gornja1 <- uniroot(f = Fpom, interval = c(3,4))$root
donja1 <- uniroot(f = Gpom, interval = c(2,3))$root
donja1
gornja1
#[2.580015,3.835457]
