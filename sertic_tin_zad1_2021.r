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
#Promatrajuæi raspon uzorka te veliku razliku u aritmetièkoj sredini izmeðu
#prvog i treæeg uzorka, zakljuèujemo da podaci ne sugeriraju da dolaze iz iste 
#distribucije
#Isto tako, možemo zakljuèiti da se na prvom aparatu prodaje dominantno manje kava,
#dok na treæem aparatu ima mjerenja (i vremena) gdje se prodaje znatno vise kava
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
H = sum((opazene_frekv-teoretske_frekv_aps)^2/(teoretske_frekv_aps))
p_vrijednost = pchisq(q = H, df = 10, lower.tail = FALSE)
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