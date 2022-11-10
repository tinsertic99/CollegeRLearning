getwd()
setwd("C:/Users/Tin/Desktop/CollegeRLearning")
getwd()
podaci <- read.table("podaci.dat")
#Mozemo i sa import dataset
etruscani <- podaci$V1
n <- length(etruscani)
etruscani
minimum <- min(etruscani)
maximum <- max(etruscani)
?seq()
breakovi <- seq(from = minimum, to = maximum, length.out=12)
hist(etruscani, breaks = breakovi)
?hist
breakovi
#modeliramo normalnom jer nam histogram liči na normalnu razdiobu
?mean
#Potrazili smo na internetu i nasli da procjenitelji metodom momenata za ocekivanje i varijancu se racunaju na sljedeci nacin
nikapa <- mean(etruscani)
sigmakapakvadrat <- sum((etruscani-nikapa)^2)/n
hist(etruscani, breaks = breakovi, probability = TRUE)
curve(expr = dnorm(x, nikapa, sqrt(sigmakapakvadrat)), add = TRUE)
#Zaključujemo da "podaci" imaju normalnu distribuciju
#Pearsonov hikvadrat test
#granice <- breakovi[-c(2,3,4,10,11)]
#hist(etruscani, breaks = granice, probability = FALSE)
granice <- breakovi
#Grupiranje radimo na osnovu teoretskih frekvencija (no to kasnije)
granice
granice_pomocni_prvi <- granice[-1]
granice_pomocni_drugi <- granice[-12]
p <- pnorm(q = granice_pomocni_prvi, mean = nikapa, sd = sqrt(sigmakapakvadrat)) - pnorm(q = granice_pomocni_drugi, mean = nikapa, sd = sqrt(sigmakapakvadrat))
#Inače bi za prvi razred, oduzimali vjerojatnosti od drugog minus prvog (u smislu granica), "ali, želimo maknuti prvu granicu"
p[1] <- pnorm(q = granice[2], mean = nikapa, sd = sqrt(sigmakapakvadrat))
p[11] <- pnorm(q = granice[11], mean = nikapa, sd = sqrt(sigmakapakvadrat), lower.tail = FALSE)
#za prvu vjerojatnost smo uzeli pnorm od druge granice (kraja prvog razreda)
#za zadnju vjerojatnost smo uzeli obrnuti pnorm od početka zadnjeg razreda (a to je 11.granica)
#to smo radili zbog toga sto nam normalna distribucija ide od -besk do +besk
sum(p)
ocek_frekv = p*n
ocek_frekv
ocek_frekv_novo <- c(sum(ocek_frekv[c(1,2,3,4)]), ocek_frekv[c(5,6,7,8,9)], ocek_frekv[10]+ocek_frekv[11])
ocek_frekv_novo
opazene_frekv <- hist(etruscani, breaks = breakovi)$counts
opazene_frekv
#pomoglo nam je ?hist, jer smo pogledali argumente, ali vaznije od toga values, ono sto vraca, i tamo pronasli counts
opazene_frekv_novo <- c(sum(opazene_frekv[c(1,2,3,4)]), opazene_frekv[c(5,6,7,8,9)], opazene_frekv[10]+opazene_frekv[11])
opazene_frekv_novo
#Grupiranje radimo na osnovu teoretskih frekvencija, ne na osnovu pravih, zato i dalje imamo jedan razred sa 4
df = 7 - 2 - 1
#(7 razreda +  2 parametra - 1)
H = sum(((opazene_frekv_novo-ocek_frekv_novo)^2)/ocek_frekv_novo)
H
pvalue <- 1 - pchisq(q=H, df = df)
pvalue
#Zelimo P(H>h|H0), pa zbog toga ili pisemo 1 - p, ili lower.tail = FALSE
?pchisq
#Dobili smo p-vrijednost od 0.6244 sto je vece od svih razina znacajnosti 0.01, 0.05,0.10, pa ne odbacujemo nultu hipotezu ni na jednoj razini znacajnosti
#Odnosno prihvatljiva je pretpostavka da je normalno distribuirana
qchisq(p = 0.95, df = df)
#kriticno podrucje je [9.487729, +besk], za te H-ove 

#Zadnja formula na predzadnjem slajdu
alpha = 0.05
#Zanimljivo, pogledaj crtez ako te zanima, (objasnjenje zasto pisemo 1-alpha/2)
mean(etruscani)-qt(1-alpha/2, df = n-1)*sd(etruscani)/sqrt(n)
mean(etruscani)+qt(1-alpha/2, df = n-1)*sd(etruscani)/sqrt(n)
#Zad 1.13
#Zadnji slajd - srednja fomrula
#cijelo vrijeme su nam ni i sigmakvadrat nepoznate, zato koristimo te formule
#kod nesimetricnih distribucija imamo standardnu interpretaciju kvantila, tako i kod hikvadrat
(n-1)*var(etruscani)/qchisq(1-alpha/2,df=n-1)
(n-1)*var(etruscani)/qchisq(alpha/2,df=n-1)
sqrt((n-1)*var(etruscani)/qchisq(1-alpha/2,df=n-1))
sqrt((n-1)*var(etruscani)/qchisq(alpha/2,df=n-1))
x = 4
n = 11
F <- function(p){
  pbinom(x, size = n, prob = p)
}
curve(F, from = 0, to = 1)
G <- function(p){
  1 - pbinom(x-1, size = n, prob = p)
}
curve(G, from = 0, to = 1)
alpha = 0.1
abline(a=alpha/2, b=0)
#ovaj p daje lijevu granicu pouzdanog intervala (sjecište-koordinata na x osi, je p)
Gpom <- function(p){
  G(p)-alpha/2
}
curve(Gpom, from = 0, to = 1)
help(uniroot)
uniroot(f = Gpom, interval = c(0,0.2))
uniroot(f = Gpom, interval = c(0,0.2))$root
#Dobili smo lijevu granicu
Fpom <- function(p){
  F(p)-alpha/2
}
curve(Fpom, from = 0, to = 1)
abline(0,0)
uniroot(f = Fpom, interval = c(0.6,0.6))
uniroot(f = Fpom, interval = c(0.6,0.8))$root
#Dobili smo desnu granicu
