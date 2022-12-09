#Start sedmih vjezbi
help("aov")
help("chisq.test")
#1.1
getwd()
setwd("C:/Users/tinsert/Desktop")
?read.table
podaci <- read.table(file="vrpce.dat", stringsAsFactors = TRUE, col.names = c("Kvaliteta", "Premaz"))
podaci
#1.2
help("aov")
model <- aov(formula = Kvaliteta ~ Premaz ,data = podaci)
#formula = brojcana vrijednost ~ grupa
model
#S ovom naredbom dobivamo ANOVA tablicu
summary(model)
#Moja p-vrijednost iznosi 0.0181
#Buduæi da je dobivena p-vrijednost manja od 0.05, odbacujemo H0
#na razini znaèajnosti 5%, tj. postoji statistièki znaèajna razlika

#H0: nema statistièki znaèajne razlike izmeðu prosjeènih kvaliteta
#reprodukcije zvuka izmeðu ova èetiri premaza.
#H1: ima statistièki znaèajne razlike -II-
#Možemo i ovako
#H0: u1 = u2 = u3 = u4
#H1: ne H0

#MST = sjeciste numericke varijable(Premaz) i mean sq stupca
#MSE = sjeciste numericke varijable(Premaz) i mean sq stupca

#MSE = tockovna procjena za o^2 = 5.222

#Malo zezanje za mene
?boxplot
mojbox <- boxplot(formula = Kvaliteta ~Premaz, data = podaci, col = c("red", "blue", "green", "yellow"))
#Možda treba i za 1.3.

#Promatrajuæi nacrtani boxplot možemo zakljuèiti da izmeðu premaza A i premaza B postoji statistièki znaèajna razlika.


#2.1.
x <- 1:6
probs <- rep(1, 6)/6
moj_ravnomjerni_uzorak <- sample(x, size = 200, prob = probs, replace = TRUE)
hist(moj_ravnomjerni_uzorak, breaks = seq(from = 0, to = 6, length = 7))
probs_iskrivljeni <- c(1/12, 1/12, 1/6, 1/6, 1/6, 1/3)
moj_iskrivljeni_uzorak <- sample(x, size = 100, prob = probs_iskrivljeni, replace = TRUE)
hist(moj_iskrivljeni_uzorak, breaks = seq(from = 0, to = 6, length = 7))
#Histogrami su samo za mene
matrica_vjerojatnosti <- matrix(data = c(unname(table(moj_ravnomjerni_uzorak)), unname(table(moj_iskrivljeni_uzorak))), byrow = TRUE, nrow = 2)
matrica_vjerojatnosti
chisq.test(x = matrica_vjerojatnosti)
#Buduæi da je p-vrijednost << 0.05, odbacujemo H0 na razini znajacnosti 0.05,
#tj. da dobiveni podaci potjeèu od iste razdiobe, odnosno na razini znacajnosti od 0.05
#mozemo zakljuciti da dobiveni nizova podataka ne potjeèu od iste vjerojatnostne razdiobe

#Ako u chisq ubacimo dva uzorka koji dolaze iz iste distribucije, p-vrijednosti koje dobivamo su uniformne na (0,1) (možes probat)
?chisq.test
#Test homogenosti na predavanjima, tj testiramo jesu li distribucije dva uzorka jednake, chisq.test

#2.2.
#              10 metara | 100 metara  | Suma
# Glasalo          16          3       | 19
# Nije glasalo     8          18       | 26
#                ______________________
# Suma            24          21         45

#Razina znacajnosti = 0.01

#Koristimo Fisher - Irwinov egzaktni test
#Mislim da radi fisher.test()
?fisher.test
moja_kontigencijska_tablica <- matrix(c(16,3,8,18), nrow = 2, byrow = TRUE)
moj_test <- fisher.test(x = moja_kontigencijska_tablica)
p_vrijednost <- moj_test$p.value
p_vrijednost
#Buduæi da je 0.0006862011 < 0.01, odbacujemo nultu hipotezu na razini znacajnosti
#od 0.01, tj možemo zakljuèiti da je proporcija vjeverica
#koje se glasaju kada ih proganja predator statistièki znaèajno veæa kada
#su bliže domu