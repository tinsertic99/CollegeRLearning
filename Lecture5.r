getwd()
setwd( "C:/Users/tinsert/Desktop")
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
#modeliramo normalnom jer nam histogram lièi na normalnu razdiobu
?mean
#Potrazili smo na internetu i nasli da procjenitelji metodom momenata za ocekivanje i varijancu se racunaju na sljedeci nacin
nikapa <- mean(etruscani)
sigmakapakvadrat <- sum((etruscani-nikapa)^2)/n
hist(etruscani, breaks = breakovi, probability = TRUE)
curve(expr = dnorm(x, nikapa, sqrt(sigmakapakvadrat)), add = TRUE)
#Zakljuèujemo da "podaci" imaju normalnu distribuciju
#Pearsonov hikvadrat test
#granice <- breakovi[-c(2,3,4,10,11)]
#hist(etruscani, breaks = granice, probability = FALSE)
granice <- breakovi
#Grupiranje radimo na osnovu teoretskih frekvencija (no to kasnije)
granice
granice_pomocni_prvi <- granice[-1]
granice_pomocni_drugi <- granice[-12]
p <- pnorm(q = granice_pomocni_prvi, mean = nikapa, sd = sqrt(sigmakapakvadrat)) - pnorm(q = granice_pomocni_drugi, mean = nikapa, sd = sqrt(sigmakapakvadrat))
#Inaèe bi za prvi razred, oduzimali vjerojatnosti od drugog minus prvog (u smislu granica), "ali, želimo maknuti prvu granicu"
p[1] <- pnorm(q = granice[2], mean = nikapa, sd = sqrt(sigmakapakvadrat))
p[11] <- pnorm(q = granice[11], mean = nikapa, sd = sqrt(sigmakapakvadrat), lower.tail = FALSE)
#za prvu vjerojatnost smo uzeli pnorm od druge granice (kraja prvog razreda)
#za zadnju vjerojatnost smo uzeli obrnuti pnorm od poèetka zadnjeg razreda (a to je 11.granica)
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
1 - pchisq(q=H, df = df)
#Zelimo P(H>h|H0), pa zbog toga ili pisemo 1 - p, ili lower.tail = FALSE
?pchisq
#Dobili smo p-vrijednost od 0.6244 sto je vece od svih razina znacajnosti 0.01, 0.05,0.10, pa ne odbacujemo nultu hipotezu ni na jednoj razini znacajnosti
#Odnosno prihvatljiva je pretpostavka da je normalno distribuirana
qchisq(p = 0.95, df = df)
#kriticno podrucje je [9.487729, +besk], za te H-ove odbacujemo
