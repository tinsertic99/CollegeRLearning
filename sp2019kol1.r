#Ako primjetite grešku ili ako mislite da bi se neki zadatak trebao
#drukcije rjesiti, molim Vas javite
#Tin Sertić
#Prvi zadatak
getwd()
setwd("C:/Users/Tin/Desktop/StatPrakt1-Kol1")
getwd()
mojatablica <- read.table(file = "statpr-1920-kol1-zad1.dat", header = TRUE)
mojatablica
mojatablica$bmi = mojatablica$tezina / ((mojatablica$visina)^2)
mojatablica
mojatablica[which(mojatablica$bmi>25),]
mojatablica$ime[which(mojatablica$bmi>25)]
razred1 <- mojatablica$bmi[which(mojatablica$razred == 1)]
razred2 <- mojatablica$bmi[which(mojatablica$razred == 2)]
razred3 <- mojatablica$bmi[which(mojatablica$razred == 3)]
razred4 <- mojatablica$bmi[which(mojatablica$razred == 4)]
min1 <- min(razred1)
min2 <- min(razred2)
min3 <- min(razred3)
min4 <- min(razred4)
max1 <- max(razred1)
max2 <- max(razred2)
max3 <- max(razred3)
max4 <- max(razred4)
u1 <- mean(razred1)
u2 <- mean(razred2)
u3 <- mean(razred3)
u4 <- mean(razred4)
var1 <- var(razred1)
var2 <- var(razred2)
var3 <- var(razred3)
var4 <- var(razred4)
#Promatrajući podatke, primijećujemo da aritmetička sredina drugog
#razreda znatno odstupa od aritmetičkih sredina ostalih razreda,
#kao što i maximum drugog razreda znatno odstupa od ostalih razreda
razred1 <- sort(razred1)
razred2 <- sort(razred2)
razred3 <- sort(razred3)
razred4 <- sort(razred4)
pdf(file = "usporedba1d.pdf")
plot(razred1, type = "o", col="red", xlim = c(0,15), ylim = c(16,33), xlab = "Ucenici", ylab = "BMI", main = "Prikaz BMI-a poredanih po veličini po razredima")
lines(razred2, type = "o", col="green")
lines(razred3, type = "o", col = "orange")
lines(razred4, type = "o", col = "blue")
legend(x="bottomright", y="bottom", legend = c("1.razred", "2.razred", "3.razred", "4.razred"), fill = c("red", "green", "orange", "blue"))
dev.off()
pdf(file = "usporedba1e-prvi.pdf")
hist(razred1, probability = TRUE)
curve(dchisq(x, df=25), add = TRUE)
dev.off()
pdf(file = "usporedba1e-cetvrti.pdf")
hist(razred4, probability = TRUE)
curve(dchisq(x, df=25), add = TRUE)
dev.off()
#Razdioba prvog razreda je jako slična hi^2(25) razdiobi, dok
#se razdioba četvrtog razreda znatno razlikuje od hi^2(25) razdiobe.
#Drugi zadatak
mojatablica2 <- read.table(file = "statpr-1920-kol1-zad2.txt", header = FALSE, sep = ",")
mojatablica2 <- as.numeric(mojatablica2)
n <- length(mojatablica2)
moja_funkcija <- function(){
  povratnik <- c()
  for(i in 0:10)
    povratnik[i+1] <- length(which(mojatablica2==i))
  return(povratnik)
}
moje_frekvencije <- moja_funkcija()
sum(moje_frekvencije) == n
#Ovo obiljezje bi moglo pripadati geometrijskoj distribuciji
#Probavao sam ovo, i razmišljao, no to nema previše smisla niti interpretacije
#Bolje se odlučiti za binomnu distribuciju, ima puno više smisla
#Geometrijska (neuspjesi do prvog uspjeha - nema smisla??)
#Bolje je odabrati binomnu (slucajno biram koji ljudi i koliko njih ce imati koju boju ociju)
suma_xeva = sum(moje_frekvencije*(0:10))
suma_xeva
pdf(file = "graf2c.pdf")
#(n povrh 0) * p na 0 puta 1-p (10-0)
curve(x^104*(1-x)^396)
dev.off()
#Ide pravo crtanje
pdf(file = "graf2c.pdf")
MLE <- function (p){
  probs <- dbinom(0:10, size=10, prob = p)
  return(prod(probs^moje_frekvencije))
}
curve(lapply(x,MLE), from = 0, to = 0.4)
dev.off()
curve(104*log(x) + 396*log(1-x))
funkcija <- function(x){
  return(104/x + (-396)/(1-x))
}
curve(funkcija)
uniroot(f = funkcija, interval = c(0.0001,0.9999))
probi <- uniroot(f = funkcija, interval = c(0.0001,0.9999))$root
teoretske_frekvencije <- 50*dbinom(0:10, size=10, prob = probi)
pdf(file = "usporedba2d.pdf")
barplot(matrix(c(teoretske_frekvencije, moje_frekvencije), nrow = 2, byrow = TRUE), names = 0:10, beside = TRUE, legend = c("Teoretske apsolutne frekvencije","Moje apsolutne frekvencije"))
dev.off()
#Dolazi li uzorak iz binomne distribucije?
#Složena hipoteza
#HO: X dolazi iz binomne distribucije
#H1: X ne dolazi iz binomne distribucije
#Prezentacija 4, slajd 9
birkokororom <- function(x){
  return(50*dbinom((0:10), size = 10, prob = x))
}
optiko <- function(x){
  return(sum(((moje_frekvencije-birkokororom(x))^2)/birkokororom(x)))
}
moje_frekvencije
optimise(f=optiko, lower = 0.001, upper = 0.999)
H <- optimise(f=optiko, lower = 0.001, upper = 0.999)$objective
H
#Testna statistika ima chisq(11-1-1) = chisq(9)
p_vrijednost <- pchisq(q = H, df = 9, lower.tail = FALSE)
p_vrijednost
#Vidimo da je p_vrijednost velika, pa ne odbacujemo nultu hipotezu
#ni za jednu razinu znacajnosti (0.01,0.05,0.10)
#Tj. ne mozemo odbaciti da dolazi iz binomne distribucije
pdf(file = "usporedba2f.pdf")
#Relativne znaci na skali od 0 do 1
drzava_opcenito <- dbinom(0:10, size=10, prob = 0.2)
barplot(matrix(c(drzava_opcenito, moje_frekvencije/n), nrow = 2, byrow = TRUE), names = 0:10, beside = TRUE, legend = c("Drzava opcenito","Moj uzorak"))
dev.off()
