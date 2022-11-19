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
#Promatrajuæi podatke, primijeæujemo da aritmetièka sredina drugog
#razreda znatno odstupa od aritmetièkih sredina ostalih razreda,
#kao što i maximum drugog razreda znatno odstupa od ostalih razreda
razred1 <- sort(razred1)
razred2 <- sort(razred2)
razred3 <- sort(razred3)
razred4 <- sort(razred4)
pdf(file = "usporedba1d.pdf")
plot(razred1, type = "o", col="red", xlim = c(0,15), ylim = c(16,33), xlab = "Ucenici", ylab = "BMI", main = "Prikaz BMI-a poredanih po velièini po razredima")
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
#Razdioba prvog razreda je jako slièna hi^2(25) razdiobi, dok
#se razdioba èetvrtog razreda znatno razlikuje od hi^2(25) razdiobe.
