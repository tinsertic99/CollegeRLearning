?barplot
barplot(dbinom(x = 0:20,size=20, prob=3/4), names.arg = 0:20)
?dbinom
alpha <-  0.1
#Nas pokusaj
L <- qbinom(p=alpha/2, size = 20, prob = 3/4) - 1
D <- qbinom(p=alpha/2, size = 20, prob = 3/4, lower.tail = FALSE)
#Lower tail vraæa strogo (strogo veæe od 18), a nama treba veæe ili jednako
D <- qbinom(p=alpha/2, size = 20, prob = 3/4, lower.tail = FALSE)+1
#Lijevo kriticno je {0,1,2,3,4,..., 11} U {19,20}
#Po uputama
curve(pbinom(x, size = 20, prob = 3/4), from = 0, to = 20)
abline(a=alpha/2, b=0)
curve(pbinom(x-1, size=20, prob = 3/4, lower.tail = FALSE), from = 0, to = 20, add = TRUE)
#Jakost testa
jakost_testa <- function(p){
  pbinom(q=11, size = 20, prob = p) + pbinom(q=18, size = 20, prob = p, lower.tail = FALSE)
}
curve(jakost_testa)
optimise(jakost_testa, lower = 0, upper = 1)
nas_minimum <- optimise(jakost_testa, lower = 0, upper = 1)$minimum
vrijednost_minimum <- optimise(jakost_testa, lower = 0, upper = 1)$objective
#Nas nacin
alpha2 = 0.05
L2 <- qbinom(p=alpha2/2, size = 20, prob = 3/4) - 1
D2 <- qbinom(p=alpha2/2, size = 20, prob = 3/4, lower.tail = FALSE)+1
L2
D2
#Kriticno podrucje je sada {0,1,..,10} U {19,20}
#Ako smanjimo razinu znaèajnosti, smanjuje se kritièno podruèje.
p1 <- pbinom(q = 12, size = 20, prob = 3/4)
p2 <- pbinom(q = 11, size = 20, prob = 3/4, lower.tail = FALSE)
p <- 2 * min(p1,p2)
#Zato sto je p otprilike 0.2036237, a nase razine znacajnosti su 0.05 i 0.10, sto je manje od naseg p, ne odbacujemo nul hipotezu.
#Zakljucak da ne odbacujemo nul hipotezu smo mogli dobiti promatrajuci kriticna podrucja.