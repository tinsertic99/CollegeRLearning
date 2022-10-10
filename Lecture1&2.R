#Lecture1
#Vektori
x1 <- c(1:10)^2
x1
x2 <- seq(from = 2,by = 2, length.out = 5)^3
x2
x3 <- seq(from = 1, by = 3, length.out = 4)
x3
xnew <- c(x1,x2,x3)
xnew
xnewy <- append(x3, 0, 0)
xnewy
x3 <- c(x3,0)
x3
x1[-c(1,2)]
x1 <- x1[-c(2)]
x1
x1 <- x1[-(2:4)]
x1
x1 <- c(1:10)^2
x1
x1 <- x1[-((length(x1)-1):length(x1))]
x1
x1 <- append(x1, 3, 1)
x1
x1[x1>=49]
which(x2 %% 2 == 0)

#Računske operacije s vektorima
x4 <- c(1,5,8)
sum(x4)
prod(x4)
x4^2
x5 <- sqrt(x4)
x <- c(1:5)
x
pi
y <- sin(x*pi/2)
y
sum(x*y)
x * 0.1
#Polako gradimo rjesenje zadatka 10
seq(from=3, by = 3.5, length.out = 7)
which(seq(from=3, by = 3.5, length.out = 7)>10)
prod(which(seq(from=3, by = 3.5, length.out = 7)>10))

#Liste i faktori
w <- list(x1, voce="ananas", sinusi = y)
w
prvi <- w[[1]]
prvi
w$voce = "banana"
w
w$sinusi=w$sinusi^2
w
w[[length(w)+1]] = c("Ivica", "Marica")
w
names(w)[length(w)] <- "bajka"
w
#Alternativno w$bajka <- c("Ivica", "Marica")
w$voce = NULL
w
#Što se dogodilo?
sudionici = factor(c("Slovenija", "Austrija", "Austrija", "Slovenija", "Slovenija", "Hrvatska"))
sudionici
levels(sudionici) <- c(levels(sudionici), "Mađarska")
sudionici
sudionici[length(sudionici)+1] = "Mađarska"
sudionici
levels(sudionici)
levels(sudionici)<-c("Beč", "Zagreb", "Ljubljana", "Budimpešta")
sudionici

#Lecture2
