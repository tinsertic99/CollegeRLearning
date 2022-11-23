#Distribucija je B(45,p).
t <- 21
n

F1 <- function(p){
  pbinom(t, size = 45, prob = p)
}

G1 <- function(p){
  1 - pbinom(t - 1, size = 45, prob = p)
}

curve(F1, from = 0, to = 1)
curve(G1, from = 0, to = 1, add = T)

#Ocito je da su injekcije. 

Fpom1 <- function(p){
  F1(p) - alpha/2
}

Gpom1 <- function(p){
  G1(p) - alpha/2
}

curve(Fpom1, from = 0, to = 1)
curve(Gpom1, from = 0, to = 1, add = T)
abline(a = 0, b = 0)

gornja2 <- uniroot(f = Fpom1, interval = c(0.6,0.8))$root
donja2 <- uniroot(f = Gpom1, interval = c(0.2,0.4))$root
donja2
gornja2
