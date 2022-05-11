library(rjags)
#Create come simple data
#Not hidden
#Data are times and states
set.seed(1234)
t <- c(0,cumsum(rexp(16,1)))
s <- c(2,1,2,1,2,1,2,1,2,3,1,2,3,1,2,1,2)

set.seed(1234)
t <- c(0,cumsum(rexp(100,1/10)),1000)
d <- diff(t)
s <- c(2,rep(c(1,2,3),length = 100))
#Two states
init <- 1
pi <- c(0,1,0)
n <- as.matrix(table(s[-length(s)], s[-1]))

#Total amount of time spent in state
tot <- c()
for (j in 1:3) {
  tot[j] <- sum(diff(c(t,120))[s == j])
}
ind1 <- matrix(c(0,1,2,3,0,4,5,6,0), byrow = TRUE, ncol = 3)
ind2 <- matrix(c(1,2,3,4,5,6), byrow = TRUE, ncol = 2)
likelihood <- function(q){
  out <- 0
  for (i in 1:3){
    for (j in 1:3){
      if (i != j){
        out <- out + log((q[ind1[i,j]] > 0)*q[ind1[i,j]]^n[i,j] * exp(-(sum(q[ind2[i,]]))*tot[i]))
      }
    }
  }
  return(-out)
}


init <- rep(0.1,6)
likelihood(init)
test <- optim(init, likelihood, control = list(maxit = 10000))

matrix(test$par,ncol = 2, byrow = TRUE)

