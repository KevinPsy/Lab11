library(tidyverse)

#Q1
rambinom <- function(n,p){
  x <- NULL
  x <- runif(n, min = 0, max = 1)
  return(sum(x < p))
  
}


rambinom(100, 0.7)  
rbinom(1, 100, 0.7)

#Q2
library(microbenchmark)

microbenchmark(rambinom(100, 0.7), rbinom(1, 100, 0.7))

#Q3

x1 <- runif(50, min = 20, max = 40)

y1 <- (15 + 0.4*x1 + rnorm(50, 0,3))

lmodel <- lm(y1~x1)

plot(x1,y1)
abline(lm(y1~x1))

summary(lm(y1~x1))

ggplot(lmodel) + 
  geom_point(aes(x=.fitted, y=.resid))

#Q4
ramnorm<-function(n){
  u1 <- runif(n, 0, 1)
  u2 <- runif(n, 0, 1)
   
  r <- (-2*log(u1))^0.5
  theta <- 2*pi*u2
  
  rv <- c(r*sin(theta),r*cos(theta))
  return(rv[1:(length(rv)/2)])
}

muller <- ramnorm(100)
normal <- rnorm(100, 0, 1)

ggplot()+
  geom_density(aes(normal, fill="normal", alpha=0.5))+
  geom_density(aes(muller, fill="boxmuller", alpha=0.5))
