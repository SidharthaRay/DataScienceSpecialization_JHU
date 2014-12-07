#1
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)
sum(w*x)/sum(w)
#Ans 0.1471

#2
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
coef(lm(y~x-1))
#Ans 0.8263

#3
library(datasets)
data(mtcars)
mtcars$mpg
yc <- mtcars$mpg - mean(mtcars$mpg)
xc <- mtcars$wt - mean(mtcars$wt)
beta1 <- sum(yc * xc) / sum(xc ^ 2)
c(beta1, coef(lm(mtcars$mpg ~ mtcars$wt))[2])
#Ans -5.344

#4
#B1=Cor(Y,X)Sd(Y)/Sd(X) Sd(X) is one half of Sd(Y) Let Sd(Y) = 1 Sd(X) = 0.5 Cor(Y,X) = 0.5 B1=0.51/0.5
0.51/0.5
#Ans 1

#5
sigma_1^2/n_1 + sigma_2^2/n_2
1.5*0.4
#Ans 0.6

#6
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
xn <- (x - mean(x))/sd(x)
xn[1]
#Ans -0.9719

#7
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
lm(y ~ x)
#Ans 1.567

#8
#Ans It must be identically 0.

#9
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
lm(x~1)
#Ans 0.573

#10
x<-mtcars$wt
y<-mtcars$mpg
coef(lm(y~x))[2]/coef(lm(x~y))[2]
var(y)/var(x)
#Ans. var(y)/var(x)