# Seminar 1, Group 3

# values
x <- 3 + sin(pi/2)
x
sqrt(x)
x <- sqrt(x)

y <- "word"
z <- "2"

## Sequence

v <- seq(1,10)
v2 <- 1:10

for(i in 1:10) {
  print(i)
}

v3 <- seq(0,100,10)

## vectors

w <- c(12,1,2,3)
sort(w)
length(w)
typeof(w)

## indexing
w[3]

n <- length(w)
for(i in 1:n) {
  print(w[i])
}

## coercion: changing type

n <- length(w)
for(i in 1:n) {
  print(as.character(w[i]))
}
## likewise, as.numeric() as.integer() 

## packages

## tidyverse
#install.package("tidyverse")
library(tidyverse)

car_frame <- mpg

ggplot(data=car_frame) +
  geom_point(mapping=aes(x=displ,y=hwy, color=class)) 

ggplot(data=car_frame) +
  geom_point(mapping=aes(x=displ,y=hwy)) + 
  geom_smooth(mapping=aes(x=displ,y=hwy))