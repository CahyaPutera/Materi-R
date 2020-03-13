#TIPE-TIPE DATA DALAM R
x <- 2L
typeof(x) #integer

y <- 2.5
typeof(y) #double

z <- 3 + 2i
typeof(z) #complex

a <- "h"
typeof(a) #character

p <- F
p2 <- FALSE
q <- T
q2 <- TRUE
typeof(p) #logical


#VARIABLE OPERATORS
#numeric operations
A <- 10
B <- 5

C <- A + B
C

D <- 2L / 5L
typeof(D)
D

#try highlighting multiple lines
var1 <- 2.5
var2 <- 4

result <- var1 / var2
result

#function. try to type ?functionname
answer <- sqrt(var2)
answer


#character operation
?paste
greeting <- "Hello"
name <- "Bob"
message <- paste(greeting, name)
message <= paste(greeting, name, name)
message

# logical operators
# TRUE T
# or FALSE F

4 < 5 #T
10 > 100 #F
4 == 5 #F

# == equal to
# != not equal to
# <
# >
# <=
# >=
# !
# | only use single line. double line has different meaning
# &
# isTRUE(x)

result <- 4 < 5
result #TRUE

result2 <- !(5 > 1)
result2 #TRUE

result | result2
result & result2 

isTRUE(result)


#LOOP STATEMENTS
a <- 0
while(a < 5){
  print("Hello")
  a <- a + 1
}

counter <- 1
while(counter < 12){
  print(counter)
  counter <- counter + 1
}


for(x in 5:10){
  print(x)
  print("Hello R")
}


#IF STATEMENTS
# ---- -2 ---- -1 ---- 0 ---- 1 ---- 2 ----

x <- 0.5

#nesting
if(x > 1){
  answer <- "Lebih besar dari 1"
}else{
  if(x >= -1){
    answer <- "Antara -1 dan 1"
  }else{
    answer <- "Kurang dari -1"
  }
}
print(answer)


#chaining
if(x > 1){
  answer <- "Lebih dari 1"
}else if (x >= -1){
  answer <- "Antara -1 dan 1"
}else{
  answer <- "Kurang dari 1"
}
print(answer)


#CREATING FUNCTIONS
print_hello <- function(nama){
  return(paste("Hello, ", nama, "!", sep=""))
}

print_hello("David")


isEvenNumber <- function(num){
  if(num %% 2 == 0){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

isEvenNumber(4L)
