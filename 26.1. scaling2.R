data1 <- read.csv("../datasets/Social_Network_Ads.csv")
s.age <- scale(data1[,3])
s.salary <- scale(data1[,4])

attr(s.age,"scaled:center")
attr(s.age,"scaled:scale")

unscale <- function(s.x){
  return(s.x * attr(s.x,"scaled:scale") + attr(s.x,"scaled:center"))
}

age <- unscale(s.age)
salary <- unscale(s.salary)
