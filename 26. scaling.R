#unscalling
d <- data.frame(
  x=runif(100),
  y=rnorm(100)
)

d <- within(d, s.x <- scale(x))

attributes(d$s.x)
d$s.x * attr(d$s.x, 'scaled:scale') + attr(d$s.x, 'scaled:center')


x <- 1:10
s.x <- scale(x)

s.x
attr(s.x,"scaled:center")
attr(s.x,"scaled:scale")

s.x * attr(s.x, 'scaled:scale') + attr(s.x, 'scaled:center')
attr(s.x,"scaled:center")
attr(s.x,"scaled:scale")









