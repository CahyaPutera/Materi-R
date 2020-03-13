getwd()
setwd("/Users/davidchristian/Documents/OneTwoCode/DataScience/Week 2")

movies <- read.csv("datasets/movie-ratings.csv")
head(movies)

colnames(movies) <- c("Film", "Genre", "CriticRating", "AudienceRating", "BudgetMillions", "Year")
head(movies)
tail(movies)
str(movies) #2 factors. Factors = categorical
summary(movies)

#ubah Year ke factor
factor(movies$Year)
movies$Year <- factor(movies$Year)
str(movies)
summary(movies)

#------------------- Aesthetics & Geometry
library(ggplot2)
?ggplot

ggplot(data=movies, aes(x=CriticRating, y=AudienceRating))

#add color
ggplot(data=movies, aes(x=CriticRating, y=AudienceRating, color=Genre)) + 
  geom_point()

#add size
ggplot(data=movies, aes(x=CriticRating, y=AudienceRating, color=Genre, size=BudgetMillions)) + 
  geom_point(alpha=0.7) #plot 1

#------------------ Plotting with layers
p <- ggplot(data=movies, aes(x=CriticRating, y=AudienceRating, color=Genre, size=BudgetMillions))

#point
p + geom_point() + scale_size(range=c(0.5,3))

#lines
p + geom_line(size=0.5)

p + geom_line(size=0.5) + geom_point() + scale_size(range=c(0.5,3))



#------------------ Overriding Aesthetics
q <- ggplot(data=movies, aes(x=CriticRating, y=AudienceRating,
                             color=Genre, size=BudgetMillions))
q + geom_point()

#example 1
q + geom_point(aes(size=CriticRating))

#example 2
q + geom_point(aes(color=BudgetMillions))
q + geom_point()

#example 3
q + geom_point(aes(x=BudgetMillions))
q + geom_point(aes(x=BudgetMillions)) +
  xlab("Budget Millions $$$") #plot2

#example 4
q + geom_line(size=1) + geom_point() + scale_size(range = c(1,3))


#------------------- Mapping vs Setting
r <- ggplot(data=movies, aes(x=CriticRating, y=AudienceRating))
r + geom_point(aes(color=Genre))
r + geom_point(color="DarkBlue")

r + geom_point(aes(color="DarkBlue")) #WRONG


#-------------------- Histogram & Density Chart
s <- ggplot(data=movies, aes(x=BudgetMillions))
s + geom_histogram(binwidth = 10)

#add color
s + geom_histogram(binwidth = 10, aes(fill=Genre))
#add border
s + geom_histogram(binwidth = 10, aes(fill=Genre), color="Black") #plot 3


#-------------------- Density Chart
s + geom_density(aes(fill=Genre))
s + geom_density(aes(fill=Genre), position = "stack")

#-------------------- Starting Layer
#you know what you want
t <- ggplot(data=movies, aes(x=AudienceRating))
t + geom_histogram(binwidth = 10, fill="White", color="Blue")

#more flexibility
t <- ggplot(data=movies)
t + geom_histogram(binwidth = 10, fill="White", color="Blue", aes(x=AudienceRating)) #plot 4

t + geom_histogram(binwidth = 10, fill="White", color="Blue", aes(x=CriticRating)) #plot 5


#-------------------- Statistical Transformation
?geom_smooth()
u <- ggplot(data=movies, aes(x=CriticRating, y=AudienceRating, color=Genre))
u + geom_point() + geom_smooth(fill=NA) #romance and horror movies

#boxplots
u <- ggplot(data=movies, aes(x=Genre, y=AudienceRating, color=Genre))
u + geom_boxplot()
u + geom_boxplot(size=1.2)

u + geom_boxplot(size=1.2) + geom_point()

#jitter
u + geom_boxplot(size=1.2) + geom_jitter(size = 0.5)
u + geom_jitter(size = 0.5) + geom_boxplot(size=1.2, alpha=0.5) #plot 6

#Challenge: Create boxplot from Critic Rating
u2 <- ggplot(data=movies, aes(x=Genre, y=CriticRating, color=Genre))
u2 + geom_jitter(size = 0.5) + geom_boxplot(size=1.2, alpha=0.5) #plot 7



#------------------------------- Facets
v <- ggplot(data=movies, aes(x=BudgetMillions))
v + geom_histogram(binwidth = 10, aes(fill=Genre), 
                   color="Black")
#susah bandingin per genre
#Solusi : Facets
v + geom_histogram(binwidth = 10, aes(fill=Genre),
                   color="Black") +
  facet_grid(Genre~., scales="free") #formula --> left=row, right=column


#scatterplot
w <- ggplot(data=movies, aes(x=CriticRating, y=AudienceRating, color=Genre))
w + geom_point()

#facets
w + geom_point() +facet_grid(Genre~.)
w + geom_point() +facet_grid(.~Year)
w + geom_point() +facet_grid(Genre~Year)

w + geom_point(aes(size=BudgetMillions)) + 
  geom_smooth() + facet_grid(Genre~Year) +
  theme_grey() + scale_size(range = c(0.5,2))

#------------------------ Coordinates
#limits
#zoom

m <- ggplot(data=movies, aes(x=CriticRating, y=AudienceRating, size=BudgetMillions, color=Genre))
m + geom_point()

#limit
m + geom_point() +
  xlim(50,100) +
  ylim(50,100)

#problem
v + geom_histogram(binwidth = 10, aes(fill=Genre), 
                   color="Black") + 
  ylim(0,50)

#zoom
v + geom_histogram(binwidth = 10, aes(fill=Genre), 
                   color="Black") +
  coord_cartesian(ylim=c(0,50))


w + geom_point(aes(size=BudgetMillions)) + 
  geom_smooth() + facet_grid(Genre~Year) +
  theme_grey() + scale_size(range = c(0.5,2)) +
  coord_cartesian(ylim=c(0,100))


#---------------------------------- Theme
o <- ggplot(data=movies, aes(x=BudgetMillions))
o2 <- o + geom_histogram(binwidth = 10, aes(fill=Genre), color="Black")

#axes label
o2 + xlab("Money Axis") + ylab("Number of Movies")

#label formatting
o2 + xlab("Money Axis") +
  ylab("Number of Movies") +
  theme(axis.title.x = element_text(color="DarkGreen", size = 10),
        axis.title.y = element_text(color="Red", size = 10),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10))
?theme

#legend formatting
o2 + xlab("Money Axis") +
  ylab("Number of Movies") +
  ggtitle("Movie Budget Distribution") +
  theme(axis.title.x = element_text(color="DarkGreen", size = 10),
        axis.title.y = element_text(color="Red", size = 10),
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size=7),
        legend.position = c(1.25,1),
        plot.margin = unit(c(0,6,0,0), "lines"),
        legend.justification = c(1,1),
        plot.title = element_text(colour = "DarkBlue",
                                  size=10,
                                  family = "Courier", hjust = 0.5))
