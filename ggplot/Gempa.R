

gempa <- data.frame(
  x=c(3.5,3,4,4.5,4.1),
  y=c(12,14,12.4,12.5,14), 
  size=c(14,4,4,6,12)
)

library(ggplot2)
install.packages("png")
library(png)
install.packages(grid)
library(grid)
install.packages(ggimage)
library(ggimage)


jak <- readPNG("jakarta.png")


g <- ggplot(data = gempa, aes(x = x, y = y, size = size)) + 
  geom_point(alpha=0.6, color="red2")
g

g <- ggplot(data = gempa, aes(x = x, y = y, size = size)) + 
  annotation_custom(rasterGrob(jak,
                               width = unit(1, "npc"),
                               height = unit(1, "npc"))) + 
  geom_point(alpha=0.6, color="red2", show.legend = FALSE) +
  scale_size(range = c(0, 20))
g

gempa$icon <- "impact.png"

jak <- readPNG("jakarta.png")


g <- ggplot(data = gempa, aes(x = x, y = y, size = size)) + 
  geom_point(alpha=0.6, color="red2")
g

g <- ggplot(data = gempa, aes(x = x, y = y, size = size)) + 
  annotation_custom(rasterGrob(jak,
                               width = unit(1, "npc"),
                               height = unit(1, "npc"))) + 
  geom_point(alpha=0.6, color="red2", show.legend = FALSE) +
  scale_size(range = c(0, 20))
g

gempa$icon <- "impact.png"
