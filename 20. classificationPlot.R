nbp <- 250;
PredA <- seq(min(twoClass$PredictorA), max(twoClass$PredictorA), length = nbp)
PredB <- seq(min(twoClass$PredictorB), max(twoClass$PredictorB), length = nbp)
Grid <- expand.grid(PredictorA = PredA, PredictorB = PredB)

PlotGrid <- function(pred,title) {
  surf <- (ggplot(data = twoClass, aes(x = PredictorA, y = PredictorB, 
                                       color = classes)) +
             geom_tile(data = cbind(Grid, classes = pred), aes(fill = classes)) +
             scale_fill_manual(name = 'classes', values = twoClassColor) +
             ggtitle("Decision region") + theme(legend.text = element_text(size = 10)) +
             scale_colour_manual(name = 'classes', values = twoClassColor)) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0))
  pts <- (ggplot(data = twoClass, aes(x = PredictorA, y = PredictorB,  
                                      color = classes)) +
            geom_contour(data = cbind(Grid, classes = pred), aes(z = as.numeric(classes)), 
                         color = "red", breaks = c(1.5)) +
            geom_point(size = 4, alpha = .5) + 
            ggtitle("Decision boundary") +
            theme(legend.text = element_text(size = 10)) +
            scale_colour_manual(name = 'classes', values = twoClassColor)) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0))
  grid.arrange(surf, pts, top = textGrob(title, gp = gpar(fontsize = 20)), ncol = 2)
}