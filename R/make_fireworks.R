make_fireworks <- function(background, n) {

  library(ggplot2)
  library(gganimate)
  library(gifski)
  library(ggpubr)

  col1 <- rbeta(500, sample(1:100, 1), sample(1:100, 1))
  col2 <- rbeta(500, sample(1:100, 1), sample(1:100, 1))
  col3 <- rbeta(500, sample(1:100, 1), sample(1:100, 1))
  col4 <- rbeta(500, sample(1:100, 1), sample(1:100, 1))
  data <- data.frame(col1, col2, col3, col4)

  fireworks <- ggplot(data=data) +
    geom_point(data = data, aes(x = col1, y = col3, color = col1), shape = 8) +
    geom_point(data = data, aes(x = col3, y = col4, color = col2), shape = 8) +
    geom_point(data = data, aes(x = col1, y = col4, color = col3), shape = 8) +
    theme_void() +
    theme(plot.background = element_rect(fill = background)) +
    theme(legend.position = "none") +
    scale_color_gradientn(colours = rainbow(n))


  fireworks + transition_reveal(col2) +
    shadow_wake(wake_length = 0.1)

}

make_fireworks("midnightblue", 20)



