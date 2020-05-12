data <- read.csv(file.choose())

str(data)

install.packages('ggplot2')
library('ggplot2')


ggplot(data = data[data$carat < 2.5,] , aes(x = carat , y = price, color = clarity)) +
  geom_point(alpha = 0.1) +
  geom_smooth()
