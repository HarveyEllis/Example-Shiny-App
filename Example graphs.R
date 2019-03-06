library(ggplot2)

iris_data <- iris

# as vectors
x <- iris_data[["Petal.Length"]]
y <- iris_data[["Petal.Width"]]

#as a new dataframe
plot_data <- data_test <- iris_data[c("Petal.Length", "Petal.Width")]

plot(iris_data[["Petal.Length"]], iris_data[["Petal.Width"]], 
     xlab = "Petal length", 
     ylab = "Petal width")


ggplot(data.frame(x,y), aes(x= x, y = y)) + geom_point() + theme_bw()
ggplot(iris_data, aes(x = Petal.Length, y = Petal.Width, color= Species)) + geom_point() + theme_bw()
