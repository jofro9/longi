library(ggplot2)

# question 1
data = read.table("../data/eno_data.txt", sep = " ", header = TRUE)

pca = prcomp(data)

regression = lm(eno_post ~ eno_pre, data=data)
summary(regression)$coefficients



ggplot(data, aes(eno_pre, eno_post)) +
  geom_point() +
  geom_abline(slope = pca$rotation[2, 1] / pca$rotation[2, 2], intercept = summary(regression)$coefficients[1, 1], col = "blue") +
  coord_fixed() +
  geom_abline(slope = summary(regression)$coefficients[2, 1], intercept = summary(regression)$coefficients[1, 1], col = "red")
