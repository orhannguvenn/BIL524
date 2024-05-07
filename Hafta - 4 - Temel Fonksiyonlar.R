#author: Orhan Guven
#output: Github Document
#title: Week 4th

#Using Basic Functions

data(iris)
View(iris)
head(iris)
str(iris)
class(iris)
length(iris)
toplam <- sum(iris$Sepal.Length)
print(toplam)
ortalama <- mean(iris$Sepal.Length)
print(ortalama)
unique_species <- unique(iris$Species)
print(unique_species)
max_deger <- max(iris$Petal.Length)
min_deger <- min(iris$Petal.Length)
print(max_deger)
print(min_deger)
sirali_iris <- sort(iris$Sepal.Width)
print(sirali_iris)
mod <- names(sort(-table(iris$Species)))[1]
print(mod)
install.packages("dplyr")
library(dplyr)
data(iris)
filtered_iris <- filter(iris, Species == "setosa")
head(filtered_iris)
boyutlar <- dim(iris)
print(boyutlar)
print(dim(iris))
View(as.data.frame(iris))
selected_columns <- select(iris, Sepal.Length, Sepal.Width)
head(selected_columns)
