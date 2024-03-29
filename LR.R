library(datasets)
library(ggplot2)
library(caret)
#importuje zestaw danych iris
data(iris)
#obs�uga brak�w danych
iris <- na.omit(iris)
summary(iris)
#regresja klasyfikuje binarnie dlatego wybieram dwa zbiory:setosa i versicolor
Bin.iris <- subset(iris, Species != "virginica")
#do porownania wezm� tylko dlugo�� i szeroko�� p�atka
Bin.iris <- Bin.iris[3:5]
#podzia� na zbior treningowy i testowy
set.seed(123)
training.samples <- createDataPartition(Bin.iris$Species,p = 0.7, list = FALSE)
train.data  <- Bin.iris[training.samples, ]
test.data <- Bin.iris[-training.samples, ]
#Regresja logistyczna
library(arm)
model <- bayesglm(Species ~ . , data=train.data, family=binomial(link = "logit"))
slope <- coef(model)[2]/(-coef(model)[3])
intercept <- coef(model)[1]/(-coef(model)[3]) 

ggplot(Bin.iris,aes(y=Petal.Width,x=Petal.Length, colour=Species))+
  geom_point()+
  geom_abline(mapping = NULL, data = NULL, slope=slope, intercept=intercept,
              na.rm = FALSE, show.legend = NA)+
              labs(title="Linia decyzyjna",x ="D�ugo�� p�atka [standaryzowana]",
                   y = "Szeroko�� p�atka [standaryzowana]")
#Predykcja
probabilities <- predict(model, test.data, type = "response")
predicted.classes <- ifelse(probabilities > 1, "versicolor", "setosa")
