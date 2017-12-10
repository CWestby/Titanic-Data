library(dplyr)
library(ggplot2)
library(caret)
library(gridExtra)

titanic <- read.csv("train.csv")
str(titanic)
glimpse(titanic)
head(titanic)

index <- is.na(titanic$Age)
titanic[index, "Age"] <- median(titanic$Age, na.rm = TRUE)



glimpse(titanic)

titanic$Survived <- factor(titanic$Survived, 
                           labels = c("Passed", "Survived"))

titanic$Pclass <- factor(titanic$Pclass, 
                         labels = c("First", "Second", "Third"))


table(titanic$Survived, titanic$Sex)
table(titanic$Survived, titanic$Pclass)
table(titanic$Survived, titanic$Embarked)

summary(titanic)

hist_age <- ggplot(titanic, aes(x=Age, fill=Survived)) +
  geom_histogram(bins = 50)
hist_age

ggplot(titanic_sub, aes(x=Survived, y = Age, col = Sex)) +
  geom_boxplot()

fa_class <- ggplot(titanic_sub, aes(x= Age, y = Fare, col = Pclass)) +
  geom_point() +
  labs(title = "Fare vs Age (Class)")
fa_class

fa_survival <- ggplot(titanic_sub, aes(x=Age, y = Fare, col = Survived)) +
  geom_point() +
  labs(title = "Fare vs Age (Survival)")
fa_survival

age_s_hist <- ggplot(titanic_sub, aes(x=Age, fill = Survived)) +
  geom_histogram() + 
  labs(title = "Age by Survival")
age_s_hist

age_c_hist <- ggplot(titanic_sub, aes(x=Age, fill = Pclass)) +
  geom_histogram() + 
  labs(title = "Age by Class")
age_c_hist

fare_s_hist <- ggplot(titanic_sub, aes(x=Fare, fill = Survived)) +
  geom_histogram() + 
  labs(title = "Fare by Survival")
fare_s_hist

fare_c_hist <- ggplot(titanic_sub, aes(x=Fare, fill = Pclass)) +
  geom_histogram() + 
  labs(title = "Fare by Class")

grid.arrange(fa_class, fa_survival, fare_c_hist, 
             fare_s_hist, age_c_hist, age_s_hist, 
             ncol = 2, nrow = 3)

ggplot(titanic_sub, aes(x=Survived, y=Age)) +
  geom_boxplot()



sf_pclass <- ggplot(titanic, aes(x=Survived, y=Fare, col = Pclass)) +
  geom_boxplot() +
  labs(title = "Fare vs Survival According to Class")


titanic_sub <- titanic %>%
  select(-PassengerId, -Name, -Ticket, -Cabin)
glimpse(titanic_sub)

sum(is.na(titanic$Age))
sum(is.na(titanic$Fare))


library(caret)
set.seed(366284)

#Partitioning the data and subsetting data into a Train Set and a Test Set
inTrain <- createDataPartition(y = titanic_sub$Survived, p = 0.7, list=FALSE)
train <- titanic_sub[inTrain, ]
test <- titanic_sub[-inTrain, ]


model_rf <- train(Survived ~ ., train, method = "ranger", 
    trControl = trainControl(method = "cv", number = 10))
model_rf

predictions_rf <- predict(model_rf, test)
confusionMatrix(predictions_rf, test$Survived)

model_mboost <- train(Survived ~ ., train, method = "ada", 
                  trControl = trainControl(method = "cv", number = 10))
model_mboost

predictions_mboost <- predict(model_mboost, test)
confusionMatrix(predictions_mboost, test$Survived)

final_test  <- read.csv("test.csv")

final_test$Pclass <- factor(final_test$Pclass, 
                         labels = c("First", "Second", "Third"))

final_test_sub <- final_test %>%
  select(-Name, -Ticket, -Cabin)

index <- is.na(final_test$Age)
final_test[index, "Age"] <- median(final_test$Age, na.rm = TRUE)

predictions_rf <- predict(model_rf, final_test)
predictions_gbm <- predict(model_gbm, final_test)
length(predictions_rf)
length(predictions_gbm)
submission_rf <- data.frame("Predictions" = predictions_rf)
submission_gbm <- data.frame("Predictions" = predictions_gbm)

write.csv(submission_rf, "titanic_rf_predictions.csv")
write.csv(submission_gbm, "titanic_gbm_predictions.csv")

titanic_sub %>%
  filter(Pclass == "Third") %>%
  summarize(mean(Fare))
