# install packages.

library(purrr)
library(tidyr)
library(ggplot2)

install.packages('plotly', dependencies = TRUE)
library(plotly)


install.packages('ggplot2', dependencies = TRUE)
library(ggplot2)

# load the dataset of student score
score<-read.csv("C:/Users/ghawt/Documents/student_scores - student_scores.csv")

# View the dataset.
View(score)

#Check for missing values in the dataset.
is.na(score)

# Plotting ggplot Scores Vs Hours.
ggplot(score, aes(x = Hours, y = Scores)) +
  geom_point() +
  stat_smooth()

# What is the correlation between Hours and Scores for the dataset?
cor(score$Hours, score$Scores)



# Dividing the dataset into testing and training dataset.

samplesize <- floor(2/3 * nrow(score)) 
set.seed(2)
scoredata <- score[sample(nrow(score)), ]
score_train <- score[1:samplesize, ]
score_test <- score[(samplesize+1):nrow(score), ]

# Since we have to predict the score we select the target variable as Score.
formula = Scores ~.

#Performing Regression.
model <- lm(formula = formula, data = score)
summary(model)$coefficients
as.formula(
  paste0("y ~ ", round(coefficients(model)[1],2), " + ",
         paste(sprintf("%.2f * %s",coefficients(model)[-1],
                       names(coefficients(model)[-1])),
               collapse=" + ")
  )
)

# How much the student will score if he studies for 9.25 hours.

Prediction<- 2.48+9.78*9.25

# View the predicted score.
Prediction

# Rounding off the predicted score.
round(Prediction)


# Predicting the score for training and testing data.
score_train$predicted.Scores <- predict(model, score_train)


score_test$predicted.Scores <- predict(model, score_test)


print("Actual_Values")
head(score_train$Scores[1:10])
print("Predicted_Values")
head(score_test$predicted.Scores[1:10])

# Performing Simple Regression.
Regression <- score_train %>%
  ggplot(aes(Scores,predicted.Scores)) +
  geom_point(alpha=0.5) +
  stat_smooth(aes(colour='red')) +
  xlab('Actual_Scores') +  
  ylab('Predicted_Scores')+ 
  theme_bw()


ggplotly(Regression)


Rsquare <- summary(model)$r.squared  
print(paste("R_Squared: ", Rsquare))

Error <- score_train$Scores-score_train$predicted.Scores
Error

Rmse<- sqrt(mean(Error^2))

head(score_train)

score_train

print(paste("Root Mean Square Error: ", Rmse))

round(Rmse)




