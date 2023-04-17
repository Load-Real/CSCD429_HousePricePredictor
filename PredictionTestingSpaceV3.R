#'*Misc.*
options(digits = 10)

#'*Libraries*
install.packages('rpart')
install.packages('caret')
install.packages('rpart.plot')
install.packages("ggplot2")
install.packages("dplyr")
library(rpart)
library(caret)
library(rpart.plot)
library(ggplot2)
library(dplyr)

test <- read.csv("C:\\Users\\Capnb\\Downloads\\HousePricesInIndia\\test.csv")
train <- read.csv("C:\\Users\\Capnb\\Downloads\\HousePricesInIndia\\train.csv")

str(train)
str(test)

#'*Dropping the Obvious Bad Predictors*
train <- train[, -9] #There were simply TOO many addresses

train <- train[, -9] #There were simply TOO many latitudes/longitudes

train <- train[, -6] #There were simply too many square ft values

train <- train[, -8]

#'*Dropping Outliers*
#get outliers from boxplot data
outliers <- boxplot(train$TARGET.PRICE_IN_LACS., plot = FALSE)$out

#add column to indicate of a row is an outlier
train$is_outlier <- ifelse(train$TARGET.PRICE_IN_LACS. %in% boxplot.stats(train$TARGET.PRICE_IN_LACS.)$out, 1, 0)

#remove rows with outliers
train <- subset(train, is_outlier==0)

train <- train[, -9]

#'*Use ChiSq to Determine Good Predictors*
postedTable <- table(train$TARGET.PRICE_IN_LACS., train$POSTED_BY)
chisq.test(postedTable, simulate.p.value = TRUE)

construcTable <- table(train$TARGET.PRICE_IN_LACS., train$UNDER_CONSTRUCTION)
chisq.test(construcTable, simulate.p.value = TRUE)

reraTable <- table(train$TARGET.PRICE_IN_LACS., train$RERA)
chisq.test(reraTable, simulate.p.value = TRUE)

bhknoTable <- table(train$TARGET.PRICE_IN_LACS., train$BHK_NO.)
chisq.test(bhknoTable, simulate.p.value = TRUE)

bhkorrhkTable <- table(train$TARGET.PRICE_IN_LACS., train$BHK_OR_RK)
chisq.test(bhkorrhkTable, simulate.p.value = TRUE)

readyTable <- table(train$TARGET.PRICE_IN_LACS., train$READY_TO_MOVE)
chisq.test(readyTable, simulate.p.value = TRUE)

resaleTable <- table(train$TARGET.PRICE_IN_LACS., train$RESALE)
chisq.test(resaleTable, simulate.p.value = TRUE)

#'*Forming a Model (Regression Tree)*
test$PREDICTED.PRICE_IN_LACS. = NA
tree <- rpart(TARGET.PRICE_IN_LACS. ~ ., data = train, method = 'anova')
rpart.plot(tree)
prd <- predict(tree, test, type = "class")
test$PREDICTED.PRICE_IN_LACS. <- as.numeric(prd)

#'*Calculating Accuracy*
confusion_mat = as.matrix(table(train$TARGET.PRICE_IN_LACS., prd[1:length(train$TARGET.PRICE_IN_LACS.)]))
print(confusion_mat)
accuracy <- sum(diag(confusion_mat)) / sum(confusion_mat)
print(accuracy)

ctrl <- trainControl(method = "cv", number = 5)
model <- train(TARGET.PRICE_IN_LACS. ~ ., data = train, method = "lm", trControl = ctrl)