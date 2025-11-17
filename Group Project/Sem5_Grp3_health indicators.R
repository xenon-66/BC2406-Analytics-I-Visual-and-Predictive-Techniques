library(data.table)
library(rpart)
library(rpart.plot)
library(caTools)
library(ggplot2)
library(corrplot)
library(car)

## import
health.ind.dt <- fread("diabetes_health_indicators.csv", header = T, na.strings = c("NA", "na", "N/A", "", ".", "m", "M"))

## EDA
dim(health.ind.dt)
summary(health.ind.dt)
head(health.ind.dt)

# check NA
sum(is.na(health.ind.dt))
colSums(is.na(health.ind.dt))
# 0 NA

# duplicates
sum(duplicated(health.ind.dt))
# 23899 duplicated rows

## cleaning

# create copy to clean
clean.dt <- copy(health.ind.dt)

# remove duplicates
clean.dt <- unique(clean.dt)
dim(clean.dt)

# remove pre diabetes (make it only yes or no)
clean.dt <- clean.dt[Diabetes_012 != 1]
clean.dt[Diabetes_012 == 2, Diabetes_012 := 1]

# rename columns (more intuitive)
setnames(clean.dt, "Diabetes_012", "Outcome")
setnames(clean.dt, "Age", "AgeBrac")
setnames(clean.dt, "Education", "EduBrac")
setnames(clean.dt, "Income", "IncBrac")

# class distribution
table(clean.dt$Outcome)
prop.table(table(clean.dt$Outcome))
barplot(table(clean.dt$Outcome),
        main = "Distribution of Diabetics & Non-diabetics",
        col = "pink",
        xlab = "",
        ylab = "Count")

# variable distribution
par(mfrow = c(4, 5))
col.names <- colnames(clean.dt)
cols <- colours()
j <- 2
for (a in col.names[-c(5, 16, 17)]) {
  counts <- table(clean.dt[[a]])
  barplot(counts,
          main = a,
          col = cols[[j]],
          xlab = "",
          ylab = "Count")
  j <- j + 5
}
par(mfrow = c(1, 3))
for (b in col.names[c(5, 16, 17)]) {
  hist(clean.dt[[b]],
          main = b,
          col = cols[[j]],
          xlab = "",
          ylab = "Count")
  j <- j + 5
}
par(mfrow = c(1, 1))

# handle BMI outliers
boxplot(clean.dt$BMI, ylab = "BMI", col = "steelblue")
summary(health.ind.dt$BMI)

Q1 <- quantile(health.ind.dt$BMI, 0.25)
Q3 <- quantile(health.ind.dt$BMI, 0.75)
IQR <- Q3 - Q1
UL <- Q3 + (1.5 * IQR)
LL <- Q1 - (1.5 * IQR)

clean.dt[BMI < LL, BMI := LL]
clean.dt[BMI > UL, BMI := UL]

summary(clean.dt$BMI)
boxplot(clean.dt$BMI, ylab = "BMI", col = "steelblue")

# correlation
corr.mtx <- cor(clean.dt, use = "complete.obs")
corrplot(corr.mtx, type = "upper", method = "shade")

# factorize
col.names <- colnames(clean.dt)
cols.factor <- col.names[-c(5, 15, 16, 17, 20, 21, 22)]
clean.dt[, (cols.factor) := lapply(.SD, as.factor), .SDcols = cols.factor]

summary(clean.dt)
str(clean.dt)

# not balanced
summary(clean.dt$Outcome)
prop.table(table(clean.dt$Outcome))

## sampling

# subset each class
set.seed(100)
dia <- clean.dt[Outcome == 1]
non.dia <- clean.dt[Outcome == 0]

# randomly select 1000 samples each
dia.sample <- dia[sample(.N, size = 1000)]
non.dia.sample <- non.dia[sample(.N, size = 1000)]
balanced.dt <- rbind(dia.sample, non.dia.sample)

# check balance
prop.table(table(balanced.dt$Outcome))
summary(balanced.dt$Outcome)

## train-test
set.seed(100)
train <- sample.split(balanced.dt$Outcome, SplitRatio = 0.7)
trainset <- balanced.dt[train == T]
testset <- balanced.dt[train == F]
table(trainset$Outcome)

## regression

# 1st
m1 <- glm(Outcome ~ ., data = trainset, family = "binomial")
summary(m1)
vif(m1)

# 2nd
m2 <- step(m1, direction = "both", trace = F)
summary(m2)
vif(m2)

# lr evaluation
pred.p <- predict(m2, newdata = testset, type = "response")
class.pred.num <- ifelse(pred.p > 0.5, 1, 0)
conf.mtx.lr <- table(Actual = factor(testset$Outcome, levels = c(1, 0), labels = c("Yes", "No")), 
                     Predicted = factor(class.pred.num, levels = c(1, 0), labels = c("Yes", "No")), 
                     deparse.level = 2)
conf.mtx.lr

a1 <- mean(testset$Outcome == class.pred.num)
# accuracy = 74.7%

OR <- exp(coef(m2))
OR

OR.CI <- exp(confint(m2))
OR.CI

## cart

# maximal
m.cart <- rpart(Outcome ~ ., data = trainset, method = "class",
                control = rpart.control(minsplit = 2, cp = 0))
plotcp(m.cart)
printcp(m.cart)

rpart.plot(m.cart, nn = T, main = "Maximal Tree in Medical Indicators")

# CP
CVerror.cap <- m.cart$cptable[which.min(m.cart$cptable[, "xerror"]), "xerror"] + m.cart$cptable[which.min(m.cart$cptable[, "xerror"]), "xstd"]

i <- 1
while (m.cart$cptable[i, 4] > CVerror.cap) {
  i <- i + 1
}

cp.opt <- ifelse(i > 1, sqrt(m.cart$cptable[i, 1] * m.cart$cptable[i-1, 1]), 1)

# prune
m.cart.pruned <- prune(m.cart, cp = cp.opt)

rpart.plot(m.cart.pruned, nn = T, main = "Optimal Tree in Medical Indicators")

# cart evaluation
pred.cart <- predict(m.cart.pruned, newdata = testset, type = "class")
conf.mtx.cart <- table(Actual = factor(testset$Outcome, levels = c(1, 0), labels = c("Yes", "No")),
                       Predicted = factor(pred.cart, levels = c(1, 0), labels = c("Yes", "No")), 
                       deparse.level = 2)
conf.mtx.cart

a2 <- mean(testset$Outcome == pred.cart)
# accuracy = 71.3%

m.cart.pruned$variable.importance
# HighBP > GenHlth > AgeBrac > BMI > HighChol

par(mar = c(9,4,4,2))
barplot(m.cart.pruned$variable.importance,
        main = "VI Score Distribution",
        col = "yellow",
        las = 2)

# ggplots
ggplot(clean.dt, aes(x = factor(Outcome, 
                                levels = c(0, 1), 
                                labels = c("Non-diabetic", "Diabetic")))) +
  geom_bar(fill = "pink") +
  labs(title = "Distribution of Diabetics & Non-diabetics",
       x = "",
       y = "Count") +
  theme_minimal(base_size = 14)

ggplot(clean.dt, aes(x = factor(Outcome, levels = c(0,1), labels = c("Non-diabetic", "Diabetic")),
                     fill = factor(HighBP, levels = c(0,1), labels = c("No HighBP", "HighBP")))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Proportion of High Blood Pressure by Diabetic Status",
       x = "Diabetic Status",
       y = "Percentage By Group",
       fill = "High Blood Pressure") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

ggplot(clean.dt, aes(x = factor(Outcome, 
                                levels = c(0,1), 
                                labels = c("Non-diabetic", "Diabetic")),
                     y = BMI,
                     fill = factor(Outcome, 
                                   levels = c(0,1), 
                                   labels = c("Non-diabetic", "Diabetic")))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "BMI Distribution by Diabetic Status",
       x = "Diabetic Status",
       y = "BMI") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
