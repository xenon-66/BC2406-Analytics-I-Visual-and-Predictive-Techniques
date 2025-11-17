library(data.table)
library(rpart)
library(rpart.plot)
library(caTools)
library(ggplot2)
library(corrplot)
library(car)

## import
early.symptoms.dt <- fread("diabetes_early_symptoms.csv", header = T, na.strings = c("NA", "na", "N/A", "", ".", "m", "M"))

# no NA
sum(is.na(early.symptoms.dt))
colSums(is.na(early.symptoms.dt))

# 269 duplicated rows
sum(duplicated(early.symptoms.dt))

# class distribution
table(early.symptoms.dt$class)
prop.table(table(early.symptoms.dt$class))
barplot(table(early.symptoms.dt$class),
        main = "Distribution of Diabetics & Non-diabetics",
        col = "pink",
        xlab = "",
        ylab = "Count")

# handle duplicates
early.symptoms.dt[duplicated(early.symptoms.dt)]
early.symptoms.dt <- unique(early.symptoms.dt)
dim(early.symptoms.dt)

# convert gender to numeric
early.symptoms.dt$gender <- ifelse(early.symptoms.dt$gender == "Male", 1, 0)

# variable distribution
par(mfrow = c(4, 4))
col.names <- colnames(early.symptoms.dt)
cols <- colors()
j <- 2
for (a in col.names[2:length(col.names)]) {
  counts <- table(early.symptoms.dt[[a]])
  barplot(counts,
          main = a,
          col = cols[[j]],
          xlab = "",
          ylab = "Count")
  j <- j + 5
}
par(mfrow = c(1, 1))
hist(early.symptoms.dt$age,
     main = "Age Distribution",
     col = "steelblue",
     xlab = "",
     ylab = "Count")

# corr
corr.mtx <- cor(early.symptoms.dt, use = "complete.obs")
corr.mtx
corrplot(corr.mtx, type = "upper", method = "shade")

# factor
early.symptoms.dt <- early.symptoms.dt[, (2:ncol(early.symptoms.dt)) := lapply(.SD, as.factor), .SDcols = 2:ncol(early.symptoms.dt)]
str(early.symptoms.dt)

# train-test
set.seed(100)
train <- sample.split(early.symptoms.dt$class, SplitRatio = 0.7)
trainset <- early.symptoms.dt[train == T]
testset <- early.symptoms.dt[train == F]
summary(trainset$class)

## regression

# 1st
e1 <- glm(class ~ ., data = trainset, family = "binomial")
summary(e1)
vif(e1)

# 2nd
e2 <- step(e1, direction = "both", trace = F)
summary(e2)
vif(e2)

# lr evaluation
pred.p <- predict(e2, newdata = testset, type = "response")
class.pred.num <- ifelse(pred.p > 0.5, 1, 0)
conf.mtx.lr <- table(Actual = factor(testset$class, levels = c(1, 0), labels = c("Yes", "No")), 
                     Predicted = factor(class.pred.num, levels = c(1, 0), labels = c("Yes", "No")), 
                     deparse.level = 2)
conf.mtx.lr

mean(testset$class == class.pred.num)
# accuracy = 86.7%

OR <- exp(coef(e2))
OR
OR.CI <- exp(confint(e2))
OR.CI
# polydipsia > polyuria > genital_thrush > sudden_weight_loss > partial_paresis

## cart

# grow tree
m.cart <- rpart(class ~ ., data = trainset, method = "class",
                 control = rpart.control(minsplit = 2, cp = 0))

plotcp(m.cart)
printcp(m.cart)
rpart.plot(m.cart, nn = T, main = "Maximal Tree")

# compute cp
CVerror.cap <- m.cart$cptable[which.min(m.cart$cptable[, "xerror"]), "xerror"] + m.cart$cptable[which.min(m.cart$cptable[, "xerror"]), "xstd"]

i <- 1
while (m.cart$cptable[i, 4] > CVerror.cap) {
  i <- i + 1
}

cp.opt <- ifelse(i > 1, sqrt(m.cart$cptable[i, 1] * m.cart$cptable[i-1, 1]), 1)

# prune tree
m.cart.pruned <- prune(m.cart, cp = cp.opt)

printcp(m.cart.pruned)

rpart.plot(m.cart.pruned, nn = T, main = "Optimal Tree")

# cart evaluation
pred.cart <- predict(m.cart.pruned, newdata = testset, type = "class")
conf.mtx.cart <- table(Actual = factor(testset$class, levels = c(1, 0), labels = c("Yes", "No")),
                        Predicted = factor(pred.cart, levels = c(1, 0), labels = c("Yes", "No")), 
                        deparse.level = 2)
conf.mtx.cart

mean(testset$class == pred.cart)
# accuracy = 93.3%

m.cart.pruned$variable.importance
# polyuria > polydipsia

par(mar = c(9,4,4,2))
barplot(m.cart.pruned$variable.importance,
        main = "VI Score Distribution",
        col = "orange",
        las = 2)
