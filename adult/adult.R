adult <- read.csv("~/Dropbox/UCI/adult/adult.data", header=F)
adult.test <- read.csv("~/Dropbox/UCI/adult/adult.test", header=F)
#change " <=50K." " >50K."  to " <=50K" " >50K" 
levels(adult.test[,15]) <- levels(adult[,15])

# make sure train and test has same factor levels
n1 <- dim(adult)[1]
n2 <- dim(adult.test)[1]
adult.test <- rbind(adult, adult.test)
adult.test <- adult.test[(n1+1):(n1+n2), ]

columns <- c('age', 'work', 'fnlwgt', 'edu', 'edu.num', 'marital', 
                  'occu', 'relation', 'race', 'sex', 'cap.gain', 
                  'cap.loss', 'hour', 'country', 'salary')

names(adult) <- columns
names(adult.test) <- columns

library(e1071)

size <- 1000

accu <- function(size) {
  s <- sample.int(dim(adult)[1], size)
  small.adult <- adult[s, ]
  
  clf <- naiveBayes(small.adult[,1:14], small.adult[,15])
  #table(predict(clf, small.adult[,-15]), small.adult[,15])
  sum(predict(clf, small.adult[,-15]) == small.adult[,15]) / size
}

confident_accu <- function(runs=100) {
  accus <- 1:runs
  for (i in 1:runs) {
    print(i)
    accus[i] <- accu(size)
  }
  accus
}

plot_norm <- function(x) {
  h <- hist(x, 
            breaks=10, 
            col='red',
            main="Histogram with normal curve and box")
  xfit <- seq(min(x), max(x), length=40)
  yfit <- dnorm(xfit, mean=mean(x), sd=sd(x))
  yfit <- yfit * diff(h$mids[1:2]) * length(x)
  lines(xfit, yfit, col='blue', lwd=2)
  box()
}

# boxplot
library(vioplot)
x1 <- small.adult$edu.num[small.adult$salary==' <=50K']
x2 <- small.adult$edu.num[small.adult$salary==' >50K']
vioplot(x1, x2, names=levels(small.adult$salary))

boxplot(hour ~ salary, data=small.adult)


full_size_pred <- function() {
  clf <- naiveBayes(adult[,1:14], adult[,15])
  print(table(predict(clf, adult[,-15]), adult[,15]))
  
  n <- dim(adult.test)[1]
  sum(predict(clf, adult.test[,-15]) == adult.test[,15]) / n
}

#C50 Decision Tree Model
library(C50)
tree.model <- C5.0(salary~., data=adult)
summary(tree.model)
table(predict(tree.model, adult[,-15]), adult[,15])
sum(predict(tree.model, adult.test[,-15]) == adult.test[,15]) / dim(adult.test)[1]

#C4.5 J48 Decision Tree Model
library(RWeka)
m3 <- J48(salary~., data=adult)
summary(m3)
sum(predict(m3, adult.test[,-15]) == adult.test[,15]) / dim(adult.test)[1]









