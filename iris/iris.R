iris <- read.csv("~/Dropbox/UCI/iris/iris.data", header=F)
names(iris) <- c('sl', 'sw', 'pl', 'pw', 'class')
plot(iris$sw, iris$pw, col=iris$class)
model = svm(class ~ ., data=iris)
summary(model)

X <- subset(iris, select=-class)
y <- iris$class
pred <- predict(model, X)
table(pred, y)

plot(cmdscale(dist(iris[,-5])),
     col = as.integer(iris[,5]),
     pch = c("o", "+")[1:150 %in% model$index + 1])
