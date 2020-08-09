


set.seed(10)

install.packages('tidyverse')
library(tidyverse)    # data manipulation and visualization
library(kernlab)      # SVM methodology
library(e1071)        # SVM methodology
library(ISLR)         # contains example data set "Khan"
library(RColorBrewer) # customized coloring of plots



# Maximal Margin Classifier

# Construct sample data set - completely separated
x <- matrix(rnorm(20*2), ncol = 2)
y <- c(rep(-1,10), rep(1,10))
x[y==1,] <- x[y==1,] + 3/2
dat <- data.frame(x=x, y=as.factor(y))

# Plot data
library(ggplot2)
ggplot(data = dat, aes(x = x.2, y = x.1, color = y, shape = y)) + 
    geom_point(size = 2) +
    scale_color_manual(values=c("#000000", "#FF0000")) +
    theme(legend.position = "none")


svmfit <- svm(y~., data = dat, kernel = "linear", scale = FALSE)


# Plot Results
plot(svmfit, dat)

kernfit <- ksvm(x, y, type = "C-svc", kernel = 'vanilladot')
plot(kernfit, data = x)  
# kernlab shows a little more detail than e1071
# showing a color gradient 


# Construct sample data set - not completely separated
x <- matrix(rnorm(20*2), ncol = 2)
y <- c(rep(-1,10), rep(1,10))
x[y==1,] <- x[y==1,] + 1
dat <- data.frame(x=x, y=as.factor(y))

# Plot data set
ggplot(data = dat, aes(x = x.2, y = x.1, color = y, shape = y)) + 
    geom_point(size = 2) +
    scale_color_manual(values=c("#000000", "#FF0000")) +
    theme(legend.position = "none")


svmfit <- svm(y~., data = dat, kernel = "linear", cost = 10) #岿粇だ摸胓籃
# Plot Results
plot(svmfit, dat)


kernfit <- ksvm(x,y, type = "C-svc", kernel = 'vanilladot', C = 100) # form 10 to 100
# Plot results
plot(kernfit, data = x)


#т程続Cost
# tune() in e1071

tune.out <- tune(svm, y~., data = dat, kernel = "linear",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
# extract the best model
(bestmod <- tune.out$best.model)


#箇代
ypred <- predict(bestmod, dat)
(misclass <- table(predict = ypred, truth = dat$y))
# #
#          truth
# predict -1  1
#     -1  10  3
#     1   0   7

# Accuracy
# 17/20 = 0.85






# Support Vector Classifiers are a subset of Support Vector Machines

# construct larger random data set
x <- matrix(rnorm(200*2), ncol = 2)
x[1:100,] <- x[1:100,] + 2.5
x[101:150,] <- x[101:150,] - 2.5
y <- c(rep(1,150), rep(2,50))
dat <- data.frame(x=x,y=as.factor(y))

# Plot data
ggplot(data = dat, aes(x = x.2, y = x.1, color = y, shape = y)) + 
    geom_point(size = 2) +
    scale_color_manual(values=c("#000000", "#FF0000")) +
    theme(legend.position = "none")


set.seed(123)

train <- base::sample(200,100, replace = FALSE)

svmfit <- svm(y~., data = dat[train,],
              kernel = "radial",   # the shape of our data and plot the results
              gamma = 1, 
              cost = 1)

# plot classifier
plot(svmfit, dat)

# Fit radial-based SVM in kernlab
kernfit <- ksvm(x[train,],y[train], type = "C-svc", kernel = 'rbfdot', C = 1, scaled = c())
# Plot training data
plot(kernfit, data = x[train,])



# tune model to find optimal cost, gamma values
tune.out <- tune(svm, y~., 
                 data = dat[train,], 
                 kernel = "radial",
                 ranges = list(cost = c(0.1,1,10,100,1000),
                               gamma = c(0.5,1,2,3,4))) # a scaling parameter used to fit nonlinear boundaries
# show best model
tune.out$best.model

#箇代

# validate model performance
(valid <- table(true = dat[-train,"y"], # test data 
                pred = predict(tune.out$best.model,newx = dat[-train,]))) #test data
#       pred
# true  1  2
# 1    56 27
# 2    12  5

# Accuray = 61/100 = 0.61






# SVMs for Multiple Classes

x <- rbind(x, matrix(rnorm(50*2), ncol = 2))
y <- c(y, rep(0,50))
x[y==0,2] <- x[y==0,2] + 2.5
dat <- data.frame(x=x, y=as.factor(y))

# plot data set
ggplot(data = dat, aes(x = x.2, y = x.1, color = y, shape = y)) + 
    geom_point(size = 2) +
    scale_color_manual(values=c("#000000","#FF0000","#00BA00")) +
    theme(legend.position = "none")


svmfit <- svm(y~., data = dat, 
              kernel = "radial",
              cost = 10, 
              gamma = 1)

# plot results
plot(svmfit, dat)

#箇代
ypred <- predict(svmfit, dat)
(misclass <- table(predict = ypred, truth = dat$y))
# #
#             truth
# predict   0   1   2
# 0        39   2   1
# 1         7 145   5
# 2         4   3  44

# Accuracy
sum(diag(misclass)) / sum(misclass)
# 0.912


# The kernlab package can fit more than 2 classes, but cannot plot the results
kernfit <- ksvm(as.matrix(dat[,2:1]),
                dat$y, 
                type = "C-svc", 
                kernel = 'rbfdot', 
                C = 100, 
                scaled = c())

# Create a fine grid of the feature space
x.1 <- seq(from = min(dat$x.1), to = max(dat$x.1), length = 100)
x.2 <- seq(from = min(dat$x.2), to = max(dat$x.2), length = 100)
x.grid <- expand.grid(x.2, x.1)

# Get class predictions over grid
pred <- predict(kernfit, newdata = x.grid)

# Plot the results
cols <- brewer.pal(3, "Set1")
plot(x.grid, pch = 19, col = adjustcolor(cols[pred], alpha.f = 0.05))

classes <- matrix(pred, nrow = 100, ncol = 100)
contour(x = x.2, y = x.1, z = classes, levels = 1:3, labels = "", add = TRUE)

points(dat[, 2:1], pch = 19, col = cols[predict(kernfit)])



# The Khan data set contains data on 83 tissue samples with 2308 gene expression measurements on each sample. 
# These were split into 63 training observations and 20 testing observations, 
# and there are four distinct classes in the set.

dat <- data.frame(x = Khan$xtrain, y=as.factor(Khan$ytrain))
(out <- svm(y~., data = dat, kernel = "linear", cost=10))


# check model performance on training set
table(out$fitted, dat$y)
# 100% Accuracy

# validate model performance
dat.te <- data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))
pred.te <- predict(out, newdata=dat.te)
table(pred.te, dat.te$y)
# 90% Accuracy















