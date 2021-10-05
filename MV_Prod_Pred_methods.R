# This dataset has varriables from 1,000 unconventional wells including well average porosity, log transform 
# of permeability (to linearize the relationships with other variables), accoustic impedance (kg/m2s*10^6), brittness ratio (%),
# total organic carbon (%), vitrinite reflectance (%), and production (MCFPD)

# Load  required libraries
library(plyr); library(scales)
library(ggplot2); library(lattice); library(corrplot)                              
library(tree); library(gbm); library(glmnet); library(randomForest); library(boot)

# Read the data from .CSV
DF0 <- read.table("mv_unconv.csv", as.is = TRUE)    #import csv (str character)
rock.data <- read.csv(text = DF0[[1]])              #fix data type to numeric
rock.data <- rock.data[,-1]                         #remove first column, WellIndex

# Attach the data
attach(rock.data)

# Visualize the data
head(rock.data); tail(rock.data)

# Summary statistics for each column
summary(rock.data)                                

# Correlation matrixand plot
cor_matrix <- round(cor(rock.data),3); print(cor_matrix)
corrplot(cor_matrix, method = "circle")      

# Scatterplot from Lattice
splom(rock.data,col=rgb(0,0,0,50,maxColorValue=440), pch=19,main = "Unconventional Dataset") 

# Comparison of Production against Porosity and Brittleness
par(mfrow=c(1,2))
plot(Por,Production, main="Production vs. Porosity", col=alpha("black",0.5),
     xlab="Porosity (%)", ylab="Production (MCF/d", pch=19)
plot(Brittle,Production, main="Production vs. Brittleness", col=alpha("black",0.5),
     xlab="Brittleness (%)", ylab="Production (MCF/d",pch=19)
par(mfrow=c(1,1))
#these two variables show highest and lowest correlation on the correlation matrix, respectively

# Comparison of Porosity and Brittleness
prod.deciles <- quantile(Production, 0:10/10); prod.deciles
cut.prod <- cut(Production, prod.deciles, include.lowest=TRUE)
par(mfrow=c(1,2))
plot(Por, Brittle, col=grey(10:2/11)[cut.prod], pch=20, 
     xlab="Porosity (%)", ylab="Brittleness (%)", main="Production (MCF/d)")

# Separate data into Training and Testing Sets
train <- rock.data[sample(nrow(rock.data),500,replace=FALSE),]
train_id <- sample(seq_len(nrow(rock.data)), size=500, replace=FALSE)
train <- rock.data[train_id,]
test <- rock.data[-train_id,]

# Check properties of training set to ensure correctness
head(train)
dim(train)

# Plot training set and compare to original/complete data set
cut.train.prod <- cut(train$Production, prod.deciles, include.lowest=TRUE)
plot(train$Por,train$Brittle, col=grey(10:2/11)[cut.train.prod], pch=20,
     xlab="Porosity (%)", ylab="Brittleness (%)", main="Training - Production (MCF/d)")    
par(mfrow=c(1,1))

#####

# BOOTSTRAP 
porosity <- rock.data$Por
summary(porosity)
hist(porosity,main="Porosity (%)", freq=FALSE, ylim=c(0,.2))
plot(ecdf(porosity), main="Porosity(%)", xlab="Porosity (%)", ylab="Cumulative Probability")
calc_average <- function(d, i=c(1:n)){
  d2 <- d[i]
  return(mean(d2))
}
boot.por.avg <- boot(data=porosity, statistic=calc_average, R=50000)
hist(boot.por.avg$t, main="Bootstrap Porosity Average", freq=FALSE)
plot(ecdf(boot.por.avg$t), main="Bootstrap Porosity Average")

# DECISION TREES
# Design Decision Tree control parameters
tree.control <- tree.control(nobs=500, mincut=5, minsize=10, mindev=0.01)

# Decision Tree on training data
tree.prod <- tree(Production~., data=train, control=tree.control)
summary(tree.prod)
plot(tree.prod); text(tree.prod, pretty=0)

# Another way to visualize
plot(rock.data$Por,rock.data$Brittle, col=grey(10:2/11)[cut.prod], pch=20, 
     xlab="Porosity (%)", ylab="Brittleness (%)")
partition.tree(tree.prod, ordvars=c("Por","Brittle"), add=TRUE)

# Pruned Tree
cv.prod <- cv.tree(tree.prod, K=10)
plot(cv.prod$size, cv.prod$dev, type="b")      #looks like 6 nodes is good pruning
prune.prod <- prune.tree(tree.prod, best=6)

# Comparison of regular vs pruned trees
par(mfrow=c(2,2))
plot(tree.prod); text(tree.prod, pretty=0)
plot(prune.prod); text(prune.prod, pretty=0)
plot(rock.data$Por,rock.data$Brittle, col=grey(10:2/11)[cut.prod], pch=20, xlab="Porosity (%)", ylab="Brittleness (%)")
    partition.tree(tree.prod, ordvars=c("Por","Brittle"), add=TRUE)
plot(rock.data$Por,rock.data$Brittle, col=grey(10:2/11)[cut.prod], pch=20, xlab="Porosity (%)", ylab="Brittleness (%)")
    partition.tree(prune.prod, ordvars=c("Por","Brittle"), add=TRUE)
par(mfrow=c(1,1))
    
# Prediciton from pruned tree
yhat.prod <- predict(prune.prod, newdata=test)
plot(yhat.prod, test$Production)   #6 bins from the 6 terminal nodes
abline(0,1) 
tree.error <- mean((yhat.prod-test$Production)^2); tree.error
RMSE.tree <- sqrt(tree.error); RMSE.tree
var.prod <- var(test$Production); var.prod

# LINEAR REGRESSION
lm.fit <- lm(Production ~., data=train); lm.fit
lm.pred <- predict(lm.fit, test)
lm.error <- mean((test$Production - lm.pred)^2); lm.error

# LASSO REGULARIZATION
x <- model.matrix(Production~., data=train)
y <- train$Production
lasso.fit <- glmnet(x,y,alpha=1)
x.test <- model.matrix(Production~., data=test)
lasso.pred <- predict(lasso.fit, s=0.01, newx=x.test)
lasso.error <- mean((test$Production - lasso.pred)^2); lasso.error

# BOOSTING
power <- seq(from=-10,to=-0.2,by=0.1)
lambdas <- 10^power
length.lambdas <- length(lambdas)
train.error <- rep(NA,length.lambdas)
test.error <- rep(NA,length.lambdas)
for (i in 1:length.lambdas) {
  boost.Prod <- gbm(Production~.,data=train,distribution="gaussian",n.trees=1000,shrinkage=lambdas[i])
  train.pred <- predict(boost.Prod, train, n.trees=1000)
  train.error[i] <- mean((train$Production-train.pred)^2)
  test.pred <- predict(boost.Prod, test, n.trees=1000)
  test.error[i] <- mean((test$Production-test.pred)^2)
}
par(mfrow=c(1,2))
plot(lambdas,train.error,type="b",xlab="Shrinkage",ylab="Train MSE",col="darkgreen",main="Train MSE vs Shrinkage")
plot(lambdas,test.error,type="b",xlab="Shrinkage",ylab="Test MSE",col="darkblue",main="Test MSE vs Shrinkage")
par(mfrow=c(1,1))
# lambda-value for minimal test MSE
test.MSE.min <- min(test.error)
lambda.min.test.MSE <- lambdas[which.min(test.error)]
data.frame(lambda.min.test.MSE,test.MSE.min)
boost.error <- test.MSE.min
# Best variables for boosting
boost.best <- gbm(Production~., data=train, distribution="gaussian", 
                  n.trees=1000, shrinkage=lambdas[which.min(test.error)])
summary(boost.best)

# RANDOM FOREST
rf.Prod <- randomForest(Production~., data=train, ntree=500, mtry=length(rock.data)-1)
importance(rf.Prod)
varImpPlot(rf.Prod)
rf.predict <- predict(rf.Prod, test)
rf.error <- mean((test$Production - rf.predict)^2); rf.error

# Comparing MSE for all methods
scientific(data.frame(tree.error,boost.error,lm.error,lasso.error,rf.error), digits=2)

# PCA
PCA <- prcomp(rock.data[,-7], scale=TRUE)
PCA$center #the means substracted from variables
PCA$scale  #the standardization factor
PCA$rotation  #principal component loadings

# Plotting the PCA biplot
biplot(PCA, cex=0.8, scale=0, pch=21)

# Analyze for two first principal components
nComp <- 2
Xhat2 <- t(t(PCA$x[,1:nComp] %*% t(PCA$rotation[,1:nComp])) *PCA$scale + PCA$center)
plot(LogPerm~Por, Xhat2, main="Log Permeability vs Porosity (from PC1 and PC2)", pch=19, 
     xlim=c(5,25), ylim=c(0,3), xlab="Porosity (%)", ylab="Log Permeability", col=alpha("black",0.3))

theta <- seq(0,2*pi,length.out=1000)
circle <- data.frame(x=cos(theta),y=sin(theta))
p <- ggplot(circle,aes(x,y)) + geom_path()
loadings <- data.frame(PCA$rotation, .names=row.names(PCA$rotation))
p + geom_text(data=loadings, mapping=aes(x=PC1, y=PC2, label=.names,colour=.names)) + 
  coord_fixed(ratio=1) + labs(x="PC1", y="PC2")

# Variance explained
pr.var <- PCA$sdev^2
pve <- pr.var / sum(pr.var); pve
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", 
     main="Principal Component vs. Variance Explained", pch=19, col="red", ylim=c(0,1))
points(cumsum(pve), xlab="Principal Component", ylim=c(0,1), 
       pch=19, col="blue"); mtext("Cumulative Proportion of Variance Explained", side = 4)
legend("right",inset=.02,title="Variance Explained by PC", c("Proportion","Cumulative"),fill=c("red","blue"),cex=0.8)
plot(PCA, type="l", main="Variance by Principal Component", ylim=c(0,5))
summary(PCA)

#ggplot for Porosity and Brittleness against Production
ggplot(data=rock.data,aes(x=Por,y=Brittle)) + geom_point(aes(color=Production)) + 
  ggtitle("Porosity vs. Brittleness \n for Unconventional Wells")



