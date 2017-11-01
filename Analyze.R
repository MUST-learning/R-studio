

#read the data
gcds <- read.csv("crdds.csv", header=TRUE, 
                   sep=",")

# get columns' name and descriptions 
source("readcolnames.R") # load the definition of functions defined in a file
colinf <- readcolnames()
colnames(gcds) <- append(colinf[[2]], 'Class')
coldes <- append(colinf[[1]], 'Class')


# Describe data
#descriptive statistics£¬one by one
#This one is not recommended, interval is more suitable
table(data1$credit_usage)
barplot(table(data1$credit_usage))



library(ggplot2)
# suppressMessages(library(ggplot2))
# The good and bad customers
freq = tabulate(gcds$Class)

# mydata$v1 <- factor(mydata$v1,
#                     levels = c(1,2,3),
#                     labels = c("red", "blue", "green"))

pie <- ggplot(gcds, aes(x="", fill = factor(Class))) + geom_bar(width=1) + coord_polar(theta = "y") +
  # theme_void() +
  labs(x = "") +
  scale_fill_discrete(name="Class", breaks=c("1", "2"), labels=c("good", "bad")) +
  theme(axis.title.x=element_blank(), axis.ticks.x=element_blank()) +
  theme(legend.position = c(0.9, 0.9), plot.title = element_text(hjust = 0.5)) +
  ggtitle("Distribution of Class") +
  annotate("text", x=1, y=700, label="bad=243") +
  annotate("text", x=1, y=150, label="good=557")

print(pie)



# Sampling 
# tranform character column to factor
coltypes <- sapply(gcds, mode)
for (i in 1:ncol(gcds))
{
  if (coltypes[[i]] == "character")
    gcds[[i]] <- factor(gcds[[i]])
}


# gcds$Class[gcds$Class == 1] <- 'Good'
# gcds$Class[gcds$Class == 2] <- 'Bad'
gcds$Class <- factor(gcds$Class, levels=c(1,2), labels=c("Good", "Bad"))


set.seed(2)
ids <- 1:nrow(gcds)
trainratio <- 0.8
trainids <- sample(ids, trunc(length(ids)*trainratio))
trainset <- gcds[trainids, ]
testset <- gcds[-trainids, ]
# head(trainset, 2)


# Feature Selection
library(caret)
# library(class)
library(LiblineaR)
# gcds2 <- gcds[complete.cases(gcds), ]

control <- trainControl(method="repeatedcv", number=5, repeats=4)
model <- train(Class~., data=trainset, method="regLogistic", trControl=control)
importance <- varImp(model, scale=FALSE)
# print(importance)
plot(importance)



# Modeling
# Select top 10 important features
fbyimp <- row.names(importance[[1]])
top15 <- fbyimp[1:15]
selfeas <- c(top15, "Class")

# decision tree model
library(tree)
treemodel <- tree(Class ~ ., data=trainset[,selfeas]) #train the model
plot(treemodel, main="classification tree")
text(treemodel, pretty=0)

cprob <-predict(treemodel, testset[, top15])
chat <- rep("Good",  nrow(cprob) )
chat[cprob[,1]>=0.5] <- "Bad"
chat0 <- factor(chat,labels = c("Good","Bad"))


#ROCR 0.734
library(ROCR)
tab <- table(chat0, testset$Class)
print(tab)

pred <- prediction(as.numeric(chat0), as.numeric(testset$Class))
pref <- performance(pred, "tpr", "fpr")
auc <- performance(pred, "auc")
print(auc@y.values)
plot(pref)


# support vector machine 0.694

control <- trainControl(method="cv", savePredictions = T, classProbs = T)
svmmodel <- train(Class~., data=trainset[, selfeas], method="svmRadialSigma",
                  trControl=control)

chat <- predict(svmmodel, newdata = testset[, top15]) 


tab <- table(chat, testset$Class)
print(tab)

pred <- prediction(as.numeric(chat), as.numeric(testset$Class))
pref <- performance(pred, "tpr", "fpr")
auc <- performance(pred, "auc")
print(auc@y.values)
plot(pref)


# neural networks model 0.744
library(caret)
parameters<- expand.grid(.decay = c(0.5, 0.1), .size = c(5, 6, 7))
nnmodel <- train(trainset[, top15],  trainset[,"Class"],
                 method="nnet", tuneLength = 10, trace = F, maxit = 1000,
                 tuneGrid= parameters)
chat <- predict(nnmodel, newdata = testset[, top15])


tab <- table(chat, testset$Class)
print(tab)

pred <- prediction(as.numeric(chat), as.numeric(testset$Class))
pref <- performance(pred, "tpr", "fpr")
auc <- performance(pred, "auc")
print(auc@y.values)
plot(pref)


