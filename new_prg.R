#Clearing the envonment
gc(verbose = getOption("verbose"), reset = FALSE)
options(java.parameters = "-Xmx6144m")
options(java.parameters = "- Xmx4g")
memory.limit(size=10000000000024)
options(shiny.maxRequestSize=100*1024^2)
rm(list = ls(all.names = TRUE))

#Setting the Directory
setwd("D:/Misc/Interactive-Exploratory-Data-Analysis-master/textmining/Household_prediction/")

# Loading Required Libraries :
loadlibraries <- c('randomForest', 'ggplot2','mice')
installlib <- loadlibraries[!loadlibraries %in% installed.packages()]
for(libs in installlib) install.packages(libs, dependences = TRUE)
sapply(loadlibraries, require, character = TRUE)


#Loading The required files
data <- read.csv("train.csv") # Full data
vald <- read.csv("test.csv") # Out of Sample


#Pool Variable ::
# Get PoolQC and add "None"
levels <- levels(data$PoolQC)
levels[length(levels) + 1] <- "No-Pool"
data$PoolQC <- factor(data$PoolQC, levels = levels)
data$PoolQC[is.na(data$PoolQC)] <- "No-Pool"

# getting the missing counts
sort(sapply(data, function(x) { sum(is.na(x)) }), decreasing=TRUE)
imputed <- mice(data, meth = "cart", minbucket = 4)
data.imputed = complete(imputed)

drops <- c('MiscFeature', 'Alley', 'Fence')
data.imputed <-  data.imputed[,!(names(data.imputed) %in% drops)]

#splitting into test and train
dt = sort(sample(nrow(data.imputed), nrow(data.imputed)*.75))
train<-data.imputed[dt,]
test<-data.imputed[-dt,]
names(train)


########Modelling Starts here ########
df <- train
x <- df[,2:77]
mtry <- 17 #round(sqrt(ncol(x)),0)

vars <- paste(names(df[,2:77]),sep="")
varables <- paste("SalePrice ~", paste(vars, collapse="+"))
as.formula(varables)

model <- randomForest(as.formula(varables),  data = df,
                      ntree = 300,
                      mtry = mtry,
                      replace = F,
                      nodesize = 1,
                      importance = T,
                      do.trace = TRUE)



imp <- importance(model, type=1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])
p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() +
  theme_light(base_size=5) +
  xlab("") +
  ylab("Importance") +
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=10))

print(p)
model

# Using Caret and trying optimal parameters::
# library(caret)
# # Create model with default paramters
# control <- trainControl(method="repeatedcv", number=10, repeats=3)
# seed <- 7
# metric <- "RMSE"
# set.seed(seed)
# mtry <- round(sqrt(ncol(x)),0)
# tunegrid <- expand.grid(.mtry=mtry)
# rf_default <- train(as.formula(varables), data=df, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
# print(rf_default)
#
# # Random Search
# control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
# set.seed(seed)
# mtry <- sqrt(ncol(x))
# rf_random <- train(as.formula(varables), data=df, method="rf", metric=metric, tuneLength=15, trControl=control)
# print(rf_random)
# plot(rf_random)


# Doing predictions on the test data:
common <- intersect(names(df), names(test))
for (p in common) {
  if (class(train[[p]]) == "factor") {
    levels(test[[p]]) <- levels(train[[p]])
  }
}

predict <- predict(model, test)


# Evaluation metric function

RMSE <- sqrt(sum((predict - test$SalePrice)^2)/length(predict))
RMSE


# Doing predictions on the validation:

#Pool Variable ::
# Get PoolQC and add "None"
levels <- levels(vald$PoolQC)
levels[length(levels) + 1] <- "No-Pool"
vald$PoolQC <- factor(vald$PoolQC, levels = levels)
vald$PoolQC[is.na(vald$PoolQC)] <- "No-Pool"



drops <- c('MiscFeature', 'Alley', 'Fence')
vald  <- vald[,!(names(vald) %in% drops)]
imputed <- mice(vald, meth = "cart", minbucket = 4)
vald = complete(imputed)


common <- intersect(names(df), names(vald))
for (p in common) {
  if (class(df[[p]]) == "factor") {
    levels(vald[[p]]) <- levels(df[[p]])
  }
}

predict <- predict(model, vald)
predict[which(is.na(predict))] <- mean(predict,na.rm=T)
#predict[ is.na(predict) ] <- 0
final<- cbind(vald,predict)
colnames(final)[which(names(final) == "predict")] <- "SalePrice"
keeps <- c("Id","SalePrice")
df = final[keeps]

write.csv(df,"predictions.csv",row.names = FALSE)
