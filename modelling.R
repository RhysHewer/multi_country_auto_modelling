#------------------------------------------------------------------------------#
# Developer: Rhys Hewer
# Project: Oplaadpalen - Ownership Classification
# Version: 1
# Purpose: modelling
#------------------------------------------------------------------------------#

#load libraries
source("scripts/libraries.R")

#load data
data <- readRDS("output/featureData.rds")

##### Controls ################################################################

country <- "BE"
sampleSize <- 3000
algorithm <- "xgbTree" 

countryList = c("NL", "BE", "LU", "NO", "SE")

##### MODELLING PREPARATION ###################################################

#Parallel processing
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

#Cross Validation
fitControl<- trainControl(method = "cv", 
                          number = 5, 
                          savePredictions = TRUE, 
                          allowParallel = TRUE)

#subset (samplesize or all observations if nrows is smaller than samplesize)
subData <- data %>% filter(countrycode == country) %>% drop_na() 
if (nrow(subData) > sampleSize){
        subData <- subData %>% sample_n(sampleSize)
} else {
        subData <- subData
}

#Creating country Testing/Training sets
set.seed(111)
trainIndex <- createDataPartition(subData$public_access_type_id, 
                                  p = 0.75, list = FALSE)

training <- subData[ trainIndex,]
testing  <- subData[-trainIndex,]

#creating other country testing sets
otherCountries <- countryList[!countryList %in% country]

for (countryCode in otherCountries){
        command <- paste0(countryCode, 
                          "test <- data %>% filter(countrycode == '", 
                          countryCode, 
                          "') %>% drop_na()",
                          " %>% sample_n(555)")
        eval(parse(text = command))
}

#plot dependent variable distribution
for (countryCode in countryList){
        plotCommand <- paste0("plot(",
                              countryCode,
                              "test$public_access_type_id,",
                              "main = '",
                              countryCode,
                              "')")
        eval(parse(text = plotCommand))
}


##### RUN MODEL ###############################################################

#Train model
set.seed(111)
system.time(model <- train(public_access_type_id ~ .,
                     data = training,
                     method = algorithm,
                     trControl = fitControl))
model


##### PREDICTIONS #############################################################

#Predictions
preds <- predict(model, testing)
testing$preds <- preds

#other country predictions
for (countryCode in otherCountries){
        predCommand <- paste0(countryCode,
                          "Preds <- predict(model, ",
                          countryCode,"test)")
        eval(parse(text = predCommand))
        
        pasteCommand <- paste0(countryCode,
                               "test$preds <- ",
                               countryCode,
                               "Preds")
        eval(parse(text = pasteCommand))
}

##### SPOT CONN LEVEL METRICS #################################################

#spot conn level Metrics
mets <- postResample(pred = testing$preds, obs = testing$public_access_type_id)
mets

#other country spot conn level Metrics
for (countryCode in otherCountries){
        metCommand <- paste0(countryCode,
                             "mets <- postResample(pred = ",
                             countryCode,
                             "test$preds, obs = ",
                             countryCode,
                             "test$public_access_type_id)")
        eval(parse(text = metCommand))
}

##### Confusion Matrix ########################################################

#Confusion Matrix
confMat <- confusionMatrix(testing$preds,testing$public_access_type_id)
confMat

for (countryCode in otherCountries){
        confMatCommand <- paste0(countryCode,
                                 "confMat <- confusionMatrix(",
                                 countryCode,
                                 "test$preds,",
                                 countryCode,
                                 "test$public_access_type_id)")
        eval(parse(text = confMatCommand))
}


##### OUTCOMES ################################################################

#column names
outcomeCols <- c("trainCountry", "testCountry", "algorithm", "accuracy", "kappa", "publicSpecificity")

#observations for modelling country
testOutcome <- c(
        country,
        country,
        algorithm,
        mets[[1]],
        mets[[2]],
        confMat$byClass[3,2]
)
testOutcome

#create dataframe
outcome <- data.frame(matrix(ncol = 5, nrow = 0))
outcome <- outcome %>% rbind(testOutcome)
colnames(outcome) <- outcomeCols

#add other country observations
for (countryCode in otherCountries){
        
        cAccCommand <- paste0(countryCode, "mets[[1]]")
        cKappaCommand <- paste0(countryCode, "mets[[2]]")
        cSpecCommand <- paste0(countryCode, "confMat$byClass[3,2]")

        otherOut <- data.frame(paste0(country), 
                      paste0(countryCode),
                      paste0(algorithm),
                      paste0(eval(parse(text = cAccCommand))),
                      paste0(eval(parse(text = cKappaCommand))),
                      paste0(eval(parse(text = cSpecCommand))))
        colnames(otherOut) <- outcomeCols
        outcome <- outcome %>% rbind(otherOut)
}
outcome

filepath <- paste0("output/",country,"_", algorithm, "_metrics.rds")

saveRDS(outcome, file = filepath)


        