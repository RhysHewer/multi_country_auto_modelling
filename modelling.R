#------------------------------------------------------------------------------#
# Developer: Rhys Hewer
# Project: Oplaadpalen - Ownership Classification
# Version: 1
# Purpose: modelling
#------------------------------------------------------------------------------#

#load libraries
source("scripts/libraries.R")


##### Controls ################################################################

#Adjust below:
dataSet <- "output/featureData.rds"  #combined dataset @ spotconnector level
CSdataSet <- "output/chargeStatDataP0DF.RDS"  #charging station table data
country <- "LU"
sampleSize <- 3000
algorithm <- "rf" 

#All countries of interest
countryList = c("NL", "BE", "LU")

#load data
data <- readRDS(dataSet)

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
training <- training %>% select(-chargingstation_id)

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

plotCommand <- paste0("plot(testing$public_access_type_id,",
                      "main = '",
                      country,
                      "')")
eval(parse(text = plotCommand))

for (countryCode in otherCountries){
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


##### Spot Conn Level PREDICTIONS #############################################

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
mets <- postResample(pred = testing$preds, 
                     obs = testing$public_access_type_id)
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

##### SPOT CONN LEVEL Confusion Matrix ########################################

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


##### SPOT CONN LEVEL OUTCOMES ################################################

#column names
outcomeCols <- c("trainCountry", "testCountry", 
                 "algorithm", "accuracy", 
                 "kappa", "publicSpecificity")

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

filepath <- paste0("output/",country,"_", algorithm, "_spotConnmetrics.rds")

saveRDS(outcome, file = filepath)


##### Charging Station Level Predictions ######################################

#convert to charging station level with group_by on chargingstation_id
CStesting <- testing %>% 
        group_by(chargingstation_id) %>% 
        summarise(CSpredsPrep = paste(unique(preds), collapse = ', '))

###repeat for other countries
for (countryCode in otherCountries){
        CStestCommand <- paste0("CS",
                                countryCode,
                                "test <- ",
                                countryCode,
                                "test %>% group_by(chargingstation_id) ",
                                "%>% summarise(CS",
                                countryCode, 
                                "predsPrep = ",
                                "paste(unique(preds), collapse = ', '))")
        eval(parse(text = CStestCommand))
}

#load charging station table and recode dependent variable
CSdata <- readRDS(CSdataSet)
CSdata <- CSdata %>% rename(id = ï..id)
CSdata$public_access_type_id <- CSdata$public_access_type_id %>% 
        recode('1' = "public",'2' = "private",'3' = "company") %>% 
        as.factor()

#subset charging station table to relevant country (by chargingstation_id)
CSdataTest <- CSdata %>% filter(id %in% CStesting$chargingstation_id)

#repeat for other countries
for (countryCode in otherCountries){
        CSsubsetCommand <- paste0("CS",
                                  countryCode,
                                  "data <- CSdata %>% filter(id %in% CS",
                                  countryCode,
                                  "test$chargingstation_id)")
        eval(parse(text = CSsubsetCommand))
}

#convert all double entries to "private"
CSlength <- nchar(CStesting$CSpredsPrep)
CStesting$CSpreds <- ifelse(CSlength > 7, "private", CStesting$CSpredsPrep) %>% as.factor()

CStesting$CSpredsPrep <- CStesting$CSpredsPrep %>% as.factor()
CStesting$CSpreds <- CStesting$CSpreds %>% as.factor()

#repeat for other countries
for (countryCode in otherCountries){
        CSlengthCommand <- paste0("CS",
                                  countryCode,
                                  "length <- nchar(CS",
                                  countryCode,
                                  "test$CS",
                                  countryCode,
                                  "predsPrep)")
        eval(parse(text = CSlengthCommand))
}

for (countryCode in otherCountries){
        CSrecodeCommand <- paste0("CS",countryCode,
                                  "test$CSpreds <- ifelse(CS", 
                                  countryCode, 
                                  "length > 7, 'private', CS", 
                                  countryCode, 
                                  "test$CS", 
                                  countryCode,
                                  "predsPrep)",
                                  "%>% as.factor()")
        eval(parse(text = CSrecodeCommand))
}


##### Charging Station Level Metrics ##########################################

#Metrics
CSmets <- postResample(pred = CStesting$CSpreds, 
                       obs = CSdataTest$public_access_type_id)

#repeat for other countries
for (countryCode in otherCountries){
        CSmetsCommand <- paste0("CS",
                                countryCode, 
                                "mets <- postResample(pred = CS", 
                                countryCode, 
                                "test$CSpreds,obs = CS", 
                                countryCode, 
                                "data$public_access_type_id)")
        eval(parse(text = CSmetsCommand))
}


##### Charging Station Level confusion matrix #################################

#Confusion Matrix
CSconfMat <- confusionMatrix(CStesting$CSpreds,
                             CSdataTest$public_access_type_id)

#repeat for other countries

for (countryCode in otherCountries){
        CSconfMatCommand <- paste0("CS",
                                   countryCode,
                                   "confMat <- confusionMatrix(CS",
                                   countryCode, 
                                   "test$CSpreds,CS", 
                                   countryCode,
                                   "data$public_access_type_id)")
        eval(parse(text = CSconfMatCommand))
}

##### Charging Station Level OUTCOMES ################################################

#column names
CSoutcomeCols <- c("trainCountry", "testCountry", 
                 "algorithm", "accuracy", 
                 "kappa", "publicSpecificity")

#observations for modelling country
CStestOutcome <- list(
        country,
        country,
        algorithm,
        CSmets[[1]],
        CSmets[[2]],
        CSconfMat$byClass[3,2]
)
CStestOutcome

#create dataframe
CSoutcome <- data.frame(matrix(ncol = 6, nrow = 0))
CSoutcome <- CSoutcome %>% rbind(CStestOutcome)
colnames(CSoutcome) <- CSoutcomeCols
CSoutcome$trainCountry  <- CSoutcome$trainCountry %>% as.character()
CSoutcome$testCountry <- CSoutcome$testCountry%>% as.character()
CSoutcome$algorithm <- CSoutcome$algorithm%>% as.character()


#lists of other country outcomes
for (countryCode in otherCountries){
        CSoutCommand <- paste0("CS",
                               countryCode, 
                               "testOutcome <- list(country,countryCode,",
                               "algorithm,CS", 
                               countryCode, 
                               "mets[[1]], CS", 
                               countryCode, 
                               "mets[[2]],CS", 
                               countryCode, "confMat$byClass[3,2])")
        eval(parse(text = CSoutCommand))
}

#lists to dataframe
for (countryCode in otherCountries){
        CSbindCommand <- paste0("CSoutcome <- CSoutcome %>% rbind(CS",
                                countryCode,
                                "testOutcome)")
        eval(parse(text = CSbindCommand))
}
CSoutcome

filepath <- paste0("output/",country,"_", algorithm, "_chargeStatmetrics.rds")

saveRDS(CSoutcome, file = filepath)
