DATA_train <- read.csv("TRAIN_DATA.csv")
write.csv(DATA_train,file = "TRAIN.CSV")
data_train <- DATA_train
data_train[data_train == "?"] = NA
data_train <- centralImputation(data_train)
###### DROPPING Columns WITH 30% beyond NAS ########
colMeans(is.na(data_train))
str(data_train)
data_train$AdmissionID<- NULL
data_train$patientID<- NULL
data_train$payer_code<-NULL
data_train$medical_specialty <- NULL
data_train$weight<- NULL
####################### SEPERATING DATE COLUMN ###############
data_train <- separate(data_train, "Admission_date", c("Day", "Month", "Year"), sep = "-")
#colnames(data_train)[1] <- "Day"
#colnames(data_train)[3] <- "Year"
data_train <- separate(data_train, "Discharge_date", c("DayD", "MonthD", "YearD"), sep = "-")
data_train$Day <- as.factor(data_train$Day)
data_train$Year <- as.factor(data_train$Year)
data_train$Month <- as.factor(data_train$Month)
data_train$DayD <- as.factor(data_train$DayD)
data_train$YearD <- as.factor(data_train$YearD)
data_train$MonthD <- as.factor(data_train$MonthD)
###########################CONVERTING THE THREE IDS AS FACTOR#################################
data_train$admission_type_id <- as.factor(data_train$admission_type_id)
data_train$discharge_disposition_id <- as.factor(data_train$discharge_disposition_id)
data_train$admission_source_id <- as.factor(data_train$admission_source_id)
str(data_train)
######################## EDITING THE AGE COLUMN ################################
data_train$age <- gsub("\\[|\\]","",data_train$age)
data_train$age <- gsub(")","",data_train$age)
data_train$age <- as.factor(data_train$age)
######################## EDITING RACE ##########################################
data_train$race <- as.factor(gsub("?", "", as.character(data_train$race)))
str(data_train)
################################### 
########################## EDITING THE DIAGONOSIS ################
data_train$diagnosis_3 <- as.numeric(as.character(data_train$diagnosis_3))
data_train$diagnosis_2 <- as.numeric(as.character(data_train$diagnosis_2))
data_train$diagnosis_1 <- as.numeric(as.character(data_train$diagnosis_1))
#data_train$diagnosis_3[data_train$diagnosis_3 == NA] = 0
data_train$diagnosis_3 <- cut(data_train$diagnosis_3, 
                       breaks = c(139,239,279,289,319,389,459,519,579,629,679,709,739,759,779,799,999), 
                       labels = c("infectious","neoplasms","endocrine","blood","mental_disorders","nervous","circulatory","respiratory","digestive","genitourinary","pregnancy","skin","musculoskeletal","congenital_anomalies","perinatal_period","ill_efined conditions","injury_and_poisoning"), 
                       right = T)
data_tr3<-select(data_train,patientID,diagnosis_1) %>% group_by(diagnosis_1) %>% mutate(diagnosis_1_bin = ifelse(diagnosis_1 >1 & diagnosis_1 <=139, "infectious",
                                                                                                          ifelse(diagnosis_1 >= 140 & diagnosis_1 <=239,"neoplasms",
                                                                                                                 ifelse(diagnosis_1 >= 240 & diagnosis_1 <=279,"endocrine",
                                                                                                                        ifelse(diagnosis_1 >= 280 & diagnosis_1 <=289,"blood",
                                                                                                                               ifelse(diagnosis_1 >= 290 & diagnosis_1 <=319,"mental disorders",
                                                                                                                                      ifelse(diagnosis_1 >= 320 & diagnosis_1 <=389,"nervous",
                                                                                                                                             ifelse(diagnosis_1 >= 390 & diagnosis_1 <=459,"circulatory",
                                                                                                                                                    ifelse(diagnosis_1 >= 460 & diagnosis_1 <=519,"respiratory",
                                                                                                                                                           ifelse(diagnosis_1 >= 520 & diagnosis_1 <=579,"digestive",
                                                                                                                                                                  ifelse(diagnosis_1 >= 580 & diagnosis_1 <=629,"genitourinary",
                                                                                                                                                                         ifelse(diagnosis_1 >= 630 & diagnosis_1 <=679,"pregnancy",
                                                                                                                                                                                ifelse(diagnosis_1 >= 680 & diagnosis_1 <=709,"skin",
                                                                                                                                                                                       ifelse(diagnosis_1 >= 710& diagnosis_1  <=739,"musculoskeletal",
                                                                                                                                                                                              ifelse(diagnosis_1 >= 740 & diagnosis_1 <=759,"congenital anomalies",
                                                                                                                                                                                                     ifelse(diagnosis_1 >= 760 & diagnosis_1 <=779,"perinatal period",
                                                                                                                                                                                                            ifelse(diagnosis_1 >= 780 & diagnosis_1 <=799,"ill-defined conditions",
                                                                                                                                                                                                                   ifelse(diagnosis_1 >= 800 & diagnosis_1 <=999,"injury and poisoning","external causes"))))))))))))))))))



data_pach1<-data_tr$diagnosis_3_bin
data_train <- data.frame(data_pach1,data_train)
data_patch2 <-data_tr2$diagnosis_2_bin
data_train <- data.frame(data_patch2,data_train)
data_patch3 <- data_tr3$diagnosis_1_bin 
data_train <- data.frame(data_patch3,data_train)
data_train <- data_train[,c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,1,2,3,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47)]
colnames(data_train)[17] <- "diagnosis-3"
colnames(data_train)[18] <- "diagnosis-2"
colnames(data_train)[19] <- "diagnosis-1"
write.csv(data_train , file = "TRAIN_DATA.CSV")
sum(is.na(data_train$`diagnosis-3`))
data_train$`diagnosis-3`<-as.character(data_train$`diagnosis-3`)
data_train$`diagnosis-3`[is.na(data_train$`diagnosis-3`)] = "EXTERNAL"
data_train$`diagnosis-3`<-as.factor(data_train$`diagnosis-3`)
data_train$`diagnosis-2`<-as.character(data_train$`diagnosis-2`)
data_train$`diagnosis-2`[is.na(data_train$`diagnosis-2`)] = "EXTERNAL"
data_train$`diagnosis-2`<-as.factor(data_train$`diagnosis-2`)
data_train$`diagnosis-1`<-as.character(data_train$`diagnosis-1`)
data_train$`diagnosis-1`[is.na(data_train$`diagnosis-1`)] = "EXTERNAL"
data_train$`diagnosis-1`<-as.factor(data_train$`diagnosis-1`)
data_train <- read.csv("TRAIN_DATA.csv")
data_train$X<-NULL
str(data_train)
data_train <- data_train[,-c(1,2,3,4,5,6)]
colnames(data_train)[10] <- "diagnosis-1"
colnames(data_train)[11] <- "diagnosis-2"
colnames(data_train)[12] <- "diagnosis-3"
target <-data_train$readmitted
train_data <- data.frame(data_train,target)
data_train$readmitted<-NULL 
##################################################################
######################## PART ######################
smot_data <-data.frame(data_train,target1$target)
smot_data<- SMOTE(readmitted~.,smot_data,k=2,perc.over =20)
table(smot_data$readmitted)
write.csv(smot_data ,file = "TRAIN_DATABINNED")
trainrows<- createDataPartition(data_train$readmitted,p=0.9,list = F)
train <- smot_data[trainrows,]
test <- smot_data[-trainrows,]
colnames(smot_data)[37] <- "readmitted"
########## MODEL ###########
smot_data$YearD<-NULL
smot_data$target <- NULL
model <- rpart(readmitted~.,smot_data,control = rpart.control(cp = 0.01))
rpart.plot.version1(model,compress = T)
fancyRpartPlot(model)
a <- names(smot_data)
summary(model)
preeds <- predict(model,smot_data,type = "class")
table(preeds)
confusionMatrix(preeds,smot_data$readmitted,positive = "Within30days")
preeds_te <- predict(model,data_test,type = "class")
table(preeds_te)
str(data_test)
write.csv(preeds_te,file = "PREDS_222.csv")
############## RULES ###############
install.packages("rpart.utils")
library(rpart.utils)
sink("rules_69.txt")
print(asRules(model))
sink()
rpart.rules(model)
train$Year<-NULL
names(data_test)
train$YearD <- NULL
str(data_test)
data_train$race <- NULL

data_test
data_train <- data_train[,c(1,2,3,4,5,6,7,8,9,12,11,10,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37)]
################################################################################
DATA_TEST <- read.csv("Test.csv")
data_test <- DATA_TEST
data_test <- data_test[,-c(4,5,6,7,8,9)]
data_tes
data_test$X <- NULL
data_test$Year <- NULL
data_test$YearD <- NULL
data_test[data_test == "?"] = NA
data_
data_test <- centralImputation(data_test)
################################################################################
data_test$AdmissionID<- NULL
data_test$patientID<- NULL
data_test$payer_code<-NULL
data_test$medical_specialty <- NULL
data_test$weight<- NULL
#####################################################
data_test <- separate(data_test, "Admission_date", c("Day", "Month", "Year"), sep = "-")
#colnames(data_train)[1] <- "Day"
#colnames(data_train)[3] <- "Year"
data_test <- separate(data_test, "Discharge_date", c("DayD", "MonthD", "YearD"), sep = "-")
data_test$Day <- as.numeric(data_test$Day)
data_test$Year <- as.numeric(data_test$Year)
data_test$Month <- as.numeric(data_test$Month)
data_test$DayD <- as.numeric(data_test$DayD)
data_test$YearD <- as.numeric(data_test$YearD)
data_test$MonthD <- as.numeric(data_test$MonthD)
##############################################
data_test$admission_type_id <- as.factor(data_test$admission_type_id)
data_test$discharge_disposition_id <- as.factor(data_test$discharge_disposition_id)
data_test$admission_source_id <- as.factor(data_test$admission_source_id)
#############################################
data_test$race <- as.factor(gsub("?","", as.character(data_test$race)))
############################################
data_test$age <- gsub("\\[|\\]","",data_test$age)
data_test$age <- gsub(")","",data_test$age)
data_test$age <- as.factor(data_test$age)
da
###########################################
data_test$diagnosis_1 <- as.numeric(as.character(data_test$diagnosis_1))
data_test$diagnosis_2 <- as.numeric(as.character(data_test$diagnosis_2))
data_test$diagnosis_3 <- as.numeric(as.character(data_test$diagnosis_3))
data_te3<-select(data_test,patientID,diagnosis_3) %>% group_by(diagnosis_3) %>% mutate(diagnosis_3_bin = ifelse(diagnosis_3 >1 & diagnosis_3 <=139, "infectious",
                                                                                                                 ifelse(diagnosis_3 >= 140 & diagnosis_3 <=239,"neoplasms",
                                                                                                                        ifelse(diagnosis_3 >= 240 & diagnosis_3 <=279,"endocrine",
                                                                                                                               ifelse(diagnosis_3 >= 280 & diagnosis_3 <=289,"blood",
                                                                                                                                      ifelse(diagnosis_3 >= 290 & diagnosis_3 <=319,"mental disorders",
                                                                                                                                             ifelse(diagnosis_3 >= 320 & diagnosis_3 <=389,"nervous",
                                                                                                                                                    ifelse(diagnosis_3 >= 390 & diagnosis_3<=459,"circulatory",
                                                                                                                                                           ifelse(diagnosis_3 >= 460 & diagnosis_3 <=519,"respiratory",
                                                                                                                                                                  ifelse(diagnosis_3 >= 520 & diagnosis_3 <=579,"digestive",
                                                                                                                                                                         ifelse(diagnosis_3 >= 580 & diagnosis_3 <=629,"genitourinary",
                                                                                                                                                                                ifelse(diagnosis_3 >= 630 & diagnosis_3 <=679,"pregnancy",
                                                                                                                                                                                       ifelse(diagnosis_3 >= 680 & diagnosis_3 <=709,"skin",
                                                                                                                                                                                              ifelse(diagnosis_3 >= 710& diagnosis_3  <=739,"musculoskeletal",
                                                                                                                                                                                                     ifelse(diagnosis_3 >= 740 & diagnosis_3 <=759,"congenital anomalies",
                                                                                                                                                                                                            ifelse(diagnosis_3 >= 760 & diagnosis_3 <=779,"perinatal period",
                                                                                                                                                                                                                ifelse(diagnosis_3 >= 780 & diagnosis_3<=799,"ill-defined conditions",
                                                                                                                                                                                                                          ifelse(diagnosis_3 >= 800 & diagnosis_3 <=999,"injury and poisoning","external causes"))))))))))))))))))






bin1 <- data_te1$diagnosis_1_bin 
bin1<- data.frame(bin1)
data_test$diagnosis.2<-NULL
data_test <- data.frame(bin1,data_test)
colnames(data_test)[3] <- "diagnosis.11"
colnames(data_test)[2] <- "diagnosis.12"
colnames(data_test)[1] <- "diagnosis.13"
bin2<- data_te1$diagnosis_2_bin
bin2<- data.frame(data_te1$diagnosis_2_bin)
data_test <- data.frame(bin2,data_test)
bin3 <- data_te3$diagnosis_3_bin
bin3<- data.frame(bin3)
#data_test$bin3.1 <-NULL
data_test <- data.frame(bin3,data_test)
data_test$patientID <- NULL
str(data_test)
#data_test$diagnosis_3 <- NULL
data_test <- data_test[,c(4,5,6,7,8,9,10,11,12,13,3,2,1,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37)]
data_test$diagnosis.11 <- as.character(data_test$diagnosis.11)
data_test$diagnosis.11[is.na(data_test$diagnosis.11)] = "EXTERNAL"
data_test$diagnosis.11 <- as.factor(data_test$diagnosis.11)
data_test$diagnosis.12<-as.character(data_test$diagnosis.12)
data_test$diagnosis.12[is.na(data_test$diagnosis.12)] = "EXTERNAL"
data_test$diagnosis.12 <- as.factor(data_test$diagnosis.12)
data_test$diagnosis.13<-as.character(data_test$diagnosis.13)
data_test$diagnosis.13[is.na(data_test$diagnosis.13)] = "EXTERNAL"
data_test$diagnosis.13 <- as.factor(data_test$diagnosis.13)
sum(is.na(data_test$diagnosis.13))
str(data_train)
str(data_test)
data_test$race <- NULL
data_train <- NULL
colnames(data_train$`diagnosis-3`)
colnames(data_test)[11] <- "diagnosis.1"
colnames(data_test)[12] <- "diagnosis.2"
colnames(data_test)[13] <- "diagnosis.3"
write.csv(data_test,file = "DATA_TESTBINNED.csv")
library(JGR)
JGR()
prop.table(table(smot_data$readmitted))
