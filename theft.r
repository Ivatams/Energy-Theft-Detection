library(tidyverse)
#install.packages("skimr")
library(skimr)
library(lubridate)
#install.packages("ggcorrplot")
library(ggcorrplot)
#install.packages("corrplot")
#library(corrplot)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
library(party)
library(lubridate)
library(knitr)
library(e1071)
library(rpart)
library(gridExtra)
library(rpart.plot)
library(xgboost)
library(yardstick)
#install.packages("yardstick")
#install.packages("shinythemes")
#install.packages("e1071")
#install.packages("gridExtra")
#install.packages("ggthemes")
#install.packages("caret")
#install.packages("MASS")
#install.packages("randomForest")
#install.packages("party")
#install.packages("tensorflow")
#install.packages("keras")

#glimpse(ENERGY.THEFT.DATA)
df <- ENERGY.THEFT.DATA

#Remove NA, NULL, NILL etc
df %>% 
  mutate_all(~ifelse(. %in% c("N/A", "null","NILL","Missing","na","NA","NIL",""), NA, .)) %>% 
  na.omit()
df
# REMOVE Address and Customer Name Col.
#df$Customer.Name<-NULL
#df$Customer.address <- NULL
df$Duplicate.Checker<-NULL
# dimensions of the data
dim_desc(df)
# check for number of no NAs
sum(complete.cases(df))
#What is the distribution of the NA’s?
  colSums(is.na(df))
# Converting to numeric string TO FACTOR
df$Status <- as.factor(df$Status)
#nums <- c("Meter.No.", "Customer.AC.No")
#df[,nums] <- lapply(df[,nums] , as.numeric)
skim(df)
# Replace all numeric values in Meter make to Missing
#df$Meter <- ifelse(!is.na(df$Meter.Make) & df$Meter.Make >1, "Missing", df$Meter.Make)
glimpse(df)

# Let drop tariff with NAs but first let's see how many of those NAs have Byepass
Tar_na <- df %>%
filter(is.na(Customer.tariff.as.per.bill))
head(Tar_na[, c("Customer.tariff.as.per.bill","Date.of.visit","Meter.Make","Status")], 20)

# Get list of status that has NAs for Tariff
tariff_status <- (Tar_na$Customer.tariff.as.per.bill)

#Extract all records for status that have at least one NA for tariff.
#TarNA_Stat <- df %>%
#filter(Status %in% tariff_status) %>%
#select(Status,Date.of.visit, Meter.Make, Customer.tariff.as.per.bill, tariff_status) %>%
#arrange(Status)
#head(TarNA_Stat, 20)
#...........
#TarNA_Stat0 <- TarNA_Stat %>%
#filter(is.na(Customer.tariff.as.per.bill))
#head(TarNA_Stat0,10)

# Let's see the outcome
#colSums(is.na(TarNA_Stat0))

#Exploring the Data
# Let's start by transforming the Status variables to factor
df <- df %>% mutate_if(is.character, as.factor)
df$Status <- as.factor(df$Status)
glimpse(df)

# Let's look for missing values
df %>% map(~ sum(is.na(.)))

#Meter No. Feeder, Customer name, Customer account number, address are the only features with
#NAs but they are not important feature for the analysis
#Let’s start by taking a look at the unique values of the factor variables.

df_tbl <- df %>%
select_if(is.factor) %>%
summarise_all(n_distinct)
#df_tbl[1:8]
#head(df$Status,4)

#To guide the analysis, I’m going to try and answer the following questions 
#about the bypass and no Bye-pass segments :::

#Which tariff class is more likely to bypass than the other?
#Which meter make is more like to bypass?
#Do individuals with a bypass more than those without a bypass?
#Do people with high availability bypass more than people that do not have high availability?
#Do people with high load bypass more than people that do not have high load?
#Dates with the highest bypass recorded?

#df$Customer.tariff.as.per.bill <- str_replace_all(df$Customer.tariff.as.per.bill, c("r2" = "R2"))
#Which tariff class is more likely to bypass than the other?
#................................................................................
ggplot(df) +
  geom_bar(aes(x = Customer.tariff.as.per.bill, fill = Status), position = "dodge")

# Let's take a look at the status to show exactly how many customers by tariff
df %>%
  group_by(Customer.tariff.as.per.bill,Status) %>%
  summarise(n=n())
#...............................................................................
# Next I’ll take a look at meter make.
#Which meter make is more like to likely bypass?
ggplot(df) +
  geom_bar(aes(x = Meter.Make, fill = Status), position = "dodge")

# Let's take a look at the status to show exactly how many customers by meter Make
df %>%
  group_by(Meter.Make) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)*100) %>%
  arrange(desc(freq))

df %>%
  group_by(Meter.Make, Status) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)*100) %>%
  arrange((freq))

# The results show that about 16% of the customer visited use Conlog 
# but Simens meters are more likely

#Now I am going to take a look at people with High load
#Do people with high load bypass more than people that do not have high load?
names(df)
#.............................................................................
#Load
ggplot(df) +
  geom_bar(aes(x=Status, fill = Estimated.Connected.Load), position = "dodge")

# Frequency
df %>%
  group_by(Estimated.Connected.Load) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)*100) %>%
  arrange((freq))

df %>%
group_by(Estimated.Connected.Load, Status) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n) *100) %>%
  arrange(desc(freq))

#...........................................................................
#Now I’m going to take a look at people with High availability
#Do people with high availability bypass more than people that do not have high availability?
ggplot(df) +
  geom_bar(aes_string(x="Availability", fill="Status"), position = "dodge")

#Frequency
df %>% group_by(Availability, Status) %>%
  summarise(n=n()) %>%
  mutate(freq = n / sum(n)) %>%
  arrange(desc(freq))

# Frequency
df %>% group_by(Availability) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  arrange(desc(freq))
#..............................................................................
#Do individuals with a bypass more than those without a bypass?
df %>%
  group_by(Status) %>%
  summarise(Number_by_rows = length(Status))

df %>%
  #group_by(Status) %>%
  summarise(Total_Visits = n())
#...............................................................................
df%>%
  group_by(Estimated.Connected.Load, Status) %>%
  summarise(n = n()) %>%
  arrange(desc(n))
  #mutate(freq = n/sum())

#..............................................................................  
#..............................................................................  
#Dates with the highest bypass recorded?
#df$Date.of.visit <- ymd(df$Date.of.visit)
  #df %>% 
    #filter(Status == "Bypass") %>%
    #group_by(Year=year(Date.of.visit),Month=month(Date.of.visit,label = T)) %>% 
    #summarise(Bypass=n())
  #class(df$Date.of.visit)
  
#...............................................................................
#...............................................................................
#Continuous Variable Distribution
  df %>%
    keep(is.numeric) %>%
    gather() %>%
    ggplot() +
    geom_histogram(mapping = aes(x=value,fill=key), color="black") +
    facet_wrap(~ key, scales = "free") +
    theme_minimal() +
    theme(legend.position = 'none')

  #Categorical variable exploration
  #df %>%
    #dplyr::select(-Status) %>% 
    #keep(is.factor) %>%
    #gather() %>%
    #group_by(key, value) %>% 
    #summarize(n = n()) %>% 
    #ggplot() +
    #geom_bar(mapping=aes(x = value, y = n, fill=key), color="black", stat='identity') + 
    #coord_flip() +
    #facet_wrap(~ key, scales = "free") +
    #theme_minimal() +
    #theme(legend.position = 'none')
  
  min(df$Average.Consumption); max(df$Average.Consumption)
  
  #Categorizing Average Consumption
  
  consumption_group <- function(Average.Consumption){
    if (Average.Consumption>= 0 & Average.Consumption <= 1846){
      return('0-1846')
    }else if(Average.Consumption > 1846 & Average.Consumption<= 3692){
      return('1846-3692')
    }else if (Average.Consumption > 3692 & Average.Consumption <= 7384){
      return('3692-7384')
    }else if (Average.Consumption > 7384 & Average.Consumption <=9233){
      return('7384-9233')
    }else if (Average.Consumption> 9233){
      return('>9233')
    }
  }
  df$group_cons <- sapply(df$Average.Consumption,consumption_group)
  df$group_cons <- as.factor(df$group_cons)
  
  # Bypass by date
  df$Date.of.visit  <- as.Date(df$Date.of.visit, format ="%m/%d/%Y")
  
    #df%>%
      #group_by(Year=year(df$Date.of.visit),Month=month(df$Date.of.visit, label = T),Bypass = Status)%>%
      #filter(Status == 1) %>%
      #summarise(n = n())
    #summarise(Bypass=sum(Status,na.rm = T))
 
  #Correlation Matrix & FEATURE ENGINEERING
  numericVarName <- names(which(sapply(df, is.numeric)))
  corr <- cor(df[,numericVarName], use = 'pairwise.complete.obs')
  ggcorrplot(corr, lab = TRUE)
  
  #Removing customerNO; doesn't add any value to the model
  #Remove multicollineated variables
  #df$Customer.AC.No <- NULL 
  #df$Estimated.Connected.Load <- NULL
  #df$Customer.Name <- NULL
  #df$Customer.address <- NULL
  #df$Meter.No. <- NULL
  #df$Duplicate.Checker <- NULL
  #df$Customer.tariff.as.per.bill <- NULL
  #df$Meter.Make<- NULL
  #df$Customer.tariff.as.per.bill <- NULL
  glimpse(df)
  #..............................................................................
# Convert target variable t 1 and O for ML
  df$Status <- ifelse(df$Status=="Bypass",1,0)
  table(df$Status)

#................................................................................
  # Check to see meters with highest bypass
  df %>%
    filter(Status=="1")%>%
    group_by(Meter.Make, Status)%>%
    summarise(n = n()) %>%
    ggplot(aes(x = Meter.Make, y = n))+
    geom_bar(stat = "identity")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  # Recode meters with bypass into High and Low
  
  df <- df %>%
    mutate(MeterBypass = 
             case_when(Meter.Make == 'ANALOGUE' ~ "high",
                       Meter.Make == 'CONLOG' ~ "high",
                       Meter.Make == 'DINRAIL' ~ "high",
                       Meter.Make == 'EDMI' ~ "high",
                       Meter.Make == 'EMCON' ~ "high",
                       Meter.Make == 'GENUS' ~ "high",
                       Meter.Make == 'HEXING' ~ "high",
                       Meter.Make == 'HOLLEY' ~ "high",
                       Meter.Make == 'ITRON' ~ "high",
                       Meter.Make == 'K-ELECTRIC' ~ "high",
                       Meter.Make == 'MOJEC' ~ "high",
                       Meter.Make == 'TECNO' ~ "high",
                       Meter.Make == 'L&G' ~ "high",
                       Meter.Make == 'LANDIS' ~ "high",
                       Meter.Make == 'MBH' ~ "high",
                       Meter.Make == 'UNISTAR' ~ "high",
                       Meter.Make == 'SIEMENS' ~ "high",
                      TRUE ~ "Low"))

  table(df$MeterBypass)
#................................................................................
  
  # Check to see Feeders with highest bypass
  df %>%
    filter(Status=="1")%>%
    group_by(Feeder, Status)%>%
    summarise(n = n()) %>%
    ggplot(aes(x = Feeder, y = n))+
    geom_bar(stat = "identity")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  df <- df %>%
    mutate(FeederBypass = 
             case_when(Feeder == 'ABAK' ~ "high",
                       Feeder == 'AIRPORT' ~ "high",
                       Feeder == 'AKANI' ~ "high",
                       Feeder == 'AMIKA' ~ "high",
                       Feeder == 'EKET' ~ "high",
                       Feeder == 'FLOUR MILL' ~ "high",
                       Feeder == 'FOUR LANE' ~ "high",
                       Feeder == 'IBESIKPO' ~ "high",
                       Feeder == 'KOM KOM' ~ "high",
                       Feeder == 'NTA' ~ "high",
                       Feeder == 'ONNE' ~ "high",
                       Feeder == 'OYIGBO' ~ "high",
                       Feeder == 'RAINBOW' ~ "high",
                       Feeder == 'REFINERY LINE 1' ~ "high",
                       Feeder == 'REFINERY LINE 2' ~ "high",
                       Feeder == 'RSPUB 1' ~ "high",
                       Feeder == 'RUKPOKWU' ~ "high",
                       Feeder == 'RUMOLUMINI' ~ "high",
                       Feeder == 'RUMUOLA' ~ "high",
                       Feeder == 'SECRETARIAT' ~ "high",
                       Feeder == 'STATE HOUSING' ~ "high",
                       Feeder == 'T1B' ~ "high",
                       Feeder == 'TIMBER' ~ "high",
                       Feeder == 'TRANSAMADI' ~ "high",
                       Feeder == 'UST' ~ "high",
                       Feeder == 'RSTV' ~ "high",
                       Feeder == 'ORON ROAD'~ "high",
                       TRUE ~ "Low"))
  table(df$FeederBypass)
  
#................................................................................
  
  df %>%
    filter(Status=="1")%>%
    group_by(Customer.tariff.as.per.bill, Status)%>%
    summarise(n = n()) %>%
    ggplot(aes(x = Customer.tariff.as.per.bill, y = n))+
    geom_bar(stat = "identity")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  df <- df %>%
    mutate(TariffBypass = 
             case_when(Customer.tariff.as.per.bill == 'R2' ~ "high",
                       Customer.tariff.as.per.bill == 'A1' ~ "high",
                       Customer.tariff.as.per.bill == 'c1' ~ "high",
                       Customer.tariff.as.per.bill == 'C2' ~ "high",
                       Customer.tariff.as.per.bill == 'R3' ~ "high",
                       TRUE ~ "Low"))
  table(df$TariffBypass)
#unique(training$group_cons)
  #c("0-1846", "1846-3692", "3692-7384", "7384-9233")
  #df <- df%>%
  #group_by(Year=year(df$Date.of.visit),Month=month(df$Date.of.visit, label = T),Bypass = Status)%>%
  #filter(Status == 1) %>%
  #summarise(n = n()) %>%
  #ungroup() %>%
  #ggplot(aes(x= month))+
  #geom_bar(stat = "identity")+
  #theme(axis.text.x = element_text(angle = 90, hjust = 1))
  #summarise(Bypass=sum(Status,na.rm = T)) 
#................................................................................
#................................................................................
# Performing Machine Learning Model. Logistic Regression
#First, we split the data into training and testing sets:
 
#Bar plots of categorical variables
  p1 <- ggplot(df, aes(x=MeterBypass)) + ggtitle("Meter") + xlab("Meter") +
    geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) +
    ylab("Percentage") + coord_flip() + theme_minimal()
  p2 <- ggplot(df, aes(x=FeederBypass)) + ggtitle("Feeder") +
    xlab("Feeder") + geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
  p3 <- ggplot(df, aes(x=TariffBypass)) + ggtitle("Tariff") + xlab("Tariff") + 
    geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
  p4 <- ggplot(df, aes(x=Status)) + ggtitle("Status") + xlab("status") +
    geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
  p5 <- ggplot(df, aes(x=group_cons)) + ggtitle("Consumption") + xlab("Consumption") +
    geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
  grid.arrange(p1, p2, p3, p4,p5, ncol=2)
  
glimpse(df)
#...........................................................................  

ml <- df
#ml$Customer.AC.No <- NULL
#ml$Customer.tariff.as.per.bill <- NULL
#ml$Feeder <- NULL
#ml$Meter.No. <- NULL
#ml$Meter.Make <- NULL
#ml$Date.of.visit <- NULL
#ml$Customer.Name <- NULL
#ml$Customer.address <- NULL
#ml$Customer.tariff.as.per.bill <- NULL

glimpse(ml)

#head(training)
#Status
#Average.Consumption
#Availability
#Estimated.Connected.Load
#group_cons
#MeterBypass
#FeederBypass
#TariffBypass
# Feature selection with Chi-square
  chi.square <- vector()
  p.value <- vector()
  cateVar <- ml %>% 
    dplyr::select(Status) %>% 
    keep(is.factor)
  
  for (i in 1:length(ml)) {
    p.value[i] <- chisq.test(ml$Status, unname(unlist(ml[i])), correct = FALSE)[3]$p.value
    chi.square[i] <- unname(chisq.test(ml$Status, unname(unlist(ml[i])), correct = FALSE)[1]$statistic)
  }
  
  chi_sqaure_test <- tibble(variable = names(ml)) %>% 
  add_column(chi.square = chi.square) %>% 
  add_column(p.value = p.value)
  knitr::kable(chi_sqaure_test)
  glimpse(ml)
  #rm(list = ls())
  
  as.factor(ml$group_cons)
  as.factor(ml$FeederBypass)
  as.factor(ml$MeterBypass)
  #memory.limit(size= 56000)
  #memory.limit()
#The Chi-square p-values are less than 0.05,
#so it confirms our hypothesis that these features will be able to provide 
#useful information on the reponse (target) variable.
#...........................................................................
#...........................................................................
#...........................................................................
 # data.frame(colnames(df))
  #BUILDING THE MACHINE LEARNING MODEL
  intrain<- createDataPartition(ml$Status,p=0.75,list=FALSE)
  set.seed(2017)
  training<- ml[intrain,]
  testing<- ml[-intrain,]
  
#.....................................................

  ml <- ml %>%
    select(id)


#Confirm the splitting is correct:
  dim(training); dim(testing)
#Fitting the Logistic Regression Model:
  LogModel <- glm(Status ~ .,data=training,family=binomial, maxit=100)
  print(summary(LogModel))
head(training)

#saveRDS(LogModel, "logmodel.rds")
#..............................................Adding Acc No back.....................

#Feature Analysis:
  anova(LogModel, test="Chisq")
  head(testing)
#Assessing the predictive ability of the Logistic Regression model
  testing$Status <- as.factor(testing$Status)
  #testing$Status[testing$Status=="0"] <- "0"
  #testing$Status[testing$Status=="1"] <- "1"
  
  fitted.results <- predict(LogModel,newdata=testing,type='response')
  fitted.results <- ifelse(fitted.results > 0.5,1,0)
  misClasificError <- mean(fitted.results != testing$Status)
  print(paste('Logistic Regression Accuracy',1-misClasificError))
  class(testing$Average.Consumption)
#Logistic Regression Confusion Matrix........................
  head(fitted.results,10)
  #print("Confusion Matrix for Logistic Regression");
  #table(testing$Status, fitted.results > 0.5)
  head(testing)
  #bypass.probs <- predict(LogModel, newdata=testing, type="response")
  #head(bypass.probs)
  
#...................................................................................
  ## Predict the outcomes against our test data
  #logit.pred.prob <- predict(LogModel, data =testing[-c(1)], type = 'response')
  #logit.pred <- as.factor(ifelse(logit.pred.prob > 0.5, 1, 0))
  
  #head(testing, 10)
  #head(logit.pred.prob,10)
  
#Logistic Regression Confusion Matrix
  print("Confusion Matrix for Logistic Regression") 
  table(testing$Status, fitted.results > 0.5)
  

#Odds Ratio
  library(MASS)
  exp(cbind(OR=coef(LogModel), confint(LogModel)))
#.....................................................................................
#.....................................................................................
  
#Decision Tree
  tree <- ctree(Status~MeterBypass+group_cons+FeederBypass, training)
  plot(tree)
#Decision Tree Confusion Matrix
pred_tree <- predict(tree, testing)
print("Confusion Matrix for Decision Tree")
table(Predicted = pred_tree, Actual = testing$Status)

#Decision Tree Acuracy
p1 <- predict(tree, training)
tab1 <- table(Predicted = p1, Actual = training$Status)
tab2 <- table(Predicted = pred_tree, Actual = testing$Status)
print(paste('Decision Tree Accuracy',sum(diag(tab2))/sum(tab2)))
#.................................................................................
#.................................................................................
#Random Forest
  #df$Status <- as.factor(df$Status)
  ## Create a control object.
  ctrl <- trainControl(method = "cv",
                       number = 10,
                       selectionFunction = "best")
  
  ## Create a grid search based on the available parameters.
  grid <- expand.grid(.mtry = c(1:8))
  
  ## Build the random forest model
  rf.mod <- 
    train(ml$Status ~.,
          data = training,
          method = 'rf',
          metric = 'Kappa',
          trControl = ctrl,
          tuneGrid = grid)
  
  rf.mod
as.factor(ml$Status)
  #Kappa was used to select the optimal model using the largest value.
  #The final value used for the model was mtry = 6.
  ## Make the predictions
  rf.pred <- predict(rf.mod, data=testing[-c(1)], type = "raw")
  rf.pred.prob <- predict(rf.mod, data=testing[-c(1)], type = "prob")
  
  
#Random Forest Prediction and Confusion Matrix
  pred_rf <- predict(rfModel, testing)
  caret::confusionMatrix(pred_rf, testing$Status)

#RandomForest Error rate
  plot(rfModel)

#Tune Random Forest Model
  
  t <- tuneRF(training[, -18], training[, 18], stepFactor = 0.5,
        plot = TRUE, ntreeTry = 200, trace = TRUE, improve = 0.05)
  
#Fit the Random Forest Model After Tuning
  rfModel_new <- randomForest(Status ~., data = training, ntree = 200,
  mtry = 2, importance = TRUE, proximity = TRUE)
  print(rfModel_new)
  
#Random Forest Predictions and Confusion Matrix After Tuning
  pred_rf_new <- predict(rfModel_new, testing)
  caret::confusionMatrix(pred_rf_new, testing$Status)
  
#Random Forest Feature Importance
  varImpPlot(rfModel_new, sort=T, n.var = 10, main = 'Top 10 Feature Importance')
#..........................................................................
#..........................................................................

