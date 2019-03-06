rm(list = ls())
setwd("F:/DATASCIENCE/EDWISOR/Project/EMPLOYEE ABSENTEEISM")

#load the data
install.packages("readxl")
library(readxl)
data1 = read_xls("work.xls", col_names   = TRUE,guess_max = 30, .name_repair = "unique")
ds = data1

#Converting tibble to dataframe
data1 = as.data.frame(data1)

#Some info of the data
str(data1)
summary(data1)
dim(data1)


################### MISSING VALUE ANALYSIS ##################################
sapply(data1, function(x) sum(is.na(x)))

########## IMPUTING USING KNN ###############

library(DMwR)
data1 = knnImputation(data1, k=3)


######### converting data types
data1$ID = as.factor(data1$ID)
data1$`Reason for absence` = as.factor(data1$`Reason for absence`)
data1$`Month of absence`=as.factor(data1$`Month of absence`)
data1$`Day of the week` = as.factor(data1$`Day of the week`)
data1$Seasons = as.factor(data1$Seasons)
data1$`Disciplinary failure` = as.factor(data1$`Disciplinary failure`)
data1$Education = as.factor(data1$Education)
data1$Son = as.factor(data1$Son)
data1$`Social drinker` = as.factor(data1$`Social drinker`)
data1$`Social smoker` = as.factor(data1$`Social smoker`)
data1$Pet = as.factor(data1$Pet)
str(data1)

############## RENAMING COLUMN NAMES ###############

names(data1) = c("ID","Reason.for.absence","Month.of.absence","Day.of.the.week","Seasons","Transportation.expense",
                 "Distance.from.residence.to.work","Service.time","Age","Workload.average.per.day","Hit.target",
                 "Disciplinary.failure","Education","Son", "Social.drinker","Social.smoker","Pet","Weight","Height",
                 "Body.mass.index","Absenteeism.time.in.hours")

################# OUTLIER ANALYSIS ###############

library(ggplot2)
#Boxplotting
numeric_index = sapply(data1, is.numeric)
numeric_data = data1[,numeric_index]
cnames = colnames(numeric_data)


for (i in 1:length(cnames)){
  
  assign(paste0("gn", i), ggplot(aes_string(y = cnames[i], x = "Absenteeism.time.in.hours"), data = subset(data1)) +
           
           stat_boxplot(geom = "errorbar", width = 0.5) + 
           geom_boxplot(outlier.colour = "red", fill = "grey", outlier.shape = 18, outlier.size = 1, notch = FALSE) + 
           theme(legend.position = "bottom") + labs(y = cnames[i], x="abs hours") + ggtitle(paste("Boxplot of abs hours for", cnames[i])))
}


#plotting plots together
gridExtra::grid.arrange(gn2, gn3, ncol = 2)
gridExtra::grid.arrange(gn4, gn5, ncol = 2)
gridExtra::grid.arrange(gn6, gn7, ncol = 2)
gridExtra::grid.arrange(gn8, gn9, ncol = 2)


##replace outliers with NA and impute
for(i in cnames) {
  print(i)
  val = data1[,i][data1[,i] %in% boxplot.stats(data1[,i])$out]
  
  print(length(val))
  data1[,i][data1[,i] %in% val] = NA
}

data1 = knnImputation(data1, k=3)
sapply(data1, function(x) sum(is.na(x)))


dim(data1)
############################ VISVUALIZATION ########################
library(ggplot2)
library(scales)
library(psych)


#ReasonsCount
ggplot(data1 , aes_string(x=data1$Reason.for.absence)) + geom_bar(stat = "count", fill = "DarkslateBlue") + 
  xlab("Reason") + ylab("Count") + ggtitle("Reason distribution") + theme(text = element_text(size = 15))
#non ICD reasons have higher count

#PetCount
ggplot(data1 , aes_string(x=data1$Pet)) + geom_bar(stat = "count", fill = "DarkslateBlue") + 
  xlab("No Of Pets") + ylab("Count") + ggtitle("Pet distribution")

plot(Absenteeism.time.in.hours ~ Pet , data = data1)
#people with atleast one pet show less absentism in hours

#transportation expenses
ggplot(data1 , aes_string(x=data1$Transportation.expense)) + geom_bar(stat = "count", fill = "DarkslateBlue") + 
  xlab("Transportation expense") + ylab("Count") + ggtitle("Transportation expanse distribution")

plot(Absenteeism.time.in.hours ~ Transportation.expense, data = data1)

#Drinker
ggplot(data1 , aes_string(x=data1$Social.drinker)) + geom_bar(stat = "count", fill = "DarkslateBlue") + 
  xlab("Drinker") + ylab("Count") + ggtitle("Drinker distribution")

plot(Absenteeism.time.in.hours ~ Social.drinker , data = data1)
#People who are social drinkers tend to be more absent

#Season
ggplot(data1 , aes_string(x=data1$Seasons)) + geom_bar(stat = "count", fill = "DarkslateBlue") + 
  xlab("Seasons") + ylab("Count") + ggtitle("Season distribution")

#Month of absence
ggplot(data1 , aes_string(x= data1$Month.of.absence)) + geom_bar(stat = "count", fill = "DarkslateBlue") + 
  xlab("Month") + ylab("Count") + ggtitle("Month distribution")
#month 3rd have higher absentism

#dayOfWeek
ggplot(data1 , aes_string(x=data1$Day.of.the.week)) + geom_bar(stat = "count", fill = "DarkslateBlue") + 
  xlab("DayOfWeek") + ylab("Count") + ggtitle("Day distribution")

plot(Absenteeism.time.in.hours ~ Day.of.the.week , data = data1)
#people tend to be least absent on thursdays.

#disciplinary failure
ggplot(data1 , aes_string(x=data1$Disciplinary.failure)) + geom_bar(stat = "count", fill = "DarkslateBlue") + 
  xlab("DayOfWeek") + ylab("Count") + ggtitle("Season distribution")

plot(Absenteeism.time.in.hours ~ Disciplinary.failure , data = data1)
#higher number of people do not have disciplinary failure




#-------------------------------------FEATURE SELECTION--------------------------------

#correlation plot
library(corrgram)

round(cor(numeric_data),2)

corrgram(data1[, numeric_index], order = F, upper.panel = panel.pie, text.panel = panel.txt, main = "correlation plot")

data1_new = subset(data1, select=-c( Weight)) 
#removing weight


################ Anova test ############

#season of absence
AnovaModel_season =(lm(Absenteeism.time.in.hours ~ Seasons, data = data1))
summary(AnovaModel_season) 
#remove

#Reason of absence
AnovaModel_reason=(lm(Absenteeism.time.in.hours ~ Reason.for.absence, data = data1))
summary(AnovaModel_reason) #keep

#month of absence
AnovaModel_month=(lm(Absenteeism.time.in.hours ~ Month.of.absence, data = data1))
summary(AnovaModel_month) #keep

#day
AnovaModel_day=(lm(Absenteeism.time.in.hours ~ Day.of.the.week, data = data1))
summary(AnovaModel_day) #remove

#Disciplinary failure
AnovaModel_disciplinary=(lm(Absenteeism.time.in.hours ~ Disciplinary.failure, data = data1))
summary(AnovaModel_disciplinary) #remove

#Education
AnovaModel_education=(lm(Absenteeism.time.in.hours ~ Education, data = data1))
summary(AnovaModel_education) #remove

#drinker
AnovaModel_drinker=(lm(Absenteeism.time.in.hours ~ Social.drinker, data = data1))
summary(AnovaModel_drinker) #keep

#smoker
AnovaModel_smoker=(lm(Absenteeism.time.in.hours ~ Social.smoker, data = data1))
summary(AnovaModel_smoker) #keep

data1_new = subset(data1_new, select=-c(ID, Seasons, Day.of.the.week, Education, Disciplinary.failure))


#-------------FEATURE SCALING---------------------

#normality check
hist(data1_new$Transportation.expense)
hist(data1_new$Service.time)
hist(data1_new$Age)
hist(data1_new$Workload.average.per.day)
hist(data1_new$Body.mass.index)
hist(data1_new$Absenteeism.time.in.hours)
#since data is not normally distributed of any column we will use normalization

numeric_index = sapply(data1_new,is.numeric) #selecting only numeric

numeric_data = data1_new[,numeric_index]

cnames = colnames(numeric_data)

for (i in cnames){
  print(i)
  data1_new[,i] = (data1_new[,i]-min(data1_new[,i]))/ (max(data1_new[,i])-min(data1_new[,i]))
}

#-------------MODELING----------------------------

#sampling
train_index = sample(1:nrow(data1_new), 0.8*nrow(data1_new))
data1_train = data1_new[train_index,] 
data1_test = data1_new[-train_index,]

#LINEAR REGRESSION
library(rpart)
library(MASS)

#check multicollinearity
install.packages("usdm")
library(usdm)

vif(numeric_data[,-13])
vifcor(numeric_data[,-13], th = 0.9) 
#no variable from the 12 input variables has collinearity problem.

#creating dummies
install.packages("dummies")
library(dummies)

#?dummy.data.frame()
df_new = dummy.data.frame(data1_new, sep = '.')
dim(df_new)
colnames(df_new)

#sampling 
train_index = sample(1:nrow(df_new), 0.8*nrow(df_new))
df_train = df_new[train_index,] 
df_test = df_new[-train_index,]

#run regression model
lm_model11 = lm(Absenteeism.time.in.hours~. , data = df_train)
summary(lm_model11)
#R square= 0.49
#Adjusted R square = 0.44


#predict
predictions_LR = predict(lm_model11, df_test[,-69])

# ERROR METRICS

#library(DMwR)
regr.eval(df_test[-69], predictions_LR, stats = c('mse','rmse','mape','mae'))
#rmse = 4.44
#mse = 19.7


########################## Random Forest ###############3


library(randomForest)
library(inTrees)
fit  = randomForest(Absenteeism.time.in.hours~. , data1_train, importance = TRUE, ntree = 350)
fit
treeList = RF2List(RF_model)
#error plotting
plot(RF_model)


#predict test data using RF model
RF_predict = predict(RF_model, data1_test[,-15])

#evaluate performance
postResample(RF_predict, data1_test$Absenteeism.time.in.hours)

#rmse = 0.17
#rsquare = 0.28
#mae = 0.12 



#-----------------------------MONTHLY LOSS

new = subset(data1, select = c(Month.of.absence, Service.time, Absenteeism.time.in.hours, Workload.average.per.day ))



new["loss"]=with(new,((new[,4]*new[,3])/new[,2]))

for(i in 1:12)
{
  d1=new[which(new["Month.of.absence"]==i),]
  cat("\n month:",i, sum(d1$loss))
  
}





