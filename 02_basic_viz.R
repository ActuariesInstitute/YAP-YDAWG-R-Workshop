#######################################################################################################################
#This file demonstrates some basic visualisations on a dataset of interest. 
#Includes both distributions of data and model diagnostics
#######################################################################################################################

#Config

#######################################################################################################################

setwd("D:/Dropbox/yap_working")
reqPackages <- c('randomForest', 'DALEX', 'xgboost', 'ggplot2', 'caret', 'reshape2')
lapply(reqPackages, require, character.only = TRUE)


#######################################################################################################################

#Dataprep

#######################################################################################################################

#We will create a claim severity feature which can be used for the first workshop. 
#Noting that there is dependence across some records (e.g. multiple fatalities in the same crash), 
#the claim severity is be specific to the fatality. 
#This is for internal use only - not intended to be shared with workshop participants.

#Load Road Fatality data
deaths <- read.csv("bitre_ardd_fatalities_dec_2018.csv")
str(deaths)
head(deaths)


#Generate "model" loadings which can be combined to generate the linear predictor
#Loadings for a handful of categorical variables
state_LP <- ifelse(deaths$State == "NSW" ,0.1,
            ifelse(deaths$State == "VIC", -0.08, 
            ifelse(deaths$State == "QLD", 0.05, 0.03))) 

holiday_LP <- ifelse(deaths$Christmas_Period == "Yes" | deaths$Easter_Period == "Yes", 0.5, 0)

hour <- substr(deaths$Time, 1, 2)
time_LP <- ifelse(hour >= 14 & hour <= 17, 0.2, 0)

weekend_LP <- ifelse(deaths$Dayweek%in%c("Saturday", "Sunday"), 0.02, 0)

crash_LP <- ifelse(deaths$Crash.Type == "Pedestrian" ,0.5,
            ifelse(deaths$Crash.Type%in%c("Multiple","Multiple vehicle"), 0.3, 0)) 

#Continuous feature: a somewhat-complex piecewise polynomial spline 
age_0_10_LP <- sapply(deaths$Age, function(y) min(max(y,0),10))
age_10_18_LP <- sapply(deaths$Age, function(y) min(max(y,10),18))-10
age_18_30_LP <- sapply(deaths$Age, function(y) min(max(y,18),30))-18
age_30_50_LP <- sapply(deaths$Age, function(y) min(max(y,30),50))-30
age_50_80_LP <- sapply(deaths$Age, function(y) min(max(y,50),80))-50
age_80_100_LP <- sapply(deaths$Age, function(y) min(max(y,80),100))-80

age_LP <- 0.02*age_0_10_LP + 0.01*age_10_18_LP**2 -0.05*age_18_30_LP +  
              0.005*age_30_50_LP + 0.01*age_50_80_LP + 0.03*age_80_100_LP

#Example interaction term
crash_fac <- ifelse(deaths$Crash.Type == "Pedestrian" ,1.1,
                   ifelse(deaths$Crash.Type%in%c("Multiple","Multiple vehicle"), 1.5, 0)) 
roaduser_fac <- ifelse(deaths$Road.User == "Driver" ,0.2,
                    ifelse(deaths$Road.User%in%c("Motorcycle rider","Motorcycle pillion", "Motorcycle pillion passenger"), 0.5, 0)) 
crash_road_LP <- crash_fac * roaduser_fac

noise_LP <- runif(nrow(deaths), min=-1, max=1)

#Apply exponential transformation and scale up to obtain claim severity
claim_severity <- 400000*exp(state_LP + holiday_LP + time_LP + weekend_LP + crash_LP + age_LP + crash_road_LP + noise_LP)

summary(claim_severity)

deaths$claim_severity <- claim_severity

#Quick check vs Excel dummy working
#claimfacs <- cbind(state_LP, holiday_LP, time_LP, weekend_LP, crash_LP, age_LP, crash_road_LP, claim_severity)
#write.csv(claimfacs, "claims.csv")

#Output new column to save the need to recalculate in future. 
#write.csv(deaths, "claims_mod.csv")


#######################################################################################################################

#Basic Visualisation

#######################################################################################################################

#deaths <- read.csv("claims_mod.csv")


#2 basic "base R" functions to begin
str(deaths)

#Basic visualisation of continuous variables using the base R histogram function 
?hist
summary(deaths$claim_severity)
hist(deaths$claim_severity)
hist(deaths$claim_severity, breaks = c(seq(0, 3000000, 250000), 5000000))

summary(deaths$Age)
table(deaths$Age)
hist(deaths$Age)
hist(deaths$Age, breaks = c(seq(-10, 110, 10)))

#Basic visualisation of categorical variables using the base R barplot function
?barplot

table(deaths$Speed.Limit)
barplot(table(deaths$Speed.Limit))

summary(deaths$Month)
barplot(table(deaths$Month))
#The default ordering is in alphabetical order. Let's apply a quick fix: 
deaths$Month = factor(deaths$Month, levels = month.name)
barplot(table(deaths$Month))


#2-d plot 
plot(deaths$Speed.Limit, deaths$Age)
plot(deaths$Age, deaths$claim_severity)

#Exercise 
# Plot histograms of variables National_Road_Type, X and Y. 
# What do you observe? 


#################ggplot2
#repeat similar visualisations but use ggplot2 instead
#plot average claim_severity with different variables along x-axis
#show how to build up graphs

#################dalex
#Let's build a couple of quick models

deaths <- read.csv("claims_mod.csv")
set.seed(960)

splitSample <- sample(1:3, size=nrow(deaths), prob=c(0.5,0.3,0.2), replace = TRUE)
#(m)odel, (t)est, (h)oldout also known as train, validation, test depending on convention
deaths.m <- deaths[splitSample==1, ]
deaths.t <- deaths[splitSample==2, ]
deaths.h <- deaths[splitSample==3, ]

#Produce two quick and dirty models
glm.model <- glm(claim_severity ~ State + Dayweek + Crash.Type + Gender + Age 
                , data=deaths.m, family=Gamma(link=log))
summary(glm.model)

rf.model <- randomForest(claim_severity ~ State + Dayweek + Crash.Type + Gender + Age   
                , data = deaths.m, ntree = 50)
summary(rf.model)


#Let's use DALEX to view some of the typical model diagnostics and explainers
glmlink <- function(model, newdata)  {
  preds <- predict(model, newdata, type = "response")
  return(preds)
}


glm_expl <- explain(model = glm.model, data = deaths.m, y = deaths.m$claim_severity, predict_fun=glmlink )
rf_expl <- explain(model = rf.model, data = deaths.m, y = deaths.m$claim_severity)

#(1): Variable importance for the two models
glm_vi <- variable_importance(glm_expl)
rf_vi <- variable_importance(rf_expl)

plot(glm_vi, rf_vi)
#randomForest probably does a better job of capturing the non-linearity in the age curve. Let's verify this!

#(2): partial dependence plots to understand 'curves' fitted by the models
glm_pd_age <- variable_response(glm_expl, "Age", "pdp")
rf_pd_age <- variable_response(rf_expl, "Age", "pdp")
plot(glm_pd_age, rf_pd_age)

#(3): Let's look at the predictions for the first row
row1 <- deaths[1, ]
#glm_pred1 <- prediction_breakdown(glm_expl, row1)

rf_pred1 <- prediction_breakdown(rf_expl, row1)
plot(rf_pred1)
###need to debug the GLM version

#(4): Let's check how the model fits on average across key variables
deaths.m$glm.pred <- predict(glm.model, type="response")
deaths.m$rf.pred <- predict(rf.model)

deaths.t$glm.pred <- predict(glm.model, newdata = deaths.t,type="response")
deaths.t$rf.pred <- predict(rf.model, newdata = deaths.t)


#Let's look at age
######should also add a count so we have a view of exposure... 
age_avg <- aggregate(deaths.m[, c("claim_severity", "glm.pred", "rf.pred")], list(Age = deaths.m$Age), mean)
age_avg_long <- melt(age_avg, id="Age")  # convert to long format

ggplot(data=age_avg_long,
       aes(x=Age, y=value, colour=variable)) +
  geom_line()

#what about crash.type? 

crash_avg <- aggregate(deaths.m[, c("claim_severity", "glm.pred", "rf.pred")], list(Crash.Type = deaths.m$Crash.Type), mean)
crash_avg_long <- melt(crash_avg, id="Crash.Type")  # convert to long format

ggplot(data=crash_avg_long,
       aes(x=Crash.Type, y=value, colour=variable)) +
  geom_line(aes(group=variable))

#how does it look on test data? 
crash_avg.t <- aggregate(deaths.t[, c("claim_severity", "glm.pred", "rf.pred")], list(Crash.Type = deaths.t$Crash.Type), mean)
crash_avg_long.t <- melt(crash_avg.t, id="Crash.Type")  # convert to long format

ggplot(data=crash_avg_long.t,
       aes(x=Crash.Type, y=value, colour=variable)) +
  geom_line(aes(group=variable))

