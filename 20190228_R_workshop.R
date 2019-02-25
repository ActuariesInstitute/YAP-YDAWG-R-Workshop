#######################################################################################################################
#This file is intended to be shared after the R workshop on Feb 28 2019.
#######################################################################################################################

#Config

#######################################################################################################################

# set working directory if needed
# setwd("D:/Dropbox/yap_working")
reqPackages <- c('randomForest', 'DALEX', 'ggplot2', 'caret', 'reshape2')
lapply(reqPackages, require, character.only = TRUE)


#######################################################################################################################

#Target Variable Generation - Purely for the sake of the workshop!

#######################################################################################################################

#We will create a claim severity feature which can be used as a sample target feature during the workshop on Feb 28.
#Noting that there is dependence across some records (e.g. multiple fatalities in the same crash),
#the claim severity is be specific to the fatality.

#Load Road Fatality data
deaths <- read.csv("bitre_ardd_fatalities_dec_2018.csv")
str(deaths)
head(deaths)

set.seed(100)

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
deaths$claim_severity <- 400000*exp(state_LP + holiday_LP + time_LP + weekend_LP + crash_LP + age_LP + crash_road_LP + noise_LP)

#Output new column to save the need to recalculate in future.
#write.csv(deaths, "claim_data.csv")

#######################################################################################################################

# Data Manipulation and Merging

#######################################################################################################################

#Load Road Fatality data
deaths_raw <- read.csv("claim_data.csv")
str(deaths_raw)
head(deaths_raw)

# We'll work with a copy of the data, incase we want to revert any of our changes later
deaths <- deaths_raw


## 1 Some basic data manipulation ---

# 1.1 filtering by rows ----

# View rows 1 - 10
deaths[1:10, ]

# Subsetting to crashes on Fridays which also involved a bus
# Note that the 'equality' operator is a double equals sign
friday_bus_deaths <- deaths[deaths$Dayweek == 'Friday' & deaths$Bus..Involvement == "Yes", ]
table(friday_bus_deaths$Dayweek,friday_bus_deaths$Bus..Involvement)

# subsetting to crashes on Fridays OR crashes involving a bus
friday_or_bus_deaths <- deaths[deaths$Dayweek == 'Friday' | deaths$Bus..Involvement == "Yes", ]
table(friday_or_bus_deaths$Dayweek,friday_or_bus_deaths$Bus..Involvement)

# You can see other operators here: https://www.statmethods.net/management/operators.html

# 1.2 Selecting rows and columns ----

# View the first 5 rows of the friday_bus_deaths to see what time of day they occurred
friday_bus_deaths[1:5, ]

# View the first 5 columns  of the friday_bus_deaths
friday_bus_deaths[,1:5]

# View the first 5 rows and first 5 columns  of the friday_bus_deaths
friday_bus_deaths[1:5,1:5]

# View columns by name
friday_bus_deaths[1:5, "State"]

# View a few columns
friday_bus_deaths[1:5, c("State", "Crash.Type")]

# Dynamic referencing
column <- "State"
friday_bus_deaths[[column]]

# Dynamic referencing - multiple columns
columns <- c("State", "Crash.Type")
friday_bus_deaths[1:5, columns]


# 1.3 Creating new variables ----

# Create a new variable Speed.Limit.MPH such that Speed.Limit.MPH = Speed.Limit * 1.6
deaths$Speed.Limit.MPH <- deaths$Speed.Limit * 1.6

# Now update Speed.Limit.MPH by adding 10 to it
deaths$Speed.Limit.MPH <- deaths$Speed.Limit.MPH + 10

# Let's have a look at the distribution of Age.
summary(deaths$Age)
table(deaths$Age)

# There are a few crashes where Age = -9 which is odd. Create Age.New which is same as Age but -9 replaced with  NA.
# We ifelse() for this. This is a vectorised function, meaning it very efficiently iterates over all the rows in the data.
deaths$Age.New <- ifelse(deaths$Age == -9, NA, deaths$Age)
summary(deaths$Age.New)


# 2 Summarising data  ----

#Calculate the average of Age.NEW by state
#We'll use the aggregate() function to calculate the conditional means

#?aggregate
state_age_summ <- aggregate(Age.New ~ State, data = deaths, FUN = mean)
state_age_summ

#We'll rename so we know it's actually the mean
names(state_age_summ)[2] <- "Mean.Age.New"
state_age_summ

# 3 Joining tables ----

# 3.1 Row-wise appending ----

# We can append tables by row or by column
# We'll use some toy examples here

# First, row-wise. For this, the datasets should have the same columns
toy_data1 <- data.frame(x = 1:10, y = rnorm(10))
toy_data2 <- data.frame(x = 11:20, y = rnorm(10))

# We use rbind() to bind by rows
toy_row_full <- rbind(toy_data1, toy_data2)

toy_data1
toy_data2
toy_row_full


# 3.2 Column-wise appending ----

# For column-wise binding, we want data with the same number of rows but with some new columns
# We can rename the columns in toy_data2
names(toy_data2) <- c("x2", "y2")

toy_data_cfull <- cbind(toy_data1, toy_data2)
toy_data_cfull


# 3.3 Merging ----

# We can be more sophisticated in the way we join datasets together
# by using the merge() function
# This allows us to merge tables by matching the values of one or multiple variables

# Let's merge the mean age by state, stored in the state_age_summ table, onto the deaths dataset
# We'll join the values by state
# ?merge
deaths_2 <- merge(deaths, state_age_summ, all.x = T, by = "State")    # all.x = TRUE performs a 'left' join
head(deaths_2[, c("State", "Age.New", "Mean.Age.New")])

# 4 Factor variables ----

# Non-numeric variables in R are typically stored as factors for modelling purposes
# Factor variables are actually a numeric vector with a 'levels' attribute which maps the numeric values back to the
# readable representation

# Each Factor variable has different levels which are the unique values it can take. Inspect the levels of Gender
levels(deaths_2$Gender)

# The mapping of values to levels can be modified. This allows us to change the way values are reprsented, without
# actually changing the underlying data, meaning we can recover the original mappings later if we wish.
# Combine the -9 and "Unspecified" into the Unknown Level
levels(deaths_2$Gender) <- list(Male = "Male", Female = "Female", Unknown = c("-9", "Unspecified"))
levels(deaths_2$Gender)

# We can tidy up some of the other factors too...
str(deaths_2)
levels(deaths_2$Crash.Type)[levels(deaths_2$Crash.Type) == "Multiple"] <- "Multiple vehicle"
levels(deaths_2$Bus..Involvement)[levels(deaths_2$Bus..Involvement) == "N"] <- "No"
levels(deaths_2$Rigid.Truck..Involvement)[levels(deaths_2$Rigid.Truck..Involvement) == "N"] <- "No"
levels(deaths_2$Rigid.Truck..Involvement)[levels(deaths_2$Rigid.Truck..Involvement) == "-9"] <- "Unknown"
levels(deaths_2$Articulated.Truck..Involvement.)[levels(deaths_2$Articulated.Truck..Involvement.)== "N"] <- "No"

levels(deaths_2$Road.User)[levels(deaths_2$Road.User) == "Other/unknown"] <- "Other/Unknown"
levels(deaths_2$Road.User)[levels(deaths_2$Road.User) == "Motorcycle pillion passenger"] <- "Motorcycle pillion"

levels(deaths_2$National_Road_Type)[levels(deaths_2$National_Road_Type) == ""] <- "Unknown"
levels(deaths_2$National_Road_Type)[levels(deaths_2$National_Road_Type) == "Undetermined"] <- "Unknown"
levels(deaths_2$National_Road_Type)[levels(deaths_2$National_Road_Type) == "Access road"] <- "Access Road"
levels(deaths_2$National_Road_Type)[levels(deaths_2$National_Road_Type) == "ARTERIAL ROAD"] <- "Arterial Road"
levels(deaths_2$National_Road_Type)[levels(deaths_2$National_Road_Type) == "LOCAL ROAD"] <- "Local Road"
levels(deaths_2$National_Road_Type)[levels(deaths_2$National_Road_Type) == "National Or State Highway"] <- "National or State Highway"
levels(deaths_2$National_Road_Type)[levels(deaths_2$National_Road_Type) == "NATIONAL OR STATE HIGHWAY"] <- "National or State Highway"
levels(deaths_2$National_Road_Type)[levels(deaths_2$National_Road_Type) == "Sub-arterial Road"] <- "Sub-Arterial Road"


# It is often good practive to convert factor variables to numeric data before modelling.
# A simple factor variable is Gender which only has two unique levels - Male and Female, plus some unknowns
# Let's look at the frequency of each level
# ?table
table(deaths_2$Gender)

# This can be represented simply by a new variable 'Female_fl' which is 1 when Gender is Female and
# 1 when Gender is Female. We choose to make Male the base (having value 0) as it is the more common level.
# We don't know how to allocate the unknown level, but it only has 23 entries.
# Given Male is much more common than Female, it is not unreasonable to treat Unknown genders as Male.
# We do not need a seperate Male_fl column because all the information is captured in Female_fl already
# because there are only two levels.
# This is a type of One-hot encoding.

# Create Female_fl
deaths_2$Female_fl <- ifelse(deaths_2$Gender == "Female", 1, 0)
table(deaths_2$Female_fl, deaths_2$Gender, useNA = 'ifany') #Check

# For factor variables such as state with 8 levels, we will need 7 such dummy variables to capture all the information.

deaths_2$ACT_fl <- ifelse(deaths_2$State == "ACT", 1, 0)
deaths_2$NT_fl <- ifelse(deaths_2$State == "NT", 1, 0)
deaths_2$QLD_fl <- ifelse(deaths_2$State == "QLD", 1, 0)
deaths_2$SA_fl <- ifelse(deaths_2$State == "SA", 1, 0)
deaths_2$TAS_fl <- ifelse(deaths_2$State == "TAS", 1, 0)
deaths_2$VIC_fl <- ifelse(deaths_2$State == "VIC", 1, 0)
deaths_2$WA_fl <- ifelse(deaths_2$State == "WA", 1, 0)

# The above encoding is a type of contrast
# Contrasts are a way of encoding categorical information to allow comparisons between groups, such as is done
# in a typical ANOVA model

# All factor variables are automatically assigned a contrast attribute when created
# We can view and modify the contrast of a variable using the contrasts() function
# The default contrasts is a treatment contrasts, which is the same as the one hot encoding we have done above, where
# one levels is left off.
# ?contrasts
contrasts(deaths_2$State)

# The matrix shown is the contrast matrix.
# The columns show the new variables which will be created -- i.e. we will get a new variable for NSW, NT, QLD etc.
# But NOT for ACT
# The rows show the factor levels -- so when State = NSW, the NSW contrast column will equal 1, and 0 any other time
# ACT has been adopted as the reference or base level, as the row is all zeroes.


# Specifying these constrasts manually can be very tedious and error prone when there are many levels.
# Fortunately, we can use R's internal contrast attributes and the model.matrix() function to do this for us.
# ?model.matrix

OHE_State <- model.matrix(~State, data = deaths_2)

head(cbind(deaths_2$State, OHE_State))
tail(cbind(deaths_2$State, OHE_State))


# One can very easily change the type of contrast too.
options("contrasts")
OHE_State <- model.matrix(~ State, data = deaths_2, contrasts = list(State = "contr.helmert"))

head(OHE_State)
tail(OHE_State)

# It is sometimes useful to 'band' your continuous variables, both for modelling and visualisation.
# More on this later!

deaths_2$Age_Band <- cut(deaths_2$Age, breaks = c(-10, seq(-5, 100, by = 5), 105))
table(deaths_2$Age_Band, deaths_2$Age)


# 5 Linear splines ----

# When modelling one might often wish to piecewise splines.
# Create a spline for Age that runs continuous from 0 to 40 and then remains flat

# To do this we use the pmin() and pmax() functions.
#These are vectorised versions of min() and max() so are very efficient at iterating over the observations
deaths_2$Age_0_40 <- pmin(pmax(deaths_2$Age, 0), 40)
plot(deaths_2$Age, deaths_2$Age_0_40)

deaths_2$Age_40_60 <- pmin(pmax(deaths_2$Age, 40), 60) - 40
plot(deaths_2$Age, deaths_2$Age_40_60)

# What would happen if we just used min() and max()?



#######################################################################################################################

#Basic Visualisation

#######################################################################################################################

# We're comfortable with the updates that have been made so far, so let's roll back to the previous dataset name
# and clear the old file to save memory
deaths <- deaths_2
rm(deaths_2)

#Basic visualisation of continuous variables using the base R histogram function
?hist
summary(deaths$claim_severity)
hist(deaths$claim_severity)
hist(deaths$claim_severity, breaks = c(seq(0, 10000000, 250000)))

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
plot(deaths$Age, deaths$claim_severity)
#This scatterplot provides a view of the spread of claim_severity at each age
#(although it can be hard to tell the density!)
#Let's look at the average claim_severity by age
age_means <- aggregate(claim_severity ~ Age,  data = deaths, FUN = mean)
plot(age_means$Age, age_means$claim_severity)


#######################################################################################################################

#Visualisation using ggplot2

#######################################################################################################################

#Let's repeat the claim_severity histogram example from earlier using ggplot2
ggplot(data=deaths, aes(deaths$claim_severity)) + geom_histogram()

#With ggplot2 you gradually build up the components of the graph. Let's customise the bin width
ggplot(data=deaths, aes(deaths$claim_severity)) + geom_histogram(binwidth = 250000)
ggplot(data=deaths, aes(deaths$claim_severity)) + geom_histogram(breaks = c(seq(0, 10000000, 250000)))
#why are these two different? (think about the summary stats)
#how might you deal with outliers?

#It's a bit hard to see the bars, so let's add some colours
ggplot(data=deaths, aes(deaths$claim_severity)) +
  geom_histogram(breaks = c(seq(0, 10000000, 250000)), col="white", fill = "black")

#Now let's give the graph a title and customise the axis labels
ggplot(data=deaths, aes(deaths$claim_severity)) +
  geom_histogram(breaks = c(seq(0, 10000000, 250000)), col="white", fill = "black")+
  labs(title="Claim Severity Distribution", x="Claim Severity", y="Count")

#By default the graph title is left-justified. If you want it centred:
ggplot(data=deaths, aes(deaths$claim_severity)) +
  geom_histogram(breaks = c(seq(0, 10000000, 250000)), col="white", fill = "black")+
  labs(title="Claim Severity Distribution", x="Claim Severity", y="Count")+
  theme(plot.title = element_text(hjust = 0.5))

#You can also customise the dimensions of the graph
ggplot(data=deaths, aes(deaths$claim_severity)) +
  geom_histogram(breaks = c(seq(0, 10000000, 250000)), col="white", fill = "black")+
  labs(title="Claim Severity Distribution", x="Claim Severity", y="Count")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlim(c(0, 10000000)) + ylim(c(0, 10000))

#There are lots of other ways to customise your graphs!
#Take a look at http://www.cookbook-r.com/Graphs/

#Here's the ggplot2 version of the Speed Limit bar graph
ggplot(data=deaths, aes(deaths$Speed.Limit)) + geom_bar()
#how is this different from the base-R version? how might you resolve this?

#Similarly, the ggplot2 version of the month graph
ggplot(data=deaths, aes(deaths$Month)) + geom_bar() + scale_x_discrete(limits = month.name)

#Finally, showing how you might plot the average claim severity by age
ggplot(data=deaths, aes(x=factor(Age), y=claim_severity)) + stat_summary(fun.y="mean", geom="bar")
#It's impossible to read the axis so let's specify which labels are shown
ggplot(data=deaths, aes(x=Age, y=claim_severity)) + stat_summary(fun.y="mean", geom="bar") +
  scale_x_continuous(breaks=seq(0, 100, 10))

#######################################################################################################################

#Collinearity and missing value checks for ML

#######################################################################################################################

#Check for missing values
#Result: There are blanks in the columns National_Remoteness_Region, SA4_Name_216, LGA_NAME_2017, Christmas_Period and Easter_Period.
#Have omitted these fields from further analysis as there's too many missing values (only 5k records) and there's no easy way to populate these fields.
deaths[deaths==''] <- NA
col.has.na <- apply(deaths, 2, function(x){any(is.na(x))})
col.has.na

#Check for collinearity
mm = model.matrix(claim_severity ~ State + Dayweek + Crash.Type + Gender + Age + Bus..Involvement + Rigid.Truck..Involvement
                  + Articulated.Truck..Involvement. + Speed.Limit + Road.User
                  , data = deaths)

#Collinearity found. Have removed the variables Rigid.Truck..Involvement and Articulated.Truck..Involvement.
combofind = findLinearCombos(mm)
combofind

if (length(combofind$linearCombos) > 0)
{
  cat("Collinearity Found\n")
  cat("Linear Combinations:\n")
  colNames = colnames(mm)
  for (combo in combofind$linearCombos)
  {
    cat(paste0(" ", paste(colNames[combo], collapse = " ")))
    cat("\n")
  }
  if (length(combofind$remove) > 0)
  {
    cat("Suggested Column Removals:\n")
    for (rem in combofind$remove)
      cat(paste0(" ", colNames[rem], "\n"))
  }
}

##Create model matrix with collinear fields removed
mm2 = model.matrix(claim_severity ~ State + Dayweek + Crash.Type + Gender + Age + Bus..Involvement + Speed.Limit + Road.User
                  , data = deaths)
print(str(mm2))

#######################################################################################################################

#K-Means Clustering Example

#######################################################################################################################

## Determine the ideal number of clusters
# 2 is the ideal number of centres for age and Speed.limit
ks <- 1:5
tot_within_ss <- sapply(ks, function(k) {
  cl <- kmeans(deaths$Age, k, nstart = 10)
  cl$tot.withinss
})
plot(ks, tot_within_ss, type = "b")

ks <- 1:5
tot_within_ss2 <- sapply(ks, function(k) {
  cl <- kmeans(deaths$Speed.Limit, k, nstart = 10)
  cl$tot.withinss
})
plot(ks, tot_within_ss2, type = "b")


#Example of kmeans clustering with age
model_cl <- kmeans(deaths$Age, centers = 2, nstart = 25)
model_cl
par(mfrow=c(1,1))
plot(deaths$Age, col = model_cl$cluster)

#Example of kmeans clustering with speed limit
#There are clear outlier values in the dataset

model_cl2 <- kmeans(deaths$Speed.Limit, centers = 2, nstart = 25)
model_cl2
par(mfrow=c(1,1))
plot(deaths$Speed.Limit, col = model_cl2$cluster)

#######################################################################################################################

#Linear Regression and RF examples

#######################################################################################################################

#Let's build a couple of quick models

set.seed(960)
#(m)odel, (t)est, (h)oldout also known as train, validation, test depending on convention

splitSample <- sample(1:3, size=nrow(deaths), prob=c(0.5,0.3,0.2), replace = TRUE)
deaths_m <- deaths[splitSample==1, ]
deaths_t <- deaths[splitSample==2, ]
deaths_h <- deaths[splitSample==3, ]

# When building models, we commonly use the "model" dataset as the input feature to our machine learning algorithm
# and a "test" dataset to ensure that we don't overfit to the "model" data.
# In doing this, there is a risk that as we incrementally make tweaks to the model structure, hyperparameters etc.
# then we will start to overfit to our 'test' data.
# It is convention to also create a third "holdout" dataset as a final check.
# Extending this further you can also add a 4th, 5th, 6th etc. dataset but having 3 datasets
# is a relatively common convention because
# (1) it's not practical to keep applying layers upon layers of checks and
# (2) you throw away a lot of data.



#Produce linear regression and random forest model with variables which are not collinear and don't have missing value issues
lm_model <- lm(claim_severity ~ State + Dayweek + Crash.Type + Gender + Age + Speed.Limit + Road.User + Bus..Involvement, data=deaths_m)

# Summary suggest the most significant variables were state.VIC, crash.type, age and road user
summary(lm_model)

# Residuals vs fitted curve not looking random; QQ-Plot is curve suggesting a non-linear graph would be better;
par(mfrow=c(2,2))
plot(lm_model)

# Try fitting a randomForest model before scrolling down for the answer

rf_model <- NULL  # your code here













































# Answer
rf_model <- randomForest(claim_severity ~ State + Dayweek + Crash.Type + Gender + Age + Speed.Limit + Road.User + Bus..Involvement , data = deaths_m, ntree = 50)
summary(rf_model)
par(mfrow=c(1,1))
plot(rf_model)


#######################################################################################################################

#Using the DALEX Package to interpret models

#######################################################################################################################

#Let's use DALEX to view some of the typical model diagnostics
#First, we need to define an 'explainer' for each model
lm_expl <- explain(model = lm_model, data = deaths_m, y = deaths_m$claim_severity)
rf_expl <- explain(model = rf_model, data = deaths_m, y = deaths_m$claim_severity)

#(1): Variable importance for the two models
lm_vi <- variable_importance(lm_expl)
rf_vi <- variable_importance(rf_expl)
plot(lm_vi, rf_vi)

#randomForest probably does a better job of capturing the non-linearity in the age curve. Let's verify this!

#(2): partial dependence plots to understand 'curves' fitted by the models
lm_pd_age <- variable_response(lm_expl, "Age", "pdp")
rf_pd_age <- variable_response(rf_expl, "Age", "pdp")
plot(lm_pd_age, rf_pd_age)

#(3): Let's check how the model fits on average across key variables
deaths_m$lm_pred <- predict(lm_model)
deaths_m$rf_pred <- predict(rf_model)

deaths_t$lm_pred <- predict(lm_model, newdata = deaths_t)
deaths_t$rf_pred <- predict(rf_model, newdata = deaths_t)

#Let's look at age
age_avg <- aggregate(deaths_m[, c("claim_severity", "lm_pred", "rf_pred")], list(Age = deaths_m$Age), mean)
age_avg_long <- melt(age_avg, id="Age")  # convert to long format
ggplot(data=age_avg_long, aes(x=Age, y=value, colour=variable)) + geom_line()

#Here's an alternative way to produce the graph without converting it to 'long' format
#However, it's somewhat unintuitive that we need to use the color attribute to label the series
ggplot(data=age_avg, aes(Age)) +
  geom_line(aes(y = claim_severity, color = "Claim Severity")) +
  geom_line(aes(y = lm_pred, color = "LM Prediction")) +
  geom_line(aes(y = rf_pred, color = "RF Prediction")) +
  labs(color="Series")

#what about crash.type?
crash_avg <- aggregate(deaths_m[, c("claim_severity", "lm_pred", "rf_pred")], list(Crash.Type = deaths_m$Crash.Type), mean)
crash_avg_long <- melt(crash_avg, id="Crash.Type")  # convert to long format

ggplot(data=crash_avg_long, aes(x=Crash.Type, y=value, colour=variable)) +
  geom_line(aes(group=variable))

#how does it look on test data?
crash_avg_t <- aggregate(deaths_t[, c("claim_severity", "lm_pred", "rf_pred")], list(Crash.Type = deaths_t$Crash.Type), mean)
crash_avg_t_long <- melt(crash_avg_t, id="Crash.Type")  # convert to long format

ggplot(data=crash_avg_t_long, aes(x=Crash.Type, y=value, colour=variable)) +
  geom_line(aes(group=variable))


#(4): Let's look at the variables contributing to predictions for a couple of the observations
#Only keep the 8 variables in the model, otherwise the graphs will be hard to read
row1 <- deaths[1, c("State", "Dayweek", "Crash.Type", "Gender","Age", "Speed.Limit", "Road.User", "Bus..Involvement")]
lm_pred1 <- prediction_breakdown(lm_expl, row1)
rf_pred1 <- prediction_breakdown(rf_expl, row1)
plot(lm_pred1, rf_pred1)

row2 <- deaths[2, c("State", "Dayweek", "Crash.Type", "Gender","Age", "Speed.Limit", "Road.User", "Bus..Involvement")]
lm_pred2 <- prediction_breakdown(lm_expl, row2)
rf_pred2 <- prediction_breakdown(rf_expl, row2)
plot(lm_pred2, rf_pred2)


#(5) There are many ways to numerically evaluate model performance.
# As a simple benchmark statistic, let's use root mean square error
lm_m_rmse <- mean((deaths_m$lm_pred - deaths_m$claim_severity)^2)^0.5
rf_m_rmse <- mean((deaths_m$rf_pred - deaths_m$claim_severity)^2)^0.5
lm_t_rmse <- mean((deaths_t$lm_pred - deaths_t$claim_severity)^2)^0.5
rf_t_rmse <- mean((deaths_t$rf_pred - deaths_t$claim_severity)^2)^0.5

lm_m_rmse
rf_m_rmse
lm_t_rmse
rf_t_rmse

#The random forest model appears to be performing better out-of-the-box

#######################################################################################################################

#Model Iterations

#######################################################################################################################

#We observed that the age curve generated by the LM was a straight line. Can we improve this?
#We can 'cheat' by using the random forest partial dependence plot as a guide
#Alternatively, let's see what the LM suggests if we fit the (banded) age curve as a factor

lm_model2 <- lm(claim_severity ~ State + Dayweek + Crash.Type + Gender + Age_Band + Speed.Limit + Road.User + Bus..Involvement, data=deaths_m)
summary(lm_model2)

deaths_m$lm_pred2 <- predict(lm_model2)
lm2_m_rmse <- mean((deaths_m$lm_pred2 - deaths_m$claim_severity)^2)^0.5
lm_m_rmse
lm2_m_rmse

AIC(lm_model)
AIC(lm_model2)

#There's a substantial decrease in RMSE, which is to be expected since we've added more features to the model
#However, there's also a reduction in AIC, which suggests that the predictive power of these
#features is sufficient to offset the additional complexity we've added to the model

# So what exactly is this new 'curve' doing?
# Unfortunately DALEX runs very slowly for me - so here's a workaround!

age_coefs <- as.data.frame(coefficients(lm_model2)[20:40])
colnames(age_coefs) <- c("coef")
age_coefs$label <- substr(names(coefficients(lm_model2)[20:40]), 9, 16)
ggplot(data=age_coefs, aes(x=factor(label), y=coef)) + geom_point()
#Looking at the parameter estimates suggests we might be able to simplify the curve using
# 4 piecewise linear splines: 0-20, 20-35, 35-80 and 80-100
# We need to add these new features to both the model and test datasets

deaths_m$Age_0_20 <- pmin(pmax(deaths_m$Age, 0), 20)
deaths_m$Age_20_35 <- pmin(pmax(deaths_m$Age, 20), 35)-20
deaths_m$Age_35_80 <- pmin(pmax(deaths_m$Age, 35), 80)-35
deaths_m$Age_80_100 <- pmin(pmax(deaths_m$Age, 80), 100)-80

deaths_t$Age_0_20 <- pmin(pmax(deaths_t$Age, 0), 20)
deaths_t$Age_20_35 <- pmin(pmax(deaths_t$Age, 20), 35)-20
deaths_t$Age_35_80 <- pmin(pmax(deaths_t$Age, 35), 80)-35
deaths_t$Age_80_100 <- pmin(pmax(deaths_t$Age, 80), 100)-80

lm_model3 <- lm(claim_severity ~ State + Dayweek + Crash.Type + Gender + Speed.Limit + Road.User + Bus..Involvement
                + Age_0_20 + Age_20_35 + Age_35_80 + Age_80_100, data=deaths_m)

summary(lm_model3)
deaths_m$lm_pred3 <- predict(lm_model3)
deaths_t$lm_pred3 <- predict(lm_model3, newdata=deaths_t)

age_avg <- aggregate(deaths_m[, c("claim_severity", "lm_pred", "lm_pred3", "rf_pred")], list(Age = deaths_m$Age), mean)
age_avg_long <- melt(age_avg, id="Age")  # convert to long format
ggplot(data=age_avg_long, aes(x=Age, y=value, colour=variable)) + geom_line()
#While it's not perfect, the updated age curve makes the model provide a better fit on average

#######################################################################################################################

#Conclusion

#######################################################################################################################

#We hope you've enjoyed this brief introduction to predictive modelling!
#After all is said and done, it seemed like the random forest was able to obtain reasonably good predictions
# and do so without much configuration e.g. compare the work to fit an age curve
#So... why not always use a random forest? We'll leave it to you to conduct some exploratory research. Have fun! :)
