##################################################################
## Kelly Kung
## Date: 2021-9-7
## Code to create the data set for analysis. We define the outcome and intervention
## variables and combine them into one. This contains the all deaths.
##################################################################


###############################################################################################
############################## Set up ###############################
#set the directory to where the data is stored
setwd("~/Documents/BU/Research-Lok")

#packages we need for this code file
library(ggplot2)
library(mgcv)
library(lubridate)
library(zoo)
library(tidyverse)
library(dplyr)

###############################################################################################
############################## Creating Data Set ###############################
#initialize a data frame to store all the information
#initialize with the time period IDs as well as the start/end dates of 2000
full_data_set<-data.frame("Time_Period_ID" = c(1:(2*(2019 - 2000 + 1))))
full_data_set$Time_Period_Start<-as.Date("01-01-2000", format = "%m-%d-%Y")
full_data_set$Time_Period_End<-as.Date("06-30-2000", format = "%m-%d-%Y")

#here, we impute the start and end dates of each six month period
#the start dates are either Jan 1 or July 1 and the end dates are either June 30 or December 31
year_value<-2000
#for each time period ID, we assign the start and end dates
for(i in full_data_set$Time_Period_ID){
  #when i is even, we impute the dates for the second half of the year
  if(i%%2 == 0){
    full_data_set$Time_Period_Start[i]<-as.Date(paste("07-01-", year_value, sep = ""), format = "%m-%d-%Y")
    full_data_set$Time_Period_End[i]<-as.Date(paste("12-31-", year_value, sep = ""), format = "%m-%d-%Y")
    year_value <- year_value + 1
  }else{
    #otherwise, we just replace the year for the Jan and June entries
    year(full_data_set$Time_Period_Start[i])<-year_value
    year(full_data_set$Time_Period_End[i])<-year_value
  }
}

#full_data_set is currently just a data frame with time period information from 2000 to 2019.
#however, we want to repeat this for all 50 states:
n <- 50
full_data_set<-do.call("rbind", replicate(n, full_data_set, simplify = FALSE))

#now add the states (given by state.name in R) into the data frame
full_data_set$State<-rep(state.name, each = 2*(2019 - 2000 + 1))

#add the population information for 2000 to 2019, downloaded from the Census
#there are two separate files because Census data is collected every 10 years
#read in population data
pop_by_age_2010<- read.csv("./Data/pop_by_age_state_2000_2010.csv")
head(pop_by_age_2010)
pop_by_age_2010_group <- pop_by_age_2010 %>% 
  #we filter by the most common ages
  # filter(NAME != "United States", SEX == 0, AGE >= 12, AGE <= 80) %>%
  #we filter according to how the DIH prosecutions were filtered, excluding cases with minors
  filter(NAME != "United States", SEX == 0) %>%
  group_by(NAME) %>%
  summarise(pop_2000 = sum(POPESTIMATE2000[SEX == 0]),
            pop_2001 = sum(POPESTIMATE2001[SEX == 0]),
            pop_2002 = sum(POPESTIMATE2002[SEX == 0]),
            pop_2003 = sum(POPESTIMATE2003[SEX == 0]),
            pop_2004 = sum(POPESTIMATE2004[SEX == 0]),
            pop_2005 = sum(POPESTIMATE2005[SEX == 0]),
            pop_2006 = sum(POPESTIMATE2006[SEX == 0]),
            pop_2007 = sum(POPESTIMATE2007[SEX == 0]),
            pop_2008 = sum(POPESTIMATE2008[SEX == 0]),
            pop_2009 = sum(POPESTIMATE2009[SEX == 0]),
            pop_2010 = sum(CENSUS2010POP[SEX == 0]))

pop_by_age_2019 <- read.csv("./Data/pop_by_age_state_2010_2019.csv")
head(pop_by_age_2019)
pop_by_age_2019_group <- pop_by_age_2019 %>% 
  # filter(NAME != "United States", SEX == 0, AGE >= 12, AGE <= 80) %>%
  #we filter according to how the DIH prosecutions were filtered, excluding cases with minors
  filter(NAME != "United States", SEX == 0) %>%
  group_by(NAME) %>%
  summarise(pop_2011 = sum(POPEST2011_CIV[SEX == 0]),
            pop_2012 = sum(POPEST2012_CIV[SEX == 0]),
            pop_2013 = sum(POPEST2013_CIV[SEX == 0]),
            pop_2014 = sum(POPEST2014_CIV[SEX == 0]),
            pop_2015 = sum(POPEST2015_CIV[SEX == 0]),
            pop_2016 = sum(POPEST2016_CIV[SEX == 0]),
            pop_2017 = sum(POPEST2017_CIV[SEX == 0]),
            pop_2018 = sum(POPEST2018_CIV[SEX == 0]),
            pop_2019 = sum(POPEST2019_CIV[SEX == 0]))


#initialize the population vector inside the full_data_set
full_data_set$population<-rep(0, nrow(full_data_set))

#add the population entries into the data set
#for each row in the data set,
for(i in 1:nrow(full_data_set)){
  #we find the year of the time period and state in that row
  year_value<-year(full_data_set$Time_Period_Start[i])
  state<-full_data_set$State[i]
  #if the year is less than 2010, we impute the population from pop_by_age_2010_group data
  if(year_value<=2010){
    full_data_set$population[i]<-unlist(pop_by_age_2010_group[which(pop_by_age_2010_group$NAME == state), paste0("pop_", year_value)])
  }else{ #if the year is at least 2010, we impute the data from pop_by_age_2019_group data set
    full_data_set$population[i]<-unlist(pop_by_age_2019_group[which(pop_by_age_2019_group$NAME == state), paste0("pop_", year_value)])
  }
}

###############################################################################################
############################## Imputing Missing Drug Overdoses Outcome Data ###############################
#read in the overdose data with data for each state for each month from 1999 to 2019
od_data <- read.csv("./Data/all_od_1999_2019_age_17_up.txt", sep = "\t", stringsAsFactors = FALSE)
od_data$Deaths <- as.numeric(od_data$Deaths)
head(od_data, 50)
dim(od_data)
View(od_data)
od_data<-od_data[!is.na(od_data$Year),] #delete the rows that just contains data set description info
tail(od_data)
sum(is.na(od_data$Deaths))

#set up the overdose data to impute the missing data
#set up the dates
od_data$Date<-mdy(od_data$Month)
length(unique(od_data$State))
#DC is being counted in this, but we do not have data on prosecutions. Remove these rows
od_data<-od_data[od_data$State!="District of Columbia",]

#interpolate the missing values
od_year <- read.csv("./Data/all_od_yearly_1999_2019_age_17_up.txt", sep = "\t", stringsAsFactors = FALSE)
od_year$Deaths <- as.numeric(od_year$Deaths)
head(od_year, 50)
#see how many states have missing yearly entries and use the totals to impute the missing yearly values.
sum_na <- od_year %>% group_by(State) %>% summarise(sum(is.na(Deaths)))
table(sum_na)

od_year <- od_year[!is.na(od_year$Year),]
tail(od_year)

od_data$interp_vals<-rep(NA, nrow(od_data))

startYear<-1999

for(state in unique(od_data$State)){
  #get the values of the deaths for state
  tempVal<-od_data$Deaths[od_data$State == state]
  #linearly interpolate the deaths that are missing
  tempInterp<-na.approx(tempVal, rule = 2)
  for(year in startYear:2019){
    #find the indices of the missing data -- gets the missing indices for that particular year
    indexMissing<-which(is.na(tempVal[(1:12) + 12*(year - startYear)]))
    if(length(indexMissing) != 0){
      #if there are missing values, we find the number of accounted deaths
      tempDataSum<-sum(tempVal[(1:12) + 12*(year - startYear)], na.rm = TRUE)
      #and calculate the deaths that are not accounted for using the yearly deaths for the state
      numNotAccounted<-od_year$Deaths[od_year$State == state & od_year$Year == year] - tempDataSum
      
      #we then calculate the weight: number of not accounted deaths/sum of accounted deaths
      denom<-sum(tempInterp[(1:12) + 12*(year - startYear)][indexMissing], na.rm = TRUE)
      tempInterp[(1:12) + 12*(year - startYear)][indexMissing]<-sapply(tempInterp[(1:12) + 12*(year - startYear)][indexMissing], function(x){
        if(!is.na(x)){x*(numNotAccounted/denom)}else{numNotAccounted/length(indexMissing)}})
    }else{
      #otherwise, if there is no missing values, we skip to the next year
      next
    }
  }
  #store the interpolated values
  od_data$interp_vals[od_data$State == state]<-tempInterp
}

# write.csv(od_data, "./Data/od_data_interpolated_all_od_1999_2019_age_17_up_9_7_21.csv")

#group into 6 month time periods now
od_data<- od_data %>%
  mutate(Time_Period_Start = lubridate::floor_date(Date , "6 months" ))

od_data<- od_data %>% group_by(State, Time_Period_Start) %>%
  summarise(sum_deaths = sum(interp_vals, na.rm = TRUE))

#merge the imputed deaths into the full data set
full_data_set<- merge(full_data_set, od_data, by = c("State", "Time_Period_Start"))

#for the logistic regression, we also want to know the number of people who did not die from drug overdoses
full_data_set$num_alive<-full_data_set$population - full_data_set$sum_deaths


###############################################################################################
############################## Defining the Intervention Variable ###############################
#read in the prosecution media alert data
prosec_data<-read.csv("./Data/dih_prosecutions_9_6_21.csv")
head(prosec_data)
colnames(prosec_data)

#data cleaning
#first take out the rows with "no info" for charge date or State
#clean up the states
prosec_data<-prosec_data %>% 
  mutate(Date = as.Date(Date.charged, "%m/%d/%Y")) %>%
  mutate(State = ifelse(State.Filed == "pennsylvania", "Pennsylvania", State.Filed),
         State = ifelse(State.Filed == "Virginia ", "Virginia", State)) %>%
  filter(!is.na(Date), State.Filed != "No Info", State.Filed != "No info", State.Filed != "No Info ")

#clean up the data by looking at the link to the article
prosec_data$Date[prosec_data$Date == "2026-08-01"] <- as.Date("2016-02-15", "%Y-%m-%d")

#set up the dates for six month periods
prosec_data <- prosec_data %>% mutate(Time_Period_Start = lubridate::floor_date(Date , "6 months" ))

#count the number of interventions per state per six month period per year
prosec_grouped_data<-prosec_data%>% group_by(State, Time_Period_Start) %>% count()
head(prosec_grouped_data)
#subset the data to only contain prosecutions from 2000 to 2019
prosec_grouped_data<-prosec_grouped_data[year(prosec_grouped_data$Time_Period_Start)<2020 &
                                           year(prosec_grouped_data$Time_Period_Start)>1999,]

#merge the outcome data and intervention data set
full_data_set<-merge(full_data_set, prosec_grouped_data, by = c("State", "Time_Period_Start"),
                     all = TRUE)

#change the column name of the intervention variable
colnames(full_data_set)[which(colnames(full_data_set) == "n")]<-"Intervention"

#if there is at least one intervention, we impute a 1 in the Intervention column
#first, we impute the rows with NA with a 0
full_data_set$Intervention[is.na(full_data_set$Intervention)]<-0

#make a function to define the intervention variable-- will be used for confounders as well
#so that it imputes a 1 for all dates that follow after the first intervention

makeRevisedVariable<-function(start_date, data_set, var_name, states_exclude = c()){
  #arguments: start_date = vector of first charge dates for each state,
  #data_set = data set where we add the intervention variables to and return at the end of function
  #var_name = name of variable being redefined
  #states_exclude = vector of states to be excluded, if any
  
  #create a data frame to store the info and initialize it with states and dates
  revised_data_set<-data.frame("State" = unique(state.name))
  revised_data_set$first_affected_date<-rep(as.Date("01-01-2000", format = "%m-%d-%Y"), nrow(revised_data_set))
  #create a column to store the proportion of days affected by the intervention or covariate in the six month perior
  revised_data_set$proportion_days_affected<-rep(0, nrow(revised_data_set))
  
  #add first date of intervention/covariate to the data_set being returned
  data_set[, paste(var_name, "_First_Date", sep = "")]<-rep(as.Date("01-01-2000", format = "%m-%d-%Y"), nrow(data_set))
  #this redefined variable is the proportion of days in the six month period where treatment is equal to 1
  data_set[, paste(var_name, "_Redefined", sep = "")]<-rep(0, nrow(data_set))
  
  #for each state,
  for(state in unique(state.name)){
    #this is the first intervention/covariate date reported in the state
    first_affected_date<-start_date[names(start_date) == state]
    if(state %in% states_exclude){
      #for the states that we are excluding, we put in NA for the first charge date and 0 for the proportion of days in six months period with treatment equal to 1
      revised_data_set$first_affected_date[revised_data_set$State == state]<-NA
      revised_data_set$proportion_days_affected[revised_data_set$State == state]<-0
      data_set[data_set$State == state, paste(var_name, "_First_Date", sep = "")]<-NA
      
    }else{
      #otherwise, we calculate the proportion of days with treatment equal to 1 by taking the difference between the end dates of the six month periods
      #and the first charge date and then dividing by the number of days in the six month periods
      revised_data_set$first_affected_date[revised_data_set$State == state]<-first_affected_date
      #find the number of days in the first and second halfs of the year
      daysBetweenPeriodFirstHalf<-as.numeric(as.Date(paste("06-30-", year(first_affected_date), sep = ""), format = "%m-%d-%Y") -
                                               as.Date(paste("01-01-", year(first_affected_date), sep = ""), format = "%m-%d-%Y"))
      daysBetweenPeriodSecondHalf<-as.numeric(as.Date(paste("12-31-", year(first_affected_date), sep = ""),, format = "%m-%d-%Y") -
                                                as.Date(paste("07-01-", year(first_affected_date), sep = ""),, format = "%m-%d-%Y"))
      if(month(first_affected_date)<7){
        #if the month of the first day is before July, we calculate the number of affected days by subtracting it from June 30
        num_affected_days<-as.numeric(as.Date(paste("06-30-", year(first_affected_date)), format = "%m-%d-%Y") - first_affected_date)
        revised_data_set$proportion_days_affected[revised_data_set$State == state]<-num_affected_days/daysBetweenPeriodFirstHalf
      }else{
        #otherwise, we calculated the number of affected days by subtracting from December 31
        num_affected_days<-as.numeric(as.Date(paste("12-31-", year(first_affected_date)), format = "%m-%d-%Y") - first_affected_date)
        revised_data_set$proportion_days_affected[revised_data_set$State == state]<-num_affected_days/daysBetweenPeriodSecondHalf
      }
      
      #we impute the first affected date into the data_set we return
      data_set[data_set$State == state, paste(var_name, "_First_Date", sep = "")]<-first_affected_date
      #for all the six month time periods after the first charge date, the redefined variable will be equal to 1
      data_set[data_set$State == state & data_set$Time_Period_Start>first_affected_date,
               paste(var_name, "_Redefined", sep = "")]<-1
      #for the time period where the treatment occurred, the variable is equal to the proportion of days with the treatment equal to 1
      data_set[data_set$State == state & data_set$Time_Period_Start<=first_affected_date
               & data_set$Time_Period_End >= first_affected_date, paste(var_name, "_Redefined", sep = "")]<-revised_data_set$proportion_days_affected[revised_data_set$State == state]
      
    }
    
  }
  
  return(data_set)
}

#vector of first charge dates for each state -- note here we are only considering those between 2000 - 2017
interventionStartDate<-as.Date(sapply(state.name, function(state){sort(prosec_data$Date[prosec_data$State == state & year(prosec_data$Date)> 1999 &
                                                                                          year(prosec_data$Date)<2020])[1]}))
#we add the intervention and revised intervention variable. We exclude Hawaii because it doesn't have any prosecutions
full_data_set<-makeRevisedVariable(interventionStartDate, full_data_set, "Intervention", "Hawaii")
head(full_data_set, 20)

###############################################################################################
############################## Defining the Confounding Variables ###############################
#some of the laws change during the study, so we want to define the confounding variable accurately.

#this function detects whether there is a change in the policy
detectChangeInPolicyStatus<-function(start_dates, data_with_variable, varName){
  #arguments: start_dates = first date of treatment for each state,
  #data_with_variable = data frame with the variable of interest
  #varName = name of variable of interest
  
  #initialize the data frame with the State, Change In Status, Change In Status Date, and Reinstate Date columns
  return_data_set<-data.frame("State" = state.name)
  return_data_set$ChangeInStatus<-rep(0, nrow(return_data_set))
  return_data_set$ChangeInStatusDate<-rep(as.Date("01-01-1999", format = "%m-%d-%Y"), nrow(return_data_set))
  return_data_set$ReinstateDate<-rep(as.Date("01-01-1999", format = "%m-%d-%Y"), nrow(return_data_set))
  
  #for each state, we look to see when the policy was not in effect. If there were times when it was not in effect,
  #we look to see if the dates of when the policy wasn't in effect occurred after it was in effect
  #if so, we find the first date when the policy was changed to not be in effect
  for(state in state.name){
    #temporary data set where the treatment did not occur
    data_not_in_effect<-data_with_variable[data_with_variable[,varName] == 0 &
                                             data_with_variable$Jurisdictions == state,]
    if(nrow(data_not_in_effect)!=0){
      #if there were dates where treatment did not occur yet, for each row of the temporary data_not_in_effect set,
      for(i in 1:nrow(data_not_in_effect)){
        #we determine whether the no status date occurred after the first date of treatment
        if(!is.na(start_dates[names(start_dates) == state]) &
           data_not_in_effect$Effective.Date[i]>start_dates[names(start_dates) == state]){
          
          #if so, there is a change in status and we record the date when it happened
          return_data_set$ChangeInStatus[return_data_set$State == state]<-1
          return_data_set$ChangeInStatusDate[return_data_set$State == state]<-as.Date(data_not_in_effect$Effective.Date[i])
          #we just need to look at the first change, so we break out of the loop after we recorded the information
          break
        }
      }
      
      #if there was a change in status (from in effect to not in effect), we see if the policy was reinstated later on
      if(return_data_set$ChangeInStatus[return_data_set$State == state] == 1){
        #to see if the policy was reinstated, we look at the effective dates given by the data set and see if there were any
        #that occurred after the change in status date
        if(sum(data_with_variable[as.Date(data_with_variable$Effective.Date)>
                                  return_data_set$ChangeInStatusDate[return_data_set$State == state],varName])>0){
          
          #we find the first date after the change in status date where the policy was reinstated, i.e. when policy equals to 1 again
          return_data_set$ReinstateDate[return_data_set$State == state]<-as.Date(sort(data_with_variable$Effective.Date[data_with_variable$Effective.Date >
                                                                                                                          return_data_set$ChangeInStatusDate[return_data_set$State == state] &
                                                                                                                          data_with_variable[,varName] == 1 &
                                                                                                                          data_with_variable$Jurisdictions == state])[1])
        }
      }
    }
  }
  
  #if there wasn't a change in status or a reinstating of the policy, put NA into the change status date and reinstate date
  return_data_set$ChangeInStatusDate[return_data_set$ChangeInStatus == 0]<-NA
  return_data_set$ReinstateDate[return_data_set$ChangeInStatus == 0]<-NA
  
  return(return_data_set)
}

############################## Naloxone ###############################
#https://pdaps.org/datasets/laws-regulating-administration-of-naloxone-1501695139
nalox<-read.csv("./Data/Naloxone Data_09112020.csv", stringsAsFactors = FALSE)

#find the first dates for each state where the naloxone access law is passed and pharmacists are able to dispense without prescription
nalox_pharm_yes_start_date <- as.Date(sapply(state.name, function(x) sort(as.Date(nalox$Effective.Date[nalox$naaddressoaayn == 1
                                                                                                       & nalox$pharmacist.dispensing == 1
                                                                                                       & nalox$Jurisdictions == x], "%m/%d/%y"))[1]))
#find the first dates for each state where the naloxone access law is passed and pharmacists are not able to dispense without prescription
nalox_pharm_no_start_date<-as.Date(sapply(state.name, function(x) sort(as.Date(nalox$Effective.Date[nalox$naaddressoaayn==1
                                                                                                    & nalox$pharmacist.dispensing == 0
                                                                                                    & nalox$Jurisdictions == x], "%m/%d/%y"))[1]))
names(nalox_pharm_yes_start_date)<-names(nalox_pharm_no_start_date)<-state.name

#add the redefined confounding variables
full_data_set<-makeRevisedVariable(nalox_pharm_yes_start_date, full_data_set, "Naloxone_Pharmacy_Yes",
                                   names(nalox_pharm_yes_start_date)[is.na(nalox_pharm_yes_start_date)])

full_data_set<-makeRevisedVariable(nalox_pharm_no_start_date, full_data_set, "Naloxone_Pharmacy_No",
                                   names(nalox_pharm_no_start_date)[is.na(nalox_pharm_no_start_date)])


#for the dates where the state switched from not allowing to allowing pharmacists to dispense without a prescription to allowing them, we set the
#Naloxone_Pharmacy_No_Redefined indicator to 0
full_data_set$Naloxone_Pharmacy_No_Redefined[full_data_set$Naloxone_Pharmacy_Yes_Redefined>0]<-0

############################## Medical Marijuana ###############################
medMar<-read.csv("./Data/20170919_med_marijuana.csv", stringsAsFactors = FALSE)
#find the first date where the medical marijuana law was passed
medMar_start_dates<-as.Date(sapply(state.name, function(x) sort(medMar$date.first.law[medMar$mmlaw == 1
                                                                                      & medMar$Jurisdictions == x])[1]))
names(medMar_start_dates)<-state.name
#LA dispensed first medical marijuana on Aug 6, 2019: https://www.pbs.org/newshour/nation/louisiana-launching-medical-marijuana-after-years-of-waiting
medMar_start_dates[names(medMar_start_dates) == "Louisiana"] <- as.Date("2019-08-06")
#MO passed medical marijuana in Nov 6, 2018: https://reason.com/2018/11/06/missouri-becomes-the-32nd-medical-mariju/
medMar_start_dates[names(medMar_start_dates) == "Missouri"] <- as.Date("2018-11-06")
#OK passed medical marijuana in June 26, 2018: https://ballotpedia.org/Oklahoma_State_Question_788,_Medical_Marijuana_Legalization_Initiative_(June_2018)
medMar_start_dates[names(medMar_start_dates) == "Oklahoma"] <- as.Date("2018-06-26")
#UT passed medical marijuana in November 6, 2018: https://www.mpp.org/states/utah/
medMar_start_dates[names(medMar_start_dates) == "Utah"] <- as.Date("2018-11-06")
#WV signed bill on April 19, 2017: https://www.mpp.org/states/west-virginia/
medMar_start_dates[names(medMar_start_dates) == "West Virginia"] <- as.Date("2017-04-19")


detectChangeInPolicyStatus(medMar_start_dates, medMar, "mmlaw")
#Checked with data on PDAPS, and I believe that there is a typo in the Minnesota date. The actual first effective date is May 30, 2014
#North Dakota changed its laws in January 2017 from 1 back to 0; medical marijuana will not be available under the Compassionate Care Act until 2018

#exclude the states without medical marijuana law
states_without_medMar<-names(medMar_start_dates[is.na(medMar_start_dates)])
full_data_set<-makeRevisedVariable(medMar_start_dates, full_data_set, "Medical_Marijuana", states_without_medMar)

# #change North Dakota's medical marijuana variable to 0 after Jan 2017
# full_data_set$Medical_Marijuana_Redefined[full_data_set$State == "North Dakota" &
#                                             full_data_set$Time_Period_Start >= as.Date("01-01-2017", format = "%m-%d-%Y")]<-0



############################## Recreational Marijuana ###############################
recMar<-read.csv("./Data/20170216-Rec_marijuana.csv", stringsAsFactors = FALSE)
recMar_start_dates<-as.Date(sapply(state.name, function(x) sort(recMar$Effective.Date[recMar$rm.rmlaw_Yes == 1
                                                                                      & recMar$Jurisdictions == x])[1]))
names(recMar_start_dates)<-state.name

#we need to impute some start dates from research
#colorado- amendment 64 passed in nov 6. 2012, but legalization of private marijuana consumption https://www.drugpolicy.org/news/2012/12/december-10-historic-day-colorado-marijuana-legalization-law-takes-effect
#washington - https://www.drugpolicy.org/news/2012/12/december-6-historic-day-washington-marijuana-legalization-law-takes-effect
#CA: nov 8, 2016: https://ballotpedia.org/California_Proposition_64,_Marijuana_Legalization_(2016)
recMar_start_dates[names(recMar_start_dates) == "Colorado"]<-as.Date("2012-12-10")
recMar_start_dates[names(recMar_start_dates) == "Washington"]<-as.Date("2012-12-06")
recMar_start_dates[names(recMar_start_dates) == "California"] <- as.Date("2016-11-08")

#Michigan legalization date: Dec 6, 2018: https://www.freep.com/story/news/marijuana/2018/12/06/recreational-marijuana-legal-michigan-today/2217854002/
recMar_start_dates[names(recMar_start_dates) == "Michigan"] <- as.Date("2018-12-06")
#VT legalization date: July 1, 2018: https://www.washingtonpost.com/news/post-nation/wp/2018/01/23/vermont-is-the-first-state-to-legalize-marijuana-through-legislature/
recMar_start_dates[names(recMar_start_dates) == "Vermont"] <- as.Date("2018-07-01")


detectChangeInPolicyStatus(recMar_start_dates, recMar, "rm.rmlaw_Yes")
states_without_recMar<-names(recMar_start_dates[is.na(recMar_start_dates)])

full_data_set<-makeRevisedVariable(recMar_start_dates, full_data_set, "Recreational_Marijuana", states_without_recMar)

############################## Good Samaritan Laws ###############################
gsl<-read.csv("./Data/Good Samaritan Overdose Prevention Laws 7.21.2021.csv", stringsAsFactors = FALSE)
gsl_start_dates<-as.Date(sapply(state.name, function(x) sort(as.Date(gsl$Effective.Date[gsl$goodsam.law == 1
                                                                                        & gsl$Jurisdictions == x], "%m/%d/%y"))[1]))
names(gsl_start_dates)<-state.name

gsl$Effective.Date <- as.Date(gsl$Effective.Date, "%m/%d/%y")
gsl$Valid.Through.Date <- as.Date(gsl$Valid.Through.Date, "%m/%d/%y")

#NOTE: 5 states with no GSL, and  RI GSL expired in July 2015 to Jan 2016 but then was reinstated
detectChangeInPolicyStatus(gsl_start_dates, gsl, "goodsam.law")

states_without_gsl<-names(gsl_start_dates[is.na(gsl_start_dates)])
full_data_set<-makeRevisedVariable(gsl_start_dates, full_data_set, "GSL", states_without_gsl)

#change the treatment variable as needed for Rhode Island
full_data_set$GSL_Redefined[full_data_set$State == "Rhode Island"
                            & full_data_set$Time_Period_Start >= as.Date("07-01-2015", format = "%m-%d-%Y")
                            & full_data_set$Time_Period_Start < as.Date("01-26-2016", format = "%m-%d-%Y")]<-0

full_data_set$GSL_Redefined[full_data_set$State == "Rhode Island" & full_data_set$Time_Period_Start ==
                              as.Date("01-01-2016", format = "%m-%d-%Y")]<-
  as.numeric(as.Date("06-30-2016", format = "%m-%d-%Y") - as.Date("01-27-2016", format = "%m-%d-%Y"))/
  as.numeric(as.Date("06-30-2016", format = "%m-%d-%Y") - as.Date("01-01-2016", format = "%m-%d-%Y"))


############################## Prescription Drug Monitoring Program (PDMP) ###############################
pdmp<-read.csv("./Data/pdmp_data_8_21_19.csv", stringsAsFactors = FALSE)
pdmp_start_dates<-pdmp$pdmpimp.effect<-as.Date(pdmp$pdmpimp.effect)
names(pdmp_start_dates)<-pdmp$Jurisdictions
#no changes in policies
detectChangeInPolicyStatus(pdmp_start_dates, pdmp, "pdmpimp.effect")
#Missouri doesn't have a PDMP
full_data_set<-makeRevisedVariable(pdmp_start_dates, full_data_set, "PDMP", "Missouri")

############################## Medicaid Expansion ###############################
medicaid<-read.csv("./Data/expansion-status-interactive-map_8.10.21.csv", stringsAsFactors = FALSE)
colnames(medicaid)

#initialize data for medicaid start dates
medicaid_start_dates_data<-data.frame("State" = medicaid$State)
medicaid_start_dates_data$yes_medicaid_expansion<-1
medicaid_start_dates_data$yes_medicaid_expansion[medicaid$Expansion.Status == "Not Adopted"]<-0
table(medicaid_start_dates_data$yes_medicaid_expansion)

#get the medicaid expansion dates
medicaid_dates<-as.Date(sapply(medicaid$Description, function(x) strsplit(strsplit(x, " ")[[1]][4], "\n")[[1]]), "%m/%d/%Y")
medicaid_start_dates_data$expansion_date<-medicaid_dates

#manually impute the dates for the 5 states based on:
#https://www.kff.org/health-reform/issue-brief/states-getting-a-jump-start-on-health/ Table 1
medicaid_start_dates_data$expansion_date[medicaid_start_dates_data$State == "California"]<-as.Date("11-1-2010", "%m-%d-%Y")
medicaid_start_dates_data$expansion_date[medicaid_start_dates_data$State == "Connecticut"]<-as.Date("4-1-2010", "%m-%d-%Y")
medicaid_start_dates_data$expansion_date[medicaid_start_dates_data$State == "District of Columbia"]<-as.Date("7-1-2010", "%m-%d-%Y")
medicaid_start_dates_data$expansion_date[medicaid_start_dates_data$State == "Minnesota"]<-as.Date("3-1-2010", "%m-%d-%Y")
medicaid_start_dates_data$expansion_date[medicaid_start_dates_data$State == "New Jersey"]<-as.Date("4-14-2011", "%m-%d-%Y")
medicaid_start_dates_data$expansion_date[medicaid_start_dates_data$State == "Washington"]<-as.Date("1-3-2011", "%m-%d-%Y")

#store the vector of start dates for medicaid expansion
medicaid_start_dates<-as.Date(medicaid_start_dates_data$expansion_date[medicaid_start_dates_data$State != "District of Columbia"])
names(medicaid_start_dates)<-state.name

states_without_medicaid<-names(medicaid_start_dates[is.na(medicaid_start_dates)])
full_data_set<-makeRevisedVariable(medicaid_start_dates, full_data_set, "Medicaid_Expansion", states_without_medicaid)

############################## Save full data set ###############################
colnames(full_data_set)
full_data_set<-full_data_set[,c("Time_Period_ID", "Time_Period_Start", "Time_Period_End",
                                "State", colnames(full_data_set)[5:ncol(full_data_set)])]
# write.csv(full_data_set, "./Data/full_data_set_11_15_21_all_od.csv", row.names = FALSE)

