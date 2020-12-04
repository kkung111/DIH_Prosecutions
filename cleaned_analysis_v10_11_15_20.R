# !diagnostics off
##################################################################
## Kelly Kung
## Date: 2020-11-15
## Code for DIH Prosecution Data Analysis
##################################################################

##############################################################################################
############################## Statistical Analysis ###############################
#read in data
setwd("~/Documents/BU/Research-Lok")
main_analysis_data<-read.csv("./Data/full_data_set_9_1_20.csv")

#packages we need for this code file
library(ggplot2)
library(mgcv)
library(lubridate)
library(zoo)
library(tidyverse)
library(dplyr)
library(DHARMa)
library(mgcViz)
library(extrafont)
library(arm)
loadfonts()

#define functions we will need for analysis
#expit function
expit<-function(x){
  return(exp(x)/(1 + exp(x)))
}

#logit function
logit<-function(x){
  return(log(x/(1 - x)))
}

###################################################################################
################################## set up data set ################################
#add the intervention dates and time period data
main_analysis_data$Intervention_First_Date<-as.Date(main_analysis_data$Intervention_First_Date)
main_analysis_data$Time_Period_Start<-as.Date(main_analysis_data$Time_Period_Start)
names(main_analysis_data)[which(colnames(main_analysis_data) == "sum_deaths")] <- "imputed_deaths"

################################## set up the Regions ##############################
#set up the regions according to Census: https://www.census.gov/geographies/reference-maps/2010/geo/2010-census-regions-and-divisions-of-the-united-states.html
NE.name <- c("Connecticut","Maine","Massachusetts","New Hampshire",
             "Rhode Island","Vermont","New Jersey","New York",
             "Pennsylvania")

MW.name <- c("Indiana","Illinois","Michigan","Ohio","Wisconsin",
             "Iowa","Kansas","Minnesota","Missouri","Nebraska",
             "North Dakota","South Dakota")

S.name <- c("Delaware","District of Columbia","Florida","Georgia",
            "Maryland","North Carolina","South Carolina","Virginia",
            "West Virginia","Alabama","Kentucky","Mississippi",
            "Tennessee","Arkansas","Louisiana","Oklahoma","Texas")

W.name <- c("Arizona","Colorado","Idaho","New Mexico","Montana",
            "Utah","Nevada","Wyoming","Alaska","California",
            "Hawaii","Oregon","Washington")

region.list <- list(
  Northeast=NE.name,
  Midwest=MW.name,
  South=S.name,
  West=W.name)

#initialize vector with "West" and then impute the other regions for the states
main_analysis_data$Region<-rep("West", nrow(main_analysis_data))
for(state in unique(main_analysis_data$State)){
  if(state %in% region.list$Northeast){
    main_analysis_data$Region[main_analysis_data$State == state]<-"Northeast"
  }else if(state %in% region.list$Midwest){
    main_analysis_data$Region[main_analysis_data$State == state]<-"Midwest"
  }else if(state %in% region.list$South){
    main_analysis_data$Region[main_analysis_data$State == state]<-"South"
  }
}


##############################################################################
############ EDA: Plot the Outcome and Intervention Trends ###################
main_analysis_data_sum <- main_analysis_data %>% group_by(year = year(Time_Period_Start)) %>%
  summarise(total_deaths = sum(imputed_deaths),
            total_prop = sum(imputed_deaths)/sum(population),
            total_prop_by_100000 = 100000*sum(imputed_deaths)/sum(population)) %>%
  mutate(date = as.Date(as.yearmon(year)))

# pdf("./Figures/total_od_deaths_all_paper.pdf")
ggplot(data = main_analysis_data_sum, mapping = aes(x = date,
                                                    y = total_deaths)) +
  geom_line() +
  labs(x = "Year", y = "Total Unintentional Deaths in 50 U.S. States") +
  theme(panel.background = element_rect("white"), panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.title=element_text(family="Times", size=10, face="bold"),
        axis.text=element_text(family="Times",size=10)) +
  scale_x_date(date_labels="%Y", breaks = seq(as.Date("2000-01-01"), as.Date("2018-01-01"), by = "2 years")) +
  ylim(c(0, 61000))
# dev.off()

#plot the number of states with an intervention for each time point
#first, create a data set to find the number of states with an intervention at each time point
#initialize the data set with the start date of the time period
num_states_with_intervention<-data.frame("Start_Date" = unique((main_analysis_data$Time_Period_Start)))
numStates<-c()

#for each time period i, we first find the states where the first intervention date occurred before i
#then, we append it to numStates
for(i in unique((num_states_with_intervention$Start_Date))){
  states_w_int<-unique(main_analysis_data$State[(main_analysis_data$Intervention_First_Date)<=i])
  numStates<-append(numStates, length(states_w_int[!is.na(states_w_int)]))
}
num_states_with_intervention$numStates<-numStates
num_states_with_intervention$Start_Date <- as.Date(num_states_with_intervention$Start_Date)
num_states_with_intervention <- rbind(data.frame("Start_Date" = c(as.Date("2000-01-01"), 
                                                                  as.Date("2017-12-31")), 
                                                 "numStates" = c(0, max(num_states_with_intervention$numStates))), 
                                      num_states_with_intervention)
num_states_with_intervention <- num_states_with_intervention %>% arrange(Start_Date) %>% 
  mutate(lag_numStates = lag(numStates))

num_states_with_intervention <- num_states_with_intervention %>% 
  pivot_longer( c("lag_numStates", "numStates"), "numStates")

# pdf("Figures/num_states_with_intervention_11_29_20.pdf")
ggplot(num_states_with_intervention, aes(x = Start_Date, y = value, group = 1)) + 
  # geom_point() +
  geom_line() +
  labs(x = "Year", y = "Number of States") +
  theme(axis.text=element_text(family="Times",size=10),
        axis.title=element_text(family="Times", size=10, face="bold"), 
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(family="Times", size=10),
        panel.background = element_rect("white")) + 
  scale_x_date(date_labels="%Y", breaks = seq(as.Date("2000-01-01"), as.Date("2018-01-01"), by = "2 years")) 

# dev.off()


##############################################################################
####################### Create Table of the Policy Dates #####################
policy_dates <- main_analysis_data %>% group_by(State) %>%
  summarise(unique(format(Intervention_First_Date, "%Y-%m")),
            unique(format(as.Date(Naloxone_Pharmacy_Yes_First_Date), "%Y-%m")),
            unique(format(as.Date(Naloxone_Pharmacy_No_First_Date), "%Y-%m")),
            unique(format(as.Date(Medical_Marijuana_First_Date), "%Y-%m")),
            unique(format(as.Date(Recreational_Marijuana_First_Date), "%Y-%m")),
            unique(format(as.Date(PDMP_First_Date), "%Y-%m")),
            unique(format(as.Date(GSL_First_Date), "%Y-%m")),
            unique(format(as.Date(Medicaid_Expansion_First_Date), "%Y-%m")))
names(policy_dates) <- c("State", "DIH Prosecutions", "NAL: Pharmacists Yes",
                         "NAL: Pharmacists No", "MML", "RML", "PDMP", "GSL",
                         "Medicaid")
# write.csv(policy_dates, "./Data/policy_dates.csv")

###############################################################################
############ Main Analysis: DIH with Spline Time Effects by Region ############
#model that we will be using for the main analysis
#cr is used for cubic regression spline -- we are smoothing time effects by region
#run the analysis for all the states
main_analysis_model<-gam(cbind(round(imputed_deaths), round(num_alive))~ State +
                           s(Time_Period_ID, bs = "cr", by = as.factor(Region)) +
                           Naloxone_Pharmacy_Yes_Redefined +
                           Naloxone_Pharmacy_No_Redefined +
                           Medical_Marijuana_Redefined +
                           Recreational_Marijuana_Redefined +
                           GSL_Redefined +
                           PDMP_Redefined +
                           Medicaid_Expansion_Redefined +
                           Intervention_Redefined,
                         data = main_analysis_data, family = "binomial")

#summary output of the model
summary(main_analysis_model)
gam.check(main_analysis_model)

############################## Model Plots ###############################
#diagnostic plots to check model
#deviance residuals plot
residTab <- data.frame(logit_fitted_vals = logit(fitted(main_analysis_model)),
                       resids = residuals(main_analysis_model))
# pdf("./Figures/deviance_resids_10_23_20.pdf")
ggplot(residTab, aes(x = logit_fitted_vals, y = resids)) +
  geom_point() +
  theme(text = element_text(size = 10, family = "Times"),
        title = element_text(size = 10, family = "Times", face = "bold"),
        panel.background = element_rect(fill = "white", color = "black")) +
  # theme_classic() +
  labs(x = "Logitic Function of Fitted Values", y = "Deviance Residuals")
# dev.off()

#binned residual plots
pred_vals <- predict(main_analysis_model)
resids <- resid(main_analysis_model, type = "response")
# pdf("./Figures/binned_resids_plot_11_4_20.pdf")
par(font.lab = 2)
par(family = "Times")
binnedplot(pred_vals, resids, main = "", xlab = "Average Logistic Function of Fitted Values",
           ylab = "Average Residuals")
# dev.off()

#plot the smoothed effects
main_analysis_model_object <- getViz(main_analysis_model)

midwest_plot <- plot(sm(main_analysis_model_object, 1)) +
  l_fitLine() +
  l_ciLine(mul = 5, linetype = 2)  + theme_classic() +
  labs(x = "Time Period", y = "Smoothed Time Effects for Midwest") +
  scale_x_continuous(breaks=c(1,11,21,31), labels=c("2000", "2005",
                                                    "2010", "2015"))  +
  theme(text=element_text(family="Times",size=10),
        title = element_text(family="Times", size=10, face = "bold"),
        panel.background = element_rect("white")) +
  ylim(c(-1,1.2))

northeast_plot <- plot(sm(main_analysis_model_object,2)) +
  l_fitLine() +
  l_ciLine(mul = 5, linetype = 2)  + theme_classic() +
  labs(x = "Time Period", y = "Smoothed Time Effects for Northeast") +
  scale_x_continuous(breaks=c(1,11,21,31), labels=c("2000", "2005",
                                                    "2010", "2015"))+
  theme(text=element_text(family="Times",size=10),
        title = element_text(family="Times", size=10, face = "bold"),
        panel.background = element_rect("white")) +
  ylim(c(-1,1.2))

south_plot <- plot(sm(main_analysis_model_object, 3)) +
  l_fitLine() +
  l_ciLine(mul = 5, linetype = 2)  + theme_classic() +
  labs(x = "Time Period", y = "Smoothed Time Effects for South") +
  scale_x_continuous(breaks=c(1,11,21,31), labels=c("2000", "2005",
                                                    "2010", "2015"))+
  theme(text=element_text(family="Times",size=10),
        title = element_text(family="Times", size=10, face = "bold"),
        panel.background = element_rect("white")) +
  ylim(c(-1,1.2))

west_plot <- plot(sm(main_analysis_model_object, 4)) +
  l_fitLine() +
  l_ciLine(mul = 5, linetype = 2)  + theme_classic() +
  labs(x = "Time Period", y = "Smoothed Time Effects for West") +
  scale_x_continuous(breaks=c(1,11,21,31), labels=c("2000", "2005",
                                                    "2010", "2015"))+
  theme(text=element_text(family="Times",size=10),
        title = element_text(family="Times", size=10, face = "bold"),
        panel.background = element_rect("white")) +
  ylim(c(-1,1.2))

# pdf("./Figures/time_smoothed_effects.pdf")
gridPrint(midwest_plot, northeast_plot, south_plot, west_plot, ncol = 2)
# dev.off()

#see which date corresponds to the median of intervention time
main_analysis_data %>% group_by(Time_Period_Start) %>%
  summarise(prop_w_intervention = mean(Intervention_Redefined > 0)) %>%
  View()


#create a data frame to store the results and compute the confidence intervals
#initialize the columns
main_analysis_plot_table<-data.frame(State = main_analysis_data$State)
main_analysis_plot_table$Fitted<-rep(NA, nrow(main_analysis_plot_table))
main_analysis_plot_table$Observed<-rep(NA, nrow(main_analysis_plot_table))
main_analysis_plot_table$Time<-main_analysis_data$Time_Period_ID
main_analysis_plot_table$Time_Date<-main_analysis_data$Time_Period_Start
main_analysis_plot_table$Intervention_Date<-main_analysis_data$Intervention_First_Date

#we want to compare the fitted probability of overdose death and the observed values to see how the model does as a goodness of fit visual test
for(i in unique(main_analysis_plot_table$State)){
  #for each state, we first subset the main analysis data to only look at the data for that state
  index_of_state<-which(main_analysis_plot_table$State == i)
  #impute the fitted and observed probability of overdose death for the state
  main_analysis_plot_table$Fitted[index_of_state]<-fitted(main_analysis_model)[index_of_state]
  main_analysis_plot_table$Observed[index_of_state]<-(main_analysis_data$imputed_deaths[main_analysis_data$State == i]/main_analysis_data$population[main_analysis_data$State == i])
}

#plot to compare the fitted values vs observed deaths
# pdf("./Figures/GAM_fitted_vs_actual_by_Region_10_23_20_with_int_date_full_data.pdf")
ggplot(data = main_analysis_plot_table, aes(x = Time_Date, y = Observed*100000, group = 1)) +
  geom_line(color = "blue")+
  geom_line(data = main_analysis_plot_table, aes(x = Time_Date, y = Fitted*100000, group = 1), col = "red",
            linetype = "dashed") +
  geom_vline(main_analysis_plot_table, mapping = aes(xintercept = Intervention_Date)) +
  facet_wrap(facets = vars(State), scales = "free_y", ncol = 5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10, family = "Times"),
        axis.text.y = element_text(size = 6, family = "Times"),
        axis.title=element_text(size = 10,face = "bold", family = "Times"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size=8),
        panel.background = element_rect("white")) +
  labs(x = "Year", y = "Drug Overdose Death Rates per 100,000 People")
# dev.off()

########### Main Analysis: Make Data Frame of Results and 95% CI #############
#store the coefficients into the table
main_analysis_full_table<-data.frame(coef(main_analysis_model))
#check to see how the table looks
head(main_analysis_full_table)
#rename the column to "Coefficient_Estimate"
colnames(main_analysis_full_table)<-c("Coefficient_Estimate")

#vector of covariates
covariates<-c("Naloxone_Pharmacy_Yes_Redefined", "Naloxone_Pharmacy_No_Redefined",
              "Medical_Marijuana_Redefined",
              "Recreational_Marijuana_Redefined",
              "GSL_Redefined", "PDMP_Redefined",
              "Medicaid_Expansion_Redefined", "Intervention_Redefined")

#rename the variable names of the regression output so that they look nicer:
#currently there are 3 types of coefficients: state effects, the covariates, and smoothed time effects
#for each row in the main analysis table
for(i in 1:length(rownames(main_analysis_full_table))){

  #if the coefficient is not in the covariates vector
  if(!(rownames(main_analysis_full_table)[i] %in% covariates)){

    #we see if it's a state effect
    if(substr(rownames(main_analysis_full_table)[i], start = 1, stop = 5) == "State"){

      #if so, here, the names look like: StateMassachusetts or StateGeorgia, so take out the "State" part
      #and just rename these rows to just the state name
      rownames(main_analysis_full_table)[i]<-substr(rownames(main_analysis_full_table)[i], start = 6,
                                                    stop = nchar(rownames(main_analysis_full_table)[i]))

    }else if(rownames(main_analysis_full_table)[i] == "(Intercept)"){

      #otherwise, if the current name is Intercept, we rename it so that we know that Alabama is the baseline
      rownames(main_analysis_full_table)[i]<-"Intercept/Alabama"

    }else if(substr(rownames(main_analysis_full_table)[i], start = 1, stop = 35) == "s(Time_Period_ID):as.factor(Region)"){

      #otherwise, it's the smoothed time effects which look like: s(Time_Period_ID):as.factor(Region)West
      #or s(Time_Period_ID):as.factor(Region)South, so we want to get rid of "s(Time_Period_ID):as.factor(Region)"
      #and change it to "Smoothed Time for Region"
      rownames(main_analysis_full_table)[i]<-paste("Smoothed Time for Region ",
                                                   substr(rownames(main_analysis_full_table)[i], start = 36,
                                                          stop = nchar(rownames(main_analysis_full_table)[i])),
                                                   sep = "")

    }
  }
}

#confidence intervals for the coefficients
main_analysis_full_table$Coefficient_Lower_Bound<-main_analysis_full_table$Coefficient_Estimate - 1.96*summary(main_analysis_model)$se
main_analysis_full_table$Coefficient_Upper_Bound<-main_analysis_full_table$Coefficient_Estimate + 1.96*summary(main_analysis_model)$se

#impute the estimates and confidence intervals in the odds ratio scale
main_analysis_full_table$Odds_Ratio<-exp(main_analysis_full_table$Coefficient_Estimate)
main_analysis_full_table$Odds_Ratio_LB<-exp(main_analysis_full_table$Coefficient_Lower_Bound)
main_analysis_full_table$Odds_Ratio_UB<-exp(main_analysis_full_table$Coefficient_Upper_Bound)

#store the standard error and p-value
main_analysis_full_table$Standard_Error<-summary(main_analysis_model)$se
#note that there is no p-value for the smoothed time effects, so we put a NA for those rows
main_analysis_full_table$p_value<-c(summary(main_analysis_model)$p.pv, rep(NA, length(coef(main_analysis_model)) - length(summary(main_analysis_model)$p.pv)))

head(main_analysis_full_table)
tail(main_analysis_full_table)

#save the table into a CSV
# write.csv(round(main_analysis_full_table,5), "./Data/coefficients_GAM_9_1_20_full_data_uninentional_od.csv")

#export a table with just the covariates
#first, find the rows that contains the covariates
covariate_Index<-which(rownames(main_analysis_full_table) %in% covariates)
main_analysis_covariate_table<-(round(main_analysis_full_table[covariate_Index,], 5))

#rename the variables so that it looks cleaner
rownames(main_analysis_covariate_table)<-c("Naloxone_Pharmacy_Yes", "Naloxone_Pharmacy_No",
                                           "Medical_Marijuana",
                                           "Recreational_Marijuana",
                                           "GSL", "PDMP", "Medicaid_Expansion",
                                           "Intervention")

#now, reorganize the data so that the covariates are on top and the rest of the variable sare below
main_analysis_covariate_table<-rbind(main_analysis_covariate_table, main_analysis_full_table[-covariate_Index,])
#remove the columns that aren't in odds ratio scale
main_analysis_covariate_table<-main_analysis_covariate_table[,-which(colnames(main_analysis_covariate_table) %in%
                                                                       c("Coefficient_Estimate", "Coefficient_Lower_Bound", "Coefficient_Upper_Bound", "Standard_Error"))]

colnames(main_analysis_covariate_table)<-c("Risk_Ratio_Estimates", "RR_95_CI_LB", "RR_95_CI_UB", "p-value")
head(main_analysis_covariate_table, 10)

#save the table into a CSV
# write.csv(round(main_analysis_covariate_table, 3), "./Data/coefficients_covariates_9_1_20_full_data_unintentional_od.csv")


######### Main Analysis: Number of Overdose Deaths Attributed to Intervention #########
#find the number of deaths attributable to the intervention
#first, we subset the data so that we only focus on the time points for which at least one state had the intervention
attr_deaths_anlys_main_analysis<-main_analysis_data[which(main_analysis_data$Intervention_Redefined>0),]

#compute the probability of overdose had intervention not occurred
prob_od_no_int_main_analysis<-expit(-coef(main_analysis_model)["Intervention_Redefined"]*attr_deaths_anlys_main_analysis$Intervention_Redefined
                                    + logit(attr_deaths_anlys_main_analysis$imputed_deaths/attr_deaths_anlys_main_analysis$population))

#compute the lower and upper bounds of 95% CI of probability of overdose had intervention not occurred
#here, we compute the lower and upper bounds of the 95% CI of all the coefficients using the standard error from the model
coef_lb<-coef(main_analysis_model) - 1.96*summary(main_analysis_model)$se
coef_ub<-coef(main_analysis_model) + 1.96*summary(main_analysis_model)$se

#we then calculate the upper and lower bounds of the probability of overdose death had intervention not occurred by using
#the lower and upper bounds of the coefficient of the intervention variable
prob_od_no_int_LB_main_analysis<-expit(-coef_lb[names(coef_lb) == "Intervention_Redefined"]*attr_deaths_anlys_main_analysis$Intervention_Redefined
                                       + logit(attr_deaths_anlys_main_analysis$imputed_deaths/attr_deaths_anlys_main_analysis$population))

prob_od_no_int_UB_main_analysis<-expit(-coef_ub[names(coef_ub) == "Intervention_Redefined"]*attr_deaths_anlys_main_analysis$Intervention_Redefined
                                       + logit(attr_deaths_anlys_main_analysis$imputed_deaths/attr_deaths_anlys_main_analysis$population))


#estimate the number of deaths attributable to the intervention
#first, initialize the vectors to store the numbers
num_attr_od_UB<-num_attr_od_LB<-num_attr_od<-rep(NA, length(unique(attr_deaths_anlys_main_analysis$Time_Period_ID)))


#for each time period, we first find the indices of rows containing data from that time point
#then, we find the total number of deaths that attributable to the intervention

index<-1 #keep track of where to store the values in the vector

for(time in sort(unique(attr_deaths_anlys_main_analysis$Time_Period_ID))){
  #find the indices of rows where the time point = time
  time_point_index<-which(attr_deaths_anlys_main_analysis$Time_Period_ID == time)

  #find the number of deaths attributable to intervention = observed number of deaths with intervention - estimated number of deaths had intervention not occurred
  num_attr_od[index]<-sum(attr_deaths_anlys_main_analysis$imputed_deaths[time_point_index]
                          - prob_od_no_int_main_analysis[time_point_index]*attr_deaths_anlys_main_analysis$population[time_point_index])
  #find the lower and upper bounds of the estimated number of deaths attributable to the intervention
  num_attr_od_LB[index]<-sum(attr_deaths_anlys_main_analysis$imputed_deaths[time_point_index]
                             - prob_od_no_int_LB_main_analysis[time_point_index]*attr_deaths_anlys_main_analysis$population[time_point_index])
  num_attr_od_UB[index]<-sum(attr_deaths_anlys_main_analysis$imputed_deaths[time_point_index]
                             - prob_od_no_int_UB_main_analysis[time_point_index]*attr_deaths_anlys_main_analysis$population[time_point_index])
  index<-index + 1
}

num_attr_od_main_analysis<-data.frame("Time_Period_ID" = sort(unique(attr_deaths_anlys_main_analysis$Time_Period_ID)),
                                      "Time_Start" = sort(unique(attr_deaths_anlys_main_analysis$Time_Period_Start)),
                                      "Num_Attr_Deaths" = num_attr_od,
                                      "Num_Attr_Deaths_LB" = num_attr_od_LB,
                                      "Num_Attr_Deaths_UB" = num_attr_od_UB)

#sum up the total number of excess deaths attributable to the intervention
sum(num_attr_od_main_analysis$Num_Attr_Deaths) #32346.65
summary(num_attr_od_main_analysis$Num_Attr_Deaths)
num_attr_od_main_analysis$Time_Start<-as.Date(num_attr_od_main_analysis$Time_Start)

#compute the 95% CI for the total
sum(num_attr_od_main_analysis$Num_Attr_Deaths_LB) #27563.78
sum(num_attr_od_main_analysis$Num_Attr_Deaths_UB) #37075.09

# pdf("Figures/num_attr_deaths_w_CI_full_data_9_1_20.pdf")
ggplot(num_attr_od_main_analysis, aes(x = Time_Start, y = Num_Attr_Deaths, group = 1), color = "Estimated Attributable Deaths",
       linetype = "Estimated Attributable Deaths") +
  geom_line() + geom_point()  +
  geom_line(num_attr_od_main_analysis, mapping = aes(x = Time_Start, y = Num_Attr_Deaths_LB, group = 1),
            color = 'red',linetype = 'dashed') +
  geom_line(num_attr_od_main_analysis, mapping = aes(x = Time_Start, y = Num_Attr_Deaths_UB, group = 1), ,
            color = 'red', linetype = 'dashed') +
  theme(axis.text=element_text(size=16), axis.title=element_text(size=15,face="bold"),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_rect("white")) +
  labs(x = "Year", y = "Estimated Number of Deaths Attributable to DIH Prosecutions") +
  scale_color_manual(values=c('red','black'))
# dev.off()


#sum up the number of excess deaths per year
yearly_num_Attr_Deaths_main_analysis<-num_attr_od_main_analysis %>%
  group_by("year" = year(Time_Start)) %>%
  summarise("deaths" = sum(Num_Attr_Deaths), death_lb = sum(Num_Attr_Deaths_LB),
            death_ub = sum(Num_Attr_Deaths_UB))

summary(yearly_num_Attr_Deaths_main_analysis$deaths) #avg: 1797.04
summary(yearly_num_Attr_Deaths_main_analysis$death_lb) #1531.32
summary(yearly_num_Attr_Deaths_main_analysis$death_ub) #2059.73

######################## Plot of Attributable Deaths ###############################
# pdf("Figures/num_attr_deaths_w_CI_full_data_10_9_20.pdf")
ggplot(yearly_num_Attr_Deaths_main_analysis, aes(x = year, y = deaths, group = 1), color = "Estimated Attributable Deaths",
       linetype = "Estimated Attributable Deaths") +
  geom_line() + geom_point()  +
  geom_line(yearly_num_Attr_Deaths_main_analysis, mapping = aes(x = year, y = death_lb, group = 1),
            color = 'red',linetype = 'dashed') +
  geom_line(yearly_num_Attr_Deaths_main_analysis, mapping = aes(x = year, y = death_ub, group = 1), ,
            color = 'red', linetype = 'dashed') +
  theme(axis.text=element_text(size=16), axis.title=element_text(size=15,face="bold"),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        panel.background = element_rect("white")) +
  labs(x = "Year", y = "Estimated Yearly Deaths Attributable to DIH Prosecutions") +
  scale_color_manual(values=c('red','black'))
# dev.off()

##############################################################################################3
############################## Secondary Analysis: All Overdose Deaths ###############################
od_all_data <- read.csv("./Data/full_data_set_9_1_20_all_od.csv")
colnames(od_all_data)[which(colnames(od_all_data) == "sum_deaths")] <- "imputed_deaths"
od_all_data$Time_Period_Start <- as.Date(od_all_data$Time_Period_Start)
od_all_data$Intervention_First_Date <- as.Date(od_all_data$Intervention_First_Date)

#initialize vector with "West" and then impute the other regions for the states
od_all_data$Region<-rep("West", nrow(od_all_data))
for(state in unique(od_all_data$State)){
  if(state %in% region.list$Northeast){
    od_all_data$Region[od_all_data$State == state]<-"Northeast"
  }else if(state %in% region.list$Midwest){
    od_all_data$Region[od_all_data$State == state]<-"Midwest"
  }else if(state %in% region.list$South){
    od_all_data$Region[od_all_data$State == state]<-"South"
  }
}

#model that we will be using for the analysis
#cr is used for cubic regression spline -- we are smoothing time effects by region
#run the analysis for all the states
od_all_model<-gam(cbind(round(imputed_deaths), round(num_alive))~ State +
                    s(Time_Period_ID, bs = "cr", by = as.factor(Region)) +
                    Naloxone_Pharmacy_Yes_Redefined +
                    Naloxone_Pharmacy_No_Redefined +
                    Medical_Marijuana_Redefined +
                    Recreational_Marijuana_Redefined +
                    GSL_Redefined +
                    PDMP_Redefined +
                    Medicaid_Expansion_Redefined +
                    Intervention_Redefined,
                  data = od_all_data, family = "binomial")

#summary output of the model
summary(od_all_model)
gam.check(od_all_model)

#create a data frame to store the results and compute the confidence intervals
#initialize the columns
od_all_plot_table<-data.frame(State = od_all_data$State)
od_all_plot_table$Fitted<-rep(NA, nrow(od_all_plot_table))
od_all_plot_table$Observed<-rep(NA, nrow(od_all_plot_table))
od_all_plot_table$Time<-od_all_data$Time_Period_ID
od_all_plot_table$Time_Date<-od_all_data$Time_Period_Start
od_all_plot_table$Intervention_Date<-od_all_data$Intervention_First_Date

#we want to compare the fitted probability of overdose death and the observed values to see how the model does as a goodness of fit visual test
for(i in unique(od_all_plot_table$State)){
  #for each state, we first subset the main analysis data to only look at the data for that state
  index_of_state<-which(od_all_plot_table$State == i)
  #impute the fitted and observed probability of overdose death for the state
  od_all_plot_table$Fitted[index_of_state]<-fitted(od_all_model)[index_of_state]
  od_all_plot_table$Observed[index_of_state]<-(od_all_data$imputed_deaths[od_all_data$State == i]/od_all_data$population[od_all_data$State == i])
}

#plot to compare the fitted values vs observed deaths
# pdf("./Figures/GAM_fitted_vs_actual_by_Region_8_10_20_with_int_date_full_data_all_od.pdf")
ggplot(data = od_all_plot_table, aes(x = Time_Date, y = Observed*100000, group = 1)) +
  geom_line(color = "blue")+
  geom_line(data = od_all_plot_table, aes(x = Time_Date, y = Fitted*100000, group = 1), col = "red",
            linetype = "dashed") +
  geom_vline(od_all_plot_table, mapping = aes(xintercept = Intervention_Date)) +
  facet_wrap(facets = vars(State)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7), axis.text.y = element_text(size = 7),
        axis.title=element_text(size = 15,face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size=8),panel.background = element_rect("white")) +
  labs(x = "Year", y = "Drug Overdose Death Rates per 100,000 People")
# dev.off()

#plot to show the time smooth effects by region
# pdf("./Figures/time_smoothed_effects_mw_ne_s_w_4_19_20_full_data.pdf")
plot(od_all_model, ylab = "Smoothed Time Effects", xlab = "Time", pages = 1)
# dev.off()

#diagnostic plots to check model
gam.check(od_all_model)
# pdf("./Figures/deviance_resids_4_19_20.pdf")
plot(logit(fitted(od_all_model)), residuals(od_all_model),
     xlab = "Logit(Fitted Values)", ylab = "Deviance Residuals" )
# dev.off()

############## Secondary Analysis: Make Data Frame of Results and 95% CI ###########
#store the coefficients into the table
od_all_full_table<-data.frame(coef(od_all_model))
#check to see how the table looks
head(od_all_full_table)
#rename the column to "Coefficient_Estimate"
colnames(od_all_full_table)<-c("Coefficient_Estimate")

#vector of covariates
covariates<-c("Naloxone_Pharmacy_Yes_Redefined", "Naloxone_Pharmacy_No_Redefined",
              "Medical_Marijuana_Redefined",
              "Recreational_Marijuana_Redefined",
              "GSL_Redefined", "PDMP_Redefined",
              "Medicaid_Expansion_Redefined", "Intervention_Redefined")

#rename the variable names of the regression output so that they look nicer:
#currently there are 3 types of coefficients: state effects, the covariates, and smoothed time effects
#for each row in the main analysis table
for(i in 1:length(rownames(od_all_full_table))){

  #if the coefficient is not in the covariates vector
  if(!(rownames(od_all_full_table)[i] %in% covariates)){

    #we see if it's a state effect
    if(substr(rownames(od_all_full_table)[i], start = 1, stop = 5) == "State"){

      #if so, here, the names look like: StateMassachusetts or StateGeorgia, so take out the "State" part
      #and just rename these rows to just the state name
      rownames(od_all_full_table)[i]<-substr(rownames(od_all_full_table)[i], start = 6,
                                             stop = nchar(rownames(od_all_full_table)[i]))

    }else if(rownames(od_all_full_table)[i] == "(Intercept)"){

      #otherwise, if the current name is Intercept, we rename it so that we know that Alabama is the baseline
      rownames(od_all_full_table)[i]<-"Intercept/Alabama"

    }else if(substr(rownames(od_all_full_table)[i], start = 1, stop = 35) == "s(Time_Period_ID):as.factor(Region)"){

      #otherwise, it's the smoothed time effects which look like: s(Time_Period_ID):as.factor(Region)West
      #or s(Time_Period_ID):as.factor(Region)South, so we want to get rid of "s(Time_Period_ID):as.factor(Region)"
      #and change it to "Smoothed Time for Region"
      rownames(od_all_full_table)[i]<-paste("Smoothed Time for Region ",
                                            substr(rownames(od_all_full_table)[i], start = 36,
                                                   stop = nchar(rownames(od_all_full_table)[i])),
                                            sep = "")

    }
  }
}

#confidence intervals for the coefficients
od_all_full_table$Coefficient_Lower_Bound<-od_all_full_table$Coefficient_Estimate - 1.96*summary(od_all_model)$se
od_all_full_table$Coefficient_Upper_Bound<-od_all_full_table$Coefficient_Estimate + 1.96*summary(od_all_model)$se

#impute the estimates and confidence intervals in the odds ratio scale
od_all_full_table$Odds_Ratio<-exp(od_all_full_table$Coefficient_Estimate)
od_all_full_table$Odds_Ratio_LB<-exp(od_all_full_table$Coefficient_Lower_Bound)
od_all_full_table$Odds_Ratio_UB<-exp(od_all_full_table$Coefficient_Upper_Bound)

#store the standard error and p-value
od_all_full_table$Standard_Error<-summary(od_all_model)$se
#note that there is no p-value for the smoothed time effects, so we put a NA for those rows
od_all_full_table$p_value<-c(summary(od_all_model)$p.pv, rep(NA, length(coef(od_all_model)) - length(summary(od_all_model)$p.pv)))

head(od_all_full_table)
tail(od_all_full_table)

#save the table into a CSV
# write.csv(round(od_all_full_table,5), "./Data/coefficients_GAM_9_1_20_full_data_all_od.csv")

#export a table with just the covariates
#first, find the rows that contains the covariates
covariate_Index<-which(rownames(od_all_full_table) %in% covariates)
od_all_covariate_table<-(round(od_all_full_table[covariate_Index,], 5))

#rename the variables so that it looks cleaner
rownames(od_all_covariate_table)<-c("Naloxone_Pharmacy_Yes", "Naloxone_Pharmacy_No",
                                    "Medical_Marijuana",
                                    "Recreational_Marijuana",
                                    "GSL", "PDMP", "Medicaid_Expansion",
                                    "Intervention")

#now, reorganize the data so that the covariates are on top and the rest of the variable sare below
od_all_covariate_table<-rbind(od_all_covariate_table, od_all_full_table[-covariate_Index,])
#remove the columns that aren't in odds ratio scale
od_all_covariate_table<-od_all_covariate_table[,-which(colnames(od_all_covariate_table) %in%
                                                         c("Coefficient_Estimate", "Coefficient_Lower_Bound", "Coefficient_Upper_Bound", "Standard_Error"))]

colnames(od_all_covariate_table)<-c("Risk_Ratio_Estimates", "RR_95_CI_LB", "RR_95_CI_UB", "p-value")
head(od_all_covariate_table, 10)

#save the table into a CSV
# write.csv(round(od_all_covariate_table, 5), "./Data/coefficients_covariates_9_1_20_full_data_all_od.csv")


############ Secondary Analysis: Number of Overdose Attributable Deaths ###############
#find the number of deaths attributable to the intervention
#first, we subset the data so that we only focus on the time points for which at least one state had the intervention
attr_deaths_anlys_od_all<-od_all_data[which(od_all_data$Intervention_Redefined>0),]

#compute the probability of overdose had intervention not occurred
prob_od_no_int_od_all<-expit(-coef(od_all_model)["Intervention_Redefined"]*attr_deaths_anlys_od_all$Intervention_Redefined
                             + logit(attr_deaths_anlys_od_all$imputed_deaths/attr_deaths_anlys_od_all$population))

#compute the lower and upper bounds of 95% CI of probability of overdose had intervention not occurred
#here, we compute the lower and upper bounds of the 95% CI of all the coefficients using the standard error from the model
coef_lb<-coef(od_all_model) - 1.96*summary(od_all_model)$se
coef_ub<-coef(od_all_model) + 1.96*summary(od_all_model)$se

#we then calculate the upper and lower bounds of the probability of overdose death had intervention not occurred by using
#the lower and upper bounds of the coefficient of the intervention variable
prob_od_no_int_LB_od_all<-expit(-coef_lb[names(coef_lb) == "Intervention_Redefined"]*attr_deaths_anlys_od_all$Intervention_Redefined
                                + logit(attr_deaths_anlys_od_all$imputed_deaths/attr_deaths_anlys_od_all$population))

prob_od_no_int_UB_od_all<-expit(-coef_ub[names(coef_ub) == "Intervention_Redefined"]*attr_deaths_anlys_od_all$Intervention_Redefined
                                + logit(attr_deaths_anlys_od_all$imputed_deaths/attr_deaths_anlys_od_all$population))


#estimate the number of deaths attributable to the intervention
#first, initialize the vectors to store the numbers
num_attr_od_UB<-num_attr_od_LB<-num_attr_od<-rep(NA, length(unique(attr_deaths_anlys_od_all$Time_Period_ID)))


#for each time period, we first find the indices of rows containing data from that time point
#then, we find the total number of deaths that attributable to the intervention

index<-1 #keep track of where to store the values in the vector

for(time in sort(unique(attr_deaths_anlys_od_all$Time_Period_ID))){
  #find the indices of rows where the time point = time
  time_point_index<-which(attr_deaths_anlys_od_all$Time_Period_ID == time)

  #find the number of deaths attributable to intervention = observed number of deaths with intervention - estimated number of deaths had intervention not occurred
  num_attr_od[index]<-sum(attr_deaths_anlys_od_all$imputed_deaths[time_point_index]
                          - prob_od_no_int_od_all[time_point_index]*attr_deaths_anlys_od_all$population[time_point_index])
  #find the lower and upper bounds of the estimated number of deaths attributable to the intervention
  num_attr_od_LB[index]<-sum(attr_deaths_anlys_od_all$imputed_deaths[time_point_index]
                             - prob_od_no_int_LB_od_all[time_point_index]*attr_deaths_anlys_od_all$population[time_point_index])
  num_attr_od_UB[index]<-sum(attr_deaths_anlys_od_all$imputed_deaths[time_point_index]
                             - prob_od_no_int_UB_od_all[time_point_index]*attr_deaths_anlys_od_all$population[time_point_index])
  index<-index + 1
}

num_attr_od_od_all<-data.frame("Time_Period_ID" = sort(unique(attr_deaths_anlys_od_all$Time_Period_ID)),
                               "Time_Start" = sort(unique(attr_deaths_anlys_od_all$Time_Period_Start)),
                               "Num_Attr_Deaths" = num_attr_od,
                               "Num_Attr_Deaths_LB" = num_attr_od_LB,
                               "Num_Attr_Deaths_UB" = num_attr_od_UB)

#sum up the total number of excess deaths attributable to the intervention
sum(num_attr_od_od_all$Num_Attr_Deaths) #16336.4
summary(num_attr_od_od_all$Num_Attr_Deaths)
num_attr_od_od_all$Time_Start<-as.Date(num_attr_od_od_all$Time_Start)

#compute the 95% CI for the total
sum(num_attr_od_od_all$Num_Attr_Deaths_LB) #10884.71
sum(num_attr_od_od_all$Num_Attr_Deaths_UB) #21736.28

#sum up the number of excess deaths per year
yearly_num_Attr_Deaths_od_all<-num_attr_od_od_all %>%
  group_by("year" = year(Time_Start)) %>%
  summarise("deaths" = sum(Num_Attr_Deaths), death_lb = sum(Num_Attr_Deaths_LB),
            death_ub = sum(Num_Attr_Deaths_UB))

summary(yearly_num_Attr_Deaths_od_all$deaths) #907.58
summary(yearly_num_Attr_Deaths_od_all$death_lb) #604.71
summary(yearly_num_Attr_Deaths_od_all$death_ub) #1207.57

#############################################################################################
############################## Sensitivity Analyses ########################################

########### Sensitivity Analysis 1: Confounding by Indication Analysis ###############
#create a variable that is equal to 1 just before intervention
#initialize the column to all zeros
main_analysis_data$justBeforeIntervention<-0

#for each state, we first subset the data so it only contains the state's data
for(state in unique(main_analysis_data$State)){
  state_data<-main_analysis_data[main_analysis_data$State == state,]
  #then, we find the first time point where intervention occurred
  index_first_intervention<-which(state_data$Intervention_Redefined>0)[1]
  #impute a 1 for the time point right before when intervention first occurs
  main_analysis_data$justBeforeIntervention[main_analysis_data$State == state][index_first_intervention-1]<-1
}

#subset the data so that we are only looking at the periods before the intervention occurs for each state
sensitivity_anlys_conf_by_indication_data<-data.frame()
for(state in unique(main_analysis_data$State)){
  state_data<-main_analysis_data[main_analysis_data$State == state,]
  #we don't include these states because Georgia and Ohio have intervention in 2000
  #Hawaii is in this list because it doesn't have an intervention, so we will encounter an error
  #if the state is Hawaii, we'll go to the else if condition
  if(!(state %in% c("Hawaii", "Georgia", "Ohio"))){
    #look for the index where it is just before the intervention
    index_first_intervention<-which(state_data$justBeforeIntervention>0)
    #add the rows that occur before the intervention to the sensitivity analysis data
    sensitivity_anlys_conf_by_indication_data<-rbind(sensitivity_anlys_conf_by_indication_data, state_data[1:(index_first_intervention),])

  }else if(state == "Hawaii"){
    #Hawaii doesn't have an intervention, so we want to include all the rows of Hawaii
    sensitivity_anlys_conf_by_indication_data<-rbind(sensitivity_anlys_conf_by_indication_data, state_data)
  }
}

#run the analysis for the sensitivity analysis
sensitivity_anlys_conf_by_indication<-gam(cbind(round(imputed_deaths), round(num_alive))~ State +
                                            s(Time_Period_ID, bs = "cr", by = as.factor(Region)) +
                                            Naloxone_Pharmacy_Yes_Redefined +
                                            Naloxone_Pharmacy_No_Redefined +
                                            Medical_Marijuana_Redefined +
                                            Recreational_Marijuana_Redefined +
                                            GSL_Redefined +
                                            PDMP_Redefined +
                                            Medicaid_Expansion_Redefined +
                                            justBeforeIntervention,
                                          data = sensitivity_anlys_conf_by_indication_data, family = "binomial")
summary(sensitivity_anlys_conf_by_indication)

############ Sensitivity Analysis 1: Make Data Frame of Results and 95% CI ############
#store the coefficients of all the terms into a table
sensitivity_anlys_conf_by_indication_full_table<-data.frame(coef(sensitivity_anlys_conf_by_indication))
head(sensitivity_anlys_conf_by_indication_full_table)
#change the column name to "Coefficient_Estimate"
colnames(sensitivity_anlys_conf_by_indication_full_table)<-c("Coefficient_Estimate")

#store a vector of the covariates
covariates<-c("Naloxone_Pharmacy_Yes_Redefined", "Naloxone_Pharmacy_No_Redefined",
              "Medical_Marijuana_Redefined",
              "Recreational_Marijuana_Redefined",
              "GSL_Redefined", "PDMP_Redefined",
              "Medicaid_Expansion_Redefined", "justBeforeIntervention")

#rename the variable names of the regression output so that they look nicer:
#currently there are 3 types of coefficients: state effects, the covariates, and smoothed time effects
#for each row in the main analysis table
for(i in 1:length(rownames(sensitivity_anlys_conf_by_indication_full_table))){

  #if the coefficient is not in the covariates vector
  if(!(rownames(sensitivity_anlys_conf_by_indication_full_table)[i] %in% covariates)){

    #we see if it is a state effect
    if(substr(rownames(sensitivity_anlys_conf_by_indication_full_table)[i], start = 1, stop = 5) == "State"){

      #if so, here, the names look like: StateMassachusetts or StateGeorgia, so take out the "State" part
      #and just rename these rows to just the state name
      rownames(sensitivity_anlys_conf_by_indication_full_table)[i]<-substr(rownames(sensitivity_anlys_conf_by_indication_full_table)[i], start = 6,
                                                                           stop = nchar(rownames(sensitivity_anlys_conf_by_indication_full_table)[i]))

    }else if(rownames(sensitivity_anlys_conf_by_indication_full_table)[i] == "(Intercept)"){

      #otherwise, if the current name is Intercept, we rename it so that we know that Alabama is the baseline
      rownames(sensitivity_anlys_conf_by_indication_full_table)[i]<-"Intercept/Alabama"

    }else if(substr(rownames(sensitivity_anlys_conf_by_indication_full_table)[i], start = 1, stop = 35) == "s(Time_Period_ID):as.factor(Region)"){

      #otherwise, it's the smoothed time effects which look like: s(Time_Period_ID):as.factor(Region)West
      #or s(Time_Period_ID):as.factor(Region)South, so we want to get rid of "s(Time_Period_ID):as.factor(Region)"
      #and change it to "Smoothed Time for Region"
      rownames(sensitivity_anlys_conf_by_indication_full_table)[i]<-paste("Smoothed Time for Region ",
                                                                          substr(rownames(sensitivity_anlys_conf_by_indication_full_table)[i], start = 36,
                                                                                 stop = nchar(rownames(sensitivity_anlys_conf_by_indication_full_table)[i])),
                                                                          sep = "")


    }
  }
}

#store the 95% CI
sensitivity_anlys_conf_by_indication_full_table$Coefficient_Lower_Bound<-sensitivity_anlys_conf_by_indication_full_table$Coefficient_Estimate - 1.96*summary(sensitivity_anlys_conf_by_indication)$se
sensitivity_anlys_conf_by_indication_full_table$Coefficient_Upper_Bound<-sensitivity_anlys_conf_by_indication_full_table$Coefficient_Estimate + 1.96*summary(sensitivity_anlys_conf_by_indication)$se

#store the odds ratio estimates and 95% CI
sensitivity_anlys_conf_by_indication_full_table$Odds_Ratio<-exp(sensitivity_anlys_conf_by_indication_full_table$Coefficient_Estimate)
sensitivity_anlys_conf_by_indication_full_table$Odds_Ratio_LB<-exp(sensitivity_anlys_conf_by_indication_full_table$Coefficient_Lower_Bound)
sensitivity_anlys_conf_by_indication_full_table$Odds_Ratio_UB<-exp(sensitivity_anlys_conf_by_indication_full_table$Coefficient_Upper_Bound)

#store the standard error and p-values
sensitivity_anlys_conf_by_indication_full_table$Standard_Error<-summary(sensitivity_anlys_conf_by_indication)$se
#since there are no p-values for the smoothed time effects, it imputes NA for those rows
sensitivity_anlys_conf_by_indication_full_table$p_value<-c(summary(sensitivity_anlys_conf_by_indication)$p.pv,
                                                           rep(NA, length(coef(sensitivity_anlys_conf_by_indication)) - length(summary(sensitivity_anlys_conf_by_indication)$p.pv)))

head(sensitivity_anlys_conf_by_indication_full_table)
tail(sensitivity_anlys_conf_by_indication_full_table)

#export the sensitivity analysis confounding by indication full table of estimates as CSV
# write.csv(round(sensitivity_anlys_conf_by_indication_full_table,3), "./Data/coefficients_JustBeforeInd_9_1_20_full_data.csv")

#export out a table with just the covariates:
#find the rows in the full table which contain estimates for the covariates and extract them
covariate_Index<-which(rownames(sensitivity_anlys_conf_by_indication_full_table) %in% covariates)
sensitivity_anlys_conf_by_indication_covariate_table<-(round(sensitivity_anlys_conf_by_indication_full_table[covariate_Index,],5))

#rename these variables so they look nicer
rownames(sensitivity_anlys_conf_by_indication_covariate_table)<-c("Naloxone_Pharmacy_Yes", "Naloxone_Pharmacy_No",
                                                                  "Medical_Marijuana",
                                                                  "Recreational_Marijuana",
                                                                  "GSL", "PDMP", "Medicaid_Expansion",
                                                                  "Just Before Indicator")

#put the covariate rows on top and the rest of the data at the bottom
sensitivity_anlys_conf_by_indication_covariate_table<-rbind(sensitivity_anlys_conf_by_indication_covariate_table,
                                                            sensitivity_anlys_conf_by_indication_full_table[-covariate_Index,])

#extract the columns that gives the odds ratio estimates
sensitivity_anlys_conf_by_indication_covariate_table<-sensitivity_anlys_conf_by_indication_covariate_table[,-which(colnames(sensitivity_anlys_conf_by_indication_covariate_table) %in%
                                                                                                                     c("Coefficient_Estimate", "Coefficient_Lower_Bound", "Coefficient_Upper_Bound", "Standard_Error"))]
colnames(sensitivity_anlys_conf_by_indication_covariate_table)<-c("Risk_Ratio_Estimates", "RR_95_CI_LB", "RR_95_CI_UB", "p-value")
head(sensitivity_anlys_conf_by_indication_covariate_table, 10)

#export the covariate table into a CSV file
# write.csv(round(sensitivity_anlys_conf_by_indication_covariate_table,3), "./Data/covariates_just_before_intervention_9_1_20_full_data.csv")

######################################################################################
######### Sensitivity Analysis 2: Exclude States with 75% missing monthly data #######
#subset the data to be between 2000 and 2017
overdose_monthly_imputed<-read.csv("./Data/od_data_interpolated_unintentional_9_1_20.csv")
od_2000_to_2017<-overdose_monthly_imputed[overdose_monthly_imputed$Year>=2000 & overdose_monthly_imputed$Year<=2017,]

#convert the date to Date objects and the number of deaths to numeric
od_2000_to_2017$Date<-as.Date(as.yearmon(od_2000_to_2017$Month.Code, format = "%Y/%m"))
od_2000_to_2017$Deaths<-as.numeric(od_2000_to_2017$Deaths)

#plot to look at the trend of the outcome and see how much data is missing
# pdf("Figures/od_outcome_data_by_state.pdf")
ggplot(od_2000_to_2017, aes(x = Date, y = Deaths)) + geom_line() + facet_wrap(vars(State))
# dev.off()

#find the states where the proportion of monthly missing data for years 2000 to 2017 is greater than 75%
statesToExcludeCriteria<-sapply(unique(od_2000_to_2017$State), function(state){
  mean(is.na(od_2000_to_2017$Deaths[od_2000_to_2017$State == state]))>.75})

statesToExclude<-unique(od_2000_to_2017$State)[statesToExcludeCriteria]
#states excluded: [1] "Alaska"       "Montana"      "Nebraska"     "North Dakota" "South Dakota" "Vermont" "Wyoming"

#calculate the median (and IQR) of missingnness from resulting data
summary_missingness <- od_2000_to_2017 %>% group_by(State) %>% summarise(missing = mean(is.na(Deaths)))
median(summary_missingness$missing[!summary_missingness$State %in% statesToExclude])
IQR(summary_missingness$missing[!summary_missingness$State %in% statesToExclude])
summary(summary_missingness$missing[!summary_missingness$State %in% statesToExclude])

#include only the states that are not excluded
sensitivity_anlys_exclude_states_data<-main_analysis_data[!(main_analysis_data$State %in% statesToExclude), ]


sensitivity_anlys_exclude_states_model<-gam(cbind(round(imputed_deaths), round(num_alive))~ State +
                                              s(Time_Period_ID, bs = "cr", by = as.factor(Region))  +
                                              Naloxone_Pharmacy_Yes_Redefined +
                                              Naloxone_Pharmacy_No_Redefined +
                                              Medical_Marijuana_Redefined +
                                              Recreational_Marijuana_Redefined +
                                              GSL_Redefined +
                                              PDMP_Redefined +
                                              Medicaid_Expansion_Redefined +
                                              Intervention_Redefined,
                                            data = sensitivity_anlys_exclude_states_data, family = "binomial")

summary(sensitivity_anlys_exclude_states_model)

########## Sensitivity Analysis 2: Make Data Frame of Results and 95% CI #############
#store the coefficients into the table
sensitivity_anlys_exclude_states_full_table<-data.frame(coef(sensitivity_anlys_exclude_states_model))
#check to see how the table looks
head(sensitivity_anlys_exclude_states_full_table)
#rename the column to "Coefficient_Estimate"
colnames(sensitivity_anlys_exclude_states_full_table)<-c("Coefficient_Estimate")

#vector of covariates
covariates<-c("Naloxone_Pharmacy_Yes_Redefined", "Naloxone_Pharmacy_No_Redefined",
              "Medical_Marijuana_Redefined",
              "Recreational_Marijuana_Redefined",
              "GSL_Redefined", "PDMP_Redefined",
              "Medicaid_Expansion_Redefined", "Intervention_Redefined")

#rename the variable names of the regression output so that they look nicer:
#currently there are 3 types of coefficients: state effects, the covariates, and smoothed time effects
#for each row in the main analysis table
for(i in 1:length(rownames(sensitivity_anlys_exclude_states_full_table))){

  #if the coefficient is not in the covariates vector
  if(!(rownames(sensitivity_anlys_exclude_states_full_table)[i] %in% covariates)){

    #we see if it's a state effect
    if(substr(rownames(sensitivity_anlys_exclude_states_full_table)[i], start = 1, stop = 5) == "State"){

      #if so, here, the names look like: StateMassachusetts or StateGeorgia, so take out the "State" part
      #and just rename these rows to just the state name
      rownames(sensitivity_anlys_exclude_states_full_table)[i]<-substr(rownames(sensitivity_anlys_exclude_states_full_table)[i], start = 6,
                                                                       stop = nchar(rownames(sensitivity_anlys_exclude_states_full_table)[i]))

    }else if(rownames(sensitivity_anlys_exclude_states_full_table)[i] == "(Intercept)"){

      #otherwise, if the current name is Intercept, we rename it so that we know that Alabama is the baseline
      rownames(sensitivity_anlys_exclude_states_full_table)[i]<-"Intercept/Alabama"

    }else if(substr(rownames(sensitivity_anlys_exclude_states_full_table)[i], start = 1, stop = 35) == "s(Time_Period_ID):as.factor(Region)"){

      #otherwise, it's the smoothed time effects which look like: s(Time_Period_ID):as.factor(Region)West
      #or s(Time_Period_ID):as.factor(Region)South, so we want to get rid of "s(Time_Period_ID):as.factor(Region)"
      #and change it to "Smoothed Time for Region"
      rownames(sensitivity_anlys_exclude_states_full_table)[i]<-paste("Smoothed Time for Region ",
                                                                      substr(rownames(sensitivity_anlys_exclude_states_full_table)[i], start = 36,
                                                                             stop = nchar(rownames(sensitivity_anlys_exclude_states_full_table)[i])),
                                                                      sep = "")

    }
  }
}

#confidence intervals for the coefficients
sensitivity_anlys_exclude_states_full_table$Coefficient_Lower_Bound<-sensitivity_anlys_exclude_states_full_table$Coefficient_Estimate - 1.96*summary(sensitivity_anlys_exclude_states_model)$se
sensitivity_anlys_exclude_states_full_table$Coefficient_Upper_Bound<-sensitivity_anlys_exclude_states_full_table$Coefficient_Estimate + 1.96*summary(sensitivity_anlys_exclude_states_model)$se

#impute the estimates and confidence intervals in the odds ratio scale
sensitivity_anlys_exclude_states_full_table$Odds_Ratio<-exp(sensitivity_anlys_exclude_states_full_table$Coefficient_Estimate)
sensitivity_anlys_exclude_states_full_table$Odds_Ratio_LB<-exp(sensitivity_anlys_exclude_states_full_table$Coefficient_Lower_Bound)
sensitivity_anlys_exclude_states_full_table$Odds_Ratio_UB<-exp(sensitivity_anlys_exclude_states_full_table$Coefficient_Upper_Bound)

#store the standard error and p-value
sensitivity_anlys_exclude_states_full_table$Standard_Error<-summary(sensitivity_anlys_exclude_states_model)$se
#note that there is no p-value for the smoothed time effects, so we put a NA for those rows
sensitivity_anlys_exclude_states_full_table$p_value<-c(summary(sensitivity_anlys_exclude_states_model)$p.pv,
                                                       rep(NA, length(coef(sensitivity_anlys_exclude_states_model)) -
                                                             length(summary(sensitivity_anlys_exclude_states_model)$p.pv)))

head(sensitivity_anlys_exclude_states_full_table)
tail(sensitivity_anlys_exclude_states_full_table)

#export a table with just the covariates
#first, find the rows that contains the covariates
covariate_Index<-which(rownames(sensitivity_anlys_exclude_states_full_table) %in% covariates)
sensitivity_anlys_exclude_states_covariate_table<-(round(sensitivity_anlys_exclude_states_full_table[covariate_Index,], 5))

#rename the variables so that it looks cleaner
rownames(sensitivity_anlys_exclude_states_covariate_table)<-c("Naloxone_Pharmacy_Yes", "Naloxone_Pharmacy_No",
                                                              "Medical_Marijuana",
                                                              "Recreational_Marijuana",
                                                              "GSL", "PDMP", "Medicaid_Expansion",
                                                              "Intervention_Redefined")

#now, reorganize the data so that the covariates are on top and the rest of the variable sare below
sensitivity_anlys_exclude_states_covariate_table<-rbind(sensitivity_anlys_exclude_states_covariate_table, sensitivity_anlys_exclude_states_full_table[-covariate_Index,])
#remove the columns that aren't in odds ratio scale
sensitivity_anlys_exclude_states_covariate_table<-sensitivity_anlys_exclude_states_covariate_table[,-which(colnames(sensitivity_anlys_exclude_states_covariate_table) %in%
                                                                                                             c("Coefficient_Estimate", "Coefficient_Lower_Bound", "Coefficient_Upper_Bound", "Standard_Error"))]

colnames(sensitivity_anlys_exclude_states_covariate_table)<-c("Risk_Ratio_Estimates", "RR_95_CI_LB", "RR_95_CI_UB", "p-value")
head(sensitivity_anlys_exclude_states_covariate_table, 10)

#save the table into a CSV
# write.csv(round(sensitivity_anlys_exclude_states_covariate_table, 3), "./Data/coefficients_covariates_9_1_20_full_data_exclude_states.csv")

#compute percent difference of the intervention coefficient
main_anlysis_coef <- main_analysis_covariate_table$Risk_Ratio_Estimates[row.names(main_analysis_covariate_table) == "Intervention"]
exclude_states_coef <- sensitivity_anlys_exclude_states_covariate_table$Risk_Ratio_Estimates[row.names(sensitivity_anlys_exclude_states_covariate_table) == "Intervention_Redefined"]

100*(exclude_states_coef - main_anlysis_coef)/main_anlysis_coef

################ Sensitivity Analysis 2: Number of Attributable Deaths #############
#first, we subset the data so that we only focus on the time points for which at least one state had the intervention
attr_deaths_anlys_exclude_states<-sensitivity_anlys_exclude_states_data[which(sensitivity_anlys_exclude_states_data$Intervention_Redefined>0),]

#compute the probability of overdose had intervention not occurred
prob_od_no_int_exclude_states<-expit(-coef(sensitivity_anlys_exclude_states_model)["Intervention_Redefined"]*attr_deaths_anlys_exclude_states$Intervention_Redefined
                                     + logit(attr_deaths_anlys_exclude_states$imputed_deaths/attr_deaths_anlys_exclude_states$population))

#compute the lower and upper bounds of 95% CI of probability of overdose had intervention not occurred
#here, we compute the lower and upper bounds of the 95% CI of all the coefficients using the standard error from the model
coef_lb<-coef(sensitivity_anlys_exclude_states_model) - 1.96*summary(sensitivity_anlys_exclude_states_model)$se
coef_ub<-coef(sensitivity_anlys_exclude_states_model) + 1.96*summary(sensitivity_anlys_exclude_states_model)$se

#we then calculate the upper and lower bounds of the probability of overdose death had intervention not occurred by using
#the lower and upper bounds of the coefficient of the intervention variable
prob_od_no_int_LB_exclude_states<-expit(-coef_lb[names(coef_lb) == "Intervention_Redefined"]*attr_deaths_anlys_exclude_states$Intervention_Redefined
                                        + logit(attr_deaths_anlys_exclude_states$imputed_deaths/attr_deaths_anlys_exclude_states$population))

prob_od_no_int_UB_exclude_states<-expit(-coef_ub[names(coef_ub) == "Intervention_Redefined"]*attr_deaths_anlys_exclude_states$Intervention_Redefined
                                        + logit(attr_deaths_anlys_exclude_states$imputed_deaths/attr_deaths_anlys_exclude_states$population))

#estimate the number of deaths attributable to the intervention
#first, initialize the vectors to store the numbers
num_attr_od_UB<-num_attr_od_LB<-num_attr_od<-rep(NA, length(unique(sensitivity_anlys_exclude_states_data$Time_Period_ID)))


#for each time period, we first find the indices of rows containing data from that time point
#then, we find the total number of deaths that attributable to the intervention

index<-1 #keep track of where to store the values in the vector

for(time in sort(unique(attr_deaths_anlys_exclude_states$Time_Period_ID))){
  #find the indices of rows where the time point = time
  time_point_index<-which(attr_deaths_anlys_exclude_states$Time_Period_ID == time)

  #find the number of deaths attributable to intervention = observed number of deaths with intervention - estimated number of deaths had intervention not occurred
  num_attr_od[index]<-sum(attr_deaths_anlys_exclude_states$imputed_deaths[time_point_index]
                          - prob_od_no_int_exclude_states[time_point_index]*attr_deaths_anlys_exclude_states$population[time_point_index])

  #find the lower and upper bounds of the estimated number of deaths attributable to the intervention
  num_attr_od_LB[index]<-sum(attr_deaths_anlys_exclude_states$imputed_deaths[time_point_index]
                             - prob_od_no_int_LB_exclude_states[time_point_index]*attr_deaths_anlys_exclude_states$population[time_point_index])
  num_attr_od_UB[index]<-sum(attr_deaths_anlys_exclude_states$imputed_deaths[time_point_index]
                             - prob_od_no_int_UB_exclude_states[time_point_index]*attr_deaths_anlys_exclude_states$population[time_point_index])


  index<-index + 1
}

num_attr_od_exclude_states<-data.frame("Time_Period_ID" = sort(unique(attr_deaths_anlys_exclude_states$Time_Period_ID)),
                                       "Time_Start" = sort(unique(attr_deaths_anlys_exclude_states$Time_Period_Start)),
                                       "Num_Attr_Deaths" = num_attr_od,
                                       "Num_Attr_Deaths_LB" = num_attr_od_LB,
                                       "Num_Attr_Deaths_UB" = num_attr_od_UB)

#sum up the total number of excess deaths attributable to the intervention
sum(num_attr_od_exclude_states$Num_Attr_Deaths) #32951.47

#sum up the number of excess deaths per year
yearly_num_Attr_Deaths_exclude_states<-num_attr_od_exclude_states %>%
  group_by("year" = year(Time_Start)) %>%
  summarise("deaths" = sum(Num_Attr_Deaths), death_lb = sum(Num_Attr_Deaths_LB),
            death_ub = sum(Num_Attr_Deaths_UB))

summary(yearly_num_Attr_Deaths_exclude_states$deaths)

# pdf("excess_deaths_yearly_exclude_states_4_19_20.pdf")
ggplot(yearly_num_Attr_Deaths_exclude_states, aes(x = year, y = deaths)) + geom_line() + geom_point()
# dev.off()


##########################################################################################
### Sensitivity Analysis 3: Divide Unaccountable Deaths Equally Among Missing Months #####
od_data <- read.csv("./Data/od_deaths_12_80_unintentional_8_2_20.txt", sep = "\t", stringsAsFactors = FALSE)
od_data$Deaths <- as.numeric(od_data$Deaths)
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
od_year <- read.csv("./Data/od_by_state_yr_restricted_age_8_10_20.txt", sep = "\t", stringsAsFactors = FALSE)
od_year$Deaths <- as.numeric(od_year$Deaths)
head(od_year, 50)
#see how many states have missing yearly entries and use the totals to impute the missing yearly values.
sum_na <- od_year %>% group_by(State) %>% summarise(sum(is.na(Deaths)))
table(sum_na)

#the 3 states are: North Dakota, South Dakota, and Rhode Island
od_year$Deaths[od_year$State == "North Dakota" & is.na(od_year$Deaths)] <- (od_year$Deaths[od_year$State == "North Dakota" & od_year$Notes == "Total"] - sum(od_year$Deaths[od_year$State == "North Dakota" & od_year$Notes != "Total"], na.rm = TRUE))/sum(is.na(od_year$Deaths[od_year$State == "North Dakota"]))
od_year$Deaths[od_year$State == "South Dakota" & is.na(od_year$Deaths)] <- (od_year$Deaths[od_year$State == "South Dakota" & od_year$Notes == "Total"] - sum(od_year$Deaths[od_year$State == "South Dakota" & od_year$Notes != "Total"], na.rm = TRUE))/sum(is.na(od_year$Deaths[od_year$State == "South Dakota"]))
od_year$Deaths[od_year$State == "Rhode Island" & is.na(od_year$Deaths)] <- (od_year$Deaths[od_year$State == "Rhode Island" & od_year$Notes == "Total"] - sum(od_year$Deaths[od_year$State == "Rhode Island" & od_year$Notes != "Total"], na.rm = TRUE))/sum(is.na(od_year$Deaths[od_year$State == "Rhode Island"]))
od_year <- od_year[!is.na(od_year$Year),]
tail(od_year)

od_data$imputed_vals<-rep(NA, nrow(od_data))

startYear<-1999

for(state in unique(od_data$State)){
  #get the values of the deaths for state
  currentDeaths<-od_data$Deaths[od_data$State == state]
  for(year in startYear:2018){
    #find the indices of the missing data -- gets the missing indices for that particular year
    indexMissing <- which(is.na(currentDeaths[(1:12) + 12*(year - startYear)]))
    if(length(indexMissing) != 0){
      #if there are missing values, we find the number of accounted deaths
      currentDeathsTotal <- sum(currentDeaths[(1:12) + 12*(year - startYear)], na.rm = TRUE)
      #and calculate the deaths that are not accounted for using the yearly deaths for the state
      numNotAccounted <- od_year$Deaths[od_year$State == state & od_year$Year == year] - currentDeathsTotal
      
      #we then divide number of unaccounted deaths evenly by the number of months with missing values 
      currentDeaths[(1:12) + 12*(year - startYear)][indexMissing] <- numNotAccounted/length(indexMissing)
    }else{
      #otherwise, if there is no missing values, we skip to the next year
      next
    }
  }
  #store the imputed values
  od_data$imputed_vals[od_data$State == state]<-currentDeaths
}

#group into 6 month time periods now and compute the total number of deaths in each period
od_data_grouped_data <- od_data %>%
  mutate(Time_Period_Start = lubridate::floor_date(Date , "6 months" )) %>% 
  group_by(State, Time_Period_Start) %>%
  summarise(sum_deaths = sum(imputed_vals, na.rm = TRUE))

#restrict the dataset to be between 2000 and 2017
od_data_grouped_data <- od_data_grouped_data[year(od_data_grouped_data$Time_Period_Start) > 1999 &
                     year(od_data_grouped_data$Time_Period_Start) < 2018,]

#create a new dataset for the analysis using columns from the main analysis data
sensitivity_anlys_imputed_od_wo_interp_data <- main_analysis_data %>%
  select(-c(imputed_deaths, num_alive)) %>% #want to remove the outcome from main analysis to not get confused
  mutate(imputed_deaths_no_interp = od_data_grouped_data$sum_deaths,
         num_alive_no_interp = population - imputed_deaths_no_interp)

#run the model for analysis
sensitivity_anlys_imputed_od_wo_interp <- gam(cbind(round(imputed_deaths_no_interp),
                                                    round(num_alive_no_interp))~ State +
                                              s(Time_Period_ID, bs = "cr",
                                                by = as.factor(Region)) +
                                              Naloxone_Pharmacy_Yes_Redefined +
                                              Naloxone_Pharmacy_No_Redefined +
                                              Medical_Marijuana_Redefined +
                                              Recreational_Marijuana_Redefined +
                                              GSL_Redefined +
                                              PDMP_Redefined +
                                              Medicaid_Expansion_Redefined +
                                              Intervention_Redefined,
                                            data = sensitivity_anlys_imputed_od_wo_interp_data,
                                            family = "binomial")

summary(sensitivity_anlys_imputed_od_wo_interp)
exp(coef(sensitivity_anlys_imputed_od_wo_interp)["Intervention_Redefined"]) #1.0782

#######################################################################################
########### Sensitivity Analysis 4: Two Year Intervention Effect ######################
#create a plot for each state to see how many prosecution media alerts there are per 6 month period
#read in the prosecution media alert data
prosecution_data<-read.csv("./Data/prosecution_media_alert_5_5_19.csv")

#data cleaning
#first take out the rows with "no info" for charge date or State
prosecution_data<-prosecution_data[-c(which(prosecution_data$Date.charged == "No Info" | prosecution_data$Date.charged == "No info")),]
prosecution_data<-prosecution_data[-c(which(prosecution_data$State.Filed == "No Info"
                                            | prosecution_data$State.Filed == "No info")),]

#clean up the state abbreviations that are coded differently
table((prosecution_data$State.Filed))
prosecution_data$State.Filed[prosecution_data$State.Filed == "Ct"]<-"CT"
prosecution_data$State.Filed[prosecution_data$State.Filed == "Il"]<-"IL"
prosecution_data$State.Filed[prosecution_data$State.Filed == "Wi"]<-"WI"
prosecution_data$State.Filed[prosecution_data$State.Filed == "wv"]<-"WV"

#change the states into Character instead of factor
prosecution_data$State.Filed<-as.character(prosecution_data$State.Filed)
#see how many prosecution data points there are for each state
table(prosecution_data$State.Filed)

#change date charged into Date object
prosecution_data$Date<-mdy(prosecution_data$Date.charged)

#group the data into six month periods
prosecution_data<-prosecution_data %>% mutate(six_month_pd = lubridate::floor_date(Date , "6 months" ))

#count the number of prosecution media alerts in each six month period
#we also get the first and last date of prosecution in time period
prosecution_data_by_six_month_pd <- prosecution_data %>%
  filter(year(six_month_pd)>1999 & year(six_month_pd)<2018) %>%
  group_by(ID, six_month_pd) %>%
  summarise(first_date_in_pd = min(Date), last_date_in_pd = max(Date))

#create the data set used for this sensitivity analysis
#first, we merge the grouped prosecution data set with the main data set by state and time period
sensitivity_anlys_redefine_int_data<-merge(main_analysis_data,
                                           prosecution_data_by_six_month_pd, by.x = c("State", "Time_Period_Start"),
                                           by.y = c("ID", "six_month_pd"), all = TRUE)

#create a intervention 2 year effect variable by initializing it to be all 0
sensitivity_anlys_redefine_int_data<-sensitivity_anlys_redefine_int_data %>% group_by(State) %>%
  mutate(int_2_yr_effect = 0)

#change the date into a date object
sensitivity_anlys_redefine_int_data$Time_Period_Start<-as.Date(sensitivity_anlys_redefine_int_data$Time_Period_Start)
sensitivity_anlys_redefine_int_data$Time_Period_End<-as.Date(sensitivity_anlys_redefine_int_data$Time_Period_End)

#we need to impute the newly defined intervention variable depending on the case
#by examining each row of the data set
for(state in unique(sensitivity_anlys_redefine_int_data$State)){
  #first, subset the data set into state_data which only contains the data for the state
  state_index<-which(sensitivity_anlys_redefine_int_data$State == state)
  state_data<-sensitivity_anlys_redefine_int_data[state_index,]

  #note that the first four rows of the 2 year effect intervention variable are the same as the
  #first four rows of the original intervention variable
  state_data$int_2_yr_effect[1:4]<-state_data$Intervention_Redefined[1:4]

  for(i in 5:nrow(state_data)){
    #next, we deal with the rows where there was at least one prosecution in the last 3 six month periods
    #These rows will be imputed with a 1
    if((!is.na(state_data$first_date_in_pd[i - 1]) |
        !is.na(state_data$first_date_in_pd[i - 2]) |
        !is.na(state_data$first_date_in_pd[i - 3]))){

      state_data$int_2_yr_effect[i]<-1

    }else{
      #next, we account for the rows with the fractions:
      # 1) an intervention occurs in row i without an intervention 2 years ago
      # 2) row i contains the lasting effects of an intervention that occurred 2 years ago
      # 3) row i contains effects from both a new intervention starting in row i and lasting
      # effects from 2 years ago

      #To compute the fraction, we add the number of days that are affected by an intervention
      #(from both the current prosecution and previous prosecution) and then divide by the total
      #number of days in the period:
      #If there is no prosecution in the period i, then the start_date is the last date in the period.
      #We subtract start_date from the last date in the period, so we will get a 0 for the number
      #of days that are affected by a prosecution in period i. Otherwise, the start_date is the
      #first date of a prosecution in the period.
      #If there is no prosecution two years ago, i.e. in period i-4, then the last_date is the first
      #date in the period. We subtract the last_date by the first date in the period, so we will get
      #a 0 for the number of days that are affected by a prosecution from period i-4. Otherwise,
      #the last_date is the last date of prosecution from period i-4, plus 2 years.

      total_len_of_pd<-as.numeric(state_data$Time_Period_End[i] - state_data$Time_Period_Start[i])

      len_of_past_effect <- ifelse(!is.na(state_data$first_date_in_pd[i - 4]),
                                   (state_data$last_date_in_pd[i - 4] + years(2)) - state_data$Time_Period_Start[i],
                                   0)

      len_of_current_effect <- ifelse(!is.na(state_data$first_date_in_pd[i]),
                                      as.numeric(state_data$Time_Period_End[i] - state_data$first_date_in_pd[i]),
                                      0)

      state_data$int_2_yr_effect[i]<-(len_of_past_effect + len_of_current_effect)/total_len_of_pd
    }
  }

  #for the case where the int_2_yr_effect is greater than 1 (could result when we add the effects of
  #previous intervention and the current intervention), we just impute a 1 instead
  state_data$int_2_yr_effect[state_data$int_2_yr_effect>1]<-1

  #lastly, we store the int_2_yr_effect variable into the sensitivity analysis data set
  sensitivity_anlys_redefine_int_data$int_2_yr_effect[state_index]<-state_data$int_2_yr_effect
}

#view the data set just to make sure the imputation looks right
# View(sensitivity_anlys_redefine_int_data %>% select(State, Time_Period_Start, Time_Period_End,
#                                                     Intervention_Redefined, first_date_in_pd,
#                                                     last_date_in_pd,
#                                                     int_2_yr_effect))



#run the analysis on the sensitivity analysis data

sensitivity_anlys_redefine_int_model<-gam(cbind(round(imputed_deaths), round(num_alive))~ State +
                                            s(Time_Period_ID, bs = "cr", by = as.factor(Region))  +
                                            Naloxone_Pharmacy_Yes_Redefined +
                                            Naloxone_Pharmacy_No_Redefined +
                                            Medical_Marijuana_Redefined +
                                            Recreational_Marijuana_Redefined +
                                            GSL_Redefined +
                                            PDMP_Redefined +
                                            Medicaid_Expansion_Redefined +
                                            int_2_yr_effect,
                                          data = sensitivity_anlys_redefine_int_data, family = "binomial")

summary(sensitivity_anlys_redefine_int_model)
plot(sensitivity_anlys_redefine_int_model)


############## Sensitivity Analysis 4: Make Data Frame of Results and 95% CI ##########
#store the coefficients into the table
sensitivity_anlys_redefine_int_full_table<-data.frame(coef(sensitivity_anlys_redefine_int_model))
#check to see how the table looks
head(sensitivity_anlys_redefine_int_full_table)
#rename the column to "Coefficient_Estimate"
colnames(sensitivity_anlys_redefine_int_full_table)<-c("Coefficient_Estimate")

#vector of covariates
covariates<-c("Naloxone_Pharmacy_Yes_Redefined", "Naloxone_Pharmacy_No_Redefined",
              "Medical_Marijuana_Redefined",
              "Recreational_Marijuana_Redefined",
              "GSL_Redefined", "PDMP_Redefined",
              "Medicaid_Expansion_Redefined", "int_2_yr_effect")

#rename the variable names of the regression output so that they look nicer:
#currently there are 3 types of coefficients: state effects, the covariates, and smoothed time effects
#for each row in the main analysis table
for(i in 1:length(rownames(sensitivity_anlys_redefine_int_full_table))){

  #if the coefficient is not in the covariates vector
  if(!(rownames(sensitivity_anlys_redefine_int_full_table)[i] %in% covariates)){

    #we see if it's a state effect
    if(substr(rownames(sensitivity_anlys_redefine_int_full_table)[i], start = 1, stop = 5) == "State"){

      #if so, here, the names look like: StateMassachusetts or StateGeorgia, so take out the "State" part
      #and just rename these rows to just the state name
      rownames(sensitivity_anlys_redefine_int_full_table)[i]<-substr(rownames(sensitivity_anlys_redefine_int_full_table)[i], start = 6,
                                                                     stop = nchar(rownames(sensitivity_anlys_redefine_int_full_table)[i]))

    }else if(rownames(sensitivity_anlys_redefine_int_full_table)[i] == "(Intercept)"){

      #otherwise, if the current name is Intercept, we rename it so that we know that Alabama is the baseline
      rownames(sensitivity_anlys_redefine_int_full_table)[i]<-"Intercept/Alabama"

    }else if(substr(rownames(sensitivity_anlys_redefine_int_full_table)[i], start = 1, stop = 35) == "s(Time_Period_ID):as.factor(Region)"){

      #otherwise, it's the smoothed time effects which look like: s(Time_Period_ID):as.factor(Region)West
      #or s(Time_Period_ID):as.factor(Region)South, so we want to get rid of "s(Time_Period_ID):as.factor(Region)"
      #and change it to "Smoothed Time for Region"
      rownames(sensitivity_anlys_redefine_int_full_table)[i]<-paste("Smoothed Time for Region ",
                                                                    substr(rownames(sensitivity_anlys_redefine_int_full_table)[i], start = 36,
                                                                           stop = nchar(rownames(sensitivity_anlys_redefine_int_full_table)[i])),
                                                                    sep = "")

    }
  }
}

#confidence intervals for the coefficients
sensitivity_anlys_redefine_int_full_table$Coefficient_Lower_Bound<-sensitivity_anlys_redefine_int_full_table$Coefficient_Estimate - 1.96*summary(sensitivity_anlys_redefine_int_model)$se
sensitivity_anlys_redefine_int_full_table$Coefficient_Upper_Bound<-sensitivity_anlys_redefine_int_full_table$Coefficient_Estimate + 1.96*summary(sensitivity_anlys_redefine_int_model)$se

#impute the estimates and confidence intervals in the odds ratio scale
sensitivity_anlys_redefine_int_full_table$Odds_Ratio<-exp(sensitivity_anlys_redefine_int_full_table$Coefficient_Estimate)
sensitivity_anlys_redefine_int_full_table$Odds_Ratio_LB<-exp(sensitivity_anlys_redefine_int_full_table$Coefficient_Lower_Bound)
sensitivity_anlys_redefine_int_full_table$Odds_Ratio_UB<-exp(sensitivity_anlys_redefine_int_full_table$Coefficient_Upper_Bound)

#store the standard error and p-value
sensitivity_anlys_redefine_int_full_table$Standard_Error<-summary(sensitivity_anlys_redefine_int_model)$se
#note that there is no p-value for the smoothed time effects, so we put a NA for those rows
sensitivity_anlys_redefine_int_full_table$p_value<-c(summary(sensitivity_anlys_redefine_int_model)$p.pv,
                                                     rep(NA, length(coef(sensitivity_anlys_redefine_int_model)) -
                                                           length(summary(sensitivity_anlys_redefine_int_model)$p.pv)))

head(sensitivity_anlys_redefine_int_full_table)
tail(sensitivity_anlys_redefine_int_full_table)

#export a table with just the covariates
#first, find the rows that contains the covariates
covariate_Index<-which(rownames(sensitivity_anlys_redefine_int_full_table) %in% covariates)
sensitivity_anlys_redefine_int_covariate_table<-(round(sensitivity_anlys_redefine_int_full_table[covariate_Index,], 5))

#rename the variables so that it looks cleaner
rownames(sensitivity_anlys_redefine_int_covariate_table)<-c("Naloxone_Pharmacy_Yes", "Naloxone_Pharmacy_No",
                                                            "Medical_Marijuana",
                                                            "Recreational_Marijuana",
                                                            "GSL", "PDMP", "Medicaid_Expansion",
                                                            "int_2_yr_effect")

#now, reorganize the data so that the covariates are on top and the rest of the variable sare below
sensitivity_anlys_redefine_int_covariate_table<-rbind(sensitivity_anlys_redefine_int_covariate_table, sensitivity_anlys_redefine_int_full_table[-covariate_Index,])
#remove the columns that aren't in odds ratio scale
sensitivity_anlys_redefine_int_covariate_table<-sensitivity_anlys_redefine_int_covariate_table[,-which(colnames(sensitivity_anlys_redefine_int_covariate_table) %in%
                                                                                                         c("Coefficient_Estimate", "Coefficient_Lower_Bound", "Coefficient_Upper_Bound", "Standard_Error"))]

colnames(sensitivity_anlys_redefine_int_covariate_table)<-c("Risk_Ratio_Estimates", "RR_95_CI_LB", "RR_95_CI_UB", "p-value")
head(sensitivity_anlys_redefine_int_covariate_table, 10)

#save the table into a CSV
# write.csv(round(sensitivity_anlys_redefine_int_covariate_table, 3), "./Data/coefficients_covariates_9_1_20_full_data_redefine_int.csv")

################ Sensitivity Analysis 4: Number of Attributable Deaths ################
#first, we subset the data so that we only focus on the time points for which at least one state had the intervention
attr_deaths_anlys_redefine_int<-sensitivity_anlys_redefine_int_data[which(sensitivity_anlys_redefine_int_data$int_2_yr_effect>0),]

#compute the probability of overdose had intervention not occurred
prob_od_no_int_redefine_int<-expit(-coef(sensitivity_anlys_redefine_int_model)["int_2_yr_effect"]*attr_deaths_anlys_redefine_int$int_2_yr_effect
                                   + logit(attr_deaths_anlys_redefine_int$imputed_deaths/attr_deaths_anlys_redefine_int$population))

#compute the lower and upper bounds of 95% CI of probability of overdose had intervention not occurred
#here, we compute the lower and upper bounds of the 95% CI of all the coefficients using the standard error from the model
coef_lb<-coef(sensitivity_anlys_redefine_int_model) - 1.96*summary(sensitivity_anlys_redefine_int_model)$se
coef_ub<-coef(sensitivity_anlys_redefine_int_model) + 1.96*summary(sensitivity_anlys_redefine_int_model)$se

#we then calculate the upper and lower bounds of the probability of overdose death had intervention not occurred by using
#the lower and upper bounds of the coefficient of the intervention variable
prob_od_no_int_LB_redefine_int<-expit(-coef_lb[names(coef_lb) == "int_2_yr_effect"]*attr_deaths_anlys_redefine_int$int_2_yr_effect
                                      + logit(attr_deaths_anlys_redefine_int$imputed_deaths/attr_deaths_anlys_redefine_int$population))

prob_od_no_int_UB_redefine_int<-expit(-coef_ub[names(coef_ub) == "int_2_yr_effect"]*attr_deaths_anlys_redefine_int$int_2_yr_effect
                                      + logit(attr_deaths_anlys_redefine_int$imputed_deaths/attr_deaths_anlys_redefine_int$population))

#estimate the number of deaths attributable to the intervention
#first, initialize the vectors to store the numbers
num_attr_od_UB<-num_attr_od_LB<-num_attr_od<-rep(NA, length(unique(sensitivity_anlys_redefine_int_data$Time_Period_ID)))


#for each time period, we first find the indices of rows containing data from that time point
#then, we find the total number of deaths that attributable to the intervention

index<-1 #keep track of where to store the values in the vector

for(time in sort(unique(attr_deaths_anlys_redefine_int$Time_Period_ID))){
  #find the indices of rows where the time point = time
  time_point_index<-which(attr_deaths_anlys_redefine_int$Time_Period_ID == time)

  #find the number of deaths attributable to intervention = observed number of deaths with intervention - estimated number of deaths had intervention not occurred
  num_attr_od[index]<-sum(attr_deaths_anlys_redefine_int$imputed_deaths[time_point_index]
                          - prob_od_no_int_redefine_int[time_point_index]*attr_deaths_anlys_redefine_int$population[time_point_index])

  #find the lower and upper bounds of the estimated number of deaths attributable to the intervention
  num_attr_od_LB[index]<-sum(attr_deaths_anlys_redefine_int$imputed_deaths[time_point_index]
                             - prob_od_no_int_LB_redefine_int[time_point_index]*attr_deaths_anlys_redefine_int$population[time_point_index])
  num_attr_od_UB[index]<-sum(attr_deaths_anlys_redefine_int$imputed_deaths[time_point_index]
                             - prob_od_no_int_UB_redefine_int[time_point_index]*attr_deaths_anlys_redefine_int$population[time_point_index])


  index<-index + 1
}

num_attr_od_redefine_int<-data.frame("Time_Period_ID" = sort(unique(attr_deaths_anlys_redefine_int$Time_Period_ID)),
                                     "Time_Start" = sort(unique(attr_deaths_anlys_redefine_int$Time_Period_Start)),
                                     "Num_Attr_Deaths" = num_attr_od,
                                     "Num_Attr_Deaths_LB" = num_attr_od_LB,
                                     "Num_Attr_Deaths_UB" = num_attr_od_UB)

#sum up the total number of excess deaths attributable to the intervention
sum(num_attr_od_redefine_int$Num_Attr_Deaths) #20411.78

#sum up the number of excess deaths per year
yearly_num_Attr_Deaths_redefine_int<-num_attr_od_redefine_int %>%
  group_by("year" = year(Time_Start)) %>%
  summarise("deaths" = sum(Num_Attr_Deaths),
            death_lb = sum(Num_Attr_Deaths_LB),
            death_ub = sum(Num_Attr_Deaths_UB))

summary(yearly_num_Attr_Deaths_redefine_int$deaths)

# pdf("excess_deaths_yearly_redefine_int_5_26_20.pdf")
ggplot(yearly_num_Attr_Deaths_redefine_int, aes(x = year, y = deaths)) + geom_line() + geom_point()
# dev.off()


####################################################################################
############# Sensitivity Analysis 5: Two Year Effect Lagged by 6 months ######################
#create a plot for each state to see how many prosecution media alerts there are per 6 month period
#read in the prosecution media alert data
prosecution_data<-read.csv("./Data/prosecution_media_alert_5_5_19.csv")

#data cleaning
#first take out the rows with "no info" for charge date or State
prosecution_data<-prosecution_data[-c(which(prosecution_data$Date.charged == "No Info" | prosecution_data$Date.charged == "No info")),]
prosecution_data<-prosecution_data[-c(which(prosecution_data$State.Filed == "No Info"
                                            | prosecution_data$State.Filed == "No info")),]

#clean up the state abbreviations that are coded differently
table((prosecution_data$State.Filed))
prosecution_data$State.Filed[prosecution_data$State.Filed == "Ct"]<-"CT"
prosecution_data$State.Filed[prosecution_data$State.Filed == "Il"]<-"IL"
prosecution_data$State.Filed[prosecution_data$State.Filed == "Wi"]<-"WI"
prosecution_data$State.Filed[prosecution_data$State.Filed == "wv"]<-"WV"

#change the states into Character instead of factor
prosecution_data$State.Filed<-as.character(prosecution_data$State.Filed)
#see how many prosecution data points there are for each state
table(prosecution_data$State.Filed)

#change date charged into Date object
prosecution_data$Date<-mdy(prosecution_data$Date.charged)

#group the data into six month periods
prosecution_data<-prosecution_data %>% mutate(six_month_pd = lubridate::floor_date(Date , "6 months" ))

#count the number of prosecution media alerts in each six month period
#we also get the first and last date of prosecution in time period
prosecution_data_by_six_month_pd <- prosecution_data %>%
  filter(year(six_month_pd)>1999 & year(six_month_pd)<2018) %>%
  group_by(ID, six_month_pd) %>%
  summarise(first_date_in_pd = min(Date), last_date_in_pd = max(Date))

#create the data set used for this sensitivity analysis
#first, we merge the grouped prosecution data set with the main data set by state and time period
sensitivity_anlys_2yr_int_lag<-merge(main_analysis_data,
                                     prosecution_data_by_six_month_pd, 
                                     by.x = c("State", "Time_Period_Start"),
                                     by.y = c("ID", "six_month_pd"), all = TRUE)

#create a intervention 2 year effect variable by initializing it to be all 0
sensitivity_anlys_2yr_int_lag<-sensitivity_anlys_2yr_int_lag %>% group_by(State) %>%
  mutate(int_2_yr_effect_lag = 0)

#change the date into a date object
sensitivity_anlys_2yr_int_lag$Time_Period_Start<-as.Date(sensitivity_anlys_2yr_int_lag$Time_Period_Start)
sensitivity_anlys_2yr_int_lag$Time_Period_End<-as.Date(sensitivity_anlys_2yr_int_lag$Time_Period_End)

#we need to calculate the 2 year intervention variable depending on the case
#by examining each row of the data set
for(state in unique(sensitivity_anlys_2yr_int_lag$State)){
  #first, subset the data set into state_data which only contains the data for the state
  state_index<-which(sensitivity_anlys_2yr_int_lag$State == state)
  state_data<-sensitivity_anlys_2yr_int_lag[state_index,]

  for(i in 1:(nrow(state_data)-1)){
    #for the states that had at least one prosecution in the first 2 years of analysis period,
    #we impute the intervention variable based on the intervention variable of main analysis:
    #if intervention occurred in time i, then for the 6-month lagged effect, we compute the 
    #proportion of days affected by intervention, taking into account the 6 month lag. Else, 
    #if the intervention had occurred by time i, we impute a 1 in the next six-month interval
    #since lagged
    if(i %in% c(1:4)){
      if(state_data$Intervention_Redefined[i] > 0 & state_data$Intervention_Redefined[i] < 1){
        state_data$int_2_yr_effect_lag[i + 1] <- as.numeric(state_data$Time_Period_End[i + 1] - (state_data$first_date_in_pd[i] %m+% months(6)))/as.numeric(state_data$Time_Period_End[i + 1] - state_data$Time_Period_Start[i + 1])
      }else if(state_data$Intervention_Redefined[i] == 1){
        state_data$int_2_yr_effect_lag[i + 1] <- 1
      }
      
      #next, if there was at least one prosecution in the last 3 six-month periods (i.e. 1.5 years) before time i
      #These rows will be imputed with a 1 in the next six-month interval since lagged
    }else if((!is.na(state_data$first_date_in_pd[i - 1]) |
              !is.na(state_data$first_date_in_pd[i - 2]) |
              !is.na(state_data$first_date_in_pd[i - 3]))){

      state_data$int_2_yr_effect_lag[i + 1]<-1

    }else{
      #next, we account for the rows with the fractions:
      # 1) an intervention occurs in row i without an intervention 2 years ago
      # 2) row i contains the lasting effects of an intervention that occurred 2 years ago
      # 3) row i contains effects from both a new intervention starting in row i and lasting
      # effects from 2 years ago
      
      #To compute the fraction, we add the number of days that are affected by an intervention
      #(from both the current prosecution and previous prosecution) and then divide by the total
      #number of days in the period:
      
      #first, we compute the number of days in the period of time interval i + 1
      total_len_of_pd<-as.numeric(state_data$Time_Period_End[i + 1] - state_data$Time_Period_Start[i + 1])
      
      #If there is no prosecution two years ago, i.e. in period i-4, then the last_date is the first
      #date in period i + 1. We subtract the last_date by the first date in period i + 1, so we will get
      #a 0 for the number of days that are affected by a prosecution from period i-4. Otherwise,
      #the last_date is the last date of prosecution from period i-4, plus 2 years. 
      #The start time is the first date of period i + 1
      
      len_of_past_effect <- ifelse(!is.na(state_data$first_date_in_pd[i - 4]),
                                   as.numeric((as.Date(state_data$last_date_in_pd[i - 4] + years(2), format = "%Y-%m-%d") %m+% months(6)) - state_data$Time_Period_Start[i + 1]),
                                   0)
      
      #If there is no prosecution in the period i, then the start_date is the last date in period i + 1 (because lagged effect).
      #We subtract start_date from the last date in period i + 1, so we will get a 0 for the number
      #of days that are affected by a prosecution in period i. Otherwise, the start_date is the
      #first date of a prosecution in period i + 6 months. The end date is the last date in period i + 1. 
    
      len_of_current_effect <- ifelse(!is.na(state_data$first_date_in_pd[i]),
                                      as.numeric(state_data$Time_Period_End[i + 1] - (state_data$first_date_in_pd[i] %m+% months(6))),
                                      0)

      state_data$int_2_yr_effect_lag[i + 1]<-(len_of_past_effect + len_of_current_effect)/total_len_of_pd
    }
  }

  #for the case where the int_2_yr_effect is greater than 1 (could result when we add the effects of
  #previous intervention and the current intervention), we just impute a 1 instead
  state_data$int_2_yr_effect_lag[state_data$int_2_yr_effect_lag>1]<-1

  #lastly, we store the int_2_yr_effect variable into the sensitivity analysis data set
  sensitivity_anlys_2yr_int_lag$int_2_yr_effect_lag[state_index]<-state_data$int_2_yr_effect_lag
}

#view the data set just to make sure the imputation looks right
# View(sensitivity_anlys_2yr_int_lag %>% select(State, Time_Period_Start, Time_Period_End,
#                                                     Intervention_Redefined, first_date_in_pd,
#                                                     last_date_in_pd,
#                                                     int_2_yr_effect_lag))


#run the analysis for all the states
lagged_analysis_model<-gam(cbind(round(imputed_deaths), round(num_alive))~ State +
                             s(Time_Period_ID, bs = "cr", by = as.factor(Region)) +
                             Naloxone_Pharmacy_Yes_Redefined +
                             Naloxone_Pharmacy_No_Redefined +
                             Medical_Marijuana_Redefined +
                             Recreational_Marijuana_Redefined +
                             GSL_Redefined +
                             PDMP_Redefined +
                             Medicaid_Expansion_Redefined +
                             int_2_yr_effect_lag,
                           data = sensitivity_anlys_2yr_int_lag, family = "binomial")

#summary output of the model
summary(lagged_analysis_model)
gam.check(lagged_analysis_model)

############## Sensitivity Analysis 5: Make Data Frame of Results and 95% CI ###########
#store the coefficients into the table
sensitivity_anlys_2yr_int_lag_full_table<-data.frame(coef(lagged_analysis_model))
#check to see how the table looks
head(sensitivity_anlys_2yr_int_lag_full_table)
#rename the column to "Coefficient_Estimate"
colnames(sensitivity_anlys_2yr_int_lag_full_table)<-c("Coefficient_Estimate")

#vector of covariates
covariates<-c("Naloxone_Pharmacy_Yes_Redefined", "Naloxone_Pharmacy_No_Redefined",
              "Medical_Marijuana_Redefined",
              "Recreational_Marijuana_Redefined",
              "GSL_Redefined", "PDMP_Redefined",
              "Medicaid_Expansion_Redefined", "int_2_yr_effect_lag")

#rename the variable names of the regression output so that they look nicer:
#currently there are 3 types of coefficients: state effects, the covariates, and smoothed time effects
#for each row in the main analysis table
for(i in 1:length(rownames(sensitivity_anlys_2yr_int_lag_full_table))){

  #if the coefficient is not in the covariates vector
  if(!(rownames(sensitivity_anlys_2yr_int_lag_full_table)[i] %in% covariates)){

    #we see if it's a state effect
    if(substr(rownames(sensitivity_anlys_2yr_int_lag_full_table)[i], start = 1, stop = 5) == "State"){

      #if so, here, the names look like: StateMassachusetts or StateGeorgia, so take out the "State" part
      #and just rename these rows to just the state name
      rownames(sensitivity_anlys_2yr_int_lag_full_table)[i]<-substr(rownames(sensitivity_anlys_2yr_int_lag_full_table)[i], start = 6,
                                                                    stop = nchar(rownames(sensitivity_anlys_2yr_int_lag_full_table)[i]))

    }else if(rownames(sensitivity_anlys_2yr_int_lag_full_table)[i] == "(Intercept)"){

      #otherwise, if the current name is Intercept, we rename it so that we know that Alabama is the baseline
      rownames(sensitivity_anlys_2yr_int_lag_full_table)[i]<-"Intercept/Alabama"

    }else if(substr(rownames(sensitivity_anlys_2yr_int_lag_full_table)[i], start = 1, stop = 35) == "s(Time_Period_ID):as.factor(Region)"){

      #otherwise, it's the smoothed time effects which look like: s(Time_Period_ID):as.factor(Region)West
      #or s(Time_Period_ID):as.factor(Region)South, so we want to get rid of "s(Time_Period_ID):as.factor(Region)"
      #and change it to "Smoothed Time for Region"
      rownames(sensitivity_anlys_2yr_int_lag_full_table)[i]<-paste("Smoothed Time for Region ",
                                                                   substr(rownames(sensitivity_anlys_2yr_int_lag_full_table)[i], start = 36,
                                                                          stop = nchar(rownames(sensitivity_anlys_2yr_int_lag_full_table)[i])),
                                                                   sep = "")

    }
  }
}

#confidence intervals for the coefficients
sensitivity_anlys_2yr_int_lag_full_table$Coefficient_Lower_Bound<-sensitivity_anlys_2yr_int_lag_full_table$Coefficient_Estimate - 1.96*summary(lagged_analysis_model)$se
sensitivity_anlys_2yr_int_lag_full_table$Coefficient_Upper_Bound<-sensitivity_anlys_2yr_int_lag_full_table$Coefficient_Estimate + 1.96*summary(lagged_analysis_model)$se

#impute the estimates and confidence intervals in the odds ratio scale
sensitivity_anlys_2yr_int_lag_full_table$Odds_Ratio<-exp(sensitivity_anlys_2yr_int_lag_full_table$Coefficient_Estimate)
sensitivity_anlys_2yr_int_lag_full_table$Odds_Ratio_LB<-exp(sensitivity_anlys_2yr_int_lag_full_table$Coefficient_Lower_Bound)
sensitivity_anlys_2yr_int_lag_full_table$Odds_Ratio_UB<-exp(sensitivity_anlys_2yr_int_lag_full_table$Coefficient_Upper_Bound)

#store the standard error and p-value
sensitivity_anlys_2yr_int_lag_full_table$Standard_Error<-summary(lagged_analysis_model)$se
#note that there is no p-value for the smoothed time effects, so we put a NA for those rows
sensitivity_anlys_2yr_int_lag_full_table$p_value<-c(summary(lagged_analysis_model)$p.pv, rep(NA, length(coef(lagged_analysis_model)) - length(summary(lagged_analysis_model)$p.pv)))

head(sensitivity_anlys_2yr_int_lag_full_table)
tail(sensitivity_anlys_2yr_int_lag_full_table)

#save the table into a CSV
# write.csv(round(sensitivity_anlys_2yr_int_lag_full_table,5), "./Data/coefficients_GAM_10_8_20_lagged_2yr_int.csv")

#export a table with just the covariates
#first, find the rows that contains the covariates
covariate_Index<-which(rownames(sensitivity_anlys_2yr_int_lag_full_table) %in% covariates)
sens_analysis_2yr_int_lag_covariate_table<-(round(sensitivity_anlys_2yr_int_lag_full_table[covariate_Index,], 5))

#rename the variables so that it looks cleaner
rownames(sens_analysis_2yr_int_lag_covariate_table)<-c("Naloxone_Pharmacy_Yes", "Naloxone_Pharmacy_No",
                                                       "Medical_Marijuana",
                                                       "Recreational_Marijuana",
                                                       "GSL", "PDMP", "Medicaid_Expansion",
                                                       "Intervention")

#now, reorganize the data so that the covariates are on top and the rest of the variable sare below
sens_analysis_2yr_int_lag_covariate_table<-rbind(sens_analysis_2yr_int_lag_covariate_table, sensitivity_anlys_2yr_int_lag_full_table[-covariate_Index,])
#remove the columns that aren't in odds ratio scale
sens_analysis_2yr_int_lag_covariate_table<-sens_analysis_2yr_int_lag_covariate_table[,-which(colnames(sens_analysis_2yr_int_lag_covariate_table) %in%
                                                                                               c("Coefficient_Estimate", "Coefficient_Lower_Bound", "Coefficient_Upper_Bound", "Standard_Error"))]

colnames(sens_analysis_2yr_int_lag_covariate_table)<-c("Risk_Ratio_Estimates", "RR_95_CI_LB", "RR_95_CI_UB", "p-value")
head(sens_analysis_2yr_int_lag_covariate_table, 10)

#save the table into a CSV
# write.csv(round(sens_analysis_2yr_int_lag_covariate_table, 3), "./Data/coefficients_covariates_10_8_20_2_yr_int_lag.csv")


################# Sensitivity Analysis 5: Number of Attributable Deaths #################
#find the number of deaths attributable to the intervention
#first, we subset the data so that we only focus on the time points for which at least one state had the intervention
attr_deaths_anlys_2yr_int_lag<-sensitivity_anlys_2yr_int_lag[which(sensitivity_anlys_2yr_int_lag$int_2_yr_effect_lag>0),]

#compute the probability of overdose had intervention not occurred
prob_od_no_int_2yr_int_lag<-expit(-coef(lagged_analysis_model)["int_2_yr_effect_lag"]
                                  + logit(attr_deaths_anlys_2yr_int_lag$imputed_deaths/attr_deaths_anlys_2yr_int_lag$population))

#compute the lower and upper bounds of 95% CI of probability of overdose had intervention not occurred
#here, we compute the lower and upper bounds of the 95% CI of all the coefficients using the standard error from the model
coef_lb<-coef(lagged_analysis_model) - 1.96*summary(lagged_analysis_model)$se
coef_ub<-coef(lagged_analysis_model) + 1.96*summary(lagged_analysis_model)$se

#we then calculate the upper and lower bounds of the probability of overdose death had intervention not occurred by using
#the lower and upper bounds of the coefficient of the intervention variable
prob_od_no_int_LB_2yr_int_lag<-expit(-coef_lb[names(coef_lb) == "int_2_yr_effect_lag"]
                                     + logit(attr_deaths_anlys_2yr_int_lag$imputed_deaths/attr_deaths_anlys_2yr_int_lag$population))

prob_od_no_int_UB_2yr_int_lag<-expit(-coef_ub[names(coef_ub) == "int_2_yr_effect_lag"]
                                     + logit(attr_deaths_anlys_2yr_int_lag$imputed_deaths/attr_deaths_anlys_2yr_int_lag$population))


#estimate the number of deaths attributable to the intervention
#first, initialize the vectors to store the numbers
num_attr_od_UB<-num_attr_od_LB<-num_attr_od<-rep(NA, length(unique(attr_deaths_anlys_2yr_int_lag$Time_Period_ID)))


#for each time period, we first find the indices of rows containing data from that time point
#then, we find the total number of deaths attributable to the intervention

index<-1 #keep track of where to store the values in the vector

for(time in sort(unique(attr_deaths_anlys_2yr_int_lag$Time_Period_ID))){
  #find the indices of rows where the time point = time
  time_point_index<-which(attr_deaths_anlys_2yr_int_lag$Time_Period_ID == time)

  #find the number of deaths attributable to intervention = observed number of deaths with intervention - estimated number of deaths had intervention not occurred
  num_attr_od[index]<-sum(attr_deaths_anlys_2yr_int_lag$imputed_deaths[time_point_index]
                          - prob_od_no_int_2yr_int_lag[time_point_index]*attr_deaths_anlys_2yr_int_lag$population[time_point_index])
  #find the lower and upper bounds of the estimated number of deaths attributable to the intervention
  num_attr_od_LB[index]<-sum(attr_deaths_anlys_2yr_int_lag$imputed_deaths[time_point_index]
                             - prob_od_no_int_LB_2yr_int_lag[time_point_index]*attr_deaths_anlys_2yr_int_lag$population[time_point_index])
  num_attr_od_UB[index]<-sum(attr_deaths_anlys_2yr_int_lag$imputed_deaths[time_point_index]
                             - prob_od_no_int_UB_2yr_int_lag[time_point_index]*attr_deaths_anlys_2yr_int_lag$population[time_point_index])
  index<-index + 1
}

num_attr_od_2yr_int_lag<-data.frame("Time_Period_ID" = sort(unique(attr_deaths_anlys_2yr_int_lag$Time_Period_ID)),
                                    "Time_Start" = sort(unique(attr_deaths_anlys_2yr_int_lag$Time_Period_Start)),
                                    "Num_Attr_Deaths" = num_attr_od,
                                    "Num_Attr_Deaths_LB" = num_attr_od_LB,
                                    "Num_Attr_Deaths_UB" = num_attr_od_UB)

#sum up the total number of excess deaths attributable to the intervention
sum(num_attr_od_2yr_int_lag$Num_Attr_Deaths) #16897.29
summary(num_attr_od_2yr_int_lag$Num_Attr_Deaths)
num_attr_od_2yr_int_lag$Time_Start<-as.Date(num_attr_od_2yr_int_lag$Time_Start)

#compute the 95% CI for the total
sum(num_attr_od_2yr_int_lag$Num_Attr_Deaths_LB) #13107.58
sum(num_attr_od_2yr_int_lag$Num_Attr_Deaths_UB) #20650.29

#sum up the number of excess deaths per year
yearly_num_Attr_Deaths_2yr_int_lag<-num_attr_od_2yr_int_lag %>%
  group_by("year" = year(Time_Start)) %>%
  summarise("deaths" = sum(Num_Attr_Deaths), death_lb = sum(Num_Attr_Deaths_LB),
            death_ub = sum(Num_Attr_Deaths_UB))

summary(yearly_num_Attr_Deaths_2yr_int_lag$deaths) #938.74
summary(yearly_num_Attr_Deaths_2yr_int_lag$death_lb) #728.20
summary(yearly_num_Attr_Deaths_2yr_int_lag$death_ub) #1147.24

# pdf("Figures/yearly_num_attr_deaths_w_CI_full_data_10_8_20_2yr_int_lag.pdf")
ggplot(yearly_num_Attr_Deaths_2yr_int_lag, aes(x = year, y = deaths)) + geom_line() + geom_point()
# dev.off()

###########################################################################################
########################## Compiled Attributable Deaths Plot###############################
# pdf("Figures/num_attr_deaths_yearly_for_all_anlys_11_29_20_all_od.pdf")
ggplot(yearly_num_Attr_Deaths_main_analysis) + 
  geom_line(aes(x = as.Date(as.yearmon(year)), y = deaths, group = 1, color = "a"), 
            linetype = "solid") + 
  geom_point(aes(x = as.Date(as.yearmon(year)), y = deaths, group = 1, color = "a"))  +
  geom_line(yearly_num_Attr_Deaths_main_analysis, mapping = aes(x = as.Date(as.yearmon(year)), y = death_lb, group = 1,
                                                                color = "a"),
            linetype = 'dashed') +
  geom_line(yearly_num_Attr_Deaths_main_analysis, mapping = aes(x = as.Date(as.yearmon(year)), y = death_ub, group = 1,
                                                                color ="a"),
            linetype = 'dashed') +
  geom_line(yearly_num_Attr_Deaths_redefine_int, mapping = aes(x = as.Date(as.yearmon(year)), y = deaths, group = 1, 
                                                               color = "d"), linetype = "solid") + 
  geom_point(yearly_num_Attr_Deaths_redefine_int, mapping = aes(x = as.Date(as.yearmon(year)), y = deaths, group = 1, 
                                                                color = "d"))  +
  geom_line(yearly_num_Attr_Deaths_redefine_int, mapping = aes(x = as.Date(as.yearmon(year)), y = death_lb, group = 1,  
                                                               color = "d"),linetype = 'dashed') +
  geom_line(yearly_num_Attr_Deaths_redefine_int, mapping = aes(x = as.Date(as.yearmon(year)), y = death_ub, group = 1,  
                                                               color = "d"), linetype = 'dashed') + 
  geom_line(yearly_num_Attr_Deaths_od_all, mapping = aes(x = as.Date(as.yearmon(year)), y = deaths, group = 1,
                                                         color = "b"), linetype = "solid", alpha = 0.5) +
  geom_point(yearly_num_Attr_Deaths_od_all, mapping = aes(x = as.Date(as.yearmon(year)), y = deaths, group = 1,
                                                          color = "b"), alpha = 0.5)  +
  geom_line(yearly_num_Attr_Deaths_od_all, mapping = aes(x = as.Date(as.yearmon(year)), y = death_lb, group = 1,
                                                         color = "b"),linetype = 'dashed', alpha = 0.5) +
  geom_line(yearly_num_Attr_Deaths_od_all, mapping = aes(x = as.Date(as.yearmon(year)), y = death_ub, group = 1,
                                                         color = "b"), linetype = 'dashed', alpha = 0.5) +
  geom_line(yearly_num_Attr_Deaths_exclude_states, mapping = aes(x = as.Date(as.yearmon(year)), y = deaths, group = 1,
                                                                 color = "c"), linetype = "solid", alpha = 0.5) +
  geom_point(yearly_num_Attr_Deaths_exclude_states, mapping = aes(x = as.Date(as.yearmon(year)), y = deaths, group = 1,
                                                                  color = "c"), alpha = 0.5)  +
  geom_line(yearly_num_Attr_Deaths_exclude_states, mapping = aes(x = as.Date(as.yearmon(year)), y = death_ub, group = 1,
                                                                 color = "c"),linetype = 'dashed', alpha = 0.5) +
  geom_line(yearly_num_Attr_Deaths_exclude_states, mapping = aes(x = as.Date(as.yearmon(year)), y = death_lb, group = 1,
                                                                 color = "c"), linetype = 'dashed', alpha = 0.5) +
 theme(axis.text.y=element_text(size=10, family = "Times"), 
        axis.title=element_text(size=10,face="bold", family = "Times"),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size = 10, family = "Times"),
        panel.background = element_rect("white"), 
        legend.text=element_text(size=10, family = "Times"), legend.position = "bottom", 
        legend.box="vertical", legend.margin=margin()) +
  guides(color=guide_legend(nrow=2,byrow=TRUE)) + 
  labs(x = "Year", y = "Yearly Deaths Attributable to DIH Prosecutions Reported in Media", color = "") + 
  scale_color_manual(values=c('black', 'red', 'green', 'blue'), 
                     labels = c("Main Analysis", "All Drug Overdose Deaths", 
                                "Excluding States with At Least 75% Missing Monthly", "2 Year Effect")) + 
  scale_x_date(date_labels="%Y", breaks = seq(as.Date("2000-01-01"), as.Date("2018-01-01"), by = "2 years")) 

# dev.off()
