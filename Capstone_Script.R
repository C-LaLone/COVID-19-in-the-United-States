###################################
# Capstone Project Script
## Chris LaLone

###################################



library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(readxl)
library(writexl)
library(ggplot2)
library(leaps)

# Load case and death data
cases = read.csv("covid_cases.csv") # Sourced from USA Facts -> url: https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/
deaths = read.csv("covid_deaths.csv") # ^^

# Convert data from fat data to long data
n = ncol(cases)
c.cases = gather(cases, "day", "cases", 5:n)
c.cases = c.cases %>% mutate(day=gsub("X","", day)) %>% mutate(day=as.Date(day, format = "%m.%d.%Y")) 
c.cases$year_month = format(c.cases$day, "%m/%Y")
c.cases = c.cases[,-4]
colnames(c.cases) = c("ID", "County", "State", "Date", "Cases", "Month.Year")
c.cases = filter(c.cases, !grepl("Statewide Unallocated", County)) # removes the statewide unallocated rows

m = ncol(deaths)
c.deaths = gather(deaths, "day", "cases", 5:m)
c.deaths = c.deaths %>% mutate(day=gsub("X","", day)) %>% mutate(day=as.Date(day, format = "%m.%d.%Y"))
c.deaths$year_month = format(c.deaths$day, "%m/%Y")
c.deaths = c.deaths[,-4]
colnames(c.deaths) = c("ID", "County", "State", "Date", "Deaths", "Month.Year")
c.deaths = filter(c.deaths, !grepl("Statewide Unallocated", County))


# age and race data
ar.data = read.csv("Age&Race.csv")  # -> US Census data
ar.data$GEO_ID = gsub("0500000US","", ar.data$GEO_ID) # remove leading US id
ar.data$GEO_ID = str_remove(ar.data$GEO_ID, "^0+")
colnames(ar.data) = c("ID", "Location", "PMale", "PFemale", "P5under", 
                     "P5to9", "P10to14", "P15to18", "P19to24", "P25to34",
                     "P35to44", "P45to54", "P55to59", "P60to64", "P64to74",
                     "P75to84", "P85over", "P18under", "P65over", "Pwhite",
                     "Pblack", "Pnative", "Pasian", "Platino")
ar.data = ar.data[-1,] # remove unnecessary row and column
ar.data = ar.data[,-2]  # will be redundant when sets merge

# load other variables
c.data = read_excel("capstone_data.xlsx") # data set compiled from various different sources
colnames(c.data) = c("ID", "state", "county", "Psmoke", "Pobese", "PUninsured65under", 
                     "RatioPCP", "PHSGrad", "PUnemp", "AirPollution", "LifeExp", 
                     "PDiabetes", "PUninsuredAdults", "Medincome", "Prural")
# filter out any row that isn't a state total
counties = c.data %>% 
  filter(str_detect(county, "County|Borough|Area|Municipality|City|Parish|city"))
# specific data frame for state totals and US total
states = setdiff(c.data, counties)
states = states[-11,] # Washington DC is entered twice. Remove one to avoid redundancy 

# load election 2020 data 
election = read.csv("Election2020.csv")
# create dummy variables for each county depending on which way it voted in the 
# election. Consideration was a simple majority, whichever had the higher total
election = election %>% mutate(Red = ifelse(votes_gop > votes_dem, 1, 0))
election = election %>% mutate(Blue = ifelse(votes_dem > votes_gop, 1, 0))
colnames(election) = c("State", "ID", "County", "GOPVotes", "DemVotes", 
                       "TVotes", "Votediff", "PGOP", "PDem", "Pdiff", 
                       "GOPWin", "DemWin")
election = subset(election, select = -c(State, County, GOPVotes, DemVotes,
                                        TVotes, Votediff)) 
# remove columns that aren't percentages 
# remove state and county names. Will be redundant when the data sets are merged



# Use the ACS 5 year survey to source more independent variables
library(tidycensus)
library(tidyverse)
library(viridis)
options(tigris_use_cache = TRUE)

acs_vars = load_variables(2019, "acs5", cache = TRUE)

# Education data by county. I split it up instead of running all at once because
# computing time was way too long to ask for all at once
# (LessthanHS = "B06009_002", HSGrad = "B06009_003", SCollege = "B06009_004",
#  Bachelor = "B06009_005", Graddeg = "B06009_006")

LessthanHS = get_acs(geography = "county",
                    variables = "B06009_002", 
                    year = 2019, 
                    survey = "acs5", 
                    geometry = F)
HSGrad = get_acs(geography = "county",
                 variables = "B06009_003", 
                 year = 2019, 
                 survey = "acs5", 
                 geometry = F)
SCollege = get_acs(geography = "county",
                   variables = "B06009_004", 
                   year = 2019, 
                   survey = "acs5", 
                   geometry = F)
Bachelor = get_acs(geography = "county",
                   variables = "B06009_005", 
                   year = 2019, 
                   survey = "acs5", 
                   geometry = F)
Graddeg = get_acs(geography = "county",
                  variables = "B06009_006", 
                  year = 2019, 
                  survey = "acs5", 
                  geometry = F)

library(plyr)
# merge all the different education levels into one set
edu.levels = list(LessthanHS, HSGrad, SCollege, Bachelor, Graddeg)
education = join_all(edu.levels, by = "GEOID", type="left")

# eliminate unnecessary columns 
education = subset(education, select = -c(NAME, variable, moe)) 
education = subset(education, select = -c(NAME, variable, moe,
                                          NAME.1, variable.1, moe.1,
                                          NAME.2, variable.2, moe.2,
                                          NAME.3, variable.3, moe.3))
education$GEOID = str_remove(education$GEOID, "^0+") # remove leading 0 from FIPS 
colnames(education) = c("ID", "HS.lower", "HS", "SomeCollege", "College", "GradDeg")

# get public transportation data from ACS 2019
# var ID: B08528_031

pubtrans =  get_acs(geography = "county",
                    variables = "B08528_031", 
                    year = 2019, 
                    survey = "acs5", 
                    geometry = F)
pubtrans = subset(pubtrans, select = -c(NAME, variable, moe)) # remove unnecessary variables
pubtrans$GEOID = str_remove(pubtrans$GEOID, "^0+") # leading 0
colnames(pubtrans) = c("ID", "PT.Use")

# get employment numbers of those who work from home
# cleaning process is same as those above
workfromhome = get_acs(geography = "county",
                      variables = "B08126_091", 
                      year = 2019, 
                      survey = "acs5", 
                      geometry = F)
workfromhome = subset(workfromhome, select = -c(NAME, variable, moe))
workfromhome$GEOID = str_remove(workfromhome$GEOID, "^0+")
colnames(workfromhome) = c("ID", "Workfromhome")

# get disability numbers 
disability = get_acs(geography = "county",
                     variables = "B18101_011", 
                     year = 2019, 
                     survey = "acs5", 
                     geometry = F)
disability = subset(disability, select = -c(NAME, variable, moe))
disability$GEOID = str_remove(disability$GEOID, "^0+")
colnames(disability) = c("ID", "Disability")


# Get total population from ACS so we can convert to percentages of total population
totalpop = get_acs(geography = "county",
                                variables = c("B01001_001"), 
                                year = 2019, 
                                survey = "acs5", 
                                geometry = F)
totalpop = subset(totalpop, select = -c(NAME, variable, moe))
totalpop$GEOID = str_remove(totalpop$GEOID, "^0+")
colnames(totalpop) = c("ID", "TotalPopulation")

# combine all independent variables
indp.var = list(totalpop, disability, workfromhome, pubtrans, education,
                election, counties, ar.data) 
df.ind = join_all(indp.var, by = "ID", type ="left")

# move the ID, State, and County to the front of the data frame
df.ind = df.ind %>% select(ID, state, county, everything())

# Rename certain columns
names(df.ind)[names(df.ind) == "state"] = "State"
names(df.ind)[names(df.ind) == "county"] = "County"

# Convert certain variables from raw number to percentage of total population
df.ind = df.ind %>% mutate(PDisability = df.ind$Disability/df.ind$TotalPopulation)%>%
  mutate(PWFH = df.ind$Workfromhome/df.ind$TotalPopulation) %>%
  mutate(PPT.Use = df.ind$PT.Use/df.ind$TotalPopulation) %>%
  mutate(PHS.lower = df.ind$HS.lower/df.ind$TotalPopulation) %>%
  mutate(PHS = df.ind$HS/df.ind$TotalPopulation) %>%
  mutate(PS.College = df.ind$SomeCollege/df.ind$TotalPopulation) %>%
  mutate(PCollege = df.ind$College/df.ind$TotalPopulation) %>% 
  mutate(PGrad = df.ind$GradDeg/df.ind$TotalPopulation)

# Remove the raw variables used to calculate the percentages from the block above
df.ind = df.ind[,-c(5:12)]

str(df.ind) # note that the age and race demographics are loaded as characters
df.ind$ID = as.numeric(df.ind$ID) # ID also loaded as a character
df.ind[,22:43] = sapply(df.ind[,22:43],as.numeric) # convert age and race to numeric
str(df.ind) # now its all better 

# drop certain age columns. Information is already covered by the under 18 variable 
# and the over 65 variable
df.ind = subset(df.ind, select = -c(P5under, P5to9, P10to14, P15to18, P64to74,
                                    P75to84, P85over))

# age and race variables need to be converted from % to decimal
df.ind[,22:36] = df.ind[,22:36]/100

# Convert election data to a factor
df.ind$GOPWin = as.factor(df.ind$GOPWin)
df.ind$DemWin = as.factor(df.ind$DemWin)

# remove other useless election data
df.ind = subset(df.ind, select = -c(PGOP, PDem, Pdiff))

# remove all rows containing to Puerto Rico --> ID < 72000
df.ind = subset(df.ind, ID < 72000)

summary(df.ind) # check for amount of NA values

# Missing value management
library(Hmisc)
# simple imputation with mean. Missing values are relatively low so the results 
# should not be too effected
df.ind$RatioPCP = impute(df.ind$RatioPCP, mean)
df.ind$PHSGrad = impute(df.ind$PHSGrad, mean)
df.ind$AirPollution = impute(df.ind$AirPollution, mean)
df.ind$LifeExp = impute(df.ind$LifeExp, mean)
df.ind$GOPWin = impute(df.ind$GOPWin, 1)
df.ind$DemWin = impute(df.ind$DemWin, 0)
df.ind$PPT.Use = impute(df.ind$PPT.Use, mean)


# Constructing the sets for specific dates of interest

# Isolate the first date of interest (early on in the pandemic) --> 04/01/2020
case.date1 = subset(c.cases, Date == "2020-04-01") # Isolate the first date to analyze
death.dates = subset(c.deaths, select = c(Date, Deaths, ID))
death.date1 = subset(death.dates, Date == "2020-04-01")

# merge case and death totals with independent variables
date1.list = list(case.date1, death.date1, df.ind)
date1 = join_all(date1.list, by = "ID", type = "left")
date1 = date1[,-c(6,7,9,10)] # remove redundant columns


# Convert Case total and Death total to case rate and death rate
date1 = date1 %>% mutate(CaseRate = date1$Cases/date1$TotalPopulation) %>%
  mutate(DeathRate = date1$Deaths/date1$TotalPopulation)
date1 = date1 %>% select(ID, State, County, Date, Cases, CaseRate, Deaths,
                         DeathRate, everything())
# correlation analysis
library(corrplot)

# select only numerical variables
date.num1 = date1 %>% select_if(is.numeric)
summary(date.num1)
date.num1 = na.omit(date.num1) # minimal NA values remaining (less than 10 per variable so this should be ok)
str(date.num1)
corrplot(cor(date.num1[,-1]), type="lower", method="square")

# construct the regression data set, (remove non-numerical variables like ID, Date, etc.)
# also remove highly correlated variables
reg.set1 = subset(date1, select = -c(ID, State, County, Date, PUninsured65under))



# Isolate the second date of interest (middle of pandemic) --> 10/01/2020
case.date2 = subset(c.cases, Date == "2020-10-01") 
death.dates2 = subset(c.deaths, select = c(Date, Deaths, ID))
death.date2 = subset(death.dates2, Date == "2020-10-01")

date2.list = list(case.date2, death.date2, df.ind)
date2 = join_all(date2.list, by = "ID", type = "left")
date2 = date2[,-c(6,7,9,10)]


# create case rate and death rate variables for 10/01/2020
date2 = date2 %>% mutate(CaseRate = date2$Cases/date2$TotalPopulation) %>%
  mutate(DeathRate = date2$Deaths/date2$TotalPopulation)
# create fatality rate variable DeathRate/CaseRate
date2 = date2 %>% mutate(FatalityRate = date2$DeathRate/date2$CaseRate)

# create mask mandate variable for states with active mandate as of 10/1/20
mask = subset(states, select = state) # isolate state abbreviations
mask = mask[-1,] # remove US row
# states with active mandates by this data
mask.yes = c("AL","AR","CA","CO","CT", "DC","DE","HI","IL","IN","KS","KY","LA",
               "ME","MD","MA","MI","MN","MS","MT","NV","NJ","NM","NY","NC",
               "OH","OR","PA","RI","SC","TX","VT","VA","WA","WV","WI")
mask$mandate = ifelse(mask$state %in% mask.yes, 1,0) # assign dummies 
colnames(mask) = c("State", "Mandate")
date2 = left_join(date2, mask, by="State") # add variable to the set

# reorder columns
date2 = date2 %>% select(ID, State, County, Date, Cases, CaseRate, Deaths,
                         DeathRate, FatalityRate, Mandate, everything())

# select only numerical variables for correlation
date.num2 = date2 %>% select_if(is.numeric)
date.num2 = na.omit(date.num2)
str(date.num2)
corrplot(cor(date.num2[,-1]), type="lower", method="square")

# construct the regression data set, (remove fips, date, county, and other highly correlated variables)
reg.set2 = subset(date2, select = -c(ID, State, County, Date, PUninsured65under))
reg.set2$Mandate = as.factor(reg.set2$Mandate) # make mask mandate a factor



# isolate the third date of interest (most recent) --> 02/20/2021
case.date3 = subset(c.cases, Date == "2021-02-20") 
death.dates3 = subset(c.deaths, select = c(Date, Deaths, ID))
death.date3 = subset(death.dates3, Date == "2021-02-20")

date3.list = list(case.date3, death.date3, df.ind)
date3 = join_all(date3.list, by = "ID", type = "left")
date3 = date3[,-c(6,7,9,10)]


# Case Rate and Death Rate variables
date3 = date3 %>% mutate(CaseRate = date3$Cases/date3$TotalPopulation) %>%
  mutate(DeathRate = date3$Deaths/date3$TotalPopulation) 
 
date3 = date3 %>% mutate(FatalityRate = date3$DeathRate/date3$CaseRate)

# create mask mandate variable for states with active mandate as of 02/20/2021
mask2 = subset(states, select = state)
mask2 = mask2[-1,]
# addition of Utah, North Dakota, Iowa, New Hampshire, and Wyoming 
mask.yes2 = c("AL","AR","CA","CO","CT", "DC","DE","HI","IL","IN","KS","KY","LA",
             "ME","MD","MA","MI","MN","MS","MT","NV","NJ","NM","NY","NC",
             "OH","OR","PA","RI","SC","TX","VT","VA","WA","WV","WI","UT","ND",
             "IA","NH","WY") 
mask2$mandate = ifelse(mask2$state %in% mask.yes2, 1,0)
colnames(mask2) = c("State", "Mandate")
date3 = left_join(date3, mask2, by="State")

date3 = date3 %>% select(ID, State, County, Date, Cases, CaseRate, Deaths,
                         DeathRate, FatalityRate, Mandate, everything())

# select only numerical variables for correlation
date.num3 = date3 %>% select_if(is.numeric)
date.num3 = na.omit(date.num3)
str(date.num3)
corrplot(cor(date.num3[,-1]), type="lower", method="square")

# construct the regression data set, (remove fips, date, county, and any highly correlated variables)
reg.set3 = subset(date3, select = -c(ID, State, County, Date, PUninsured65under))
reg.set3$Mandate = as.factor(reg.set3$Mandate)


# EDA


library(tidyverse)
# EDA Section for date1

# The 10 counties with highest total cases on 04/01/2020 
highestcases1 = date1 %>% rownames_to_column() %>% top_n(10, Cases) %>% pull(rowname)
topcases1 = date1[c(205,611,1313,1831,1852,1858,1859,1869,1880,1888),c(3,5)]

ggplot(topcases1, aes(x=County, y = Cases))+
  geom_bar(stat = "identity", fill = "steelblue")+
  theme_classic()+coord_flip()+
  labs(title = "Highest Case Totals, 04/1/20", y = "Total Cases")+
  geom_text(aes(label=Cases), position = "identity", hjust = 1.1, size=4, color = "white")

# 10 Counties with highest case rates on 04/01/2020
h.caserate1 = date1 %>% rownames_to_column() %>% top_n(10, CaseRate) %>% pull(rowname)
t.caserate1 = date1[c(264,434,558,1149,1831,1858,1869,1871,1872,1888),c(3,6)]

ggplot(t.caserate1, aes(x=County, y = CaseRate))+
  geom_bar(stat= "identity", fill = "steelblue")+ coord_flip()+
  labs(title = "Highest Infection Rates, 04/01/20", y = "Infection Rate")+
  theme_classic()


# Top 10 counties by total deaths 04/01/2020
highestdeaths1 = date1 %>% rownames_to_column() %>% top_n(10, Deaths) %>% pull(rowname)
topdeaths1 = date1[c(highestdeaths1),c(3,7)]

ggplot(topdeaths1, aes(x=County, y = Deaths))+
  geom_bar(stat = "identity", fill = "steelblue")+
  theme_classic()+coord_flip()+
  labs(title = "Highest Total Deaths, 04/01/2020", y = "Total Deaths")+
  geom_text(aes(label=Deaths), hjust = 1.2, size=4, color = "white")



# EDA Date 2

# Top 10 counties by total cases 10/01/2020
highestcases2 = date2 %>% rownames_to_column() %>% top_n(10, Cases) %>% pull(rowname)
topcases2 = date2[c(highestcases2),c(3,5)]

ggplot(topcases2, aes(x=County, y = Cases))+
  geom_bar(stat = "identity", fill = "darkgreen")+
  theme_classic()+coord_flip()+
  labs(title = "Highest Total Cases, 10/01/20", y="Total Cases")+
  geom_text(aes(label=Cases), position = "identity", hjust = 1.1, size=4, color = "white")

h.caserate2 = date2 %>% rownames_to_column() %>% top_n(10, CaseRate) %>% pull(rowname)
t.caserate2 = date2[c(h.caserate2),c(3,6,11)]

ggplot(t.caserate2, aes(x=County, y = CaseRate))+
  coord_flip()+
  geom_bar(stat="identity", fill = "darkgreen")+
  labs(title = "Highest Infection Rates, 10/01/20", y="Infection Rate")+theme_classic()+
  geom_text(aes(label=TotalPopulation), position = "identity", hjust = 1.1, size=4, color = "white")

highestdeaths2 = date2 %>% rownames_to_column() %>% top_n(10, Deaths) %>% pull(rowname)
topdeaths2 = date2[c(highestdeaths2),c(3,7,11)]

h.dr2 = date2 %>% rownames_to_column() %>% top_n(10, DeathRate) %>% pull(rowname)
t.dr2 = date2[c(h.dr2), c(3,8,11)]

ggplot(topdeaths2, aes(x=County, y = Deaths))+
  geom_bar(stat = "identity", fill = "darkgreen")+
  theme_classic()+coord_flip()+
  labs(title = "Highest Total Deaths, 10/01/20")+
  geom_text(aes(label=Deaths), position = "identity", hjust = 1.1, size=4, color = "white")

ggplot(t.dr2, aes(x=County, y = DeathRate))+
  coord_flip()+
  geom_bar(stat="identity", fill = "darkgreen")+
  labs(title = "Highest Death Rates, 10/01/20", y = "Death Rate")+
  geom_text(aes(label=TotalPopulation), position = "identity", hjust = 1.1, size=4, color = "white")+
  theme_classic()

# Date 3
highestcases3 = date3 %>% rownames_to_column() %>% top_n(10, Cases) %>% pull(rowname)
topcases3 = date3[c(highestcases3),c(3,5)]

highestdeaths3 = date3 %>% rownames_to_column() %>% top_n(10, Deaths) %>% pull(rowname)
topdeaths3 = date3[c(highestdeaths3),c(3,7)]

h.caserate3 = date3 %>% rownames_to_column() %>% top_n(10, CaseRate) %>% pull(rowname)
caserate3 = date3[c(h.caserate3),c(3,6,11)]

h.dr3 = date3 %>% rownames_to_column() %>% top_n(10, DeathRate) %>% pull(rowname)
deathrate3 = date3[c(h.dr3),c(3,8,11)]

ggplot(topcases3, aes(x=County, y = Cases))+
  geom_bar(stat = "identity", fill = "deeppink3")+
  theme_classic()+coord_flip()+
  labs(title = "Highest Total Cases, 02/20/21")+
  geom_text(aes(label=Cases), position = "identity", hjust = 1.1, size=4, color = "white")

ggplot(topdeaths3, aes(x=County, y = Deaths))+
  geom_bar(stat = "identity", fill = "deeppink3")+
  theme_classic()+coord_flip()+
  labs(title = "Highest Total Deaths, 02/20/21")+
  geom_text(aes(label=Deaths), position = "identity", hjust = 1.1, size=4, color = "white")

ggplot(caserate3, aes(x=County, y = CaseRate))+
  geom_bar(stat = "identity", fill = "deeppink3")+
  theme_classic()+coord_flip()+
  labs(title = "Highest Infection Rate, 02/20/21", y="Infection Rate")+
  geom_text(aes(label=TotalPopulation), position = "identity", hjust = 1.1, size=4, color = "white")

ggplot(deathrate3, aes(x=County, y = DeathRate))+
  geom_bar(stat = "identity", fill = "deeppink3")+
  theme_classic()+coord_flip()+
  labs(title = "Highest Death Rate, 02/20/21", y="Death Rate")+
  geom_text(aes(label=TotalPopulation), position = "identity", hjust = 1.1, size=4, color = "white")

# More Correlation
# Just a few different plots with different variables to see their correlations using
# the different dates


df.corr1 = subset(date.num3, select = -c(ID, PHSGrad, Cases, Deaths))
co.plot1 = corrplot(cor(df.corr1), type="lower", method="square")

df.corr2 = subset(df.corr1, select = c(CaseRate, DeathRate, FatalityRate,
                                       Mandate, TotalPopulation, Medincome,
                                       Prural, PMale, PFemale, Pwhite, Pblack,
                                       Pnative, Pasian, Platino, PPT.Use))
co.plot2 = corrplot(cor(df.corr2), type="lower", method="square")

df.corr3 = subset(df.corr1, select = c(CaseRate, DeathRate, FatalityRate,
                                       Mandate, TotalPopulation, Medincome,
                                       Psmoke, Pobese, RatioPCP,
                                       PUnemp, AirPollution, PDiabetes,
                                       P25to34, P35to44, P45to54, P55to59, P60to64, 
                                       P18under, P65over, PDisability, PHS.lower, 
                                       PHS, PS.College, PCollege, PGrad))
co.plot3 = corrplot(cor(df.corr3), type="lower", method="square")

df.corr4 = subset(date1, select = c(CaseRate, DeathRate,
                                      TotalPopulation, Medincome,
                                      Prural, PMale, PFemale, Pwhite, Pblack,
                                      Pnative, Pasian, Platino, PPT.Use))
df.corr4 = na.omit(df.corr4)
co.plot4 = corrplot(cor(df.corr4), type="lower", method="square")


# Regression

# Date 1 -->  4/01/2020

# Dependent Variable: Case Rate
reg.set4 = subset(reg.set1, select = -c(DemWin,PHSGrad, PMale, Pwhite, Cases, Deaths,
                                        DeathRate))
fitdate1.1 = lm(CaseRate ~ ., reg.set4)
summary(fitdate1.1)

# Dependent Variable: Cases 
reg.set5 = subset(reg.set1, select = -c(DemWin, PHSGrad, PMale, Pwhite, CaseRate, Deaths,
                                        DeathRate))
fitdate1.2 = lm(Cases ~ ., reg.set5)
summary(fitdate1.2)

# Dependent Variable: Death Rate
reg.set6 = subset(reg.set1, select = -c(DemWin, PHSGrad, PMale, Pwhite, CaseRate, Deaths,
                                        Cases))

fitdate1.3 = lm(DeathRate ~ ., reg.set6)
summary(fitdate1.3)

# Dependent Variable: Deaths
reg.set7 = subset(reg.set1, select = -c(DemWin, PHSGrad, PMale, Pwhite, CaseRate, DeathRate,
                                        Cases))
fitdate1.4 = lm(Deaths ~ ., reg.set7)
summary(fitdate1.4)

# Selecting Specific variables as predictors
# Dependent Variable: Case Rate 
fitdate1.5 = lm(CaseRate~ Psmoke+Pobese+AirPollution+
                  PUnemp+Medincome+P19to24+P25to34+P35to44+P45to54+P55to59+
                  P60to64+Pblack+Pasian+Platino+Pnative+PPT.Use+PDisability+
                  PHS.lower+PHS+PS.College+PCollege+PGrad+PWFH,
                reg.set1)
summary(fitdate1.5)

# Dependent Variable: Death Rate
fitdate1.6 = lm(DeathRate~ TotalPopulation+Psmoke+Pobese+AirPollution+
                  PUnemp+Medincome+P19to24+P25to34+P35to44+P45to54+P55to59+
                  P60to64+Pblack+Pasian+Platino+Pnative+PPT.Use+PDisability+
                  PHS.lower+PHS+PS.College+PCollege+PGrad+PWFH,
                reg.set1)
summary(fitdate1.6)



# Date 2 --> 10/1/2020


# Dependent Variable: Case Rate
reg.set8 = subset(reg.set2, select = -c(DemWin,PHSGrad, PMale, Pwhite, Cases, Deaths,
                                        DeathRate))
fitdate2.1 = lm(CaseRate ~ ., reg.set8)
summary(fitdate2.1)

# Dependent Variable: Cases
reg.set9 = subset(reg.set2, select = -c(DemWin, PHSGrad, PMale, Pwhite, CaseRate, Deaths,
                                        DeathRate, TotalPopulation))
fitdate2.2 = lm(Cases ~ ., reg.set9)
summary(fitdate2.2)

# Dependent Variable: Death Rate
reg.set10 = subset(reg.set2, select = -c(DemWin, PHSGrad, PMale, Pwhite, CaseRate, Deaths,
                                         Cases, FatalityRate))

fitdate2.3 = lm(DeathRate ~ ., reg.set10)
summary(fitdate2.3)

# Dependent Variable: Deaths
reg.set11 = subset(reg.set2, select = -c(DemWin, PHSGrad, PMale, Pwhite, CaseRate, DeathRate,
                                         Cases, TotalPopulation))
fitdate2.4 = lm(Deaths ~ ., reg.set11)
summary(fitdate2.4)

# Selecting Specific variables as predictors
# Dependent Variable: Case Rate
fitdate2.5 = lm(CaseRate~ Psmoke+Pobese+AirPollution+
                  PUnemp+Medincome+P19to24+P25to34+P35to44+P45to54+P55to59+
                  P60to64+P65over+Pblack+Pasian+Platino+Pnative+PPT.Use+PDisability+
                  PHS.lower+PHS+PS.College+PCollege+PGrad+PWFH,
                reg.set2)
summary(fitdate2.5)

# Dependent Variable: Death Rate
fitdate2.6 = lm(DeathRate~ TotalPopulation+Psmoke+Pobese+AirPollution+
                  PUnemp+Medincome+P19to24+P25to34+P35to44+P45to54+P55to59+
                  P60to64+Pblack+Pasian+Platino+Pnative+PPT.Use+PDisability+
                  PHS.lower+PHS+PS.College+PCollege+PGrad+PWFH,
                reg.set2)
summary(fitdate2.6)

# Date 3 --> 02/20/2021

# Dependent Variable: Case Rate
reg.set12 = subset(reg.set3, select = -c(DemWin,PHSGrad, PMale, Pwhite, Cases, Deaths,
                                        DeathRate))
fitdate3.1 = lm(CaseRate ~ ., reg.set12)
summary(fitdate3.1)

# Dependent Variable: Cases
reg.set13 = subset(reg.set3, select = -c(DemWin,PHSGrad, PMale, Pwhite, CaseRate, Deaths,
                                         DeathRate, TotalPopulation))
fitdate3.2 = lm(Cases ~ ., reg.set13)
summary(fitdate3.2)

# Dependent Variable: Death Rate
reg.set14 = subset(reg.set3, select = -c(DemWin,PHSGrad, PMale, Pwhite, CaseRate, Deaths,
                                         Cases, FatalityRate))
fitdate3.3 = lm(DeathRate ~ ., reg.set14)
summary(fitdate3.3)

# Dependent Variable: Deaths
reg.set15 = subset(reg.set3, select = -c(DemWin,PHSGrad, PMale, Pwhite, CaseRate, DeathRate,
                                         Cases, TotalPopulation))
fitdate3.4 = lm(Deaths ~ ., reg.set15)
summary(fitdate3.4)

# Selecting Specific variables as predictors
# Dependent Variable: Case Rate
fitdate3.5 = lm(CaseRate~ TotalPopulation+Psmoke+Pobese+AirPollution+
                  PUnemp+Medincome+P19to24+P25to34+P35to44+P45to54+P55to59+
                  P60to64+Pblack+Pasian+Platino+Pnative+PPT.Use+PDisability+
                  PHS.lower+PHS+PS.College+PCollege+PGrad+PWFH,
                reg.set3)
summary(fitdate3.5)

# Dependent Variable: Death Rate
fitdate3.6 = lm(DeathRate~ Psmoke+Pobese+AirPollution+
                  PUnemp+Medincome+P19to24+P25to34+P35to44+P45to54+P55to59+
                  P60to64+Pblack+Pasian+Platino+Pnative+PPT.Use+PDisability+
                  PHS.lower+PHS+PS.College+PCollege+PGrad+PWFH,
                reg.set3)
summary(fitdate3.6)


# Additional Analysis 

# Regression Subsets model selection:
# Use the regsubsets function from the "leaps" library to pinpoint the most 
# important variables from the given model

# For the sake of simplicity, nvmax = 5. Top 5 most important variables.
# Removed variables like  Total Population and ones that would draw too much influence. 
# This will stay consistent during each iteration.


# 04/01/2020


# Predicting Case Rate
reg.full1 = regsubsets(CaseRate~ Psmoke+Pobese+AirPollution+
                         PUnemp+Medincome+P19to24+P25to34+P35to44+P45to54+P55to59+
                         P60to64+Pblack+Pasian+Platino+Pnative+PPT.Use+PDisability+
                         PHS.lower+PHS+PS.College+PCollege+PGrad+PWFH+
                         P65over+GOPWin+P18under+LifeExp, reg.set1, nvmax=5)
summary(reg.full1)
# 1) GOPWin1 ---> (Counties where majority voted for Trump in 2020)
# 2) Medincome, Pblack
# 3) Medincome, Pblack, PPT.Use
# 4) Medincome, Pblack, PPT.Use, GOPWin1
# 5) Medincome, Pblack, PPT.Use, Pobese, PS.College

# Predicting Death Rate
reg.full2 = regsubsets(DeathRate~ Psmoke+Pobese+AirPollution+
                         PUnemp+Medincome+P19to24+P25to34+P35to44+P45to54+P55to59+
                         P60to64+Pblack+Pasian+Platino+Pnative+PPT.Use+PDisability+
                         PHS.lower+PHS+PS.College+PCollege+PGrad+PWFH+
                         P65over+GOPWin+P18under+LifeExp, reg.set1, nvmax=5)
summary(reg.full2)
# 1) Pblack
# 2) Pblack, Pasian
# 3) Pblack, Pasian, PPT.Use
# 4) Pblack, Pasian, PPT.Use, Pobese
# 5) Pblack, Pasian, PPT.Use, Pobese, P45to54


# 10/01/2020

# Predicting Case Rate
reg.full3 = regsubsets(CaseRate~ Psmoke+Pobese+AirPollution+
                         PUnemp+Medincome+P19to24+P25to34+P35to44+P45to54+P55to59+
                         P60to64+Pblack+Pasian+Platino+Pnative+PPT.Use+PDisability+
                         PHS.lower+PHS+PS.College+PCollege+PGrad+PWFH+
                         P65over+GOPWin+P18under+LifeExp, reg.set2, nvmax=5)
summary(reg.full3)
# 1) Pblack
# 2) Pblack, P60to64
# 3) Pblack, P60to64, PHS.lower
# 4) Pblack, P60to64, PHS.lower, P55to59                  
# 5) Pblack, P60to64, PHS.lower, P55to59, PUnemp

# Predicting Death Rate
reg.full4 = regsubsets(DeathRate~ Psmoke+Pobese+AirPollution+
                         PUnemp+Medincome+P19to24+P25to34+P35to44+P45to54+P55to59+
                         P60to64+Pblack+Pasian+Platino+Pnative+PPT.Use+PDisability+
                         PHS.lower+PHS+PS.College+PCollege+PGrad+PWFH+
                         P65over+GOPWin+P18under+LifeExp, reg.set2, nvmax=5)
summary(reg.full4)
# 1) Pblack
# 2) Pblack, Platino
# 3) Pblack, Platino, PHS.lower
# 4) Pblack, Platino, PHS.lower, PS.College
# 5) Pblack, Platino, PS.College, P65over, P18under


# 02/20/21

# Predicting Case Rate
reg.full5 = regsubsets(CaseRate~ Psmoke+Pobese+AirPollution+
                         PUnemp+Medincome+P19to24+P25to34+P35to44+P45to54+P55to59+
                         P60to64+Pblack+Pasian+Platino+Pnative+PPT.Use+PDisability+
                         PHS.lower+PHS+PS.College+PCollege+PGrad+PWFH+
                         P65over+GOPWin+P18under+LifeExp, reg.set3, nvmax=5)
summary(reg.full5)
# 1) PGrad
# 2) PGrad, P60to64
# 3) PGrad, P60to64, PUnemp
# 4) PGrad, P60to64, PUnemp, P55to59
# 5) PGrad, P60to64, PUnemp, P55to59, Pblack

#Predicting Death Rate
reg.full6 = regsubsets(DeathRate~ Psmoke+Pobese+AirPollution+
                         PUnemp+Medincome+P19to24+P25to34+P35to44+P45to54+P55to59+
                         P60to64+Pblack+Pasian+Platino+Pnative+PPT.Use+PDisability+
                         PHS.lower+PHS+PS.College+PCollege+PGrad+PWFH+
                         P65over+GOPWin+P18under+LifeExp, reg.set3, nvmax=5)
summary(reg.full6)
# 1) PGrad
# 2) PGrad, Pblack
# 3) PGrad, Pblack, Platino
# 4) PGrad, Pblack, P60to64, P65over
# 5) PGrad, Pblack, P65over, P18under, PS.College




# Combine Case and Death totals for each date in order to export to .csv file
# This is the data used to construct the geographical maps in Tableau
totals1 = merge(case.date1, death.date1, by = "ID")
totals1 = subset(totals1, select = -c(Month.Year, Date.x, Date.y))
names(totals1)[names(totals1) == "Date.x"] = "Date"
names(totals1)[names(totals1) == "Cases"] = "Cases1"
names(totals1)[names(totals1) == "Deaths"] = "Deaths1"

totals2 = merge(case.date2, death.date2, by = "ID")
totals2 = subset(totals2, select = -c(Month.Year, Date.x, Date.y))
names(totals2)[names(totals2) == "Date.x"] = "Date"
names(totals2)[names(totals2) == "Cases"] = "Cases2"
names(totals2)[names(totals2) == "Deaths"] = "Deaths2"


totals3 = merge(case.date3, death.date3, by = "ID")
totals3 = subset(totals3, select = -c(Month.Year, Date.x, Date.y))
names(totals3)[names(totals3) == "Date.x"] = "Date"
names(totals3)[names(totals3) == "Cases"] = "Cases3"
names(totals3)[names(totals3) == "Deaths"] = "Deaths3"

totals_list = list(totals1, totals2, totals3)
full_totals = join_all(totals_list, by ="ID", type = "left")
full_totals = full_totals[,-c(6,7,10,11)]

write.csv(full_totals, "COVID_cases_deaths.csv", row.names = F)


# Time Series Visulaization 

# isolate specific counties/ areas of note

# Los Angeles County --> FIPS ID = 6037 
lacases = subset(c.cases, ID == 6037) 
ladeaths = subset(c.deaths, ID == 6037)

lacases = lacases[,c(4,5)]
ladeaths = ladeaths[,c(4,5)]

names(lacases)[names(lacases) == "Cases"] = "LA"
names(ladeaths)[names(ladeaths) == "Deaths"] = "LA"

# Maricopa County, AZ --> FIPS ID = 4013
mari.cases = subset(c.cases, ID == 4013)
mari.deaths = subset(c.deaths, ID == 4013)

mari.cases = mari.cases[,c(4,5)]
mari.deaths = mari.deaths[,c(4,5)]

names(mari.cases)[names(mari.cases) == "Cases"] = "Maricopa"
names(mari.deaths)[names(mari.deaths) == "Deaths"] = "Maricopa"

# Cook County, IL --> FIPS ID = 17031
cook.cases = subset(c.cases, ID == 17031)
cook.deaths = subset(c.deaths, ID == 17031)

cook.cases = cook.cases[,c(4,5)]
cook.deaths = cook.deaths[,c(4,5)]

names(cook.cases)[names(cook.cases) == "Cases"] = "Cook"
names(cook.deaths)[names(cook.deaths) == "Deaths"] = "Cook"

# Queens County, NY --> FIPS ID = 36081
queens.cases = subset(c.cases, ID == 36081)
queens.deaths = subset(c.deaths, ID == 36081)

queens.cases = queens.cases[,c(4,5)]
queens.deaths = queens.deaths[,c(4,5)]

names(queens.cases)[names(queens.cases) == "Cases"] = "Queens"
names(queens.deaths)[names(queens.deaths) == "Deaths"] = "Queens"

# Harris County, TX --> FIPS ID = 48201
harris.cases = subset(c.cases, ID == 48201)
harris.deaths = subset(c.deaths, ID == 48201)

harris.cases = harris.cases[,c(4,5)]
harris.deaths = harris.deaths[,c(4,5)]

names(harris.cases)[names(harris.cases) == "Cases"] = "Harris"
names(harris.deaths)[names(harris.deaths) == "Deaths"] = "Harris"


# Merge the data sets into one 
county.cases.list = list(lacases, mari.cases, cook.cases, queens.cases, harris.cases) 
county.deaths.list = list(ladeaths, mari.deaths, cook.deaths, queens.deaths, harris.deaths)

ts.cases = join_all(county.cases.list, by = "Date", type = "left")
ts.deaths = join_all(county.deaths.list, by = "Date", type = "left")

# Convert Data from fat form to long form
ts.cases.long = gather(ts.cases, key="County", value="Cases", LA, Maricopa, Cook, Queens, Harris)
ts.deaths.long = gather(ts.deaths, key="County", value="Deaths", LA, Maricopa, Cook, Queens, Harris)


min1 = as.Date("2020-02-28")
max1 = as.Date("2021-02-28")

ggplot(ts.cases.long, aes(x = Date, y = Cases))+ 
  geom_line(aes(color = County), size = 1.5)+
  scale_color_manual(values = c("green4", "darkorange2", "dodgerblue3", "violetred","grey5")) +
  theme_minimal()+
  labs(title = "# Case (03/20-03/21)", x="")+
  scale_x_date(limits = c(min1,max1))

ggplot(ts.deaths.long, aes(x = Date, y = Deaths))+ 
  geom_line(aes(color = County), size = 1.5)+
  scale_color_manual(values = c("green4", "darkorange2", "dodgerblue3", "violetred","grey5")) +
  theme_minimal()+
  labs(title = "# Deaths (03/20-03/21)", x = "")+
  scale_x_date(limits = c(min1,max1))
