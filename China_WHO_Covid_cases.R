##########################ADVANCE DATA Visualization of COVID-19 CASES############################
##################################################################################################
##################################################################################################
df <- read.csv('WHO-COVID-19-global-data.csv')

install.packages("viridis")
install.packages("hrbrthemes")
install.packages("tidyverse")

library(dplyr)
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(tidyr)
library(viridis)

######## Setting the parameters#############
#############################################

colnames(df)[colnames(df) == "ï..Date_reported"] <- "Year"
colnames(df)[colnames(df) == "Country_code"] <- "Code"
colnames(df)[colnames(df) == "Country"] <- "Country"
colnames(df)[colnames(df) == "New_cases"] <- "NewCases"
colnames(df)[colnames(df) == "Cumulative_cases"] <- "CumCases"
colnames(df)[colnames(df) == "New_deaths"] <- "NewDeaths"
colnames(df)[colnames(df) == "Cumulative_deaths"] <- "CumDeaths"
colnames(df)[colnames(df) == "WHO_region"] <- "Region"

View(df)

#################################################
##################################################
head(df)

China <- df %>%
  filter(Country == 'China')

View(China)

China2020 <- df %>%
  filter(Country == 'China' | Year < '2021-01-01')

year_2020 <- df%>%
  filter(Year <'2021-01-01')

View(China2020)
View(year_2020)
##########################################################################
#######################Region Specific###################################
########################################################################

#Histogram
ggplot(df, aes(x = NewCases))+   # New Cases
  geom_histogram(bins = 30, fill="#F4460A", color="#F4460A", alpha=0.9)+
  ggtitle("New Cases")+
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  theme_ipsum()


ggplot(df, aes(x = CumCases))+   # cum cases
  geom_histogram(fill="#F4460A", color="#F4460A", alpha=0.9)+
  ggtitle("Cummulative Cases")+
  xlab("Cases")+
  ylab("Count of Cases")+
  theme_ipsum()

ggplot(df, aes(x = NewDeaths))+   # new deaths
  geom_histogram(fill="#F4460A", color="#F4460A", alpha=0.9)+
  ggtitle("Deaths Cases ")+
  xlab("Cases")+
  ylab("Count of Cases")+
  theme_ipsum()

ggplot(df, aes(x = CumDeaths))+   # Cum deaths
  geom_histogram(fill="#F4460A", color="#F4460A", alpha=0.9)+
  ggtitle("Cummulative Deaths Cases")+
  xlab("Cases in China")+
  ylab("Count of Cases")+
  theme_ipsum()


############################################################################
##########NEW Cases Visualization for China Country########################
###########################################################################

#Histogram
ggplot(China, aes(x = NewCases))+   # New Cases
  geom_histogram(binwidht = 50, fill="#F4460A", color="#F4460A", alpha=0.9)+
  ggtitle("New Cases in China")+
  xlab("New Cases in China")+
  ylab("Count of Cases")+
  theme_ipsum()


ggplot(China, aes(x = CumCases))+   # cum cases
  geom_histogram(fill="#F4460A", color="#F4460A", alpha=0.9)+
  ggtitle("Cummulative Cases in China")+
  xlab("Cases in China")+
  ylab("Count of Cases")+
  theme_ipsum()

ggplot(China, aes(x = NewDeaths))+   # new deaths
  geom_histogram(fill="#F4460A", color="#F4460A", alpha=0.9)+
  ggtitle("Deaths Cases in China")+
  xlab("Cases in China")+
  ylab("Count of Cases")+
  theme_ipsum()

ggplot(China, aes(x = CumDeaths))+   # Cum deaths
  geom_histogram(fill="#F4460A", color="#F4460A", alpha=0.9)+
  ggtitle("Deaths Cases in China")+
  xlab("Cases in China")+
  ylab("Count of Cases")+
  theme_ipsum()

###################################################################################
# #################Hist + Density##################################################
###################################################################################

ggplot(China, aes(x = NewCases))+   # New Cases deaths
  geom_histogram(aes(y = ..density..),fill="#F4460A", color="#F4460A", alpha=0.9) +
  geom_density(lwd = 1, colour = 3,
               fill = 4, alpha = 0.25)+
  ggtitle("New Cases in China")+
  xlab("Cases in China")+
  ylab("Count of Cases")+
  theme_ipsum()

ggplot(China, aes(x = CumCases))+   # Cum Cases
  geom_histogram(aes(y = ..density..),fill="#F4460A", color="#F4460A", alpha=0.9) +
  geom_density(lwd = 1, colour = 3,
               fill = 4, alpha = 0.25)+
  ggtitle("Cummulative Cases in Deaths")+
  xlab("Cases in China")+
  ylab("Count of Cases")+
  theme_ipsum()

ggplot(China, aes(x = NewDeaths))+   # New deaths
  geom_histogram(aes(y = ..density..),fill="#F4460A", color="#F4460A", alpha=0.9) +
  geom_density(lwd = 1, colour = 3,
               fill = 4, alpha = 0.25)+
  ggtitle("New Deaths Cases in China")+
  xlab("Cases in China")+
  ylab("Count of Cases")+
  theme_ipsum()

ggplot(China, aes(x = CumDeaths))+   # Cum deaths
  geom_histogram(aes(y = ..density..),fill="#F4460A", color="#F4460A", alpha=0.9) +
  geom_density(lwd = 1, colour = 3,
               fill = 4, alpha = 0.25)+
  ggtitle("Cummulative Deaths Cases in China")+
  xlab("Cases in China")+
  ylab("Count of Cases")+
  theme_ipsum()

#############################################################################
######################################Density##############################
#############################################################################
ggplot(China, aes(x = NewCases))+
  geom_density(lwd = 1, colour = 3,
               fill = 4, alpha = 0.25)+
  ggtitle("China New Cases")+
  xlab("New Cases in China")+
  ylab("Count of Cases")+
  theme_ipsum()

ggplot(China, aes(x = CumCases))+
  geom_density(lwd = 1, colour = 3,
               fill = 4, alpha = 0.25)+
  ggtitle("China Cumlative Cases")+
  xlab("Cases in China")+
  ylab("Count of Cases")+
  theme_ipsum()

ggplot(China, aes(x = NewDeaths))+
  geom_density(lwd = 1, colour = 3,
               fill = 4, alpha = 0.25)+
  ggtitle("China New Death Cases")+
  xlab("New Deaths in China")+
  ylab("Count of Cases")+
  theme_ipsum()

ggplot(China, aes(x = CumDeaths))+
  geom_density(lwd = 1, colour = 3,
               fill = 4, alpha = 0.25)+
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("China Cumulative Death Cases")+
  theme_ipsum()

##############################################################################
##################BOXPLOT#####################################################
##############################################################################
ggplot(China,aes(x=Country, y=NewCases, fill=NewCases, color = Country)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A boxplot of China - New Cases") +
  xlab("")

ggplot(China,aes(x=Country, y=CumCases, fill=CumCases, color = Country)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A boxplot of China - CumCases") +
  xlab("")

ggplot(China,aes(x=Country, y=NewDeaths, fill=NewDeaths, color = Country)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A boxplot of China") +
  xlab("")

ggplot(China,aes(x=Country, y=CumDeaths, fill=CumDeaths, color = Country)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A boxplot of China") +
  xlab("")


#########################################################
############China, USA and INDIA#########################
#########################################################

IndiaChinUSA<-df%>% filter(Country=='India'|Country=='China'| Country == 'United States of America' & Year < '2020-12-31')
View(IndiaChinUSA)

#Boxplot of China, US, India
  ggplot(IndiaChinUSA,aes(x=Country, y=NewCases, fill=NewCases, color = Country)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A boxplot of China, India and USA") +
  xlab("")


#year -2020
#scatterplot for year 2020

ggplot(year_2020, aes(x = CumCases, y = NewCases, color = Region))+
  geom_point()+
  ggtitle("2020 Cases")+
  xlab(" Cases in 2020")+
  ylab("Count of Cases")+
  theme_ipsum()

ggplot(year_2020, aes(x = CumDeaths, y = NewDeaths, color = Region))+
  geom_point()+
  ggtitle("2020 Death Cases")+
  xlab("Cummulative Deaths in 2020")+
  ylab("New Deaths")+
  theme_ipsum()

ggplot(year_2020, aes(x = NewDeaths, y = NewDeaths, color = Region))+
  geom_point()+
  ggtitle("2020 Death Cases")+
  xlab("Deaths in 2020")+
  ylab("New Deaths")+
  theme_ipsum()

ggplot(year_2020, aes(x = NewCases, y = NewDeaths, color = Region))+
  geom_point()+
  ggtitle("2020 Death Cases")+
  xlab(" Cases in 2020")+
  ylab("New Cases")+
  theme_ipsum()
########################################################
# scatterplot of new_cases and cum cases for # Countries####
########################################################
IndiaChinUSA_All<-df%>% filter(Country=='India'|Country=='China'| Country == 'United States of America')

View(IndiaChinUSA_All)

ggplot(IndiaChinUSA_All, aes(x = Year, y = NewDeaths, color = Country, group = Country))+
geom_line(size = 1.0)+
  scale_x_discrete(breaks=c(2020,2021,2022))+
  xlab("Year")+
  ylab("New Deaths in years")+
  ggtitle("Comparing New Covid Deaths for India USA and China over the Years")+
  theme_ipsum()

ggplot(IndiaChinUSA, aes(x=Year, y = CumDeaths, group=Country, color = Country)) +
  geom_line(size = 1.0) +
  scale_x_discrete(breaks=c(2020,2021,2022))+
  theme_ipsum()+
  ggtitle("Comparing Cumulative Covid Deaths for India USA and China over the Years")+
  xlab("Years")+
  ylab("Cumulative Deaths")

ggplot(IndiaChinUSA, aes(x=Year, y = NewCases, group=Country, color = Country)) +
  geom_line(size = 1.0) +
  scale_x_discrete(breaks=c(2020,2021,2022))+
  theme_ipsum()+
  ggtitle("Comparing New Covid Cases for India USA and China over the Years")+
  xlab("Years")+
  ylab("New Cases")

ggplot(IndiaChinUSA, aes(x=Year, y = CumCases, group=Country, color = Country)) +
  geom_line(size = 1.0) +
  scale_x_discrete(breaks=c(2020,2021,2022))+
  theme_ipsum()+
  ggtitle("Comparing Cumulative Covid Cases for India USA and China over the Year")+
  xlab("Years")+
  ylab("Cumulative Cases")


######################################################################################
######################################################################################
ggplot(IndiaChinUSA, aes(x=Year, y = CumCases, group=Country, color = Country)) +
  geom_line(size = 1.0) +
  scale_x_discrete(breaks=c(2020,2021,2022))+
  theme_ipsum()+
  ggtitle("Comparing Cases for India USA and China over the Years")+
  xlab("Years")+
  ylab("New Cases")+
  geom_vline(xintercept = 500, linetype="dashed", color = "red", size=1.5)+
  annotate(geom="text", x=500, y=5000, label="Re-Rising India Cases and Deaths", color="red")

ggplot(IndiaChinUSA, aes(x=Year, y = CumDeaths, group=Country, color = Country)) +
  geom_line(size = 1.0) +
  scale_x_discrete(breaks=c(2020,2021,2022))+
  theme_ipsum()+
  ggtitle("Comparing Registered Deaths for India USA and China over the Years")+
  xlab("Years")+
  ylab("New Cases")+
  geom_vline(xintercept = 500, linetype="dashed", color = "red", size=1.5)+
  annotate(geom="text", x=500, y=5000, label="Re-Rising India Cases and Deaths", color="red")

#####################Region Specific#######################################################
ggplot(df, aes(x=Region, fill=Region)) + geom_bar() + 
  coord_flip()+
  theme_ipsum()

ggplot(df, aes(x=Region, fill=Region)) + geom_bar() + 
  coord_polar()+
  theme_ipsum()
