## Survey Project in R

#install and load "dplyr" and "ggplot2" libraries
install.packages("dplyr")
install.packages ("ggplot2")
library(dplyr)
library(ggplot2)

# download survey data file from website
download.file("http://files.figshare.com/2236372/combined.csv",  "data/portal_data_joined.csv")# data for project is in the portal_data_joined.csv

# import survey data file
surveys <- read.csv('data/portal_data_joined.csv')

## explore relationship between weight and hindfoot length for each species

#data parsing

# Select the species id, hindfoot length and weight data from datafile "surveys", removes NA from weight and hindfoot length. Puts it all in the new object " survey_sml
surveys_sml<- surveys %>%  
  select(species_id, hindfoot_length, weight)%>%
  filter(!is.na(weight))%>%  
  filter(!is.na(hindfoot_length))

#Find the mean for weight and group by species_id from object surveys_sml and put it into a new object called weight_mean
weight_mean<-surveys_sml%>%
  group_by(species_id)%>%
  summarize(mean_weight = mean(weight, na.rm = TRUE))

# Find the mean for hindfoot length  and group by species _id from object surveys_sml and put it into new object called hindfoot_length_mean
hindfoot_length_mean<-surveys_sml%>%
group_by(species_id)%>%
  summarize(mean_hindfoot_length = mean(hindfoot_length, na.rm = TRUE))

# combine data from mean_weight and mean_hindfoot_length into one object called Combined_means
Combined_means<-cbind(weight_mean, hindfoot_length_mean)
  
#build figure to show relationship between average weight and hindfoot length

#Scatter plot with modified color, size of points in the point layer,  modify x-axis and y-axis labels and add a title
ggplot(data = Combined_means, aes(x=mean_weight, y=mean_hindfoot_length, color = species_id)) + geom_point(size = 2.5) + xlab("Average Weight - g") + ylab("Average Hindfoot Length - mm") + ggtitle("Figure 1")


#export graph
ggsave("Figures/Figure 1.pdf")

## Run ANOVA statistical test on relationship between weight and hindfoot length to see if numbers indicate a relationship

# ANOVA: is variation among groups different? Do the species have different morphologie
fit<- aov(weight ~ hindfoot_length, data=surveys_sml) # fit model
fit # look at fit
summary(fit) #summarize and show results

## Comparison of average weight by year by species

# select year and weight data only from "surveys" data, and filter out the NA data from weight and save it into a new object "survey_com
survey_com<- surveys %>%  
  select(weight, year)%>%
  filter(!is.na(weight))

# Create a new object "weight_mean from survey_com.  Group data by year and find mean for the weight.  Remove all NA from data.
weight_mean<-survey_com%>%
  group_by(year)%>%
  summarize(mean_weight = mean(weight, na.rm = TRUE))

# create a line graph, modify the color, size of line,  modify x-axis and y-axis labels and add a title
ggplot(data = weight_mean, aes(x=year, y=mean_weight, color = mean_weight)) + geom_line(size =1.5) + xlab("Year of Collection") + ylab("Average Weight-g") + ggtitle("Figure 2")

#export the graph
ggsave("Figures/Figure 2.pdf")

## Distribution of weight in males

# select only data concerned with weight and males of organisms, remove all NA in the weight and species id data; store data in a new object survey_dis
survey_dis<-surveys%>%
  filter(sex == 'M')%>%
  select(sex, weight, species_id)%>%
   filter(!is.na(weight))%>%
  filter(!is.na(species_id))

# create histogram of weight of males; choosing color and fill; labeling x-aXIS and y-axis and titling the figure

ggplot(survey_dis, aes(x=weight)) +
  geom_histogram(binwidth= 1.0, colour="black", fill="green") + xlab("weight-g") + ylab("Number of Males") + ggtitle("Figure 3")

# export to pdf
ggsave("Figures/Figure 3.pdf")

# filter weight to between 0-100 to better visualize the majority of the data. Filter out the NA from weight and species ID and keep only the Male data. Store data in a new object called survey_dis_weight_limited
survey_dis_weight_limited<-surveys%>%
  filter(sex == 'M')%>%
  select(sex, weight, species_id)%>%
  filter(!is.na(weight))%>%
  filter(!is.na(species_id))%>%
  filter(weight< 100)

# create histogram save as figure 4
ggplot(survey_dis_weight_limited, aes(x=weight)) +
  geom_histogram(binwidth= 1.0, colour="black", fill="green") + xlab("weight-g") + ylab("Number of Males") + ggtitle("Figure 4")

# export as pdf
ggsave("Figures/Figure 4.pdf")

# create new object called survey_species containing data from "surveys" with data for weight,selection for male and species ID, filter out NA from weight and species ID; 
survey_species<-surveys%>%
  filter(sex == 'M')%>%
  select(sex, weight, species_id)%>%
  filter(!is.na(weight))%>%
  filter(!is.na(species_id))

# create histogram of weight of male per species using a facet wrap
ggplot(survey_species, aes(x=weight)) +
  geom_histogram(binwidth= 1.0, colour="green") + xlab("weight=g") + ylab("Number of Males") + ggtitle("Figure 5") + facet_wrap(~ species_id)

#export as pdf
ggsave("Figures/Figure 5.pdf")

# citations for ggplot2
citation(package = "ggplot2", lib.loc = NULL) 

#citations for dplyr
citation(package = "dplyr", lib.loc = NULL)

#citation for stats
citation(package = "stats",  lib.loc = NULL)


