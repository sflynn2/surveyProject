## Survey Project in R

#install and load library
install.packages("dplyr")
install.packages ("ggplot2")
library(dplyr)
library(ggplot2)

# download a file
download.file("http://files.figshare.com/2236372/combined.csv",  "data/portal_data_joined.csv")# data for project is in the portal_data_joined.csv

# import that file
surveys <- read.csv('data/portal_data_joined.csv')

## explore relationship between weight and hindfoot length for each species

#data parsing

<<<<<<< HEAD
# filter weight less than 100 and selects the species id, hindfoot length and weight, removes NA from weight and hindfoot length. Puts it all in the file survey sml
surveys_sml<- surveys %>%  
  filter(weight < 100) %>%

# filter weight less than 15 and selects the species id, hindfoot length and weight, removes NA from weight and hindfoot length. Puts it all in the file survey sml
surveys_sml<- surveys %>%  
  filter(weight < 15) %>%
  select(species_id, hindfoot_length, weight)%>%
  filter(!is.na(weight))%>%  
  filter(!is.na(hindfoot_length))

#Find the mean for weight and hindfoot length and group by species_id
weight_mean<-surveys_sml%>%
  group_by(species_id)%>%
  summarize(mean_weight = mean(weight, na.rm = TRUE))
  group_by(species_id)%>%
  summarize(mean_hindfoot_length = mean(hindfoot_length, na.rm = TRUE))
Combined_means<-cbind(weight_mean, Hindfoot_length_mean)
  
#build figure
# scatter plot
ggplot(data = Combined_means, aes(x=mean_weight, y=mean_hindfoot_length)) + geom_point()
#modify the color, size of points in the point layer,  modify x-axis and y-axis labels and add a title
ggplot(data = Combined_means, aes(x=mean_weight, y=mean_hindfoot_length, color = species_id)) + geom_point(size = 2.5) + xlab("Average Weight") + ylab("Average Hindfoot Length") + ggtitle("Figure 1")


#export graph
ggsave("Figure 1.pdf")

# ANOVA: is variation among groups different? Do the species have different morphologie
data(surveys_sml)
head(surveys_sml)
fit<- aov(weight ~ hindfoot_length, data=surveys_sml) # fit model
fit # look at fit
summary(fit) #summarize and show results

## Comparison of average weight by year (line graph)by species

# select year and weight data only and filter out the NA data
survey_com<- surveys %>%  
  select(weight, year)%>%
  filter(!is.na(weight))

# Create a new object "weight_mean from survey_dis.  Group data by year
weight_mean<-survey_com%>%
  group_by(year)%>%
  summarize(mean_weight = mean(weight, na.rm = TRUE))

# create a line graph
ggplot(data = weight_mean, aes(x=year, y=mean_weight)) + geom_line()

#modify the color, size of line,  modify x-axis and y-axis labels and add a title
ggplot(data = weight_mean, aes(x=year, y=mean_weight, color = mean_weight)) + geom_line(size =1.5) + xlab("Year of Collection") + ylab("Average Weight") + ggtitle("Figure 2")

#export the graph
ggsave("Figure 2.pdf")

## Distribution of weight in males

# select only data concerned with weight and males of organisms, remove all NA in the weight data
survey_com<-surveys%>%
  filter(sex == 'M')%>%
  select(sex, weight, species_id)%>%
   filter(!is.na(weight))%>%
  filter(!is.na(species_id))

# create histogram of weight of males

ggplot(survey_com, aes(x=weight)) +
  geom_histogram(binwidth= 1.0, colour="black", fill="green") + xlab("weight") + ylab("Number of Males") + ggtitle("Figure 3")

# export to pdf
ggsave("Figure 3.pdf")

# filter weight to between 0-100
survey_com<-surveys%>%
  filter(sex == 'M')%>%
  select(sex, weight, species_id)%>%
  filter(!is.na(weight))%>%
  filter(!is.na(species_id))%>%
  filter(weight< 100)

# create histogram save as figure 4
ggplot(survey_com, aes(x=weight)) +
  geom_histogram(binwidth= 1.0, colour="black", fill="green") + xlab("weight") + ylab("Number of Males") + ggtitle("Figure 4")

# export as pdf
ggsave("Figure 4.pdf")

# create new object with no filter for weight
survey_species<-surveys%>%
  filter(sex == 'M')%>%
  select(sex, weight, species_id)%>%
  filter(!is.na(weight))%>%
  filter(!is.na(species_id))

# create histogram of weight of male per species
ggplot(survey_species, aes(x=weight)) +
  geom_histogram(binwidth= 1.0, colour="green") + xlab("weight") + ylab("Number of Males") + ggtitle("Figure 5") + facet_wrap(~ species_id)

#export as pdf
ggsave("Figure 5.pdf")




