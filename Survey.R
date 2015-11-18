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

## Comparison of average weight by year (line graph)by species


## Distribution of weight in males (by species ?)
