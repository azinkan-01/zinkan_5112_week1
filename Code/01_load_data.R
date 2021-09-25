# Import packages
library(tidyverse)
library(haven)
library(magrittr)
library(dplyr)
library(ggplot2)

# Load Raw data as Data Frame
nfhs <- read_dta("Raw_Data/IAHR52FL.dta")

# Print a sample of what the data looks like
head(nfhs)

# Number of unique households in the data set hv002 is the house id
print(count(unique(nfhs['hv002'])))

# How many variables are there -- counting the length of the column
print(length(colnames(nfhs)))


# Sub setting columns hhid : shstruc 
nfhs_cut_col = subset(nfhs, select = c(hhid : shstruc))
# Print number of columns remaining
print(length(colnames(nfhs_cut_col)))


#Plot histogram as save the image to figures folder

png("Figures/rplot.pdf") 

ggplot(data= nfhs_cut_col,
       mapping = aes(x= hv009), binwidth = 1) +
       geom_histogram() +
       xlab("Number of house hold members")
# Saving the plot to the relative path
dev.off()



#Create a new data frame that contains only urban households (the variable is "hv025").
# rural == 2
new_df <- nfhs %>%
  filter(hv025 == 2)

# Summarize the records count by the rural group (Should only see one group)
new_df %>%
  group_by(hv025) %>%
  summarise(
    count = n()
  )
  

# box plot of distribution of household size by type of urban area
png("Figures/rplot_hist.pdf") 

urban_df <- filter(nfhs, hv025 == 1)
p <- ggplot(urban, mapping = aes(x= hv026 , y= hv009, group =1)) + 
  geom_boxplot()
p

# Saving plot to relative path
dev.off()


# Use "group_by" and "summaries" to find the means and medians of the number of 
#household members by type of urban area. Match the correct means to their urban areas.

# Get Mean, Median, and row count for urban df, grouped by the place of residence
urban_df %>%
  group_by(hv026) %>%
  summarise(
    mean(hv009), 
    median(hv009), 
    count = n()
  )

