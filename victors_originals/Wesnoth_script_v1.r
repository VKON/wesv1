#
# wesv1
#


#####

# tidy up 
rm(list = ls()) 
# CTRL W
# CTRL L

#####

# define current working directory
#############################################

# define on which system the scripts run, 
# by default from home

# Rfromhome <- 0
Rfromhome <- 1

if (Rfromhome == 0) { 
print ('working from elsewhere')
setwd("")
} else {
print ('working from home')
setwd("C:/Users/VK/Desktop/VKOR/wes")
}
getwd() 


# in/decrease print size
# options(max.print=1000000)
options(max.print=200)


# move into big data location (4GB), load files, append-read files into one data frame, save results as data frame
source("Wes_read_big_df_v1.r")


# move into metadata location (winloss)
source("Wes_read_winloss_df_v1.R")


# join big data with metadata and corrected misspelled names
source("Wes_join_misspellings_v1.R")


# explore dataset
source("Wes_explore_data_v1.R")




