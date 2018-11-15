#
# Read json Elo1 metadataset
#


# SKIP - data not provided in github repo for now

# move to where data is stored - local ladder database

# Setting your working directory in a script that you are intend to load is a bit dangerous, 
# as this is an unintentional side-effect 
# setwd("H:/ladder_replays/history")

# read metadata to find out who won and who lost (ELO)

# These files are missing
library('jsonlite') 
Elo1 <- fromJSON('elo.json')

# append .json ending to match game_file name in other dataframe
# Interesting, I prefer the str_ commands from the stringr package.
Elo1$id <- paste0(Elo1$id,'.json')

# rename column id with game_file to match col names in big dataframe
colnames(Elo1)[1] <- 'game_file'


# write file 
# Instead of changing the working directory, it might be more convenient to change the file path
# This has the benefit as the file path can be passed as an Argument inside a function, making your code 
# More flexible without having to rewrite it.
setwd("C:/Users/VK/Desktop/VKOR/wes")
write.table(Elo1, file = 'Elo1.csv', append = TRUE, sep = ',', col.names = TRUE, row.names = FALSE) # 


###################################################

# continue here until all data is on the repository

# restart R session
# setwd("C:/Users/VK/Desktop/VKOR/wes")

# Have you tried read_csv from the tidyverse? Their read functions have better defauls and are better to read in date formats and the such. You typically dont have to add stringsAsFactors=FALSE either
Wes1 <- read.csv(file = 'Wesv7.csv') 
Elo1 <- read.csv(file = 'Elo1.csv', stringsAsFactors=FALSE) 

# operate on full dataset
sub_Wes1 <- Wes1

# test approach on subset 
# sub_Wes1 <- Wes1[,c(1:9,55)]

# turn game_file into a character vector in both datasets
sub_Wes1$game_file <- as.character(sub_Wes1$game_file)


View(sub_Wes1)
View(Elo1)


