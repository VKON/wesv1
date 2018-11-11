#
# Read json Elo1 metadataset
#


# SKIP - data not provided in github repo for now

# move to where data is stored - local ladder database
setwd("H:/ladder_replays/history")

# read metadata to find out who won and who lost (ELO)
library('jsonlite') 
Elo1 <- fromJSON('elo.json')

# append .json ending to match game_file name in other dataframe
Elo1$id <- paste0(Elo1$id,'.json')

# rename column id with game_file to match col names in big dataframe
colnames(Elo1)[1] <- 'game_file'


# write file 
setwd("C:/Users/VK/Desktop/VKOR/wes")
write.table(Elo1, file = 'Elo1.csv', append = TRUE, sep = ',', col.names = TRUE, row.names = FALSE) # 


###################################################

# continue here until all data is on the repository

# restart R session
# setwd("C:/Users/VK/Desktop/VKOR/wes")
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


