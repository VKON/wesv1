#
# Sort, add time features and limit dataset
#



# restart R session
# setwd("C:/Users/VK/Desktop/VKOR/wes")
# Sort1 <- read.csv(file = 'Wesv14.csv') 

Sort1 <- Join1
View(Sort1)

# limit dataset
options(max.print=600)
# Sort1 <- A[,c(1:8,10,11,13,52,54,561,563,565,567)]
# Sort1 <- A[,c(1:8,52,54,561,563,565,567)] # 9 10 and 11 have missing values
# Sort1 <- A[,c(3:8,52,561,567)] # smaller dataset 
# View(Sort1)

# add year, month, weekday information
Sort1$year <- strptime(Sort1$date, format = '%Y.%m.%d %H:%M')$year + 1900
Sort1$mon <- strptime(Sort1$date, format = '%Y.%m.%d %H:%M')$mon 
Sort1$wday <- strptime(Sort1$date, format = '%Y.%m.%d %H:%M')$wday 

#rename wdays to week days
Sort1$year <- factor(Sort1$year)
Sort1$mon <- factor(Sort1$mon)
Sort1$wday <- factor(Sort1$wday)

# order of factors, start with sunday and Jan
levels(Sort1$wday) <- c('Sun','Mon','Tue','Wed','Thu','Fri','Sat')
levels(Sort1$mon) <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
View(Sort1)

# arrange columns by column name 
Sort1 <- select( Sort1, colnames(Sort1)[1:14], colnames(Sort1)[52:55], colnames(Sort1)[561:570], everything() ) 
View(Sort1)


# write full dataset without NAs 
# Wesv15 is the shared dataset in initial commit
# write.table(Sort1, file = 'Wesv15.csv', append = TRUE, sep = ',', col.names = TRUE, row.names = FALSE, na = '') # 

# Print all unique variable factors as text file
alluniques <- sapply(Sort1, levels)
sink("alluniques.txt")
print(alluniques)
sink()  

# restart R session 
# setwd("C:/Users/VK/Desktop/VKOR/wes")
# Sort1 <- read.csv(file = 'Wesv15.csv') # 

# arrange columns by column name 
library('dplyr')


# work with limited dataset first
# focus on non-sparse data first, subset dataset for dense matrix
Sort2 <- select( Sort1, colnames(Sort1)[1:28]) 
View(Sort2)
dim(Sort2)

# write dataset without NAs
# write.table(Sort2, file = 'Wesv16.csv', append = TRUE, sep = ',', col.names = TRUE, row.names = FALSE, na = '') # 

# restart session and read final file
# setwd("C:/Users/VK/Desktop/VKOR/wes")
# Sort1 <- read.csv(file = 'Wesv16.csv') # 

Sort1 <- Sort2
