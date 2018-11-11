#
# Join both dataset and correct misspelling in player name
#


# join datasets
library('dplyr')
# library(stringr)

# test approach on subset 
# sub_w1 <- sub_Wes1 %>% dplyr::sample_n(100)
# sub_e1 <- Elo1 %>% dplyr::sample_n(100)

sub_w1 <- sub_Wes1 
sub_e1 <- Elo1 

# easy merge of both databases using game_file as foreign key
Join1 <- left_join(sub_w1, sub_e1, by = "game_file")

# winner matches
Join_win <- as.data.frame(cbind(as.character(Join1$player), as.character(Join1$winner)))
# loser matches
Join_los <- as.data.frame(cbind(as.character(Join1$player), as.character(Join1$loser)))


# use different methods to find string distances in true database name and misspelled name
library('stringr')
library('stringdist')


# http://dni-institute.in/blogs/r-fuzzy-string-match/

method_list <- c("osa", "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw", "soundex")
for( i in method_list){
 
 Join_win[,i] <- stringdist(Join_win$V1,Join_win$V2,method=i) 
 Join_los[,i] <- stringdist(Join_los$V1,Join_los$V2,method=i)
 
}

View(Join_win)
View(Join_los)

# identical row lengths
dim(Join_win) ; dim(Join_los) ; dim(Wes1) 


# numeric screening method
num_win <- Join_win[,c(3:12)]
num_los <- Join_los[,c(3:12)]

# all values other than zero are potential misspellings
sum_win <- apply(num_win, 1, sum)
sum_los <- apply(num_los, 1, sum)

# find all non-identical names in V1 and V2, both the two opponents and misspelled names
# this excludes all names that were found by many different string distance methods
diff_win <- Join_win[sum_win != 0, ]
diff_los <- Join_los[sum_los != 0, ]

# identify only misspelled names using the soundex method, it finds most misspellings
# sim_win <- diff_win[ diff_win$soundex == 0,] 
# sim_los <- diff_los[ diff_los$soundex == 0,] 

# identify only misspelled names using the soundex method in combination with a cos threshold 
# manually checked where the first mismatch occured (cos > 0.45 ): Jann is not Johny, solange is not Solymos
sim_win <- diff_win[ diff_win$soundex == 0 & diff_win$cos < 0.45,] 
sim_los <- diff_los[ diff_los$soundex == 0 & diff_los$cos < 0.45,] 


# View both 
View(sim_win)
View(sim_los)

# identify misspelled winner rows in original dataframe
row1_win <- rownames(sim_win)
row1_los <- rownames(sim_los)

# one example
Join_win [ rownames(sim_win[ sim_win$V2 == 'abhijit',]) , ]
Join_los [ rownames(sim_los[ sim_los$V2 == 'abhijit',]) , ]

# one example
Join_win [ rownames(sim_win[ sim_win$V2 == 'NanRoig',]) , ]
Join_los [ rownames(sim_los[ sim_los$V2 == 'NanRoig',]) , ]

# one example
Join_win [ rownames(sim_win[ sim_win$V2 == 'GaretJax',]) , ]
Join_los [ rownames(sim_los[ sim_los$V2 == 'GaretJax',]) , ]

# one example
Join_win [ rownames(sim_win[ sim_win$V2 == 'gamelle',]) , ]
Join_los [ rownames(sim_los[ sim_los$V2 == 'gamelle',]) , ]

# one example
Join_win [ rownames(sim_win[ sim_win$V2 == 'Solymos',]) , ]
Join_los [ rownames(sim_los[ sim_los$V2 == 'Solymos',]) , ]


# identify misspelled name with database name
# change factor to char
Join1$player <- as.character(Join1$player)

# find all misspelled rows and replace by db name
# in the winner column
Join1[row1_win,]$player <- as.character(sim_win[row1_win,]$V2 )
# in the loser column
Join1[row1_los,]$player <- as.character(sim_los[row1_los,]$V2 )

# replace all misspellings
View(Join1)

# turn players back to factors
Join1$player <- factor(Join1$player)

# misspelled names
pre <- length(levels(Wes1$player))
post <- length(levels(Join1$player))

# misspelled name fraction
(pre - post )/ pre *100

# add win loss column
Join1$win <- Join1$player == Join1$winner
# use 1/0 binary classifier
Join1$win <- Join1$win * 1
Join1$win <- factor(Join1$win)

# write results
# setwd("C:/Users/VK/Desktop/VKOR/wes")
# write.table(Join1, file = 'Wesv14.csv', append = TRUE, sep = ',', col.names = TRUE, row.names = FALSE, na = '') # 


