#
# Read json main dataset
#


# SKIP - data not provided in github repo for now

###################################################


# start with reading Wesv7.csv as Wes1 dataframe

# move to where data is stored - local ladder database
setwd("H:/ladder_replays/parsed")

list.files()

# load libraries for json and structured processing 
library('jsonlite') 
library('plyr')
library('purrr')


singlegame_json2df <- function(json) {
	# read json and expand by flatten
	jsonlite::fromJSON(json, flatten = TRUE) %>%
    # select criteria of interest
    magrittr::extract(c("era", "map", "sides", "turns", "title", "version")) %>%
    # expand again with flatten and remove nested data frames by creating a list with depth 1
    purrr::flatten() %>%
    # liste in tibble umwandeln, list elements turn into columns
    # values that have only one value such as map are duplicated
    tibble::as.tibble() %>%
    # in some game versions team names are south north while in others they are 1 and 2
    # turn all into characters
    mutate(team = as.character(team)) %>%
    # create a game_file ID of the json file name 
    mutate(game_file = json) 
}

# SKIP!
# execute function on all json files is expensive (4GB), do only once
# Wes1 <- list.files() %>% purrr::map_df(singlegame_json2df, .id = "game_id")
# write.table(Wes1, file = 'Wesv7.csv', append = TRUE, sep = ',', col.names = TRUE, row.names = FALSE) # 


# restart R session
setwd("C:/Users/VK/Desktop/VKOR/wes")
Wes1 <- read.csv(file = 'Wesv7.csv') 

# instructions: 
# read Wesv7.csv and call it Wes1
