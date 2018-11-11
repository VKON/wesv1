library(tidyverse)
library(rvest)
library(stringr)
library(rebus)

base_url <- 'https://units.wesnoth.org/1.14/mainline/en_US/mainline.html'


find_unit_pages <- function(url = base_url){
    main_unit_page <- read_html(url)
    base_unit_url <- 'https://units.wesnoth.org/1.14'
    
    unit_endpoints <- main_unit_page %>% 
        html_nodes('td.unitcell') %>% 
        html_children() %>% 
        html_nodes('a') %>% 
        html_attr('href') %>% 
        str_replace('../..', '') %>% 
        str_c(base_unit_url,.)
    return(unit_endpoints)
     
}


extract_description_text <- function(html_page){
    flavour_text <- html_page %>% 
        html_nodes('p') %>% 
        html_text()
    return(flavour_text)
}

extract_description_text(sample_html)

clean_name <- function(name_string){
    name_string %>% 
        tolower() %>% 
        str_replace_all(' ', '_') %>% 
        str_replace_all(':','')
}

extract_tables <- function(html_page){
    unit_tables <- html_page %>% 
        html_nodes('.unitinfo') %>% 
        html_table()
    return(unit_tables)
}


html_text_collapse <- function(x, trim = FALSE, collapse = "\n"){
    UseMethod("html_text_collapse")
}

html_text_collapse.xml_nodeset <- function(x, trim = FALSE, collapse = ";"){
    vapply(x, html_text_collapse.xml_node, character(1), trim = trim, collapse = collapse)
}

html_text_collapse.xml_node <- function(x, trim = FALSE, collapse = ";"){
    paste(xml2::xml_find_all(x, ".//text()"), collapse = collapse)
}






attack_pattern <- START %R% capture(one_or_more(WRD)) %R% capture(or('ranged', 'melee')) %R% END

attack_stats_pattern <- START %R% capture(one_or_more(DGT)) %R%
                        SPACE %R% any_char() %R% SPACE %R% 
                        capture(one_or_more(DGT)) %R% END


extract_attacks <- function(unit_html, unit){
    attack_info <- unit_html %>% 
        html_node('.attacks') %>% 
        html_text_collapse() %>% 
        str_split(';') %>% 
        unlist()
    
    melee_ranged_mask <-  if_else(attack_info == 'melee' | attack_info == 'ranged', 1, 0)
    starts <- melee_ranged_mask[2:length(melee_ranged_mask)]
    starts <- c(starts, 0)
    
    group_lengths <- starts %>% as.tibble() %>% 
        mutate(group = cumsum(value)) %>% 
        group_by(group) %>% 
        summarise(len_vec = n()) %>% 
        select(len_vec) %>% 
        pull()
    
    i <- 0
    j <- 1
    attack_list = list()
    for (len in group_lengths){
        row <-  attack_info[i+1:(len)]
        
        
        if (len == 4){
            row <- c(row, NA, NA)
        } 
        if (len == 5){
            row <- c(row,  NA)
        } 
        
        attack_tbl <- tibble(att_name = row[1],
                             att_category = row[2],
                             attack_stats = row[3],
                             att_type =row[4],
                             special1 = row[5],
                             special2 = row[6])
        attack_list[[j]] <- attack_tbl
        i <- i+len
        j <- j+1
    }
    
    
    attack_tbl <- attack_list %>% bind_rows() %>% 
        mutate(att_name = clean_name(att_name),
               att_damage = str_match(attack_stats, attack_stats_pattern)[,2],
               att_count = str_match(attack_stats, attack_stats_pattern)[,3],
               unit = unit) %>% 
        select(-attack_stats)
    
    return(attack_tbl)
}

extract_ability_table <- function(table_list){
    ability_table <- table_list[1][[1]]
    colnames(ability_table) <- c('property', 'value')
    
    return(ability_table %>% 
               mutate(property = clean_name(property),
                      value = clean_name(value)) %>% 
               spread(key = property, value = value) %>% 
               rename('unit' = 'id') %>% 
               as_tibble()
           
    )
}

extract_resistances <- function(unit_tables, unit){
    if (length(unit_tables) ==3){
        resistance_table <- unit_tables[2][[1]]
    }else{resistance_table <- unit_tables[3][[1]]}
    
    first_resistances <- resistance_table[,c(2,3)]
    colnames(first_resistances) <- c('category', 'percentage')
    second_resistances <- resistance_table[,c(6,7)]
    colnames(second_resistances) <- c('category', 'percentage')
    resistances <- bind_rows(first_resistances, second_resistances) %>% 
        mutate(category = factor(category),
               percentage = str_replace(percentage, '%', ''),
               percentage = as.numeric(percentage),
               unit = unit) %>% 
        as.tibble()
    return(resistances)
}

extract_movements <- function(unit_tables, unit){
    if (length(unit_tables) == 3){
        movement_table <- unit_tables[3][[1]]
    } else{movement_table <- unit_tables[4][[1]]}
    
    colnames(movement_table) <- c('item', 'terrain', 'movement_cost', 'defense')
    movement_table <- movement_table %>% 
        mutate(terrain = clean_name(terrain),
               terrain = factor(terrain),
               movement_cost = as.numeric(movement_cost),
               defense = str_replace(defense, '%', '') ,
               defense = as.numeric(defense),
               unit = unit) %>% 
        select(-1) %>% 
        as_tibble()
    return(movement_table)
}


extract_unit_info <- function(page_url){
    # try({
        html_page <- read_html(page_url)
        unit_tables <- extract_tables(html_page)
        description <- extract_description_text(html_page)
        
        
        ability_table <- extract_ability_table(unit_tables) %>% 
            mutate(description = description,
                   unit_page = page_url)
        unit_name <- ability_table %>% select(unit) %>% pull()
        
        # Not all units have attacks
        try({attack_table <-extract_attacks(html_page, unit_name)}, silent = T)
        
        if (!exists('attack_table')){
            attack_table <- NULL
        }
        
        resistance_table <- extract_resistances(unit_tables, unit_name)
        movement_table <- extract_movements(unit_tables, unit_name)
    
        unit_data <- list(info = ability_table,
                          attack = attack_table,
                          resistances = resistance_table,
                          movement = movement_table)
        return(unit_data)
        # }, silent = T)
}

all_unit_pages <- find_unit_pages()

all_unit_data <- map(all_unit_pages, extract_unit_info)

unit_basic_info <- all_unit_data %>% map(1) %>% bind_rows()
unit_attack_info <- all_unit_data %>% map(2) %>% bind_rows()
unit_resistance_info <- all_unit_data %>% map(3) %>% bind_rows()
unit_movement_info <- all_unit_data %>% map(4) %>% bind_rows()

unit_basic_info <- unit_basic_info %>% 
    mutate(advances_to = str_replace_all(advances_to,',_', ',')) %>% 
    mutate(abilities = str_replace_all(abilities,',_', ','),
           abilities = str_replace_all(abilities ,'_+', '+'),
           unit_id = row_number()) %>% 
    mutate(alignment = factor(alignment),
           unit = factor(unit)
           )

unit_lookup <- unit_basic_info %>% 
    select(unit_id, unit) %>% 
    unique()

unit_attack_info <- unit_attack_info %>% 
    left_join(unit_lookup, by = 'unit') %>% 
    select(-unit)

unit_resistance_info <- unit_resistance_info %>% 
    left_join(unit_lookup, by = 'unit') %>% 
    select(-unit)

unit_movement_info <- unit_movement_info %>% 
    left_join(unit_lookup, by = 'unit') %>% 
    select(-unit)

write_csv(unit_basic_info, path = 'unit_basic.csv')
write_csv(unit_attack_info, path = 'unit_attack.csv')
write_csv(unit_movement_info, path = 'unit_movement.csv')
write_csv(unit_resistance_info, path = 'unit_resistance.csv')
