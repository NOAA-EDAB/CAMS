#Pull AA data
#Using get_comland_raw_data.R from comlandr
library(here); library(data.table); library(dbutils); library(comlandr)

#Connect to sole
channel <- dbutils::connect_to_database(server = "sole", uid = "slucey")

aa <- comlandr::get_comland_raw_data(channel, filterByYear = 2019)

save(aa, file = here('data-raw', 'AA_2019.RData'))