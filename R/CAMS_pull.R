#Pull CAMS data
#Using get_comland_raw_data.R as template from comlandr

library(here); library(data.table); library(dbutils)

#Connect to nova
channel <- dbutils::connect_to_database(server = "nova", uid = "slucey")

cam.qry <- "select year, month, negear, toncl1, nespp3, nespp4, area,
            spplivlb, spplndlb, sppvalue, utilcd
            from CAMS_GARFO.CAMS_CFDETS2019AA@NOVA"
cam.qry <- "select a.year, a.month, a.negear, a.toncl1, a.nespp3, 
                           a.nespp4, a.area, a.spplivlb, a.spplndlb, a.sppvalue, 
                           a.utilcd, b.mesh
                           from CAMS_GARFO.CAMS_CFDETS2019AA@NOVA a,
                           CAMS_GARFO.CAMS_CFDETT2019AA@NOVA b
                           where a.link = b.link"

cams <- data.table::as.data.table(DBI::dbGetQuery(channel, cam.qry))

#Identify small/large mesh fisheries
cams[MESH <= 3, MESHCAT := 'SM']
cams[MESH >  3, MESHCAT := 'LG']
cams[, MESH := NULL]
    
# Use landed weight instead of live weight for shellfish
cams[NESPP3 %in% 743:800, SPPLIVLB := SPPLNDLB]
    
# Remove fish parts so live weight is not double counted
    
cams <- cams[!NESPP4 %in% c('0119', '0123', '0125', '0127', '0812', '0819', 
                            '0828', '0829', '1731', '2351', '2690', '2699',
                            '3472', paste0(348:359, 8), '3868', paste0(469:471, 4),
                            paste0(480:499, 8), '5018', '5039', '5261', '5265'), ]
    
#Sum landings and value
data.table::setkey(cams,
                   YEAR,
                   MONTH,
                   NEGEAR,
                   MESHCAT,
                   TONCL1,
                   NESPP3,
                   AREA,
                   UTILCD)
#landings
cams[, V1 := sum(SPPLIVLB, na.rm = T), by = key(cams)]
#value
cams[, V2 := sum(SPPVALUE, na.rm = T), by = key(cams)]

#Create market category
cams[, MKTCAT := substr(NESPP4, 4, 4)]

#Remove extra rows/columns
cams <- unique(cams, by = key(cams))
cams[, c('SPPLIVLB', 'SPPLNDLB', 'SPPVALUE', 'NESPP4') := NULL]

#Rename summed columns
data.table::setnames(cams, c('V1', 'V2'), c('SPPLIVLB', 'SPPVALUE'))



#Convert number fields from chr to num
numberCols <- c('YEAR', 'MONTH', 'NEGEAR', 'TONCL1', 'NESPP3', 'UTILCD', 'AREA',
                'MKTCAT')
cams[, (numberCols):= lapply(.SD, as.numeric), .SDcols = numberCols][]

#Adjust pounds to metric tons
cams[, SPPLIVMT := SPPLIVLB * 0.00045359237]
cams[, SPPLIVLB := NULL]

save(cams, file = here('data-raw', 'CAMS_2019.RData'))
