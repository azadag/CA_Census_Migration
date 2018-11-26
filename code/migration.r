library(tidycensus)
library(tidyverse)

v15 <- load_variables(2016, "acs", cache = TRUE)


# C07001
# 
# get_acs(geography = "county",
#         state = "CA",
#         table = "B07413"
# )
# 
# us_county_income <- get_acs(geography = "county", variables = "B19013_001", 
#                             shift_geo = TRUE, geometry = TRUE)

bay <- c("Alameda CA", "Marin CA", "San Francisco CA",  "San Mateo CA", "Santa Clara CA", "Contra Costa CA")
sl <- c("Los Angeles CA", "Orange CA")
ie <- c("Riverside CA", "San Bernardino CA")


pow_puma <- read.csv("C:\\Users\\azada\\OneDrive\\Work\\@Data\\CensusAPI\\CA_Census_Migration\\data\\geocorr14.csv")
ca_puma_map <- readxl::read_xlsx("Z:\\Shared With Me\\MemInfo (2)\\R&E (Dec 2017)\\Data\\Census\\ACS\\Raw\\PUMA Maps.xlsx")
# acs_data <- read_csv("C:\\Users\\azada\\OneDrive\\Work\\@Data\\CensusAPI\\CA_Census_Migration\\data\\usa_00049.csv")

mig_puma_xwalk <- read_csv("C:\\Users\\azada\\OneDrive\\Work\\@Data\\CensusAPI\\CA_Census_Migration\\data\\puma_migpuma1_pwpuma00.csv")
names(mig_puma_xwalk) <- c("State", "PUMA", "MIGPLAC1", "MIGPUMA")
# mig_puma_xwalk$MIGPLC2 <- stringr::str_sub( mig_puma_xwalk$MIGPLAC1, start =2, end = 3) 
mig_puma_xwalk <- mig_puma_xwalk %>% select(State, PUMA, MIGPLAC1, MIGPUMA, MIGPLC2)
mig_puma_xwalk <- mig_puma_xwalk %>% mutate(PUMA_fullx = paste0(str_pad(State, 2, side = "left", pad = 0), " ",str_pad(PUMA,5, side = "left", pad = 0)),
                                            MIG_fullx = paste0(str_pad(MIGPLAC1, 2, side = "left", pad = 0), " ", str_pad(MIGPUMA,5, side = "left", pad = 0))) 
mig_puma_xwalkD <- mig_puma_xwalk %>% select(PUMA_fullx, MIG_fullx) %>% distinct(MIG_fullx, .keep_all = TRUE)
pow_puma <- pow_puma %>% mutate( code = paste0(str_pad(state, 2, side = "left", pad = 0), " ",str_pad(puma12,5, side = "left", pad = 0)))
pow_puma1 <- pow_puma %>% distinct(code, .keep_all = TRUE) %>% mutate(MIGCOUNTY1 = paste(cntyname ))

mig_puma_xwalkDa <- mig_puma_xwalkD %>% inner_join((pow_puma1 %>% select(code, stab, cntyname, PUMAname)), by = c("PUMA_fullx"= "code"))
# acs_data2 <- acs_data %>% filter(is.na(MULTYEAR)) %>% mutate(, MIG_full = paste0("06 ",str_pad(MIGPUMA1,5, side = "left", pad = 0))) %>% 
#   group_by(PUMA_full, MIG_full) %>% summarise( HH = sum(HHWT), PER = sum(PERWT))

# acs_data2 <- acs_data %>% filter(is.na(MULTYEAR)) %>% mutate(PUMA_full = paste0("06 ",str_pad(PUMA,5, side = "left", pad = 0)), MIG_full = paste0(str_pad(MIGPLAC1, 2, side = "left", pad = 0), " ", str_pad(MIGPUMA1,5, side = "left", pad = 0))) %>% 
#   group_by(PUMA_full, MIG_full) %>% summarise( HH = sum(HHWT), PER = sum(PERWT))
# 
# acs_data3 <- acs_data2 %>% filter(MIG_full != "06 00001" , MIG_full != "06 00000") %>%  left_join(puma_map, by =c( "PUMA_full" = "Code")) 
# 
# acs_data4 <- acs_data3 %>%  left_join(pow_puma1, by= c( "MIG_full" = "code")) %>% rename(County_From = MIGCOUNTY1)  %>% 
#   group_by(County_From, pow_puma1) %>% summarise( HH1 = sum(HH), PER1 = sum(PER)) %>%
#   filter(HH1 != 0, MIGCOUNTY != "")

# names(acs_data4) <- c("CountyTo", "CountyFrom", "HH_In", "Per_In")

acs_out <- read_csv("C:\\Users\\azada\\OneDrive\\Work\\@Data\\CensusAPI\\CA_Census_Migration\\data\\usa_00050.csv")
acs_out <- acs_out %>% filter(MIGRATE1D >= 24)
acs_out1 <- acs_out %>% filter(is.na(MULTYEAR)) %>% mutate(PUMA_full = paste0(str_pad(STATEFIP,2,side = "left", pad =0)," ",str_pad(PUMA,5, side = "left", pad = 0)), MIG_full = paste0(str_pad(MIGPLAC1, 2, side = "left", pad = 0), " ", str_pad(MIGPUMA1,5, side = "left", pad = 0)))
acs_out1a <- acs_out1 %>% inner_join((mig_puma_xwalk %>% select(MIG_fullx, PUMA_fullx )), by = c("PUMA_full" = "PUMA_fullx")) 
acs_out1b <- acs_out1a %>% group_by(MIG_fullx, MIG_full) %>% summarise( HH = sum(HHWT), PER = sum(PERWT))

# acs_in1 <- acs_out %>% filter(is.na(MULTYEAR)) %>% mutate(PUMA_full = paste0(str_pad(STATEFIP,2,side = "left", pad =0)," ",str_pad(PUMA,5, side = "left", pad = 0)), MIG_full = paste0(str_pad(MIGPLAC1, 2, side = "left", pad = 0), " ", str_pad(MIGPUMA1,5, side = "left", pad = 0))) %>% 
#   inner_join((mig_puma_xwalk %>% select(MIG_fullx, PUMA_fullx)), by = c("PUMA_full" = "PUMA_fullx")) %>% 
#   filter(MIG_fullx != PUMA_full) %>% group_by(PUMA_full, MIG_fullx) %>% summarise( HH = sum(HHWT), PER = sum(PERWT))


acs_out3 <- acs_out1b %>% left_join( mig_puma_xwalkDa, by =c( "MIG_fullx" = "MIG_fullx")) %>% rename(County_T1 = cntyname) %>% 
  left_join( mig_puma_xwalkDa, by =c( "MIG_full" = "MIG_fullx")) %>% rename(County_T0 = cntyname) 

# acs_in3 <- acs_in1  %>%  left_join( pow_puma1, by =c( "MIG_full" = "code")) %>% rename(County_From = MIGCOUNTY)

acs_out4 <- acs_out3 %>% 
  group_by(County_T0, County_T1) %>% summarise( HH1 = sum(HH), PER1 = sum(PER)) %>% 
  filter(HH1 != 0, County_T0 != "", County_T0 != County_T1) 

names(acs_out4 ) <- c("CountyFrom", "CountyTo", "HH_Out", "Per_Out")

# data_out <- inner_join(acs_data4, acs_out4, by = c("CountyTo" = "CountyFrom", "CountyFrom" = "CountyTo"))
# data_out <- data_out %>% mutate( HH_net = HH_In - HH_Out, Per_net = Per_In - Per_Out )
acs_out4$CountyFrom <- as.character(acs_out4$CountyFrom)
acs_out4$CountyTo <- as.character(acs_out4$CountyTo)
data_out_group <- acs_out4 %>% mutate(CountyToGroups = case_when(CountyTo %in% bay ~ "Bay Area",
                                                                 CountyTo %in% sl ~ "LA/Orange",
                                                                 CountyTo %in% ie ~ "Inland Empire",
                                                                 TRUE ~ CountyTo))  %>% 
        mutate(CountyFromGroups = case_when(CountyFrom %in% bay ~ "Bay Area",
                                    CountyFrom %in% sl ~ "LA/Orange",
                                    CountyFrom %in% ie ~ "Inland Empire",
                                    TRUE ~ CountyFrom)) 


data_out_group1 <- data_out_group %>% group_by(CountyToGroups, CountyFromGroups) %>% select(-CountyTo, -CountyFrom) %>% summarise_all(sum) %>% filter(CountyFromGroups != CountyToGroups)


group_out <- c( "Bay Area", "LA/Orange","Inland Empire")
data_out_group_out <- data_out_group1 %>% filter( CountyFromGroups %in% group_out)  
data_out_group_out %>% arrange(CountyFromGroups, desc(Per_Out)) %>% filter(Per_Out > 1000) %>% View()

data_out_group_out %>% select(-Per_Out) %>% filter(HH_Out > 1000) %>% spread(CountyFromGroups, HH_Out) %>% arrange(desc(`Bay Area`)) %>% write.csv( "migration_out_HH.csv" )
data_out_group_out %>% select(-HH_Out) %>% filter(Per_Out > 1000) %>% spread(CountyFromGroups, Per_Out) %>% arrange(desc(`Bay Area`)) %>% write.csv( "migration_out_Per.csv" )
data_out_group_out %>% filter(HH_Out > 1000) %>% write.csv("data_hhper_long.csv")

## 5 year estimate
acs_out <- acs_out %>% filter(MIGRATE1D >= 24)
acs_out1 <- acs_out %>% filter(!is.na(MULTYEAR)) %>% mutate(PUMA_full = paste0(str_pad(STATEFIP,2,side = "left", pad =0)," ",str_pad(PUMA,5, side = "left", pad = 0)), MIG_full = paste0(str_pad(MIGPLAC1, 2, side = "left", pad = 0), " ", str_pad(MIGPUMA1,5, side = "left", pad = 0)))
acs_out1a <- acs_out1 %>% inner_join((mig_puma_xwalk %>% select(MIG_fullx, PUMA_fullx )), by = c("PUMA_full" = "PUMA_fullx")) 
acs_out1b <- acs_out1a %>% group_by(MIG_fullx, MIG_full) %>% summarise( HH = sum(HHWT), PER = sum(PERWT))

# acs_in1 <- acs_out %>% filter(is.na(MULTYEAR)) %>% mutate(PUMA_full = paste0(str_pad(STATEFIP,2,side = "left", pad =0)," ",str_pad(PUMA,5, side = "left", pad = 0)), MIG_full = paste0(str_pad(MIGPLAC1, 2, side = "left", pad = 0), " ", str_pad(MIGPUMA1,5, side = "left", pad = 0))) %>% 
#   inner_join((mig_puma_xwalk %>% select(MIG_fullx, PUMA_fullx)), by = c("PUMA_full" = "PUMA_fullx")) %>% 
#   filter(MIG_fullx != PUMA_full) %>% group_by(PUMA_full, MIG_fullx) %>% summarise( HH = sum(HHWT), PER = sum(PERWT))


acs_out3 <- acs_out1b %>% left_join( mig_puma_xwalkDa, by =c( "MIG_fullx" = "MIG_fullx")) %>% rename(County_T1 = cntyname) %>% 
  left_join( mig_puma_xwalkDa, by =c( "MIG_full" = "MIG_fullx")) %>% rename(County_T0 = cntyname) 

# acs_in3 <- acs_in1  %>%  left_join( pow_puma1, by =c( "MIG_full" = "code")) %>% rename(County_From = MIGCOUNTY)

acs_out4 <- acs_out3 %>% 
  group_by(County_T0, County_T1) %>% summarise( HH1 = sum(HH), PER1 = sum(PER)) %>% 
  filter(HH1 != 0, County_T0 != "", County_T0 != County_T1) 

names(acs_out4 ) <- c("CountyFrom", "CountyTo", "HH_Out", "Per_Out")

# data_out <- inner_join(acs_data4, acs_out4, by = c("CountyTo" = "CountyFrom", "CountyFrom" = "CountyTo"))
# data_out <- data_out %>% mutate( HH_net = HH_In - HH_Out, Per_net = Per_In - Per_Out )
acs_out4$CountyFrom <- as.character(acs_out4$CountyFrom)
acs_out4$CountyTo <- as.character(acs_out4$CountyTo)
data_out_group <- acs_out4 %>% mutate(CountyToGroups = case_when(CountyTo %in% bay ~ "Bay Area",
                                                                 CountyTo %in% sl ~ "LA/Orange",
                                                                 CountyTo %in% ie ~ "Inland Empire",
                                                                 TRUE ~ CountyTo))  %>% 
  mutate(CountyFromGroups = case_when(CountyFrom %in% bay ~ "Bay Area",
                                      CountyFrom %in% sl ~ "LA/Orange",
                                      CountyFrom %in% ie ~ "Inland Empire",
                                      TRUE ~ CountyFrom)) 


data_out_group1 <- data_out_group %>% group_by(CountyToGroups, CountyFromGroups) %>% select(-CountyTo, -CountyFrom) %>% summarise_all(sum) %>% filter(CountyFromGroups != CountyToGroups)


group_out <- c( "Bay Area", "LA/Orange","Inland Empire")
data_out_group_out <- data_out_group1 %>% filter( CountyFromGroups %in% group_out)  
# data_out_group_out %>% arrange(CountyFromGroups, desc(Per_Out)) %>% filter(Per_Out > 1000) 

data_out_group_out %>% select(-Per_Out) %>% filter(HH_Out > 1000) %>% spread(CountyFromGroups, HH_Out) %>% arrange(desc(`Bay Area`)) %>% write.csv( "migration_out_HH_5yr.csv" )
data_out_group_out %>% select(-HH_Out) %>% filter(Per_Out > 1000) %>% spread(CountyFromGroups, Per_Out) %>% arrange(desc(`Bay Area`)) %>% write.csv( "migration_out_Per_5yr.csv" )
data_out_group_out %>% filter(HH_Out > 1000) %>% write.csv("data_hhper_long_5yr.csv")
