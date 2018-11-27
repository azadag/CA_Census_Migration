library(tidyverse)

# areas to combine
bay <- c("Alameda CA", "Marin CA", "San Francisco CA",  "San Mateo CA", 
         "Santa Clara CA", "Contra Costa CA")
sl <- c("Los Angeles CA", "Orange CA")
ie <- c("Riverside CA", "San Bernardino CA")


pow_puma <- read.csv(".\\data\\geocorr14.csv")
ca_puma_map <- readxl::read_xlsx(".\\data\\PUMA Maps.xlsx")
mig_puma_xwalk <- read_csv(".\\data\\puma_migpuma1_pwpuma00.csv")

names(mig_puma_xwalk) <- c("State", "PUMA", "MIGPLAC1", "MIGPUMA")

# All of the hard work happens in these cross-walks!

mig_puma_xwalk <- mig_puma_xwalk %>% 
  select(State, PUMA, MIGPLAC1, MIGPUMA) #, MIGPLC2)

mig_puma_xwalk <- mig_puma_xwalk %>% 
  mutate(PUMA_fullx = paste0(str_pad(State, 2, side = "left", pad = 0), " ",
                             str_pad(PUMA,5, side = "left", pad = 0)),
         MIG_fullx = paste0(str_pad(MIGPLAC1, 2, side = "left", pad = 0), " ", 
                            str_pad(MIGPUMA,5, side = "left", pad = 0)))

mig_puma_xwalkD <- mig_puma_xwalk %>% 
  select(PUMA_fullx, MIG_fullx) %>% distinct(MIG_fullx, .keep_all = TRUE)

pow_puma <- pow_puma %>% 
  mutate( code = paste0(str_pad(state, 2, side = "left", pad = 0), " ",
                        str_pad(puma12,5, side = "left", pad = 0)))

pow_puma1 <- pow_puma %>% 
  distinct(code, .keep_all = TRUE) %>% mutate(MIGCOUNTY1 = paste(cntyname ))

mig_puma_xwalkDa <- mig_puma_xwalkD %>% 
  inner_join((pow_puma1 %>% select(code, stab, cntyname, PUMAname)), 
             by = c("PUMA_fullx"= "code"))


## data from IPUMS
acs_out <- read_csv(".\\data\\usa_00050.csv")  #IPUMS data file, not included
acs_out <- acs_out %>% filter(MIGRATE1D >= 24)
acs_out1 <- acs_out %>% filter(is.na(MULTYEAR)) %>% 
  mutate(PUMA_full = paste0(str_pad(STATEFIP,2,side = "left", pad =0)," ",
                            str_pad(PUMA,5, side = "left", pad = 0)), 
         MIG_full = paste0(str_pad(MIGPLAC1, 2, side = "left", pad = 0), " ", 
                           str_pad(MIGPUMA1,5, side = "left", pad = 0)))

acs_out1a <- acs_out1 %>% inner_join((mig_puma_xwalk %>% select(MIG_fullx, PUMA_fullx )), 
                                     by = c("PUMA_full" = "PUMA_fullx")) 
acs_out1b <- acs_out1a %>% group_by(MIG_fullx, MIG_full) %>% 
                           summarise( HH = sum(HHWT), PER = sum(PERWT))

acs_out3 <- acs_out1b %>% 
  left_join( mig_puma_xwalkDa, by =c( "MIG_fullx" = "MIG_fullx")) %>% 
  rename(County_T1 = cntyname) %>% 
  left_join( mig_puma_xwalkDa, by =c( "MIG_full" = "MIG_fullx")) %>% 
  rename(County_T0 = cntyname) 

acs_out4 <- acs_out3 %>% 
  group_by(County_T0, County_T1) %>% summarise( HH1 = sum(HH), PER1 = sum(PER)) %>% 
  filter(HH1 != 0, County_T0 != "", County_T0 != County_T1) 

names(acs_out4 ) <- c("CountyFrom", "CountyTo", "HH_Out", "Per_Out")

acs_out4$CountyFrom <- as.character(acs_out4$CountyFrom)
acs_out4$CountyTo <- as.character(acs_out4$CountyTo)

# summarise to California Areas
data_out_group <- acs_out4  %>% ungroup() %>% 
  mutate(CountyToGroups = case_when(CountyTo %in% bay ~ "Bay Area",
                                    CountyTo %in% sl ~ "LA/Orange",
                                    CountyTo %in% ie ~ "Inland Empire",
                                    TRUE ~ CountyTo))  %>% 
  mutate(CountyFromGroups = case_when(CountyFrom %in% bay ~ "Bay Area",
                                      CountyFrom %in% sl ~ "LA/Orange",
                                      CountyFrom %in% ie ~ "Inland Empire",
                                      TRUE ~ CountyFrom)) 

data_out_group1 <- data_out_group %>% group_by(CountyToGroups, CountyFromGroups) %>% 
  select(-CountyTo, -CountyFrom) %>% summarise_all(sum) %>% 
  filter(CountyFromGroups != CountyToGroups)

# Flip the process for people coming in
acs_out1bIN <- acs_out1a %>% group_by(MIG_full, MIG_fullx) %>% 
  summarise( HH = sum(HHWT), PER = sum(PERWT))

acs_out3IN <- acs_out1bIN %>%  
  left_join( mig_puma_xwalkDa, by =c( "MIG_fullx" = "MIG_fullx")) %>% 
  rename(County_T1 = cntyname) %>% 
  left_join( mig_puma_xwalkDa, by =c( "MIG_full" = "MIG_fullx")) %>% 
  rename(County_T0 = cntyname) 

acs_out4IN <- acs_out3IN %>% 
  group_by(County_T0, County_T1) %>% summarise( HH1 = sum(HH), PER1 = sum(PER)) %>% 
  filter(HH1 != 0, County_T0 != "",  County_T0 != County_T1) 

names( acs_out4IN ) <- c("CountyFrom", "CountyTo", "HH_IN", "Per_IN")


# Join Out-Movers and In-Movers To Calculate Net Change

data_join <- inner_join(acs_out4, acs_out4IN, by = 
                          c("CountyTo" = "CountyFrom", "CountyFrom" = "CountyTo"))
data_join <- data_join %>% mutate( HH_net = HH_IN - HH_Out, Per_net = Per_IN - Per_Out )


data_join$CountyFrom <- as.character(data_join$CountyFrom)
data_join$CountyTo <- as.character(data_join$CountyTo)

# summarise to California Arease
data_net_group <- data_join  %>% ungroup() %>% 
  mutate(CountyToGroups = case_when(CountyTo %in% bay ~ "Bay Area",
        CountyTo %in% sl ~ "LA/Orange",
        CountyTo %in% ie ~ "Inland Empire",
        TRUE ~ CountyTo))  %>% 
  mutate(CountyFromGroups = case_when(CountyFrom %in% bay ~ "Bay Area",
                                      CountyFrom %in% sl ~ "LA/Orange",
                                      CountyFrom %in% ie ~ "Inland Empire",
                                      TRUE ~ CountyFrom)) 

data_net_group1 <- data_net_group %>% ungroup() %>% 
  group_by(CountyFromGroups, CountyToGroups ) %>% 
  select(-CountyTo, -CountyFrom) %>% summarise_all(sum)


group_out <- c( "Bay Area", "LA/Orange","Inland Empire")
data_out_group_out <- data_net_group1 %>% filter( CountyFromGroups %in% group_out)  
data_out_group_out %>% arrange(CountyFromGroups, desc(Per_Out)) %>% filter(abs(Per_Out) > 400) %>% View()

# write-out csvs
data_out_group_out %>% select(-Per_Out, -HH_Out, -HH_IN, -Per_IN, -Per_net) %>% 
  spread(CountyFromGroups, HH_net) %>% arrange(desc(`Bay Area`)) %>% 
  write.csv( ".\\data_out\\migration_net_HH.csv" )
data_out_group_out %>% select(-Per_Out, -HH_Out, -HH_IN, -Per_IN, -HH_net)  %>% 
  spread(CountyFromGroups, Per_net) %>% arrange(desc(`Bay Area`)) %>% 
  write.csv( ".\\data_out\\migration_net_Per.csv" )

data_out_group_out %>% write.csv(".\\data_out\\data_hhper_long.csv")


# Redo for just one-year 1 year ACS estimate ####
acs_out <- read_csv(".\\data\\usa_00050.csv")
acs_out <- acs_out %>% filter(MIGRATE1D >= 24)
acs_out1 <- acs_out %>% filter(is.na(MULTYEAR)) %>% 
  mutate(PUMA_full = paste0(str_pad(STATEFIP,2,side = "left", pad =0)," ",
                            str_pad(PUMA,5, side = "left", pad = 0)), 
         MIG_full = paste0(str_pad(MIGPLAC1, 2, side = "left", pad = 0), " ", 
                           str_pad(MIGPUMA1,5, side = "left", pad = 0)))

acs_out1a <- acs_out1 %>% 
  inner_join((mig_puma_xwalk %>% select(MIG_fullx, PUMA_fullx )), by = c("PUMA_full" = "PUMA_fullx")) 
acs_out1b <- acs_out1a %>% group_by(MIG_fullx, MIG_full) %>% 
  summarise( HH = sum(HHWT), PER = sum(PERWT))

acs_out3 <- acs_out1b %>% 
  left_join( mig_puma_xwalkDa, by =c( "MIG_fullx" = "MIG_fullx")) %>% 
  rename(County_T1 = cntyname) %>% 
  left_join( mig_puma_xwalkDa, by =c( "MIG_full" = "MIG_fullx")) %>% 
  rename(County_T0 = cntyname) 

acs_out4 <- acs_out3 %>% 
  group_by(County_T0, County_T1) %>% summarise( HH1 = sum(HH), PER1 = sum(PER)) %>% 
  filter(HH1 != 0, County_T0 != "", County_T0 != County_T1) 

names(acs_out4 ) <- c("CountyFrom", "CountyTo", "HH_Out", "Per_Out")

acs_out4$CountyFrom <- as.character(acs_out4$CountyFrom)
acs_out4$CountyTo <- as.character(acs_out4$CountyTo)
data_out_group <- acs_out4 %>% 
  mutate(CountyToGroups = case_when(CountyTo %in% bay ~ "Bay Area",
         CountyTo %in% sl ~ "LA/Orange",
         CountyTo %in% ie ~ "Inland Empire",
         TRUE ~ CountyTo))  %>% 
  mutate(CountyFromGroups = case_when(CountyFrom %in% bay ~ "Bay Area",
                                      CountyFrom %in% sl ~ "LA/Orange",
                                      CountyFrom %in% ie ~ "Inland Empire",
                                      TRUE ~ CountyFrom)) 


data_out_group1 <- data_out_group %>% group_by(CountyToGroups, CountyFromGroups) %>% 
  select(-CountyTo, -CountyFrom) %>% summarise_all(sum) %>% 
  filter(CountyFromGroups != CountyToGroups)


group_out <- c( "Bay Area", "LA/Orange","Inland Empire")
data_out_group_out <- data_out_group1 %>% 
  filter( CountyFromGroups %in% group_out)  

data_out_group_out %>% arrange(CountyFromGroups, desc(Per_Out)) %>% 
  filter(Per_Out > 1000) %>% View()

data_out_group_out %>% select(-Per_Out) %>% filter(abs(HH_Out) > 1000) %>% 
  spread(CountyFromGroups, HH_Out) %>% arrange(desc(`Bay Area`)) %>% 
  write.csv( ".\\data_out\\migration_out_HH.csv" ) #%>% View()

data_out_group_out %>% select(-HH_Out) %>% filter(Per_Out > 1000) %>% 
  spread(CountyFromGroups, Per_Out) %>% arrange(desc(`Bay Area`)) %>% 
  write.csv( ".\\data_out\\migration_out_Per.csv" )

data_out_group_out %>% filter(HH_Out > 1000) %>% 
  write.csv(".\\data_out\\data_hhper_long.csv")


## 5 year estimate
acs_out <- acs_out %>% filter(MIGRATE1D >= 24)
acs_out1 <- acs_out %>% filter(!is.na(MULTYEAR)) %>% 
  mutate(PUMA_full = paste0(str_pad(STATEFIP,2,side = "left", pad =0)," ",
                            str_pad(PUMA,5, side = "left", pad = 0)), 
         MIG_full = paste0(str_pad(MIGPLAC1, 2, side = "left", pad = 0), " ", 
                           str_pad(MIGPUMA1,5, side = "left", pad = 0)))

acs_out1a <- acs_out1 %>% 
  inner_join((mig_puma_xwalk %>% select(MIG_fullx, PUMA_fullx )), 
             by = c("PUMA_full" = "PUMA_fullx")) 

acs_out1b <- acs_out1a %>% 
  group_by(MIG_fullx, MIG_full) %>% summarise( HH = sum(HHWT), PER = sum(PERWT))

acs_out3 <- acs_out1b %>% left_join( mig_puma_xwalkDa, by =c( "MIG_fullx" = "MIG_fullx")) %>% 
  rename(County_T1 = cntyname) %>% 
  left_join( mig_puma_xwalkDa, by =c( "MIG_full" = "MIG_fullx")) %>% 
  rename(County_T0 = cntyname) 

acs_out4 <- acs_out3 %>% 
  group_by(County_T0, County_T1) %>% summarise( HH1 = sum(HH), PER1 = sum(PER)) %>% 
  filter(HH1 != 0, County_T0 != "", County_T0 != County_T1) 

names(acs_out4 ) <- c("CountyFrom", "CountyTo", "HH_Out", "Per_Out")

acs_out4$CountyFrom <- as.character(acs_out4$CountyFrom)
acs_out4$CountyTo <- as.character(acs_out4$CountyTo)
data_out_group <- acs_out4 %>% 
  mutate(CountyToGroups = case_when(CountyTo %in% bay ~ "Bay Area",
                                    CountyTo %in% sl ~ "LA/Orange",
                                    CountyTo %in% ie ~ "Inland Empire",
                                    TRUE ~ CountyTo))  %>% 
  mutate(CountyFromGroups = case_when(CountyFrom %in% bay ~ "Bay Area",
                                      CountyFrom %in% sl ~ "LA/Orange",
                                      CountyFrom %in% ie ~ "Inland Empire",
                                      TRUE ~ CountyFrom)) 


data_out_group1 <- data_out_group %>% group_by(CountyToGroups, CountyFromGroups) %>% 
  select(-CountyTo, -CountyFrom) %>% summarise_all(sum) %>% 
  filter(CountyFromGroups != CountyToGroups)


group_out <- c( "Bay Area", "LA/Orange","Inland Empire")
data_out_group_out <- data_out_group1 %>% filter( CountyFromGroups %in% group_out)  
# data_out_group_out %>% arrange(CountyFromGroups, desc(Per_Out)) %>% filter(Per_Out > 1000) 

data_out_group_out %>% select(-Per_Out) %>% filter(HH_Out > 1000) %>% 
  spread(CountyFromGroups, HH_Out) %>% arrange(desc(`Bay Area`)) %>% 
  write.csv( ".\\data_out\\migration_out_HH_5yr.csv" )

data_out_group_out %>% select(-HH_Out) %>% filter(Per_Out > 1000) %>% 
  spread(CountyFromGroups, Per_Out) %>% arrange(desc(`Bay Area`)) %>% 
  write.csv( ".\\data_out\\migration_out_Per_5yr.csv" )

data_out_group_out %>% filter(HH_Out > 1000) %>%
  write.csv(".\\data_out\\data_hhper_long_5yr.csv")


#### Circalize Visualization

library(circlize)
mig_hh <- read_csv("./data_out/data_hhper_long.csv") %>% select(-X1)
data_viz_1000plus_mig <- mig_hh %>% filter(HH_Out > 5000) %>% arrange(HH_Out)
names(data_viz_1000plus_mig) <- c("to", "from", "value", "Per_Out")

chordDiagram(data_viz_1000plus_mig,  big.gap = 50)
circos.clear()
chordDiagram(data_viz_1000plus_mig, directional = 1, big.gap = 50)
