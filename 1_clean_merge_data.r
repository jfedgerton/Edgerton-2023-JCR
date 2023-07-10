## ---------------------------------- ##
##    Cleaning Edgerton (2023 JCR)    ## 
## ---------------------------------- ##

## Read in data 
prio_yearly           <- readRDS(paste0("replication_data/", "prio_yearly.rds"))
aggregated_fixed_data <- readRDS(paste0("replication_data/", "isis_count_data.rds"))
prio_static           <- readRDS(paste0("replication_data/", "prio_static.rds"))
EPR_2018_1            <- readRDS(paste0("replication_data/", "EPR_2018_1.rds"))
country_data          <- readRDS(paste0("replication_data/", "country_data.rds"))
gtd                   <- readRDS(paste0("replication_data/", "gtd.rds"))

## Impute missing data with regressions
nighttime_lights_model <- lme4::lmer(nlights_calib_mean ~ year + (1|gid), data = prio_yearly)

## Create data for imputation
data_predict_nlights   <-
  data.frame(
    expand.grid(unique(aggregated_fixed_data$gid), 2013:2014)
  )

## Rows to fix
missing_2012 <- filter(
  prio_yearly,
  is.na(nlights_calib_mean),
  year == 2012
) %>% 
  dplyr::select(
    gid, year
  ) 

## Data to fix
colnames(data_predict_nlights) <- c("gid", "year")
data_predict_nlights$gid <- as.character(data_predict_nlights$gid)
data_predict_nlights <- bind_rows(
  data_predict_nlights, 
  missing_2012
)
data_predict_nlights$nlights_calib_mean <- predict(nighttime_lights_model, data_predict_nlights, allow.new.levels = T)

## Imputed values
n_lights_merge <- dplyr::select(
  prio_yearly, 
  year, gid, nlights_calib_mean
) %>% 
  filter(
    !is.na(nlights_calib_mean)
  ) %>% 
  bind_rows(
    data_predict_nlights
  ) %>% 
  filter(
    year >= 2012
  )

## Merge back in the values
aggregated_fixed_data <- left_join(
  aggregated_fixed_data,
  n_lights_merge, 
  by = c("gid", "year")
)

## Create data for imputation
population_model <- lme4::lmer(pop_gpw_sum ~ year + (1|gid), data = prio_yearly)

## Rows to fix
data_predict_population   <-
  data.frame(
    expand.grid(unique(aggregated_fixed_data$gid), 2012:2014)
  )

## Data to fix
colnames(data_predict_population) <- c("gid", "year")
data_predict_population$gid <- as.character(data_predict_population$gid)

## Imputed values
data_predict_population$pop_gpw_sum <- predict(population_model, data_predict_population, allow.new.levels = T)

## Merge back in the values
aggregated_fixed_data <- left_join(
  aggregated_fixed_data, 
  data_predict_population,
  by = c("gid", "year")
)

## Impute missing data with regressions
gcp_model <- lme4::lmer(gcp_ppp ~ year + (1|gid), data = prio_yearly)

## Create data for imputation
data_predict_gcp   <-
  data.frame(
    expand.grid(unique(aggregated_fixed_data$gid), 2012:2014)
  )

## Rows to fix
colnames(data_predict_gcp) <- c("gid", "year")
data_predict_gcp$gid <- as.character(data_predict_gcp$gid)
data_predict_gcp$gcp_ppp <- predict(gcp_model, data_predict_gcp, allow.new.levels = T)

## Mege back in values
aggregated_fixed_data <- left_join(
  aggregated_fixed_data, 
  data_predict_gcp,
  by = c("gid", "year")
)

## Other control variables
capital_dist <- dplyr::select(
  prio_yearly,
  year, gid, capdist
)

border_dist <- dplyr::select(
  prio_yearly,
  year, gid, bdist1
)


aggregated_fixed_data <- left_join(
  aggregated_fixed_data, 
  capital_dist,
  by = c("gid", "year")
)

aggregated_fixed_data <- left_join(
  aggregated_fixed_data, 
  border_dist,
  by = c("gid", "year")
)

drought_data <- dplyr::select(
  prio_yearly,
  year, gid, droughtyr_speigdm
)

aggregated_fixed_data <- left_join(
  aggregated_fixed_data, 
  drought_data,
  by = c("gid", "year")
)

## Create exclusion variables
prio_yearly_excluded <- filter(
  prio_yearly,
  !is.na(excluded), 
  excluded > 0
) %>% 
  dplyr::select(
    gid, excluded, year
  )

prio_yearly_excluded_10_years_2014 <- filter(
  prio_yearly_excluded,
  year >= 2004
)

prio_yearly_excluded_10_years_2013 <- filter(
  prio_yearly_excluded,
  year >= 2003,
  year < 2014
)

prio_yearly_excluded_10_years_2012 <- filter(
  prio_yearly_excluded,
  year >= 2002,
  year < 2013
)

prio_yearly_excluded_20_years_2014 <- filter(
  prio_yearly_excluded,
  year < 2004,
  year >= 1994
)

prio_yearly_excluded_20_years_2013 <- filter(
  prio_yearly_excluded,
  year < 2003,
  year >= 1993
)

prio_yearly_excluded_20_years_2012 <- filter(
  prio_yearly_excluded,
  year < 2002,
  year >= 1992
)

prio_yearly_excluded_30_years_2014 <- filter(
  prio_yearly_excluded,
  year < 1994,
  year >= 1984
)

prio_yearly_excluded_30_years_2013 <- filter(
  prio_yearly_excluded,
  year < 1993,
  year >= 1983
)

prio_yearly_excluded_30_years_2012 <- filter(
  prio_yearly_excluded,
  year < 1992,
  year >= 1982
)

## Merge in exclusion variables
aggregated_fixed_data$excluded_10_years <- 0
aggregated_fixed_data$excluded_20_years <- 0
aggregated_fixed_data$excluded_30_years <- 0
aggregated_fixed_data$excluded_10_years <- ifelse(aggregated_fixed_data$year == 2014 & 
                                                    aggregated_fixed_data$gid %in% prio_yearly_excluded_10_years_2014$gid, 1, aggregated_fixed_data$excluded_10_years)

aggregated_fixed_data$excluded_10_years <- ifelse(aggregated_fixed_data$year == 2013 & 
                                                    aggregated_fixed_data$gid %in% prio_yearly_excluded_10_years_2013$gid, 1, aggregated_fixed_data$excluded_10_years)

aggregated_fixed_data$excluded_10_years <- ifelse(aggregated_fixed_data$year == 2012 & 
                                                    aggregated_fixed_data$gid %in% prio_yearly_excluded_10_years_2012$gid, 1, aggregated_fixed_data$excluded_10_years)

aggregated_fixed_data$excluded_20_years <- ifelse(aggregated_fixed_data$year == 2014 & 
                                                    aggregated_fixed_data$gid %in% prio_yearly_excluded_20_years_2014$gid, 1, aggregated_fixed_data$excluded_20_years)

aggregated_fixed_data$excluded_20_years <- ifelse(aggregated_fixed_data$year == 2013 & 
                                                    aggregated_fixed_data$gid %in% prio_yearly_excluded_20_years_2013$gid, 1, aggregated_fixed_data$excluded_20_years)

aggregated_fixed_data$excluded_20_years <- ifelse(aggregated_fixed_data$year == 2012 & 
                                                    aggregated_fixed_data$gid %in% prio_yearly_excluded_20_years_2012$gid, 1, aggregated_fixed_data$excluded_20_years)

aggregated_fixed_data$excluded_30_years <- ifelse(aggregated_fixed_data$year == 2014 & 
                                                    aggregated_fixed_data$gid %in% prio_yearly_excluded_30_years_2014$gid, 1, aggregated_fixed_data$excluded_30_years)

aggregated_fixed_data$excluded_30_years <- ifelse(aggregated_fixed_data$year == 2013 & 
                                                    aggregated_fixed_data$gid %in% prio_yearly_excluded_30_years_2013$gid, 1, aggregated_fixed_data$excluded_30_years)

aggregated_fixed_data$excluded_30_years <- ifelse(aggregated_fixed_data$year == 2012 & 
                                                    aggregated_fixed_data$gid %in% prio_yearly_excluded_30_years_2012$gid, 1, aggregated_fixed_data$excluded_30_years)



excluded_2013 <- subset(prio_yearly_excluded, 
                        year == 2013)
aggregated_fixed_data$excluded_present <- ifelse(aggregated_fixed_data$gid %in% excluded_2013$gid, 1, 0)

## Bring together state codes for merging
data(gwstates)
head(gwstates)

# Merge in state identifier
aggregated_fixed_data <- gwstates %>%
  filter(end > as.Date("2010-01-01")) %>%
  distinct(
    gwcode, gwc, country_name) %>%
  merge(aggregated_fixed_data,
        by.y = "gwno",
        by.x = "gwcode") 

## Identify areas where Sunni Muslims are discriminated against
EPR_2018_1 <- filter(EPR_2018_1, 
                     gwid %in% unique(aggregated_fixed_data$gwcode),
                     status %in% c("POWERLESS", "DISCRIMINATED"))

sunni_groups <- filter(
  EPR_2018_1, 
  group %in% c("Sunni Arabs", "Sunni Shafi'i (Arab)", "Chechens", "Uyghur", "Albanians",
               "Sunni Shafii/Sofi (Hijazi) (Arab)", "Palestinian Arabs", "Palestinians (Arab)", 
               "Turkmen") | 
    grepl("sunni", group, ignore.case = T)
)

## Merge in the Sunni discrimination variable
aggregated_fixed_data$sunni_10_years <- 0
aggregated_fixed_data$sunni_20_years <- 0
aggregated_fixed_data$sunni_30_years <- 0

aggregated_fixed_data$sunni_10_years <- ifelse(aggregated_fixed_data$year == 2014 & 
                                                 aggregated_fixed_data$gwcode %in% subset(sunni_groups, 
                                                                                          from < 2005 &  
                                                                                            to >= 2014  )$gwid,
                                               1, aggregated_fixed_data$sunni_10_years
)

aggregated_fixed_data$sunni_10_years <- ifelse(aggregated_fixed_data$year == 2013 & 
                                                 aggregated_fixed_data$gwcode %in% subset(sunni_groups, 
                                                                                          from < 2004 &  
                                                                                            to >= 2013  )$gwid,
                                               1, aggregated_fixed_data$sunni_10_years
)

aggregated_fixed_data$sunni_10_years <- ifelse(aggregated_fixed_data$year == 2012 & 
                                                 aggregated_fixed_data$gwcode %in% subset(sunni_groups, 
                                                                                          from < 2003 &  
                                                                                            to >= 2012  )$gwid,
                                               1, aggregated_fixed_data$sunni_10_years
)

aggregated_fixed_data$sunni_20_years <- ifelse(aggregated_fixed_data$year == 2014 & 
                                                 aggregated_fixed_data$gwcode %in% subset(sunni_groups, 
                                                                                          from < 1995 &  
                                                                                            to >= 2004  )$gwid,
                                               1, aggregated_fixed_data$sunni_20_years
)

aggregated_fixed_data$sunni_20_years <- ifelse(aggregated_fixed_data$year == 2013 & 
                                                 aggregated_fixed_data$gwcode %in% subset(sunni_groups, 
                                                                                          from < 1994 &  
                                                                                            to >= 2003  )$gwid,
                                               1, aggregated_fixed_data$sunni_20_years
)

aggregated_fixed_data$sunni_20_years <- ifelse(aggregated_fixed_data$year == 2012 & 
                                                 aggregated_fixed_data$gwcode %in% subset(sunni_groups, 
                                                                                          from < 1993 &  
                                                                                            to >= 2002  )$gwid,
                                               1, aggregated_fixed_data$sunni_20_years
)

aggregated_fixed_data$sunni_30_years <- ifelse(aggregated_fixed_data$year == 2014 & 
                                                 aggregated_fixed_data$gwcode %in% subset(sunni_groups, 
                                                                                          from < 1985 &  
                                                                                            to >= 1994  )$gwid,
                                               1, aggregated_fixed_data$sunni_30_years
)

aggregated_fixed_data$sunni_30_years <- ifelse(aggregated_fixed_data$year == 2013 & 
                                                 aggregated_fixed_data$gwcode %in% subset(sunni_groups, 
                                                                                          from < 1984 &  
                                                                                            to >= 1993  )$gwid,
                                               1, aggregated_fixed_data$sunni_30_years
)

aggregated_fixed_data$sunni_30_years <- ifelse(aggregated_fixed_data$year == 2012 & 
                                                 aggregated_fixed_data$gwcode %in% subset(sunni_groups, 
                                                                                          from < 1983 &  
                                                                                            to >= 1992  )$gwid,
                                               1, aggregated_fixed_data$sunni_30_years
)

## Create a variable for excluded Sunni Muslims groups at the subnational level

aggregated_fixed_data$sunni_present <- ifelse(
  aggregated_fixed_data$gwcode %in% subset(sunni_groups, 
                                           to >= 2013  )$gwid, 1, 0
)

aggregated_fixed_data$excluded_sunni_10_yrs <- ifelse(
  aggregated_fixed_data$excluded_10_years == 1 & 
    aggregated_fixed_data$sunni_10_years == 1, 
  1, 0
)

aggregated_fixed_data$excluded_sunni_20_yrs <- ifelse(
  aggregated_fixed_data$excluded_20_years == 1 & 
    aggregated_fixed_data$sunni_20_years == 1, 
  1, 0
)

aggregated_fixed_data$excluded_sunni_30_yrs <- ifelse(
  aggregated_fixed_data$excluded_30_years == 1 & 
    aggregated_fixed_data$sunni_30_years == 1, 
  1, 0
)

aggregated_fixed_data$excluded_sunni_present <- ifelse(
  aggregated_fixed_data$excluded_present == 1 & 
    aggregated_fixed_data$sunni_present == 1, 
  1, 0
)

## Clean the country level data
country_data$cname[country_data$cname == "Ethiopia (1993-)"] <- "Etheopia"
country_data$cname[country_data$cname == "France (1963-)"] <- "France"
country_data$cname[country_data$cname == "Iran"] <- "Iran (Persia)"
country_data$cname[country_data$cname == "Italy"] <- "Italy/Sardinia"
country_data$cname[country_data$cname == "Kyrgyzstan"] <- "Kyrgyz Republic"
country_data$cname[country_data$cname == "Macedonia"] <- "Macedonia (Former Yugoslav Republic of)"
country_data$cname[country_data$cname == "Russia"] <- "Russia (Soviet Union)"
country_data$cname[country_data$cname == "Tanzania"] <- "Tanzania/Tanganyika"
country_data$cname[country_data$cname == "Turkey"] <- "Turkey (Ottoman Empire)"
country_data$cname[country_data$cname == "United States"] <- "United States of America"
country_data$cname[country_data$cname == "Malaysia (1966-)"] <- "Malaysia"
country_data$cname[country_data$cname == "Bosnia and Herzegovina"] <- "Bosnia-Herzegovina"
country_data$cname[country_data$cname == "Germany"] <- "German Federal Republic"
country_data$cname[country_data$cname == "Pakistan (1971-)"] <- "Pakistan"
country_data$cname[country_data$cname == "Sudan (2012-)"] <- "Sudan"
country_data$cname[country_data$cname == "Sudan (-2011)"] <- "Sudan"
country_data$cname[country_data$cname == "Yemen"] <- "Yemen (Arab Republic of Yemen)"

kosovo <- subset(country_data, cname == "Albania")
kosovo$cname <- "Kosovo"
country_data <- rbind(kosovo, country_data)

country_data_for_analysis <- dplyr::select(
  country_data,
  c(
    year, cname,
    al_ethnic, al_language, al_religion, 
    p_polity2, wbgi_pve, wdi_unempmne, ht_colonial
  )
) %>% 
  filter(
    year %in% unique(aggregated_fixed_data$year)
  )

## Merge into a single data frame
data_for_analysis <- left_join(
  aggregated_fixed_data, 
  country_data_for_analysis,
  by = c("country_name" = "cname", "year"),
  multiple = "all"
)

## Simplfy prio static data
prio_static$gid <- as.character(prio_static$gid)
prio_static <- dplyr::select(
  prio_static, 
  c(
    gid, petroleum_s, ttime_mean, mountains_mean, landarea
  )
)

data_for_analysis <- left_join(
  data_for_analysis, 
  prio_static, 
  by = "gid"
)     

data_for_analysis <- filter(
  data_for_analysis, 
  year > 2012
)

## Add in local terrorist dynamics
change_coordinats <- function(input){
  input$xcoord = round((input$longitude - 0.25)*2, 0)/2+0.25
  input$ycoord = round((input$latitude - 0.25)*2, 0)/2+0.25
  return(input)
}
gtd <- change_coordinats(gtd)

gid_coords <- dplyr::select(aggregated_fixed_data, gid, xcoord, ycoord)

gtd <- merge(gtd, gid_coords, by = c("xcoord", "ycoord"))
gtd_isis <- filter(
  gtd, 
  grepl("Islamic State", gname, ignore.case = T) +  
    grepl("Islamic State", gname2, ignore.case = T) + 
    grepl("Islamic State", gname3, ignore.case = T) != 0, 
  iyear %in% 2013:2014
) %>% 
  dplyr::rename(
    year = iyear
  ) %>% 
  mutate(
    count = 1
  ) %>% 
  aggregate(
    count ~ gid + year, data = ., FUN = sum
  )


## Merge in local data
data_for_analysis <- left_join(
  data_for_analysis,
  gtd_isis, 
  by = c("gid", "year")
)

## Export data
data_for_analysis <- dplyr::rename(
  data_for_analysis, 
  count = count.x,
  isis_attacks = count.y
)

## Export data for analysis
save(data_for_analysis, file = "data_for_analysis.Rda")
