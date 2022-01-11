# Original Author: Kaitlyn Johnson

# Date originated: 12-13-2021
rm(list = ls())
USE_CASE = Sys.getenv("USE_CASE")
if(USE_CASE == ""){
    USE_CASE<-'local'
}
#USE_CASE<-'domino' # 'domino' or 'local'


# test


# ----- Load in libraries and datasets ---------------------------------

#library(lintr)
if (USE_CASE == 'domino'){
install.packages("lubridate", dependencies = TRUE, repos = 'http://cran.us.r-project.org')
install.packages("readxl", dependencies = TRUE, repos = 'http://cran.us.r-project.org')
install.packages("styler", dependencies = TRUE, repos = 'http://cran.us.r-project.org')
install.packages("tidyverse", dependencies = TRUE, repos = 'http://cran.us.r-project.org')
install.packages("scales", dependencies=TRUE, repos='http://cran.us.r-project.org')
}
library(styler)
library(tidyverse)
library(readxl)
library(lubridate)
library(scales)



# file paths
NYT_CASE_BY_COUNTY_DAY <- url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-recent.csv")
VAX_RATES_BY_COUNTY<- url("https://raw.githubusercontent.com/bansallab/vaccinetracking/main/vacc_data/data_county_current.csv")
STATES_BY_REGION<- url("https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv")
# local
if (USE_CASE == 'local'){
SHAPEFILE_PATH<-'../data/county_shape_file.txt'
COUNTY_POPS<-'../data/county_pops.csv'
}
# Domino
if (USE_CASE=='domino'){
SHAPEFILE_PATH<-'/mnt/data/county_shape_file.txt'
COUNTY_POPS<-'/mnt/data/county_pops.csv'
}

# read files
cases_by_county_t <- read_csv(NYT_CASE_BY_COUNTY_DAY)
county_pop<-read.csv(COUNTY_POPS)
county_pop$pop <- as.numeric(gsub(",","",county_pop$pop))
vax_rate_by_county_t<-read_csv(VAX_RATES_BY_COUNTY)
state_regions<-read_csv(STATES_BY_REGION)



# ----- Set parameters----------------------------------------------------
DUR_INF<- 7 
yesterday<- lubridate::today('EST')- days(1) # look at reported cases since yesterday
reporting_rate<- 1/4 # https://www.cdc.gov/coronavirus/2019-ncov/cases-updates/burden.html
test_sens <-0.786 # https://www.cdc.gov/mmwr/volumes/70/wr/mm7003e3.htm, https://www.medrxiv.org/content/10.1101/2020.10.30.20223198v1.full.pdf
VE_inf<- 0.30 # https://www.medrxiv.org/content/10.1101/2021.12.10.21267594v2
event_size<-c(10,20,30)
event_size<-sort(event_size)
prop_vax_event<-1


# ---- Risk functions ---------------------------------------------------
calc_risk_inf_at_event <- function(p_I, n) {
    r<-1-(1-p_I)**n
    round(100 * r, 1)
}

# ------ Summarise daily data to county-level ---------------------------

county_pop$pop <- as.numeric(gsub(",","",county_pop$pop)) # not curerntly used, using Bansal lab population

# case data starting from the last day that the county reported data
county_cases<-cases_by_county_t%>%
    mutate(county_fips = as.character(fips))%>%
    group_by(county_fips, county, state)%>%
    mutate(last_updated = which.max(cases), # For each county, find the most recent date case data was updated
           date_updated = date[last_updated])%>%ungroup()%>%
    filter(date>=(date_updated -DUR_INF) & date<=date_updated)%>%
    group_by(county_fips, county, state)%>%
    summarise(sum_cases_7_days = max(cases) - min(cases),
              last_updated = max(last_updated),
              date_updated = max(date_updated))


county_cases['county_state_formatted']<-paste0(county_cases$county, ", ",county_cases$state)
county_cases$county_fips[county_cases$county == "New York City"]<-36061
#county_cases<-left_join(county_cases, county_pop, by = "county_state_formatted")



# If date update is more than 7 days ago
county_cases$sum_cases_7_days[county_cases$date_updated<=yesterday-7]<-NA
counties_w_no_data<-county_cases[county_cases$sum_cases_7_days ==0 | is.na(county_cases$sum_cases_7_days),]


# vax data
county_vax<-vax_rate_by_county_t%>%filter(CASE_TYPE == "Complete Coverage")%>%
    group_by(COUNTY)%>%filter(DATE == max(DATE, na.rm = TRUE) | is.na(DATE))%>%
    rename(pop = POPN)%>%
    mutate(vax_rate = CASES/100)%>%select(COUNTY, vax_rate, GEOFLAG, pop)




# Calculate the effect of vaccination and  day of testing on 
county_risk<-county_cases%>%
    left_join(county_vax, by = c("county_fips" = "COUNTY"))%>%
    left_join(state_regions, by = c("state" = "State"))%>%
    mutate(cases_last_7_days_per_100k=round((sum_cases_7_days/pop)*100000,0),
           prev_per_100k = (1/reporting_rate)*sum_cases_7_days/pop*100000,
           prev = (1/reporting_rate)*sum_cases_7_days/pop,
           m_test = (1-test_sens), 
           m_v = (1-VE_inf*prop_vax_event)/(1-VE_inf*vax_rate),
           risk_inf_no_mitig_small = calc_risk_inf_at_event(prev, event_size[1]),
           risk_inf_testing_no_vax_small = calc_risk_inf_at_event(m_test*prev, event_size[1]),
           risk_inf_fully_vax_no_test_small = calc_risk_inf_at_event(m_v*prev, event_size[1]),
           risk_inf_fully_vax_and_test_small = calc_risk_inf_at_event(m_test*m_v*prev, event_size[1]),
           risk_inf_no_mitig_med = calc_risk_inf_at_event(prev, event_size[2]),
           risk_inf_testing_no_vax_med = calc_risk_inf_at_event(m_test*prev, event_size[2]),
           risk_inf_fully_vax_no_test_med = calc_risk_inf_at_event(m_v*prev, event_size[2]),
           risk_inf_fully_vax_and_test_med = calc_risk_inf_at_event(m_test*m_v*prev, event_size[2]),
           risk_inf_no_mitig_big = calc_risk_inf_at_event(prev, event_size[3]),
           risk_inf_testing_no_vax_big = calc_risk_inf_at_event((m_test*prev), event_size[3]),
           risk_inf_fully_vax_no_test_big = calc_risk_inf_at_event(m_v*prev, event_size[3]),
           risk_inf_fully_vax_and_test_big = calc_risk_inf_at_event(m_test*m_v*prev, event_size[3])
           )%>%
    drop_na(county_fips)

# Write unit test for missing data 
stopifnot('More than 200 counties are missing data'= sum(is.na(county_risk$risk_inf_no_mitig_small))<200)

# Load in shapefile and join to it
shapefile <- read_delim(SHAPEFILE_PATH, delim = "\t") %>%select(geometry, FIPS)
county_risk<-left_join(shapefile, county_risk, by = c("FIPS" = "county_fips"))%>%drop_na(county_state_formatted)

# Clean the dataframe for Flourish
county_risk_long<-county_risk%>%pivot_longer(
    cols = starts_with("risk_"),
    names_to = "mitigation",
    names_prefix = "risk_", 
    values_to = "chance_someone_inf",
    values_drop_na = FALSE
)
# Add % to all risks
county_risk_long$pct_chance_someone_inf<-paste0(county_risk_long$chance_someone_inf, ' %')

# Add note to for counties not reporting data 
county_risk_long$pct_chance_someone_inf[is.na(county_risk_long$sum_cases_7_days)]<- 'Insufficient case data'
county_risk_long$cases_last_7_days_per_100k[is.na(county_risk_long$sum_cases_7_days)]<-'No cases in last 7 days'
#county_risk_long$vax_rate[is.na(county_risk_long$vax_rate)]<-'Missing county-level vax data'


#------ Make 3 separate data frames for each event size (so column names can be same)---------
# SMALL
county_risk_small<-county_risk_long[grep("small", county_risk_long$mitigation),]
county_risk_small$event_size<-event_size[1]
county_risk_small_clean<-county_risk_small%>%
    pivot_wider(names_from = "mitigation", values_from = c("pct_chance_someone_inf", "chance_someone_inf"))%>%
    mutate(event_size = event_size[1])%>%
    rename(
        `Same vax % as county` = pct_chance_someone_inf_inf_no_mitig_small,
        `Rapid test right before` = pct_chance_someone_inf_inf_testing_no_vax_small,
        `100% vaccinated` = pct_chance_someone_inf_inf_fully_vax_no_test_small,
        `100% vax & rapid test` = pct_chance_someone_inf_inf_fully_vax_and_test_small,
        `7-day cases per 100k` = cases_last_7_days_per_100k,
        chance_someone_inf_no_mitig = chance_someone_inf_inf_no_mitig_small,
        chance_someone_inf_testing_no_vax = chance_someone_inf_inf_testing_no_vax_small,
        chance_someone_inf_fully_vax_no_test = chance_someone_inf_inf_fully_vax_no_test_small,
        chance_someone_inf_fully_vax_and_test = chance_someone_inf_inf_fully_vax_and_test_small,
        `Data last reported` = date_updated
        
    )%>%mutate(
        `Data updated` =substr(lubridate::now('EST'), 1, 10)
    )



# Check that missing data is being noted in correct column
counties_w_no_data<-county_risk_small_clean[county_risk_small_clean$sum_cases_7_days ==0 | is.na(county_risk_small_clean$sum_cases_7_days),]


#MEDIUM
county_risk_med<-county_risk_long[grep("med", county_risk_long$mitigation),]
county_risk_med$event_size <- event_size[2]
county_risk_med_clean<-county_risk_med%>%
    pivot_wider(names_from = "mitigation", values_from = c("pct_chance_someone_inf", "chance_someone_inf"))%>%
    rename(
        `Same vax % as county` =pct_chance_someone_inf_inf_no_mitig_med,
        `Rapid test right before` = pct_chance_someone_inf_inf_testing_no_vax_med,
        `100% vaccinated` = pct_chance_someone_inf_inf_fully_vax_no_test_med,
        `100% vax & rapid test` = pct_chance_someone_inf_inf_fully_vax_and_test_med,
        `7-day cases per 100k` = cases_last_7_days_per_100k,
        chance_someone_inf_no_mitig = chance_someone_inf_inf_no_mitig_med,
        chance_someone_inf_testing_no_vax = chance_someone_inf_inf_testing_no_vax_med,
        chance_someone_inf_fully_vax_no_test = chance_someone_inf_inf_fully_vax_no_test_med,
        chance_someone_inf_fully_vax_and_test = chance_someone_inf_inf_fully_vax_and_test_med,
        `Data last reported` = date_updated
    )%>%mutate(
        `Data updated` =substr(lubridate::now('EST'), 1, 10)
    )

#BIG
county_risk_big<-county_risk_long[grep("big", county_risk_long$mitigation),]
county_risk_big$event_size <- event_size[3]
county_risk_big_clean<-county_risk_big%>%
    pivot_wider(names_from = "mitigation", values_from = c( "pct_chance_someone_inf", "chance_someone_inf"))%>%
   rename(
        `Same vax % as county` = pct_chance_someone_inf_inf_no_mitig_big,
        `Rapid test right before` = pct_chance_someone_inf_inf_testing_no_vax_big,
        `100% vaccinated` = pct_chance_someone_inf_inf_fully_vax_no_test_big,
        `100% vax & rapid test` = pct_chance_someone_inf_inf_fully_vax_and_test_big,
        `7-day cases per 100k` = cases_last_7_days_per_100k,
        chance_someone_inf_no_mitig = chance_someone_inf_inf_no_mitig_big,
        chance_someone_inf_testing_no_vax = chance_someone_inf_inf_testing_no_vax_big,
        chance_someone_inf_fully_vax_no_test = chance_someone_inf_inf_fully_vax_no_test_big,
        chance_someone_inf_fully_vax_and_test = chance_someone_inf_inf_fully_vax_and_test_big,
        `Data last reported` = date_updated
    )%>%mutate(
        `Data updated` =substr(lubridate::now('EST'), 1, 10)
    )

#-----Write data files for maps-----------------------------------------
#write.csv(county_risk, '../out/county_risk.csv')
# local paths
if (USE_CASE == 'local'){
write.csv(county_risk_small_clean, '../out/county_risk_small_Omicron.csv') # Flourish 1
write.csv(county_risk_med_clean, '../out/county_risk_med_Omicron.csv') # Flourish 2
write.csv(county_risk_big_clean, '../out/county_risk_big_Omicron.csv')  # Flourish 3
}
# Domino paths
if (USE_CASE == 'domino'){
write.csv(county_risk_small_clean, '/mnt/out/county_risk_small_Omicron.csv') # Flourish 1
write.csv(county_risk_med_clean, '/mnt/out/county_risk_med_Omicron.csv') # Flourish 2
write.csv(county_risk_big_clean, '/mnt/out/county_risk_big_Omicron.csv')  # Flourish 3
}




#------Summarise by region---------------------------------------------
# Exclude counties not reporting cases from regional sums
county_risk$pop[is.na(county_risk$sum_cases_7_days)]<-NA 

region_risk<-county_risk%>%mutate(
    n_vax = vax_rate*pop)%>%
    group_by(Region)%>%
    summarise(
        region_pop = sum(pop, na.rm = TRUE),
        region_cases_last_7_days =sum(sum_cases_7_days, na.rm = TRUE),
        region_cases_last_7_days_per_100k = round(100000*sum(sum_cases_7_days, na.rm = TRUE)/sum(pop, na.rm = TRUE),0),
        region_vax_rate = sum(n_vax,na.rm = TRUE)/sum(pop, na.rm = TRUE)
    )%>%drop_na(Region)%>%
    mutate(
        prev_per_100k = (1/reporting_rate)*region_cases_last_7_days_per_100k,
        prev = (1/reporting_rate)*region_cases_last_7_days/region_pop,
        m_test = (1-test_sens), 
        m_v = (1-VE_inf*prop_vax_event)/(1-VE_inf*region_vax_rate)
        )



# ----- Make a bar chart df for Flourish--------------------------------
for (i in 1:length(event_size)){
    event_sizes<-rep(event_size[i], nrow(region_risk))
    df<-region_risk%>%mutate(
        event_size = event_size[i],
        risk_inf_no_mitig = calc_risk_inf_at_event(prev, event_size[i]),
        risk_inf_testing_no_vax = calc_risk_inf_at_event(m_test*prev, event_size[i]),
        risk_inf_fully_vax_no_test= calc_risk_inf_at_event(m_v*prev, event_size[i]),
        risk_inf_fully_vax_and_test = calc_risk_inf_at_event(m_test*m_v*prev, event_size[i])
    )
    if (i==1){
        region_df<-df
    }
    else{
        region_df<-rbind(region_df, df)
    }
}

region_df<-region_df%>%rename(`Same vax % as county` = risk_inf_no_mitig,
                              `Rapid test right before` = risk_inf_testing_no_vax,
                              `100% vaccinated` = risk_inf_fully_vax_no_test,
                              `100% vax & rapid test` = risk_inf_fully_vax_and_test)
region_df<-region_df%>%arrange(desc(region_cases_last_7_days_per_100k))
# local
if (USE_CASE == 'local'){
write.csv(region_df, '../out/region_bar_chart_Omicron.csv')
}
#Domino
if (USE_CASE == 'domino'){
write.csv(region_df, '/mnt/out/region_bar_chart_Omicron.csv')
}

#------  Make a line plot dataframe for Flourish -----------------------------------
event_size_vec<-seq(from = 1, to = 50, by = 1)
for (i in 1:length(event_size_vec)){
    event_sizes<-rep(event_size_vec[i], nrow(region_risk))
    df<-region_risk%>%mutate(
        event_size = event_size_vec[i],
        risk_inf_no_mitig = calc_risk_inf_at_event(prev, event_size_vec[i]),
        risk_inf_testing_no_vax = calc_risk_inf_at_event(m_test*prev, event_size_vec[i]),
        risk_inf_fully_vax_no_test= calc_risk_inf_at_event(m_v*prev, event_size_vec[i]),
        risk_inf_fully_vax_and_test = calc_risk_inf_at_event(m_test*m_v*prev, event_size_vec[i])
    )
    if (i==1){
        region_event_size_df<-df
    }
    else{
        region_event_size_df<-rbind(region_event_size_df, df)
    }
}

region_event_size_df<-region_event_size_df%>%rename(`Same vax % as county` = risk_inf_no_mitig,
                              `Rapid test right before` = risk_inf_testing_no_vax,
                              `100% vaccinated` = risk_inf_fully_vax_no_test,
                              `100% vax & rapid test` = risk_inf_fully_vax_and_test)
region_event_size_df$Region<-factor(region_event_size_df$Region, ordered = TRUE, stringr::str_wrap(c(
    "Northeast", "Midwest", "South", "West")))
region_event_size_df<-region_event_size_df%>%arrange(desc(region_cases_last_7_days_per_100k))
# local 
if (USE_CASE == 'local'){
write.csv(region_event_size_df, '../out/region_line_plot_Omicron.csv')
}
# Domino
if (USE_CASE == 'domino'){
write.csv(region_event_size_df, '/mnt/out/region_line_plot_Omicron.csv')
}

