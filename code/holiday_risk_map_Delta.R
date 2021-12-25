# Original Author: Kaitlyn Johnson

# Date originated: 12-13-2021






# ----- Load in libraries and datasets ---------------------------------
rm(list = ls())
install.packages("lubridate", dependencies = TRUE, repos = 'http://cran.us.r-project.org')
install.packages("readxl", dependencies = TRUE, repos = 'http://cran.us.r-project.org')
install.packages("styler", dependencies = TRUE, repos = 'http://cran.us.r-project.org')
install.packages("tidyverse", dependencies = TRUE, repos = 'http://cran.us.r-project.org')
#library(lintr)
library(styler)
library(tidyverse)
library(readxl)
library(lubridate)

#lint("holiday_risk_map.R")

# file paths
NYT_CASE_BY_COUNTY_DAY <- url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-recent.csv")
VAX_RATES_BY_COUNTY<- url("https://raw.githubusercontent.com/bansallab/vaccinetracking/main/vacc_data/data_county_current.csv")
#COUNTY_POPULATION_PATH<-'../data/county_pops.csv'
STATES_BY_REGION<- url("https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv")
#local
#SHAPEFILE_PATH<-'../data/county_shape_file.txt'
#Domino 
SHAPEFILE_PATH<-'/mnt/data/county_shape_file.txt'

# read files
cases_by_county_t <- read_csv(NYT_CASE_BY_COUNTY_DAY)
#county_pops<-read_csv(COUNTY_POPULATION_PATH)
vax_rate_by_county_t<-read_csv(VAX_RATES_BY_COUNTY)
state_regions<-read_csv(STATES_BY_REGION)


# test

# ----- Set parameters----------------------------------------------------
DUR_INF<- 7 
yesterday<- lubridate::today('EST')- days(1) # look at reported cases since yesterday
reporting_rate<- 1/4 # https://www.cdc.gov/coronavirus/2019-ncov/cases-updates/burden.html
test_sens <-0.786 # https://www.cdc.gov/mmwr/volumes/70/wr/mm7003e3.htm, https://www.medrxiv.org/content/10.1101/2020.10.30.20223198v1.full.pdf
VE_inf<- 0.53 # https://www.cdc.gov/mmwr/volumes/70/wr/mm7034e3.htm
event_size<-c(10,20,30)
event_size<-sort(event_size)
prop_vax_event<-1


# ---- Risk functions ---------------------------------------------------
calc_risk_inf_at_event <- function(p_I, n) {
    r<-1-(1-p_I)**n
    round(100 * r, 1)
}

# ------ Summarise daily data to county-level ---------------------------
#case data
county_cases<-cases_by_county_t%>%filter(date>=(yesterday-DUR_INF) & date<=yesterday)%>%
    mutate(county_fips = as.character(fips))%>%
    group_by(county_fips, county, state)%>%
    summarise(sum_cases_7_days = max(cases) - min(cases)) 
county_cases['county_state_formatted']<-paste0(county_cases$county, ", ",county_cases$state)
county_cases$county_fips[county_cases$county == "New York City"]<-36061



# vax data
county_vax<-vax_rate_by_county_t%>%filter(CASE_TYPE == "Complete Coverage")%>%
    group_by(COUNTY)%>%filter(DATE == max(DATE))%>%
    rename(pop = POPN)%>%
    mutate(vax_rate = CASES/100)%>%select(COUNTY, pop,vax_rate)

# Replace those with no cases with NA
county_cases$sum_cases_7_days[county_cases$sum_cases_7_days ==0]<-NA
counties_w_no_data<-county_cases[county_cases$sum_cases_7_days ==0 | is.na(county_cases$sum_cases_7_days),]




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



# Load in shapefile and join to it
shapefile <- read_delim(SHAPEFILE_PATH, delim = "\t") %>%select(geometry, FIPS)
county_risk<-left_join(shapefile, county_risk, by = c("FIPS" = "county_fips"))

# Clean the dataframe for Flourish
county_risk_long<-county_risk%>%pivot_longer(
    cols = starts_with("risk_"),
    names_to = "mitigation",
    names_prefix = "risk_", 
    values_to = "chance_someone_inf",
    values_drop_na = TRUE
)

# Add % to all risks
county_risk_long$pct_chance_someone_inf<-paste0(county_risk_long$chance_someone_inf, ' %')

# Add note to for counties not reporting data 
county_risk_long$pct_chance_someone_inf[is.na(county_risk_long$sum_cases_7_days)]<- 'Insufficient case data'
county_risk_long$cases_last_7_days_per_100k[is.na(county_risk_long$sum_cases_7_days)]<-'No reported cases'

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
        chance_someone_inf_fully_vax_and_test = chance_someone_inf_inf_fully_vax_and_test_small
        
    )%>%mutate(
        `Updated` = paste0(substr(lubridate::now('EST'), 1, 16), ' EST')
    )

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
        chance_someone_inf_fully_vax_and_test = chance_someone_inf_inf_fully_vax_and_test_med
    )%>%mutate(
        `Updated` = paste0(substr(lubridate::now('EST'), 1, 16), ' EST')
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
        chance_someone_inf_fully_vax_and_test = chance_someone_inf_inf_fully_vax_and_test_big
    )%>%mutate(
        `Updated` = paste0(substr(lubridate::now('EST'), 1, 16), ' EST')
    )

#-----Write data files for maps-----------------------------------------
# local paths
#write.csv(county_risk_small_clean, '../out/county_risk_small_Delta.csv') # Flourish 1
#write.csv(county_risk_med_clean, '../out/county_risk_med_Delta.csv') # Flourish 2
#write.csv(county_risk_big_clean, '../out/county_risk_big_Delta.csv')  # Flourish 3




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
        region_vax_rate = sum(n_vax, na.rm = TRUE)/sum(pop, na.rm = TRUE)
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
region_df$Region<-factor(region_df$Region, ordered = TRUE, stringr::str_wrap(c(
    "Northeast", "Midwest", "South", "West")))
region_df<-region_df%>%arrange(desc(region_cases_last_7_days_per_100k))
#local
#write.csv(region_df, '../out/region_bar_chart_Delta.csv')
#Domino
write.csv(region_df, '/mnt/out/region_bar_chart_Delta.csv')

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
region_event_size_df<-region_event_size_df%>%arrange(desc(region_cases_last_7_days_per_100k))
# local
#write.csv(region_event_size_df, '../out/region_line_plot_Delta.csv')
# Domino
write.csv(region_event_size_df, '/mnt/out/region_line_plot_Delta.csv')




