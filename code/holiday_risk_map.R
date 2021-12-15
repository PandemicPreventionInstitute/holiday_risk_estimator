# Original Author: Kaitlyn Johnson

# Date originated: 12-13-2021






# ----- Load in libraries and datasets ---------------------------------
rm(list = ls())
library(lintr)
library(styler)
library(tidyverse)
library(readxl)
library(lubridate)

#lint("holiday_risk_map.R")

# file paths
NYT_CASE_BY_COUNTY_DAY <- url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-recent.csv")
VAX_RATES_BY_COUNTY<- url("https://raw.githubusercontent.com/bansallab/vaccinetracking/main/vacc_data/data_county_current.csv")
COUNTY_POPULATION_PATH<-'../data/county_pops.csv'
SHAPEFILE_PATH<-'../data/county_shape_file.txt'

# read files
cases_by_county_t <- read_csv(NYT_CASE_BY_COUNTY_DAY)
county_pops<-read_csv(COUNTY_POPULATION_PATH)
vax_rate_by_county_t<-read_csv(VAX_RATES_BY_COUNTY)

# test

# ----- Set parameters----------------------------------------------------
DUR_INF<- 7 
today<- lubridate::today('EST')
reporting_rate<- 1/4 # https://www.cdc.gov/coronavirus/2019-ncov/cases-updates/burden.html
test_sens <-0.88 # https://pubmed.ncbi.nlm.nih.gov/33971580/
VE_inf<- 0.53 # https://www.cdc.gov/mmwr/volumes/70/wr/mm7034e3.htm
event_size<-c(10,20,30)
event_size<-sort(event_sizes)
prop_vax_event<-1


# ---- Risk functions ---------------------------------------------------
calc_risk_inf_at_event <- function(p_I, n) {
    r<-1-(1-p_I)**n
    round(100 * r, 1)
}

# ------ Summarise daily data to county-level ---------------------------
#case data
county_cases<-cases_by_county_t%>%filter(date>=(today-DUR_INF-1) & date<=today)%>%
    mutate(county_fips = as.character(fips))%>%
    group_by(county_fips, county, state)%>%
    summarise(sum_cases_7_days = max(cases) - min(cases)) 
county_cases['county_state_formatted']<-paste0(county_cases$county, ", ",county_cases$state)
county_cases$county_fips[county_cases$county == "New York City"]<-36061

# vax data
county_vax<-vax_rate_by_county_t%>%filter(CASE_TYPE == "Complete Coverage")%>%
    group_by(COUNTY)%>%filter(DATE == max(DATE))%>%
    mutate(vax_rate = CASES/100)%>%select(COUNTY, vax_rate)


# Calculate the effect of vaccination and  day of testing on 
county_risk<-county_cases%>%left_join(county_pops, by = "county_state_formatted")%>%
    left_join(county_vax, by = c("county_fips" = "COUNTY"))%>%
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
# add % to all risks
county_risk_long$pct_chance_someone_inf<-paste0(county_risk_long$chance_someone_inf, ' %')

# Make 3 separate dataframes so column names can be the same
county_risk_small<-county_risk_long[grep("small", county_risk_long$mitigation),]
county_risk_small$event_size<-event_size[1]
county_risk_small_clean<-county_risk_small%>%
    pivot_wider(names_from = "mitigation", values_from = c("pct_chance_someone_inf", "chance_someone_inf"))%>%
    mutate(event_size = event_size[1])%>%
    rename(
        `No mitigation` = pct_chance_someone_inf_inf_no_mitig_small,
        `Rapid test morning of` = pct_chance_someone_inf_inf_testing_no_vax_small,
        `100% vaccinated` = pct_chance_someone_inf_inf_fully_vax_no_test_small,
        `100% vax & rapid test` = pct_chance_someone_inf_inf_fully_vax_and_test_small,
        `7-day cases per 100k` = cases_last_7_days_per_100k
    )


county_risk_med<-county_risk_long[grep("med", county_risk_long$mitigation),]
county_risk_med$event_size <- event_size[2]
county_risk_med_clean<-county_risk_med%>%
    pivot_wider(names_from = "mitigation", values_from = c("pct_chance_someone_inf", "chance_someone_inf"))%>%
    rename(
        `No mitigation` =pct_chance_someone_inf_inf_no_mitig_med,
        `Rapid test morning of` = pct_chance_someone_inf_inf_testing_no_vax_med,
        `100% vaccinated` = pct_chance_someone_inf_inf_fully_vax_no_test_med,
        `100% vax & rapid test` = pct_chance_someone_inf_inf_fully_vax_and_test_med,
        `7-day cases per 100k` = cases_last_7_days_per_100k
    )


county_risk_big<-county_risk_long[grep("big", county_risk_long$mitigation),]
county_risk_big$event_size <- event_size[3]
county_risk_big_clean<-county_risk_big%>%
    pivot_wider(names_from = "mitigation", values_from = c( "pct_chance_someone_inf", "chance_someone_inf"))%>%
   rename(
        `No mitigation` = pct_chance_someone_inf_inf_no_mitig_big,
        `Rapid test morning of` = pct_chance_someone_inf_inf_testing_no_vax_big,
        `100% vaccinated` = pct_chance_someone_inf_inf_fully_vax_no_test_big,
        `100% vax & rapid test` = pct_chance_someone_inf_inf_fully_vax_and_test_big,
        `7-day cases per 100k` = cases_last_7_days_per_100k
    )


# Rename back to original and pivot logner
county_risk_clean<-rbind(county_risk_small_clean, county_risk_med_clean)
county_risk_clean<-rbind(county_risk_clean, county_risk_big_clean)
county_risk_long<-county_risk_clean%>%rename(
    inf_no_mitig = `No mitigation`,
    inf_testing_no_vax = `Rapid test morning of`,
    inf_fully_vax_no_test= `100% vaccinated`,
    inf_fully_vax_and_test = `100% vax & rapid test`,
    cases_last_7_days_per_100k = `7-day cases per 100k`)%>%
pivot_longer(
    cols = starts_with("inf_"),
    names_to = "mitigation",
    names_prefix = "inf_", 
    values_to = "chance_someone_inf",
    values_drop_na = TRUE
)

# Write data files 
write.csv(county_risk, '../out/county_risk.csv')
write.csv(county_risk_small_clean, '../out/county_risk_small.csv')
write.csv(county_risk_med_clean, '../out/county_risk_med.csv')
write.csv(county_risk_big_clean, '../out/county_risk_big.csv') 
write.csv(county_risk_long, '../out/county_risk_long.csv')