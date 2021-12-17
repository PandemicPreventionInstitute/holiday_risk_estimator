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
        `Same vax rate as county` = pct_chance_someone_inf_inf_no_mitig_small,
        `Rapid test right before` = pct_chance_someone_inf_inf_testing_no_vax_small,
        `100% vaccinated` = pct_chance_someone_inf_inf_fully_vax_no_test_small,
        `100% vax & rapid test` = pct_chance_someone_inf_inf_fully_vax_and_test_small,
        `7-day cases per 100k` = cases_last_7_days_per_100k,
        chance_someone_inf_no_mitig = chance_someone_inf_inf_no_mitig_small,
        chance_someone_inf_testing_no_vax = chance_someone_inf_inf_testing_no_vax_small,
        chance_someone_inf_fully_vax_no_test = chance_someone_inf_inf_fully_vax_no_test_small,
        chance_someone_inf_fully_vax_and_test = chance_someone_inf_inf_fully_vax_and_test_small
        
    )


county_risk_med<-county_risk_long[grep("med", county_risk_long$mitigation),]
county_risk_med$event_size <- event_size[2]
county_risk_med_clean<-county_risk_med%>%
    pivot_wider(names_from = "mitigation", values_from = c("pct_chance_someone_inf", "chance_someone_inf"))%>%
    rename(
        `Same vax rate as county` =pct_chance_someone_inf_inf_no_mitig_med,
        `Rapid test right before` = pct_chance_someone_inf_inf_testing_no_vax_med,
        `100% vaccinated` = pct_chance_someone_inf_inf_fully_vax_no_test_med,
        `100% vax & rapid test` = pct_chance_someone_inf_inf_fully_vax_and_test_med,
        `7-day cases per 100k` = cases_last_7_days_per_100k,
        chance_someone_inf_no_mitig = chance_someone_inf_inf_no_mitig_med,
        chance_someone_inf_testing_no_vax = chance_someone_inf_inf_testing_no_vax_med,
        chance_someone_inf_fully_vax_no_test = chance_someone_inf_inf_fully_vax_no_test_med,
        chance_someone_inf_fully_vax_and_test = chance_someone_inf_inf_fully_vax_and_test_med
    )


county_risk_big<-county_risk_long[grep("big", county_risk_long$mitigation),]
county_risk_big$event_size <- event_size[3]
county_risk_big_clean<-county_risk_big%>%
    pivot_wider(names_from = "mitigation", values_from = c( "pct_chance_someone_inf", "chance_someone_inf"))%>%
   rename(
        `Same vax rate as county` = pct_chance_someone_inf_inf_no_mitig_big,
        `Rapid test right before` = pct_chance_someone_inf_inf_testing_no_vax_big,
        `100% vaccinated` = pct_chance_someone_inf_inf_fully_vax_no_test_big,
        `100% vax & rapid test` = pct_chance_someone_inf_inf_fully_vax_and_test_big,
        `7-day cases per 100k` = cases_last_7_days_per_100k,
        chance_someone_inf_no_mitig = chance_someone_inf_inf_no_mitig_big,
        chance_someone_inf_testing_no_vax = chance_someone_inf_inf_testing_no_vax_big,
        chance_someone_inf_fully_vax_no_test = chance_someone_inf_inf_fully_vax_no_test_big,
        chance_someone_inf_fully_vax_and_test = chance_someone_inf_inf_fully_vax_and_test_big
    )


# Rename back to original and pivot longer
county_risk_clean<-rbind(county_risk_small_clean, county_risk_med_clean)
county_risk_clean<-rbind(county_risk_clean, county_risk_big_clean)
county_risk_clean<-county_risk_clean%>%select(!`Same vax rate as county` & !`Rapid test right before`& 
                                              !`100% vaccinated` & !`100% vax & rapid test`) # remove cols with %s
county_risk_long<-county_risk_clean%>%
pivot_longer(
    cols = starts_with("chance_someone_inf_"),
    names_to = "mitigation",
    names_prefix = "chance_someone_inf_", 
    values_to = "chance_someone_inf",
    values_drop_na = TRUE
)

# Write data files 
write.csv(county_risk, '../out/county_risk.csv')
write.csv(county_risk_small_clean, '../out/county_risk_small.csv') # Flourish 1
write.csv(county_risk_med_clean, '../out/county_risk_med.csv') # Flourish 2
write.csv(county_risk_big_clean, '../out/county_risk_big.csv')  # Flourish 3
write.csv(county_risk_long, '../out/county_risk_long.csv')

# Select an example county for Flourish
example_county_risk<-county_risk_long%>%filter(county_state_formatted == "Union, New Jersey")
#find_df$archetype_clean<-factor(find_df$archetype_clean, ordered = TRUE, stringr::str_wrap(c("Test", "Connect", "Leverage", "Strengthen")))
example_county_risk$mitigation<-factor(example_county_risk$mitigation, ordered = TRUE, stringr::str_wrap(c(
    "fully_vax_and_test", "testing_no_vax", "fully_vax_no_test", "no_mitig")))

# Make dataframe for Flourish
example_county_risk_wide<-example_county_risk%>%select(
    event_size, mitigation, chance_someone_inf)%>%
    pivot_wider(names_from = mitigation, values_from = chance_someone_inf)%>%
    rename(`Same vax % as county` = no_mitig,
           `Rapid test right before` = testing_no_vax,
           `100% vaccinated` = fully_vax_no_test,
           `100% vax & rapid test` = fully_vax_and_test)%>%
    select(event_size, `Same vax % as county`, `100% vaccinated`, `Rapid test right before`, `100% vax & rapid test`)
write.csv(example_county_risk_wide, '../out/example_bar_chart.csv') # Flourish bar chart


# For this county, vary event size and find probability someone infected 
event_sizes<-seq(from = 1, to = 50, by = 1)
prev = rep(example_county_risk$prev[1], length(event_sizes))
m_v = rep(example_county_risk$m_v[1], length(event_sizes))
m_test = rep(example_county_risk$m_test[1], length(event_sizes))
df<-data.frame(event_sizes, prev, m_v, m_test)
df<-df%>%mutate(
    risk_inf_no_mitig = calc_risk_inf_at_event(prev, event_sizes),
    risk_inf_testing_no_vax = calc_risk_inf_at_event(m_test*prev, event_sizes),
    risk_inf_fully_vax_no_test = calc_risk_inf_at_event(m_v*prev, event_sizes),
    risk_inf_fully_vax_and_test= calc_risk_inf_at_event(m_test*m_v*prev, event_sizes)
)%>%rename(
    `Same vax % as county` = risk_inf_no_mitig,
    `Rapid test right before` = risk_inf_testing_no_vax,
    `100% vaccinated` = risk_inf_fully_vax_no_test,
     `100% vax & rapid test` = risk_inf_fully_vax_and_test)

write.csv(df, '../out/example_line_plot.csv') # Flourish bar chart
