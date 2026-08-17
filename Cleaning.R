library(tidyverse)
library(scales)
disease = read_csv("PLACES__Local_Data_for_Better_Health__County_Data_2021_release.csv")

med_income = read_csv("MedianIncome3.csv")
----------------------------------------------------------------------------------------

disease_heart = disease %>%
  filter(
    Measure == "Coronary heart disease among adults aged >=18 years",
    Data_Value_Type == "Crude prevalence",
    StateDesc != "District of Columbia",
    StateDesc != "Guam",
    StateDesc != "Puerto Rico",
    StateDesc != "Virgin Islands",
    StateDesc != "United States"
  )

med_income_clean = med_income %>%
  select(
    NAME, S1901_C01_012E
  ) %>%
  separate(
    col = NAME,
    into = c("county", "state"),
    sep = ","
  ) %>%
  rename(
    med_income = S1901_C01_012E
  )

combined_heart = inner_join(disease_heart, med_income_clean, 
                            by = c("LocationName" = "county", "StateDesc" = "state"))


combined_heart %>%
  ggplot() + 
  geom_point(aes(x = med_income, y = Data_Value)) + 
  theme_classic()
--------------------------------------------------------------------------------------

disease_copd = disease %>%
  filter(
    Measure == "Chronic obstructive pulmonary disease among adults aged >=18 years",
    Data_Value_Type == "Crude prevalence",
    StateDesc != "District of Columbia",
    StateDesc != "Guam",
    StateDesc != "Puerto Rico",
    StateDesc != "Virgin Islands",
    StateDesc != "United States"
  )

combined_copd = inner_join(disease_copd, med_income_clean, 
                            by = c("LocationName" = "county", "StateDesc" = "state"))

combined_copd %>%
  ggplot() + 
  geom_point(aes(x = med_income, y = Data_Value)) + 
  theme_classic()
  
------------------------------------------------------------------
  
disease_diabetes = disease %>%
  filter(
    #Measure == "Diagnosed diabetes among adults aged >=18 years",
    Measure == "Obesity among adults aged >=18 years",
    Data_Value_Type == "Crude prevalence",
    StateDesc != "District of Columbia",
    StateDesc != "Guam",
    StateDesc != "Puerto Rico",
    StateDesc != "Virgin Islands",
    StateDesc != "United States"
  )

combined_diabetes = inner_join(disease_diabetes, med_income_clean, 
                           by = c("LocationName" = "county", "StateDesc" = "state"))


combined_diabetes %>%
  ggplot() + 
  geom_point(aes(x = med_income, y = Data_Value)) +
  theme_classic() + 
  theme(text = element_text(size = 16))

--------------------------------------------------------------------

obesity = disease %>%
  filter(
    Measure == "Obesity among adults aged >=18 years",
    Data_Value_Type == "Crude prevalence",
    StateDesc != "District of Columbia",
    StateDesc != "Guam",
    StateDesc != "Puerto Rico",
    StateDesc != "Virgin Islands",
    StateDesc != "United States"
  )
  
  
--------------------------------------------------------------------

disease_general = disease %>%
  select(
    Year, StateAbbr, StateDesc, LocationName, Measure, Data_Value_Type, Data_Value
  ) %>%
  filter(
    Measure == "Coronary heart disease among adults aged >=18 years" |
    Measure == "Chronic obstructive pulmonary disease among adults aged >=18 years" | 
    Measure == "Obesity among adults aged >=18 years",
    Data_Value_Type == "Crude prevalence",
    StateDesc != "Guam",
    StateDesc != "Puerto Rico",
    StateDesc != "Virgin Islands",
    StateDesc != "United States"
  )

combined_general = inner_join(disease_general, med_income_clean, 
                           by = c("LocationName" = "county", "StateDesc" = "state"))

combined_general = combined_general %>%
  mutate(
    Measure = case_when(
      Measure == "Obesity among adults aged >=18 years" ~ "Obesity",
      Measure == "Chronic obstructive pulmonary disease among adults aged >=18 years" ~ "COPD",
      Measure == "Coronary heart disease among adults aged >=18 years" ~ "Heart Disease"
    )
  )

combined_general = combined_general %>%
  rename(
    State = StateDesc,
    County = LocationName
  )

combined_general %>%
  ggplot() + 
  geom_point(aes(x = med_income, y = Data_Value)) +
  facet_wrap(~ Measure) +
  labs(y = "Prevalence(%)", x = "Median Household Income (000's USD)") +
  theme_classic() +
  theme(text = element_text(size = 30)) + 
  scale_x_continuous(labels = 
                       label_number(scale = 1e-3, 
                                    accuracy = 1
                                    ))

# display the data for 3 counties with lowest income
top_3_low = combined_general %>%
  pivot_wider(
    names_from = Measure,
    values_from = Data_Value
  ) %>%
  arrange(desc(med_income)) %>%
  tail(3) %>%
  select(
    Year,
    StateAbbr,
    County,
    med_income,
    COPD,
    "Heart Disease",
    Obesity
  ) %>%
  rename(
    "State Abbr." = StateAbbr,
    "Median Income ($)" = med_income,
    "COPD (%)" = COPD,
    "Heart Disease (%)" = "Heart Disease",
    "Obesity (%)" = Obesity
  )

# display the data for 3 counties with highest income
top_3_high = combined_general %>%
  pivot_wider(
    names_from = Measure,
    values_from = Data_Value
  ) %>%
  arrange(desc(med_income)) %>%
  head(3) %>%
  select(
    Year,
    StateAbbr,
    County,
    med_income,
    COPD,
    "Heart Disease",
    Obesity
  ) %>%
  rename(
    "State Abbr." = StateAbbr,
    "Median Income ($)" = med_income,
    "COPD (%)" = COPD,
    "Heart Disease (%)" = "Heart Disease",
    "Obesity (%)" = Obesity
  )
  

# write the lowest income dataset to csv
write.csv(top_3_low, file = "top_3_low.csv", row.names = FALSE)

# write the highest income dataset to csv
write.csv(top_3_high, file = "top_3_high.csv", row.names = FALSE)

# write the median household income dataset to csv
write.csv(med_income_clean, file = "med_income_clean.csv", row.names = FALSE)

# write the heart disease dataset to csv
write.csv(obesity, file = "obesity.csv", row.names = FALSE)

# Below is trash!!!!!!!!!!!!!!!
-------------------------------------------------------------------
air_quality = read.csv("annual_conc_by_monitor_2019.csv")

ozone_clean = air_quality%>%
  select(
    State.Name, County.Name,
    Parameter.Name, Metric.Used, Units.of.Measure,Year, Arithmetic.Mean
  ) %>%
  filter(
    Parameter.Name == "Ozone",
    Metric.Used == "Daily maximum of 8 hour running average of observed hourly values"
  ) %>%
  group_by(State.Name, County.Name, Parameter.Name, Units.of.Measure, Year) %>%
  summarize(
    mean = mean(Arithmetic.Mean)
  )

combined_Ozone = inner_join(ozone_clean, disease_respiratory, 
                            by = c("County.Name" = "LocationName"), 
                            c("State.Name" = "StateDesc"))

combined_Ozone %>%
  ggplot() + 
  geom_point(aes(x = mean, y = Data_Value))

----------------------------------------------------------------
air_quality= read.csv("annual_conc_by_monitor_2019.csv")

sulfur_clean = air_quality%>%
  select(
    State.Name, County.Name, City.Name,
    Parameter.Name, Metric.Used, Units.of.Measure,Year, 
    Units.of.Measure, Arithmetic.Mean
  ) %>%
  filter(
    Parameter.Name == "Sulfur dioxide",
    Metric.Used == "Daily maximum 1-hour average"
  ) %>%
  group_by(State.Name, County.Name, Parameter.Name, Units.of.Measure, Year) %>%
  summarize(
    mean = mean(Arithmetic.Mean)
  )

combined_sulfur = inner_join(sulfur_clean, disease_respiratory, 
                            by = c("County.Name" = "LocationName"), 
                                   c("State.Name" = "StateDesc"))

combined_sulfur %>%
  ggplot() + 
  geom_point(aes(x = mean, y = Data_Value))

---------------------------------------------------------------
air_quality= read.csv("annual_conc_by_monitor_2019.csv")

nitrogen_clean = air_quality%>%
  select(
    State.Name, County.Name, City.Name,
    Parameter.Name, Metric.Used, Units.of.Measure,Year, 
    Units.of.Measure, Arithmetic.Mean
  ) %>%
  filter(
    Parameter.Name == "Nitrogen dioxide (NO2)",
    Metric.Used == "Daily Maximum 1-hour average"
  ) %>%
  group_by(State.Name, County.Name, Parameter.Name, Units.of.Measure, Year) %>%
  summarize(
    mean = mean(Arithmetic.Mean)
  )

combined_nitrogen = inner_join(nitrogen_clean, disease_respiratory, 
                             by = c("County.Name" = "LocationName"), 
                                    c("State.Name" = "StateDesc"))

combined_nitrogen %>%
  ggplot() + 
  geom_point(aes(x = mean, y = Data_Value))
  
--------------------------------------------------------------
air_quality= read.csv("annual_conc_by_monitor_2019.csv")

carbon_monoxide_clean = air_quality%>%
  select(
    State.Name, County.Name, City.Name,
    Parameter.Name, Metric.Used, Units.of.Measure,Year, 
    Units.of.Measure, Arithmetic.Mean
  ) %>%
  filter(
    Parameter.Name == "Carbon monoxide",
    Metric.Used == "8-Hour running average (end hour) of observed hourly values"
  ) %>%
  group_by(State.Name, County.Name, Parameter.Name, Units.of.Measure, Year) %>%
  summarize(
    mean = mean(Arithmetic.Mean)
  )


combined_carbon_monoxide = inner_join(carbon_monoxide_clean, 
                                      disease_respiratory, by = c("County.Name" = "LocationName",
                                                            "State.Name" = "StateDesc"))

combined_carbon_monoxide %>%
  ggplot() + 
  geom_point(aes(x = mean, y = Data_Value))

--------------------------------------------------------
air_quality= read.csv("annual_conc_by_monitor_2019.csv")

PM2.5_clean = air_quality%>%
  select(
    State.Name, County.Name, City.Name,
    Parameter.Name, Metric.Used, Units.of.Measure,Year, 
    Units.of.Measure, Arithmetic.Mean
  ) %>%
  filter(
    Parameter.Name == "PM2.5 - Local Conditions",
    Metric.Used == "Observed Values"
  ) %>%
  group_by(State.Name, County.Name, Parameter.Name, Units.of.Measure, Year) %>%
  summarize(
    mean = mean(Arithmetic.Mean)
)


combined_PM2.5 = inner_join(PM2.5_clean, disease_respiratory,
                            by = c("County.Name" = "LocationName",
                                   "State.Name" = "StateDesc"))

combined_PM2.5 %>%
  ggplot() + 
  geom_point(aes(x = mean, y = Data_Value))
  
  
test ---------------------------------------------------------
  
  
  
  
  
  
  
  
aqi = read.csv("daily_aqi_by_county_2019.csv")

aqi_clean = aqi%>%
  mutate(
    Date_2 = Date
  ) %>%
  separate(col = Date_2,
           into = c("year", "month", "day"),
           sep = "-") %>%
  mutate(
    year = as.integer(year),
    month = as.integer(month),
    day = as.integer(day)
  ) %>%
  filter(
    State.Name != "Puerto Rico",
    State.Name != "District of Columbia"
  )

aqi2 = read.csv("daily_aqi_by_county_2018.csv")

aqi2_clean = aqi2%>%
  mutate(
    Date_2 = Date
  ) %>%
  separate(col = Date_2,
           into = c("year", "month", "day"),
           sep = "-") %>%
  mutate(
    year = as.integer(year),
    month = as.integer(month),
    day = as.integer(day)
  ) %>%
  filter(
    State.Name != "Puerto Rico",
    State.Name != "District of Columbia"
  )

group_county = aqi_clean %>%
  group_by(State.Name, county.Name, Defining.Parameter, year) %>%
  summarize(
    avg_AQI = mean(AQI)
  )

group2_county = aqi2_clean %>%
  group_by(county.Name, Defining.Parameter, year) %>%
  summarize(
    avg_AQI = mean(AQI)
  )

together = inner_join(group_county, disease_respiratory, 
                      by = c("county.Name" = "LocationName"))

together2 = inner_join(group2_county, copd2_clean, by = c("county.Name" = "LocationName"))

together3 = bind_rows(together, together2)


together3 %>%
  ggplot() + 
  geom_point(mapping = aes(x = avg_AQI, y = Data_Value)) +
  facet_wrap(~Defining.Parameter)

together %>%
  ggplot() + 
  geom_histogram(mapping = aes(x = avg_AQI))

together %>%
  ggplot() + 
  geom_histogram(mapping = aes(x = Data_Value))

---------------------------------------------------------------

  
  
  
---------------------------------------------------------------
  test = read.csv("aqi_daily_1980_to_2021.csv")

test_clean = test%>%
  mutate(
    Date_2 = Date
  ) %>%
  separate(col = Date_2,
           into = c("year", "month", "day"),
           sep = "-") %>%
  mutate(
    year = as.integer(year),
    month = as.integer(month),
    day = as.integer(day)
  ) %>%
  filter(
    State.Name != "Puerto Rico",
    State.Name != "District of Columbia",
    year %in% c(2018, 2019)
  )

test2 = test_clean %>%
  group_by(County.Name, Defining.Parameter, year) %>%
  summarize(
    avg_AQI = mean(AQI)
  )

test_together = inner_join(test2, copd_clean, by = c("County.Name" = "LocationName"))

test_together %>%
  ggplot() + 
  geom_point(mapping = aes(x = avg_AQI, y = Data_Value)) +
  facet_wrap(~Defining.Parameter)

library(RedditExtractoR)
links = find_thread_urls(subreddit="GlobalOffensive", period="year")

library(RedditExtractoR)
library(dplyr)
library(magrittr)

i <- 1
min_date <- as.Date("2021/01/01")
max_date <- as.Date("2021/12/31")

Post_min <- min_date +1

while(Post_min > min_date){
  
  Data_out <- find_thread_urls(subreddit = "GlobalOffensive", sort_by = "new") %>%
    mutate(date = as.Date(date, "%d-%m-%y"))
  
  
  i <- i+1
  Post_min <- min(Data_out$date)
}

Data_out %<>% filter(date >=min_date  & date <=max_date)

copd2 = read.csv("PLACES__Local_Data_for_Better_Health__County_Data_2021_release.csv")

copd2_clean = copd2 %>% filter(
  Measure == "Coronary heart disease among adults aged >=18 years",
  Data_Value_Type == "Crude prevalence",
  StateDesc != "District of Columbia",
  StateDesc != "Guam",
  StateDesc != "Puerto Rico",
  StateDesc != "Virgin Islands",
  StateDesc != "United States",
  StateDesc != "Hawaii",
  StateDesc != "Alaska"
)

