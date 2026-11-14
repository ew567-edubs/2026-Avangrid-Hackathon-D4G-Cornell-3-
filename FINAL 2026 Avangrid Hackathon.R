library(ggplot2)
library(dplyr)
library(data.table)
library(duckdb)
library(DBI)
library(dbplyr)
library(tidyr)

#-----------------------------------------------------------------------------------------------------

###Importing 4 given CSVs
PV <- read.csv("/Users/ethan/Downloads/Avangrid Hackathon/EV_Heat Pump_Solar PV Customers(PV Customers).csv")
HP <- read.csv("~/Downloads/Avangrid Hackathon/EV_Heat Pump_Solar PV Customers(HP Customers).csv")
EV <- read.csv('/Users/ethan/Downloads/Avangrid Hackathon/EV_Heat Pump_Solar PV Customers(EV Customers).csv')
weather <- read.csv("/Users/ethan/Downloads/Avangrid Hackathon/Weather Data_Avangrid_Hackathon (Sheet1).csv")
#-----------------------------------------------------------------------------------------------------

###Making the master data table/frame with the AMI data
con <- dbConnect(duckdb(), dbdir = "customers.duckdb")

dbExecute(con, "
  CREATE TABLE all_customers_v2 AS
  SELECT *
  FROM read_csv_auto([
    '/Users/ethan/Downloads/Avangrid Hackathon/2026 Avangrid Hackathon Data_1_10000', 
    '/Users/ethan/Downloads/Avangrid Hackathon/2026 Avangrid Hackathon Data_10001_20000_v2',
    '/Users/ethan/Downloads/Avangrid Hackathon/2026 Avangrid Hackathon Data_20001_30000', 
    '/Users/ethan/Downloads/Avangrid Hackathon/2026 Avangrid Hackathon Data_30001_35725'
  ], union_by_name = true)
")

dbWriteTable(con, "ev_customers", EV, overwrite = TRUE)
dbWriteTable(con, "hp_customers", HP, overwrite = TRUE)
dbWriteTable(con, "pv_customers", PV, overwrite = TRUE)
customers <- tbl(con, "all_customers_v2")
customers %>% filter(CUSTOMER_NUMBER == 9501) 
EVs <- tbl(con, "ev_customers") %>%
  distinct(CUSTOMER_NUMBER) %>%
  mutate(EV = 1)
HPs <- tbl(con, "hp_customers") %>%
  distinct(CUSTOMER_NUMBER) %>%
  mutate(HP = 1)
PVs <- tbl(con, "pv_customers") %>%
  distinct(CUSTOMER_NUMBER) %>%
  mutate(PV = 1)
customers <- customers %>%
  left_join(EVs, by = "CUSTOMER_NUMBER") %>%
  mutate(EV = coalesce(EV, 0L))
customers <- customers %>%
  left_join(HPs, by = "CUSTOMER_NUMBER") %>%
  mutate(HP = coalesce(HP, 0L))
customers <- customers %>%
  left_join(PVs, by = "CUSTOMER_NUMBER") %>%
  mutate(PV = coalesce(PV, 0L))
#-----------------------------------------------------------------------------------------------------
###Adding the 3 EV, HP, PV columns
customer_summary <- customer_summary %>%
  left_join(
    customers %>%
      group_by(CUSTOMER_NUMBER) %>%
      summarize(
        EV = max(EV),
        HP = max(HP),
        PV = max(PV)
      ) %>%
      collect(),
    by = "CUSTOMER_NUMBER"
  )
#-----------------------------------------------------------------------------------------------------
weather <- weather %>%
  separate(
    date.time.temp.humidity.windspeed,
    into = c("date", "time", "temperature", "humidity", "windspeed"),
    sep = ";"
  )
###There are some NAs
weather %>%
  summarize(
    total_hours = n(),
    missing_temperature = sum(is.na(temperature)),
    missing_humidity = sum(is.na(humidity)),
    missing_windspeed = sum(is.na(windspeed)),
    complete_hours = sum(
      !is.na(temperature) &
        !is.na(humidity) &
        !is.na(windspeed)
    )
  )
weather <- weather %>%
  mutate(
    hour = as.integer(sub(pattern=":.*", replacement="", x=time))
  )
head(weather)
customers <- customers %>%
  mutate(hour = hour_ending - 1)
customers %>%
  select(hour_ending, hour) %>%
  distinct() %>%
  arrange(hour_ending) %>%
  collect()

#-----------------------------------------------------------------------------------------------------
### Re-running the customers table because it doesn't work for some reason
customers <- tbl(con, "all_customers_v2")
customers <- customers %>%
  left_join(
    tbl(con, "ev_customers") %>% distinct(CUSTOMER_NUMBER) %>% mutate(EV = 1L),
    by = "CUSTOMER_NUMBER"
  ) %>%
  left_join(
    tbl(con, "hp_customers") %>% distinct(CUSTOMER_NUMBER) %>% mutate(HP = 1L),
    by = "CUSTOMER_NUMBER"
  ) %>%
  left_join(
    tbl(con, "pv_customers") %>% distinct(CUSTOMER_NUMBER) %>% mutate(PV = 1L),
    by = "CUSTOMER_NUMBER"
  ) %>%
  mutate(
    EV = coalesce(EV, 0L),
    HP = coalesce(HP, 0L),
    PV = coalesce(PV, 0L),
    hour = hour_ending - 1
  )
customers %>%
  select(CUSTOMER_NUMBER, EV, HP, PV, hour) %>%
  head(5) %>%
  collect()
customers <- customers %>%
  mutate(
    month = sql("EXTRACT(MONTH FROM usage_date)")
  )

### Seasonal Analysis, if required
customers <- customers %>%
  mutate(
    season = case_when(
      month %in% c(12, 1, 2) ~ "Winter",
      month %in% c(3, 4, 5) ~ "Spring",
      month %in% c(6, 7, 8) ~ "Summer",
      TRUE ~ "Fall"
    )
  )
hourly_profiles <- customers %>%
  group_by(PV, season, hour) %>%
  summarize(
    avg_usage = mean(hourly_kwh, na.rm = TRUE)
  ) %>%
  collect()
head(hourly_profiles)
hourly_profiles <- hourly_profiles %>%
  arrange(PV, season, hour)
#-----------------------------------------------------------------------------------------------------
###Characteristic: Solar PV
solar_profiles <- customers %>%
  filter(PV == 1) %>%
  mutate(
    period = case_when(
      hour < 6 ~ "Night",
      hour < 12 ~ "Morning",
      hour < 17 ~ "Daytime",
      TRUE ~ "Evening"
    )
  ) %>%
  group_by(CUSTOMER_NUMBER, season, period) %>%
  summarize(
    avg_usage = mean(hourly_kwh, na.rm = TRUE)
  ) %>%
  collect()

solar_profiles %>%
  count(CUSTOMER_NUMBER) %>%
  count(n)

battery_profiles <- solar_profiles %>%
  group_by(CUSTOMER_NUMBER, season) %>%
  summarize(
    night = mean(avg_usage[period == "Night"]),
    morning = mean(avg_usage[period == "Morning"]),
    daytime = mean(avg_usage[period == "Daytime"]),
    evening = mean(avg_usage[period == "Evening"]),
    .groups = "drop"
  )

battery_profiles <- battery_profiles %>%
  mutate(
    evening_ratio = evening / daytime
  )

battery_ranking <- battery_profiles %>%
  group_by(CUSTOMER_NUMBER) %>%
  summarize(
    avg_evening_ratio = mean(evening_ratio),
    avg_daytime_usage = mean(daytime),
    .groups = "drop"
  )
battery_candidates <- battery_ranking %>%
  filter(
    avg_daytime_usage > 1.0,
    avg_evening_ratio > 1.0
  ) %>%
  arrange(avg_evening_ratio)
battery_candidates
#-----------------------------------------------------------------------------------------------------
###Characteristic: HP
seasonal_daily <- customers %>%
  group_by(CUSTOMER_NUMBER, HP, usage_date, season) %>%
  summarize(
    daily_kwh = sum(hourly_kwh, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(CUSTOMER_NUMBER, HP, season) %>%
  summarize(
    avg_daily_kwh = mean(daily_kwh, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  collect()
dbWriteTable(con, "weather", weather, overwrite = TRUE)
customers_weather <- customers %>%
  left_join(
    tbl(con, "weather"),
    by = c("usage_date" = "date", "hour" = "hour")
  )

customers_weather %>%
  select(CUSTOMER_NUMBER, usage_date, hour, hourly_kwh, temperature) %>%
  head() %>%
  collect()
head(customers_weather)
daily_usage <- customers_weather %>%
  group_by(CUSTOMER_NUMBER, HP, usage_date) %>%
  summarize(
    daily_kwh = sum(hourly_kwh, na.rm = TRUE),
    .groups = "drop"
  )
daily_weather <- weather %>%
  group_by(date) %>%
  summarize(
    avg_temp = mean(temperature, na.rm = TRUE),
    .groups = "drop"
  )
dbWriteTable(con, "daily_weather", daily_weather, overwrite = TRUE)
daily_usage <- daily_usage %>%
  left_join(
    tbl(con, "daily_weather"),
    by = c("usage_date" = "date")
  )
daily_usage %>%
  select(CUSTOMER_NUMBER, HP, usage_date, daily_kwh, avg_temp) %>%
  head() %>%
  collect()
daily_usage <- daily_usage %>%
  mutate(
    temp_group = case_when(
      avg_temp <= 35 ~ "Cold",
      avg_temp >= 65 ~ "Hot",
      TRUE ~ "Mild"
    )
  )
hp_temp_patterns <- daily_usage %>%
  group_by(CUSTOMER_NUMBER, HP, temp_group) %>%
  summarize(
    avg_daily_kwh = mean(daily_kwh, na.rm = TRUE),
    days_observed = n(),
    .groups = "drop"
  )
head(hp_temp_patterns)
known_hp_patterns <- hp_temp_patterns %>%
  filter(HP == 1) %>%
  group_by(temp_group) %>%
  summarize(
    avg_usage = mean(avg_daily_kwh, na.rm = TRUE),
    median_usage = median(avg_daily_kwh, na.rm = TRUE),
    customer_count = n(),
    .groups = "drop"
  )
head(known_hp_patterns)
unknown_hp_patterns <- hp_temp_patterns %>%
  filter(HP == 0) %>%
  group_by(temp_group) %>%
  summarize(
    avg_usage = mean(avg_daily_kwh, na.rm = TRUE),
    median_usage = median(avg_daily_kwh, na.rm = TRUE),
    customer_count = n(),
    .groups = "drop"
  )
head(unknown_hp_patterns)
hp_comparison <- hp_temp_patterns %>%
  tidyr::pivot_wider(
    id_cols = c(CUSTOMER_NUMBER, HP),
    names_from = temp_group,
    values_from = avg_daily_kwh
  ) %>%
  filter(!is.na(Cold), !is.na(Mild), !is.na(Hot)) %>%
  mutate(
    cold_increase = (Cold - Mild) / Mild * 100,
    hot_increase = (Hot - Mild) / Mild * 100
  )
hp_comparison %>%
  group_by(HP) %>%
  summarize(
    avg_cold_increase = mean(cold_increase, na.rm = TRUE),
    median_cold_increase = median(cold_increase, na.rm = TRUE),
    avg_hot_increase = mean(hot_increase, na.rm = TRUE),
    median_hot_increase = median(hot_increase, na.rm = TRUE),
    customers = n()
  )
hp_comparison <- hp_comparison %>%
  mutate(
    cold_change = Cold - Mild,
    hot_change = Hot - Mild
  )
hp_comparison
hp_comparison %>%
  group_by(HP) %>%
  summarize(
    median_cold_change = median(cold_change, na.rm = TRUE),
    median_hot_change = median(hot_change, na.rm = TRUE),
    customers = n()
  )
hp_comparison %>%
  group_by(HP) %>%
  summarize(
    q25 = quantile(cold_change, 0.25, na.rm = TRUE),
    median = median(cold_change, na.rm = TRUE),
    q75 = quantile(cold_change, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

hp_comparison <- hp_comparison %>%
  mutate(
    baseline_usage = (Cold + Mild + Hot) / 3,
    cold_sensitivity = cold_change / baseline_usage * 100
  )
hp_comparison %>%
  group_by(HP) %>%
  summarize(
    median_sensitivity = median(cold_sensitivity, na.rm = TRUE),
    q25 = quantile(cold_sensitivity, 0.25, na.rm = TRUE),
    q75 = quantile(cold_sensitivity, 0.75, na.rm = TRUE),
    customers = n()
  )
hp_comparison %>%
  group_by(HP) %>%
  summarize(
    above_threshold = sum(cold_sensitivity >= 14.3, na.rm = TRUE),
    total_customers = n(),
    percent_above = mean(as.numeric(cold_sensitivity >= 14.3), na.rm = TRUE) * 100
  ) %>%
  collect()
top_hp_candidates <- hp_comparison %>%
  filter(
    baseline_usage > 0,
    is.finite(cold_sensitivity)
  ) %>%
  filter(HP == 0) %>%
  arrange(desc(cold_sensitivity)) %>%
  select(
    CUSTOMER_NUMBER,
    Cold,
    Mild,
    Hot,
    cold_change,
    cold_sensitivity
  ) %>%
  head(100) %>%
  collect()
head(top_hp_candidates)
top_hp_candidates <- hp_comparison %>%
  filter(
    HP == 0,
    baseline_usage >= 10,
    is.finite(cold_sensitivity)
  ) %>%
  arrange(desc(cold_sensitivity)) %>%
  select(
    CUSTOMER_NUMBER,
    Cold,
    Mild,
    Hot,
    baseline_usage,
    cold_change,
    cold_sensitivity
  ) %>%
  head(100) %>%
  collect()
head(top_hp_candidates, 20)
hp_candidates <- hp_comparison %>%
  filter(
    HP == 0,
    baseline_usage >= 10,
    is.finite(cold_sensitivity),
    cold_change >= 5
  ) %>%
  mutate(
    HP_confidence = case_when(
      cold_sensitivity >= 45 ~ "High",
      cold_sensitivity >= 25 ~ "Medium",
      TRUE ~ "Low"
    )
  ) %>%
  arrange(desc(cold_sensitivity)) %>%
  collect()
hp_candidates %>%
  count(HP_confidence)
hp_comparison %>%
  filter(HP == 1) %>%
  pull(cold_sensitivity) %>%
  summary()
hp_shortlist <- hp_comparison %>%
  filter(
    HP == 0,
    baseline_usage >= 10,
    cold_change >= 5,
    cold_sensitivity >= 45.36,
    is.finite(cold_sensitivity)
  ) %>%
  arrange(desc(cold_sensitivity)) %>%
  collect()
nrow(hp_shortlist)
hp_comparison %>%
  filter(HP == 0) %>%
  summarize(total = n()) %>%
  collect()
#-----------------------------------------------------------------------------------------------------
### Characteristic: AC
ac_patterns <- daily_usage %>%
  group_by(CUSTOMER_NUMBER, temp_group = case_when(
    avg_temp <= 35 ~ "Cold",
    avg_temp >= 65 ~ "Hot",
    TRUE ~ "Mild"
  )) %>%
  summarize(
    avg_daily_kwh = mean(daily_kwh, na.rm = TRUE),
    days_observed = n(),
    .groups = "drop"
  ) %>%
  collect()
head(ac_patterns)

ac_comparison <- ac_patterns %>%
  tidyr::pivot_wider(
    id_cols = CUSTOMER_NUMBER,
    names_from = temp_group,
    values_from = avg_daily_kwh
  ) %>%
  filter(!is.na(Hot), !is.na(Mild)) %>%
  mutate(
    hot_change = Hot - Mild,
    baseline_usage = (Hot + Mild) / 2,
    hot_sensitivity = hot_change / baseline_usage * 100
  )
summary(ac_comparison$hot_sensitivity)
ac_shortlist <- ac_comparison %>%
  filter(
    is.finite(hot_sensitivity),
    baseline_usage >= 10,
    hot_change >= 5,
    hot_sensitivity >= 52.338
  ) %>%
  arrange(desc(hot_sensitivity))
ac_threshold <- quantile(
  ac_comparison$hot_sensitivity,
  0.90,
  na.rm = TRUE
)

ac_threshold
ac_shortlist <- ac_comparison %>%
  filter(
    is.finite(hot_sensitivity),
    baseline_usage >= 10,
    hot_change >= 10,
    hot_sensitivity >= 69.89093
  ) %>%
  arrange(desc(hot_sensitivity))
nrow(ac_shortlist)
#-----------------------------------------------------------------------------------------------------
###Making the final candidate list:
PV_candidates <- battery_candidates$CUSTOMER_NUMBER
HP_candidates <- hp_shortlist$CUSTOMER_NUMBER
AC_candidates <- ac_shortlist$CUSTOMER_NUMBER

technology_candidates <- data.frame(
  CUSTOMER_NUMBER = unique(c(
    HP_candidates,
    PV_candidates, 
    AC_candidates
  ))
)

technology_candidates$HP <- as.integer(
  technology_candidates$CUSTOMER_NUMBER %in% HP_candidates
)

technology_candidates$PV <- as.integer(
  technology_candidates$CUSTOMER_NUMBER %in% PV_candidates
)

technology_candidates$AC <- as.integer(
  technology_candidates$CUSTOMER_NUMBER %in% AC_candidates
)

technology_candidates
#-----------------------------------------------------------------------------------------------------
###FINAL GRAPHS
hp_graph_data <- hp_temp_patterns %>%
  group_by(HP, temp_group) %>%
  summarize(
    avg_usage = mean(avg_daily_kwh, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  collect()
hp_graph_data$group <- ifelse(
  hp_graph_data$HP == 1,
  "Known HP",
  "Unlabeled"
)
hp_graph_data
hp_graph_data$temp_group <- factor(
  hp_graph_data$temp_group,
  levels = c("Cold", "Mild", "Hot")
)

ggplot(hp_graph_data,
       aes(x = temp_group, y = avg_usage,
           color = group, group = group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Average Daily Electricity Use Across Temperatures",
    x = "Temperature Category",
    y = "Average Daily Electricity Use (kWh)",
    color = "Customer Group"
  ) +
  scale_x_discrete(limits = c("Cold", "Mild", "Hot")) + theme_minimal()

hp_box_data <- hp_comparison %>%
  filter(is.finite(cold_sensitivity)) %>%
  mutate(
    group = ifelse(HP == 1, "Known HP", "Unlabeled")
  )

ggplot(hp_box_data,
       aes(x = group, y = cold_sensitivity, fill = group)) +
  geom_boxplot(outlier.alpha = 0.15) +
  geom_hline(
    yintercept = 45.36,
    linetype = "dashed"
  ) +
  labs(
    title = "Cold-Weather Sensitivity: Known HP vs. Unlabeled",
    x = "Customer Group",
    y = "Cold-Weather Sensitivity (%)",
    fill = "Customer Group"
  ) +
  theme_minimal()

ac_graph_data <- daily_usage %>%
  mutate(
    group = if_else(
      CUSTOMER_NUMBER %in% AC_candidates,
      "Potential AC",
      "Other Customers"
    )
  ) %>%
  group_by(avg_temp, group) %>%
  summarize(
    avg_usage = mean(daily_kwh, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  collect()
ggplot(ac_graph_data,
       aes(x = avg_temp, y = avg_usage, color = group)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Daily Electricity Use vs. Outdoor Temperature",
    x = "Average Outdoor Temperature (°F)",
    y = "Average Daily Electricity Use (kWh)",
    color = "Customer Group"
  ) +
  theme_minimal()
pv_graph_data <- customers %>%
  group_by(PV, hour) %>%
  summarize(
    avg_usage = mean(hourly_kwh, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  collect()

pv_graph_data$group <- ifelse(
  pv_graph_data$PV == 1,
  "Known Solar PV",
  "Other Customers"
)

ggplot(pv_graph_data,
       aes(x = hour, y = avg_usage,
           color = group, group = group)) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Average Hourly Electricity Consumption: Solar vs. Other Customers",
    x = "Hour of Day",
    y = "Average Hourly Electricity Consumption (kWh)",
    color = "Customer Group"
  ) +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  theme_minimal()