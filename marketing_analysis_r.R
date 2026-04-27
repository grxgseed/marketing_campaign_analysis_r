## Load data
install.packages("tidyverse")
library(tidyverse)
install.packages("readx")
library(readxl)
marketing_data <- read_excel("C:/Users/glsee/OneDrive/Documents/data analysis/portfolio/global_ads_performance_dataset.xlsx")

## View data
head(marketing_data)
glimpse(marketing_data)
names(marketing_data)

## Cleaning column names
marketing_data <- marketing_data %>% rename(
  ctr = CTR,
  cpc = CPC,
  cpa = CPA,
  roas = ROAS
)
names(marketing_data)

## Aggregating data
marketing_data %>%
  summarise(
    total_spend = sum(ad_spend, na.rm = TRUE),
    total_conversions = sum(conversions, na.rm = TRUE),
    total_revenue = sum(revenue, na.rm = TRUE),
    avg_ctr = mean(ctr, na.rm = TRUE),
    avg_roas = mean(roas, na.rm = TRUE)
  )

## Summarizing data
platform_summary <- marketing_data %>%
  group_by(platform) %>%
  summarise(
    total_spend = sum(ad_spend, na.rm = TRUE),
    total_conversions = sum(conversions, na.rm = TRUE),
    total_revenue = sum(revenue, na.rm = TRUE),
    avg_ctr = mean(ctr, na.rm = TRUE),
    avg_roas = mean(roas, na.rm = TRUE)
  ) %>%
  arrange(desc(total_conversions))
platform_summary
campaign_summary <- marketing_data %>%
  group_by(campaign_type) %>%
  summarise(
    total_spend = sum(ad_spend, na.rm = TRUE),
    total_conversions = sum(conversions, na.rm = TRUE),
    total_revenue = sum(revenue, na.rm = TRUE),
    avg_ctr = mean(ctr, na.rm = TRUE),
    avg_roas = mean(roas, na.rm = TRUE)
  ) %>%
  arrange(desc(total_conversions))
campaign_summary
time_summary <- marketing_data %>%
  group_by(date) %>%
  summarise(
    total_conversions = sum(conversions, na.rm = TRUE),
    total_spend = sum(ad_spend, na.rm = TRUE)
  )

## Visualizing data
ggplot(platform_summary, aes(x = reorder(platform, total_conversions), y = total_conversions)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Total Conversions by Platform",
    x = "Platform",
    y = "Total Conversions"
  )
ggplot(campaign_summary, aes(x = reorder(campaign_type, avg_roas), y = avg_roas)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Average ROAS by Campaign Type",
    x = "Campaign Type",
    y = "Average ROAS"
  )
ggplot(time_summary, aes(x = date, y = total_conversions)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Conversions Over Time",
    x = "Date",
    y = "Total Conversions"
  )
ggplot(time_summary, aes(x = date, y = total_spend)) +
  geom_line() +
  scale_y_continuous(labels = scales::commas) +
  labs(
    title = "Total Spend Over Time",
    x = "Date",
    y = "Total Spend (£)"
  )