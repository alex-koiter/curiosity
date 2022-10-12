library(tidyverse)
library(weathercan)
library(lubridate)

# Find station(s)
stations_search(name = "brandon", interval = "day")

# Download data from ECCC
## Brandon A has two entries 1941- 2012 and 2012-current so we need to bind them together. 
data <- weather_dl(station_ids = "50821", start = "2012-10-01", end = "2022-09-30", interval = "day") %>%
  bind_rows(weather_dl(station_ids = "3471", start = "1959-10-01", end = "2012-09-30", interval = "day"))

# Assign water year e.g., 2019-10-01 to 2020-09-30 is the 2020 water year
water_year <- data %>%
  mutate(day = as.numeric(day), month = as.numeric(month), year = as.numeric(year)) %>%
  mutate(water_year = ifelse(month >= 10 & month <= 12, (year + 1) , year)) %>%
  mutate(new_date = ymd(ifelse(month < 10, paste("2020", month(date), day(date)), paste("2019", month(date), day(date))))) %>%
  mutate(total_precip = replace_na(total_precip, 0)) %>%
  group_by(water_year) %>%
  mutate(cum_precip = cumsum(total_precip))

# plot all years
p <- ggplot(data = water_year, aes(y = cum_precip , x = new_date, group = water_year)) +
  geom_line() +
  geom_line(data = filter(water_year, water_year == 2021), aes(y = cum_precip , x = new_date), colour = "red", size = 1.5) +
  geom_line(data = filter(water_year, water_year == 2022), aes(y = cum_precip , x = new_date), colour = "blue", size = 1.5) +
  scale_x_date(date_labels = "%B") +
  labs(y = "Cumulative Precipitation (mm)", x = "Date") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

p

# Plotting the historical data as percentiles makes more sense
# Need to exclude current year
avg <- data %>%
  mutate(day = as.numeric(day), month = as.numeric(month), year = as.numeric(year)) %>%
  mutate(water_year = ifelse(month >= 10 & month <= 12, (year + 1) , year)) %>%
  mutate(new_date = ymd(ifelse(month < 10, paste("2020", month(date), day(date)), paste("2019", month(date), day(date))))) %>%
  filter(water_year < 2021) %>%
  mutate(total_precip = replace_na(total_precip, 0)) %>%
  group_by(water_year, station_id) %>%
  mutate(cum_precip = cumsum(total_precip)) %>%
  group_by(new_date) %>%
  summarise(median = median(cum_precip),
            per.10 = quantile(cum_precip, 0.1),
            per.90 = quantile(cum_precip, 0.9))

p1 <- ggplot(data = avg, aes(y = median , x = new_date)) +
  geom_ribbon(aes(ymin = per.10, ymax = per.90, fill = "Interval")) +
  geom_line(aes(colour = "1960-2020 Median"), size = 1.5) +
  geom_line(data = filter(water_year, water_year == 2021), aes(y = cum_precip , x = new_date, colour = "2021"), size = 1.5) +
  geom_line(data = filter(water_year, water_year == 2022), aes(y = cum_precip , x = new_date, colour = "2022"), size = 1.5) +
  scale_colour_manual(name='', 
                      values = c("1960-2020 Median" = "black", "2021" = "red", "2022" = "blue")) +
  scale_fill_manual(name = "", 
                    values = c("10-90th Percentile" = "grey50")) +
  scale_x_date(date_labels = "%B", 
               expand = c(0,0),
               breaks = "1 month") +
  scale_y_continuous(expand = c(0,0), breaks = seq(0, 600, 200), limits = c(0, 600)) +
  labs(y = "Cumulative Precipitation (mm)", 
       x = "Date", 
       title = "Water Year (Oct-Sept) Cumulative Precipitation \nBrandon MB") +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") 

p1


ggsave(plot = p1, filename = "./Weather/Brandon/Brandon water year precip 2021_2022.png", width = 230, height = 200, units = "mm", dpi = 600)

