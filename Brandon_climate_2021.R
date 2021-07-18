library(tidyverse)
library(weathercan)
library(lubridate)

# Find station(s)
stations_search(name = "brandon", interval = "day")

# Download data from ECCC
## Brandon A has two entries 1941- 2012 and 2012-2021 so we need to bind them together
data <- weather_dl(station_ids = "50821", start = "2012-10-01", interval = "day") %>%
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
  scale_x_date(date_labels = "%B") +
  labs(y = "Cumulative Precipitation (mm)", x = "Date") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

p

# Plotting the historical data as pecentiles makes more sense
# Need to exclude current year
avg <- data %>%
  filter(year != 2021) %>%
  mutate(day = as.numeric(day), month = as.numeric(month), year = as.numeric(year)) %>%
  mutate(water_year = ifelse(month >= 10 & month <= 12, (year + 1) , year)) %>%
  mutate(new_date = ymd(ifelse(month < 10, paste("2020", month(date), day(date)), paste("2019", month(date), day(date))))) %>%
  mutate(total_precip = replace_na(total_precip, 0)) %>%
  group_by(water_year, station_id) %>%
  mutate(cum_precip = cumsum(total_precip)) %>%
  group_by(new_date) %>%
  summarise(median = median(cum_precip),
            per.10 = quantile(cum_precip, 0.1),
            per.90 = quantile(cum_precip, 0.9))

p1 <- ggplot(data = avg, aes(y = median , x = new_date)) +
  geom_ribbon(aes(ymin = per.10, ymax = per.90), fill = "grey70") +
  geom_line() +
  geom_line(data = filter(water_year, water_year == 2021), aes(y = cum_precip , x = new_date), colour = "red", size = 1.5) +
  scale_x_date(date_labels = "%B") +
  labs(y = "Cumulative Precipitation (mm)", x = "Date", title = "Water Year (Oct-Sept) Cumulative Precipitation \nBrandon MB 1960-2020") +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

p1

ggsave(plot = p1, filename = "Brandon water year precip.png", width = 230, height = 200, units = "mm", dpi = 600)

# Precipitation and growing degree days over the growing seasons (May 01 to Sept 30)
# The base temperature for canola is 5 degrees
# Need to exclude the current year
avg_grow <- data %>%
  filter(year != 2021) %>%
  mutate(day = as.numeric(day), month = as.numeric(month), year = as.numeric(year)) %>%
  mutate(water_year = ifelse(month >= 10 & month <= 12, (year + 1) , year)) %>%
  mutate(new_date = ymd(ifelse(month < 10, paste("2020", month(date), day(date)), paste("2019", month(date), day(date))))) %>%
  mutate(total_precip = replace_na(total_precip, 0), max_temp = replace_na(max_temp, 0), min_temp = replace_na(min_temp, 0)) %>%
  mutate(gdd = ifelse((max_temp + min_temp)/2  - 5 < 0, 0, (max_temp + min_temp)/2 - 5)) %>%
  filter(month >= 5, month < 9) %>%
  group_by(water_year) %>%
  mutate(cum_precip = cumsum(total_precip), cum_gdd = cumsum(gdd)) %>%
  group_by(new_date) %>%
  summarise(median = median(cum_precip),
            per.10 = quantile(cum_precip, 0.1),
            per.90 = quantile(cum_precip, 0.9),
            median_gdd = median(cum_gdd),
            per.10_gdd = quantile(cum_gdd, 0.1),
            per.90_gdd = quantile(cum_gdd, 0.9))

# Current growing year precipitation and degree growing days

current_grow <-  data %>%
  mutate(day = as.numeric(day), month = as.numeric(month), year = as.numeric(year)) %>%
  filter(year == 2021) %>%
  filter(month >= 5, month < 9) %>%
  #mutate(day = as.numeric(day), month = as.numeric(month), year = as.numeric(year)) %>%
  mutate(new_date = ymd(ifelse(month < 10, paste("2020", month(date), day(date)), paste("2019", month(date), day(date))))) %>%
  mutate(total_precip = replace_na(total_precip, 0), max_temp = replace_na(max_temp, 0), min_temp = replace_na(min_temp, 0)) %>%
  mutate(gdd = ifelse((max_temp + min_temp)/2  - 5 < 0, 0, (max_temp + min_temp)/2 - 5)) %>%
  mutate(cum_precip = cumsum(total_precip), cum_gdd = cumsum(gdd)) 


p2 <- ggplot(data = avg_grow, aes(y = median , x = new_date)) +
  geom_ribbon(aes(ymin = per.10, ymax = per.90), fill = "grey70") +
  geom_line() +
  geom_line(data = current_grow, aes(y = cum_precip , x = new_date), colour = "red", size = 1.5) +
  scale_x_date(date_labels = "%B") +
  labs(y = "Cumulative Precipitation (mm)", x = "Date", title = "Growing Season Cumulative Precipitation \nBrandon MB 1960-2020") +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

p2

ggsave(plot = p2, filename = "Brandon growing precip.png", width = 230, height = 200, units = "mm", dpi = 600)


p3 <- ggplot(data = avg_grow, aes(y = median_gdd , x = new_date)) +
  geom_ribbon(aes(ymin = per.10_gdd, ymax = per.90_gdd), fill = "grey70") +
  geom_line() +
  geom_line(data = current_grow, aes(y = cum_gdd , x = new_date), colour = "red", size = 1.5) +
  scale_x_date(date_labels = "%B") +
  labs(y = "Cummulative Growing Degree Days", x = "Date", title = "Cummulative Growing Degree Days (5°C Base) \nBrandon MB 1960-2020") +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p3

ggsave(plot = p3, filename = "Brandon GDD.png", width = 230, height = 200, units = "mm", dpi = 600)

