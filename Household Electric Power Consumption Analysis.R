library('dplyr')
library('lubridate')
library('ggplot2')
library('tidyr')
library('plotly')
library('psych')
library('corrplot')

### Read Data
wd = getwd()
setwd("/Users/robertoruizfelix/Downloads/")
raw = readLines("household_power_consumption.txt")

### View Data
head(raw)

## Data Frame Preparation
raw = raw[raw != ""]
split_data = strsplit(raw, ";")

### Convert the list into a data frame
data = as.data.frame(do.call(rbind, split_data), stringsAsFactors = FALSE)
colnames(data) = data[1, ]  # Use the first row as column names
data = data[-1, ]           # Remove the first row from data

### Convert numeric columns from character to numeric
numeric_cols = c("Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
data[numeric_cols] = lapply(data[numeric_cols], as.numeric)


### Null Value Handling
missing_vals = sapply(data, function(x) sum(is.na(x)))
missing_vals = data.frame(Column = names(missing_vals), Missing_Count = missing_vals)
missing_vals$Total = nrow(data)
ggplot(missing_vals, aes(x = Column)) + 
  geom_bar(aes(y = Total), stat = "identity", fill = "lightblue") +
  geom_bar(aes(y = Missing_Count), stat = "identity", fill = "red") + 
  labs(title = "Total Data vs Missing Values by Column", 
       x = "Columns", 
       y = "Count") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

data = na.omit(data)
missing_vals = sapply(data, function(x) sum(is.na(x)))
missing_vals = data.frame(Column = names(missing_vals), Missing_Count = missing_vals)
missing_vals$Total = nrow(data)
ggplot(missing_vals, aes(x = Column)) + 
  geom_bar(aes(y = Total), stat = "identity", fill = "lightblue") +
  geom_bar(aes(y = Missing_Count), stat = "identity", fill = "red") + 
  labs(title = "Missing Values Cleaned", 
       x = "Columns", 
       y = "Count") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


### Feature Engineering
colnames(data)[7:9] = c("Kitchen(W/hr)", "Laundry_Room(W/hr)", "Electric_WaterHeater/AC(W/hr)")
data = data %>%
  mutate(
    `Total_metering(W/hr)` = `Kitchen(W/hr)` + `Laundry_Room(W/hr)` + `Electric_WaterHeater/AC(W/hr)`,
    Apparent_Power = sqrt(Global_active_power^2 + Global_reactive_power^2),
    Power_Factor = Global_active_power / Apparent_Power,
    Date = dmy(Date),
    DateTime = as.POSIXct(paste(Date, Time)),
    Time = hms(Time),
    Year = year(DateTime),
    Month = month(DateTime),
    Week = week(DateTime),
    Day = yday(DateTime)
    ) %>%
  select(-Date, -Time)


### Descriptive Statistics 
describe(data)

pf_tmp = ggplot(data, aes("", Power_Factor)) +
  geom_violin(fill = "lightblue") + 
  labs(title = "Power Factor Violin Plot",
       y = "Power Factor")
pf_violin = ggplotly(pf_tmp)
pf_violin

pf_hist_tmp = ggplot(data, aes(Power_Factor)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Power Factor", 
       x = "Power Factor", 
       y = "Frequency") +
  theme_minimal()
pf_hist = ggplotly(pf_hist_tmp)
pf_hist



### Time Series Plot of Total Metering 
ttl_mtring_time_tmp = ggplot(data, aes(DateTime, `Total_metering(W/hr)`)) +
  geom_line(color = "purple") +  # Line plot
  geom_point(color = "black", alpha = 0.2) +   # Points on the line
  labs(title = "Time Series Plot of Total Metering",
       x = "DateTime",
       y = "Total Metering (W/hr)") +
  theme_minimal()  # A clean theme for better visualization
ttl_mtring_time = ggplotly(ttl_mtring_time_tmp)
ttl_mtring_time

### Time Series & Bar Plots of Total Metering by Year, Month, Week
yearly_data = data %>%
  group_by(Year)

yearly_data_bar = yearly_data %>%
  summarise(Total_Metering = sum(`Total_metering(W/hr)`))
tmp_year_total_metering = ggplot(yearly_data_bar, aes(Year, Total_Metering)) + 
  geom_bar(stat = "identity", fill = "lightgreen") + 
  labs(title = "Total Energy Sub-Metering per Year",
       x = "Year", 
       y = "Watts per Hour")
yearly_metering = ggplotly(tmp_year_total_metering)
yearly_metering

months_by_year <- data %>%
  mutate(Month = format(DateTime, "%B")) %>%  # Extract the month name
  group_by(Year) %>%
  summarise(Months = list(unique(Month)))
data = data[data$Year != 2006, ]


monthly_data_total = data %>%
  group_by(Year, Month) %>%
  summarise(Total_Metering = sum(`Total_metering(W/hr)`), .groups = "drop")
monthly_data_avg = data %>%
  group_by(Year, Month) %>%
  summarise(Mean_Metering = mean(`Total_metering(W/hr)`), .groups = "drop")
tmp_month_ttl = ggplot(monthly_data_total, aes(Month, Total_Metering, color = as.factor(Year))) +
  geom_line() +
  scale_x_continuous(limits = c(1,12), breaks = 1:12) +
  labs(title = "Total Energy Sub-metering per Month",
       x = "Month",
       y = "Watts per Hour",
       color = "Year") 
month_total_metering = ggplotly(tmp_month_ttl)
month_total_metering
tmp_month_avg = ggplot(monthly_data_avg, aes(Month, Mean_Metering, color = as.factor(Year))) +
  geom_line() +
  scale_x_continuous(limits = c(1,12), breaks = 1:12) +
  labs(title = "Mean Energy Sub-metering per Month",
       x = "Month",
       y = "Watts per Hour",
       color = "Year") 
month_avg_metering = ggplotly(tmp_month_avg)
month_avg_metering
tmp_boxmonth = ggplot(monthly_data_total, aes(as.factor(Month), Total_Metering, fill = as.factor(Month))) +
  geom_boxplot() +
  labs(title = "Monthly Total Energy Metering",
       x = "Month",
       y = "Watts per Hour") +
  theme(legend.position = "none") 
monthly_box = ggplotly(tmp_boxmonth)
monthly_box
tmp_boxmonth_avg = ggplot(monthly_data_avg, aes(as.factor(Month), Mean_Metering, fill = as.factor(Month))) +
  geom_boxplot() +
  labs(title = "Monthly Mean Energy Metering",
       x = "Month",
       y = "Watts per Hour") +
  theme(legend.position = "none") 
monthly_box_avg = ggplotly(tmp_boxmonth_avg)
monthly_box_avg




### WEEKLY
weekly_data_ttl = data %>%
  group_by(Year, Week) %>%
  summarise(Total_Metering = sum(`Total_metering(W/hr)`), .groups = "drop")
weekly_data_avg = data %>%
  group_by(Year, Week) %>%
  summarise(Mean_Metering = mean(`Total_metering(W/hr)`), .groups = "drop")
tmp_weekly_ttl <- ggplot(weekly_data_ttl, aes(x = Week, y = Total_Metering, color = as.factor(Year))) +
  geom_line() +
  scale_x_continuous(breaks = seq(0, 53, by = 10)) + 
  labs(title = "Total Energy Sub-metering per Week",
       x = "Week",
       y = "Watts per Hour",
       color = "Year") 
weekly_total_metering = ggplotly(tmp_weekly_ttl)
weekly_total_metering
tmp_weekly_avg <- ggplot(weekly_data_avg, aes(x = Week, y = Mean_Metering, color = as.factor(Year))) +
  geom_line() +
  scale_x_continuous(breaks = seq(0, 53, by = 10)) + 
  labs(title = "Total Energy Sub-metering per Week",
       x = "Week",
       y = "Watts per Hour",
       color = "Year") 
weekly_avg_metering = ggplotly(tmp_weekly_avg)
weekly_avg_metering
tmp_boxweek = ggplot(weekly_data_ttl, aes(as.factor(Week), Total_Metering, fill = as.factor(Week))) +
  geom_boxplot() +
  scale_x_discrete(breaks = seq(0, 53, by = 10)) +
  labs(title = "Box Plot of Weekly Total Energy Sub-metering",
       x = "Week",
       y = "Watts per Hour") +
  theme(legend.position = "none") 
weekly_box = ggplotly(tmp_boxweek)
weekly_box
tmp_boxweek_avg = ggplot(weekly_data_avg, aes(as.factor(Week), Mean_Metering, fill = as.factor(Week))) +
  geom_boxplot() +
  scale_x_discrete(breaks = seq(0, 53, by = 10)) +
  labs(title = "Box Plot of Weekly Mean Energy Sub-metering",
       x = "Week",
       y = "Watts per Hour") +
  theme(legend.position = "none") 
weekly_box_avg = ggplotly(tmp_boxweek_avg)
weekly_box_avg

sub_metering_data = data %>% 
  group_by(Year, Week) %>%
  summarise(Laundry_Room_avg = mean(`Laundry_Room(W/hr)`),
            Kitchen_avg = mean(`Kitchen(W/hr)`),
            `Electric_WaterHeater/Ac` = mean(`Electric_WaterHeater/AC(W/hr)`)
            ) %>%
  ungroup()
long_sub_metering = sub_metering_data %>%
  pivot_longer(cols = c(Laundry_Room_avg, Kitchen_avg, `Electric_WaterHeater/Ac`),
               names_to = "Category", 
               values_to = "Average_Watts")
facet_weekly_avg_energy_tmp = ggplot(long_sub_metering, aes(Week, Average_Watts, color = Category)) +
  geom_line() +
  facet_wrap(~ Year, scales = "fixed") +
  labs(title = "Average Energy Consumption by Category",
       x = "Week",
       y = "Average Watts per Hour",
       color = "Category")
facet_weekly_avg_engy = ggplotly(facet_weekly_avg_energy_tmp)
facet_weekly_avg_engy
avg_energy_weekly_tmp = ggplot(long_sub_metering, aes(Week, Average_Watts, color = Category))+
  geom_line() + 
  scale_x_continuous(breaks = seq(0, 53, by = 10)) + 
  labs(title = "Weekly Average Category Energy Consumption", 
       x = "Week", 
       y = "Watts per Hour")
avg_energy_year_round = ggplotly(avg_energy_weekly_tmp, tooltip = "text")
avg_energy_year_round


tmp_data <- data %>%
  mutate(Hour = hour(DateTime)) %>%
  group_by(Year, Hour) %>%
  summarise(Mean_Metering = mean(`Total_metering(W/hr)`))
hourly_use_tmp = ggplot(tmp_data, aes(x = Hour, y = Mean_Metering, color = as.factor(Year))) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = seq(0, 24, by = 4)) + 
  labs(title = "Average Energy Consumption by Hour of the Day",
       x = "Hour",
       y = "W/hr")
hourly_use = ggplotly(hourly_use_tmp)
hourly_use


### Correlation Matrix
numeric_data = data %>% 
  select(where(is.numeric))
corr_matrix = cor(numeric_data)
corrplot(corr_matrix, type = "lower", order = "AOE", tl.srt = 60, tl.col = "black", col = COL2("PRGn"))


