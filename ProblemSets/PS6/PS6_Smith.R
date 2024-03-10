library(dplyr)
library(ggplot2)


#Question 3 data cleaning
library(fredr)
HVAC_PPI = fredr(series_id = "PCU3334133341")
head(HVAC_PPI)

#save as data frame
HVAC_PPI = data.frame(HVAC_PPI)

#save date column as a date
HVAC_PPI$date = as.Date(HVAC_PPI$date)

#renamed columns and eliminated redundant column of date when the data was pulled
HVAC_PPI = rename(HVAC_PPI, Date_Pulled = realtime_start)
HVAC_PPI = select(HVAC_PPI, -realtime_end)

#Look at the last 5 years of data only
PPI_last5 = filter(HVAC_PPI, date >=Sys.Date() - lubridate::years(5))
PPI_last5

#Added a percentage change column with respect to the older date value
PPI_last5$PctChange = (PPI_last5$value / PPI_last5$value[1] -1) * 100

#Add a column to indicate in the % change was positive or negative from the previous data point
PPI_last5$ChangeDirection <- c(0, diff(PPI_last5$value) < 0)

#created second data frame to show columns by year and quarter for future plot
library(lubridate)
PPI_last5Q = PPI_last5 %>%
  mutate(Quarter = quarter(date), Year = year(date)) %>%
  group_by(Year, Quarter) %>%
  summarise(Avg_PPI = mean(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(QoQ_Change = (Avg_PPI/lag(Avg_PPI) - 1) * 100)
PPI_last5Q

#add a time column, combining Year and Quarter
PPI_last5Q$Time = interaction(PPI_last5Q$Year,PPI_last5Q$Quarter, sep = "Q")
PPI_last5Q$Time = factor(PPI_last5Q$Time, levels = unique(PPI_last5Q$Time))


#Question 4
#Time Series plot
library(ggplot2)
Timeseries=ggplot(PPI_last5, aes(x = date, y=value)) +
  geom_line(colour="blue") +
  labs(x = "Date", y="HVAC PPI", title = "HVAC Producer Price Index Last 5 Years")
Timeseries
ggsave("PS6a_Smith.png", plot = Timeseries, width = 10, height =6, dpi = 300)

#Show the % change in value from 5 years ago
PctChange=ggplot(PPI_last5, aes(x = date, y=PctChange))+
  geom_line(colour = "green")+
  labs(x = "Date", y = "% Increase in HVAC PPI", title = "Percentage Increase in HVAC PPI over Last 5 Years")
PctChange
ggsave("PS6b_Smith.png",plot = PctChange, width = 10, height =6, dpi = 300)


#Show the % change values quarter to quarter to identify spikes

#plot quarter to quarter %change changes
Q2Q_change = ggplot(PPI_last5Q, aes(x = Time, y = QoQ_Change)) +
  geom_point(size = 2, colour = "hot pink") +
  labs(x = "Time (Quarter)", y = "% Change in HVAC PPI", title = "Quarter-to-Quarter % Changes in HVAC PPI") +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust =))
Q2Q_change
ggsave("PS6c_Smith.png",plot = Q2Q_change, width = 10, height =6, dpi = 300)

