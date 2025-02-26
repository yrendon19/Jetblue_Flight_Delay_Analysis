install.packages("tidyverse") # install packages
library(tidyverse)
df <- read.csv("Jetblue_Delay_Cause_Nov2023-Nov2024.csv") #import data
str(df) # get to know your data
summary(df)
colSums(is.na(df)) # check for missing values
df <- df %>% drop_na() # removes rows with NA values
df <- df %>% mutate(date = as.Date(paste(year, month, "01", sep = "-"))) # convert month and year to a date format

summary(df %>% select(arr_del15, carrier_ct, weather_ct, nas_ct, security_ct, late_aircraft_ct)) # summary of key delay-related columns

df %>% summarise(
  Carrier = sum(carrier_ct),
  Weather = sum(weather_ct),
  NAS = sum(nas_ct),
  Security = sum(security_ct),
  Late_Aircraft = sum(late_aircraft_ct)
) %>% pivot_longer(everything(), names_to = "Cause", values_to = "Total_Delays") %>%
  arrange(desc(Total_Delays)) # calculate total delays by category

install.packages("ggplot2")
library(ggplot2)

ggplot(df, aes(x = date, y = arr_del15)) +
  geom_line(color = "blue") +
  labs(title = "Monthly Flight Delays", x = "Month", y = "Delayed Flights") # line chart of delays over time

ggplot(df %>% pivot_longer(cols = carrier_ct:late_aircraft_ct, names_to = "Cause", values_to = "Count"),
       aes(x = Cause, y = Count, fill = Cause)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Total Delays by Cause", x = "Delay Cause", y = "Total Count") # bar chart of total delays by cause