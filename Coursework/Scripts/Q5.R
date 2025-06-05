#Q5 yearly average for all the pollutants 


Year2018 <- read.csv("Data/Year2018.csv")
Year2019 <- read.csv("Data/Year2019.csv")
Year2020 <- read.csv("Data/Year2020.csv")
Year2021 <- read.csv("Data/Year2021.csv")
Year2022 <- read.csv("Data/Year2022.csv")
Year2023 <- read.csv("Data/Year2023.csv")


Q5_total_year <- bind_rows(Year2018, Year2019, Year2020, Year2021, Year2022, Year2023)


Q5_PM10 <- Q5_total_year %>% select(c(1:5))
Q5_NO <- Q5_total_year %>% select(c(1,2,6:8))
Q5_NO2 <- Q5_total_year %>% select(c(1,2,9:11))
Q5_NOx_NO2 <- Q5_total_year %>% select(c(1,2,12:14))


Q5_PM10_filtered <- Q5_PM10 %>% 
  mutate(PM10 = na.approx(PM10, maxgap = 2, na.rm =FALSE)) %>%
  filter(!is.na(PM10), PM10 >= 0)

Q5_NO_filtered <- NO %>% 
  mutate(Q5_Nitric_Oxide = na.approx(Nitric_Oxide, maxgap = 2, na.rm =FALSE)) %>%
  filter(!is.na(Nitric_Oxide), Nitric_Oxide >= 0)

Q5_NO2_filtered <- NO2 %>%
  mutate(Q5_Nitrogen_Dioxide = na.approx(Nitrogen_Dioxide, maxgap = 2, na.rm =FALSE)) %>%
  filter(!is.na(Nitrogen_Dioxide), Nitrogen_Dioxide >= 0)

Q5_NOx_NO2_filtered <- NOx_NO2 %>% 
  mutate(Q5_Nitrogen_Oxides_as_Nitrogen_Dioxide = na.approx(Nitrogen_Oxides_as_Nitrogen_Dioxide, maxgap = 2, na.rm =FALSE)) %>%       
  filter(!is.na(Nitrogen_Oxides_as_Nitrogen_Dioxide), Nitrogen_Oxides_as_Nitrogen_Dioxide >= 0)

#average
PM10_yearly_avg <- Q5_PM10_filtered %>% 
                  mutate(Year = year(dmy(Date))) %>% 
                    group_by(Year) %>% summarise(PM10 = mean(PM10))

NO_yearly_avg <- Q5_NO_filtered %>% 
                   mutate(Year = year(dmy(Date))) %>% 
                   group_by(Year) %>% summarise(NO = mean(Nitric_Oxide))


NO2_yearly_avg <- Q5_NO2_filtered %>% 
                    mutate(Year = year(dmy(Date))) %>% 
                    group_by(Year) %>% summarise(NO2 = mean(Nitrogen_Dioxide))

NOx_NO2_yearly_avg <- Q5_NOx_NO2_filtered %>% 
                       mutate(Year = year(dmy(Date))) %>% 
                       group_by(Year) %>% summarise(NOx_NO2 = mean(Nitrogen_Oxides_as_Nitrogen_Dioxide))

#combine
Q5_combined_averages <- PM10_yearly_avg %>%
  left_join(NO_yearly_avg, by = "Year") %>%
  left_join(NO2_yearly_avg, by = "Year") %>%
  left_join(NOx_NO2_yearly_avg, by = "Year")

Q5_combined_averages <- Q5_combined_averages %>%
                    pivot_longer(
                    cols = !Year, 
                    names_to = "Pollutants",
                    values_to = "Average")

write.csv(Q5_combined_averages, "Data/Q5_cleaned.csv", row.names = FALSE)

ggplot(Q5_combined_averages, aes(fill = Pollutants, x= Year, y = Average)) +
  geom_bar(position = "dodge", stat= "identity") +
  labs(title = "Yearly Average of Pollutants",
       y= "Average(ugm-3)") 

ggplotly()


# Pre CAZ there is more number of pollutants level than post CAZ. The yearly average of 
# 2018 and 2019 are higher than the yearly average of the year 2022 and 2023. During they year
# 2020 there was the lowest averages of the pollutants level. This could be assumed due to lock 
# down. After lock down there is a significant increase in the nitric oxide as nitrogen dioxide. 
# This gas is formed when oxygen in the atmosphere combines with the gases released by the vehicles.
# There is evidence  that the introduction of CZA has been positive in reducing the harmful gases.
# The two years 2022 and 2023 averages can be taken as sign that introduction of CZA is impacting
# the air quality positively.