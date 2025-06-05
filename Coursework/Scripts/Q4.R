#Q4 Monthly averages for the year 2020


Year2020 <- read.csv("Data/Year2020.csv")

Q4_PM10 <- Year2020 %>% select(c(1:5))
Q4_NO <- Year2020 %>% select(c(1,2,6:8))
Q4_NO2 <- Year2020 %>% select(c(1,2,9:11))
Q4_NOx_NO2 <- Year2020 %>% select(c(1,2,12:14))


Q4_PM10_filtered <- Q4_PM10 %>% 
                  mutate(PM10 = na.approx(PM10, maxgap = 2, na.rm =FALSE)) %>%
                  filter(!is.na(PM10), PM10 >= 0)

Q4_NO_filtered <- Q4_NO %>% 
                mutate(Nitric_Oxide = na.approx(Nitric_Oxide, maxgap = 2, na.rm =FALSE)) %>%
                filter(!is.na(Nitric_Oxide), Nitric_Oxide >= 0)

Q4_NO2_filtered <- Q4_NO2 %>%
                  mutate(Nitrogen_Dioxide = na.approx(Nitrogen_Dioxide, maxgap = 2, na.rm =FALSE)) %>%
                  filter(!is.na(Nitrogen_Dioxide), Nitrogen_Dioxide >= 0)

Q4_NOx_NO2_filtered <- Q4_NOx_NO2 %>% 
                   mutate(Nitrogen_Oxides_as_Nitrogen_Dioxide = na.approx(Nitrogen_Oxides_as_Nitrogen_Dioxide, maxgap = 2, na.rm =FALSE)) %>%       
                filter(!is.na(Nitrogen_Oxides_as_Nitrogen_Dioxide), Nitrogen_Oxides_as_Nitrogen_Dioxide >= 0)


PM10_monthly_avg <- Q4_PM10_filtered %>% 
                    mutate(Month = month(Date, label = TRUE, abbr = FALSE)) %>% 
                    group_by(Month) %>% summarise(PM10 = mean(PM10))

NO_monthly_avg <- Q4_NO_filtered %>% 
                  mutate(Month = month(Date, label = TRUE, abbr = FALSE)) %>% 
                  group_by(Month) %>% summarise(NO = mean(Nitric_Oxide))

NO2_monthly_avg <- Q4_NO2_filtered %>% 
                  mutate(Month = month(Date, label = TRUE, abbr = FALSE)) %>%  
                  group_by(Month) %>% summarise(NO2 = mean(Nitrogen_Dioxide))

NOx_NO2_monthly_avg <- Q4_NOx_NO2_filtered %>% 
                        mutate(Month = month(Date, label = TRUE, abbr = FALSE)) %>% 
                        group_by(Month) %>% summarise(NOx_NO2 = mean(Nitrogen_Oxides_as_Nitrogen_Dioxide))


Q4_combined_averages <- PM10_monthly_avg %>%
  left_join(NO_monthly_avg, by = "Month") %>%
  left_join(NO2_monthly_avg, by = "Month") %>%
  left_join(NOx_NO2_monthly_avg, by = "Month")


Q4_combined_averages <- Q4_combined_averages %>%
                      pivot_longer(
                      cols = !Month, 
                      names_to = "Pollutants",
                       values_to = "Average")

write.csv(Q4_combined_averages, "Data/Q4_cleaned.csv", row.names = FALSE)

ggplot(Q4_combined_averages, aes(fill = Pollutants, x= Month, y = Average)) +
  geom_bar(position = "dodge", stat= "identity") +
  labs(title = "Monthly Average of Pollutants",
       y = "Average ug/m³",
       x = "Months in year 2020")

ggplotly()


# Looking at the grouped bar graph, we can see that Nitrogen oxides as nitrogen dioxide is 
# by far the largest pollutant as it is in most cases throughout the year. And Nitrogen oxide
# seems to be the least. The highest level was during the month of January. During the middle 
# of the year the averages were the lowest, might be due to the covid restriction. But afterwards 
# there is increase in levels amongst all the pollutant. This also suggests that there are other
# factors beside vehicles that can lead to these pollutants level being present, as this was 
# during the lock down.