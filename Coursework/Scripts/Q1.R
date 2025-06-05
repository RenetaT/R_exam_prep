#PM10 for specific dates.


Q1_PM10 <- read.csv("Data/PM10.csv")

Q1_PM10_filtered <- PM10 %>% 
                  mutate(PM10 = na.approx(PM10, maxgap = 2, na.rm =FALSE)) %>%
                  filter(!is.na(PM10))

write.csv(Q1_PM10_filtered, "Data/Q1_cleaned.csv", row.names = FALSE)


ggplot(Q1_PM10_filtered, aes(x = Time, y = PM10, color = Date, group= Date,
                          text = paste("Date:", Date,
                                       "<br>Time:", Time,
                                        "<br>PM10:", PM10))) +
                    geom_line(size = 0.5) +
                    geom_point( size = 1) +
                    labs(title = "PM10 Levels for selcted dates",
                         x= "Time/Hrs", y= "PM10 (ugm-3/Ref.eq)")  +
                    theme(axis.text.x = element_text(angle = 90)) 

ggplotly(tooltip = "text")

# Key Findings

# This chart shows that the PM10 levels in the selected dates show that the there is 
# less levels of PM10 pollutant levels after the introduction of the CAZ, but not drastically.
# There is less levels after the introduction of CAZ, and it's significantly lower than in the
# year 2020. This could suggest that vehicle itself might not be the biggest contributor of PM10 
# pollutants because this was during the covid lockdown looking at the timeline.
# The lowest level was 2.899 on the 20 December 2018 around 12pm, while the highest was 
# on the 26 March 2020 at 10am, value 43.347. 