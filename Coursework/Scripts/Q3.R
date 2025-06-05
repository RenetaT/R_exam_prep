#Q3 nitrogen oxides as nitrogen dioxide for specific date

Q3_NOx_NO2 <- read.csv("Data/NOx_NO2.csv")


Q3_NOx_NO2_filtered <- Q3_NOx_NO2 %>% 
                    mutate(Nitrogen_Oxides_as_Nitrogen_Dioxide = na.approx(Nitrogen_Oxides_as_Nitrogen_Dioxide, na.rm = FALSE)) 

write.csv(Q3_NOx_NO2_filtered, "Data/Q3_cleaned.csv", row.names = FALSE)

ggplot(Q3_NOx_NO2_filtered, aes(x = Time, y = Nitrogen_Oxides_as_Nitrogen_Dioxide, color = Date, group= Date,
                        text = paste("Date:", Date,
                                     "<br>Time:", Time,
                                     "<br>Nitrogen Oxides as Nitrogen_Dioxide:", Nitrogen_Oxides_as_Nitrogen_Dioxide))) +
                         geom_line(size = 0.5) +
                         geom_point( size = 1) +
                         labs(title = "Nitrogen Oxides as Nitrogen Dioxide Levels for selected dates",
                         x= "Time/Hrs", y = "Nitrogen Oxides as Nitrogen Dioxide (ugm-3)") +
                         theme(axis.text.x = element_text(angle = 90)) 

ggplotly(tooltip = "text")


# Very high peaks of 172.72095 on 29 November 2021 during the introduction of CZA.Looking at the 
# 2019 and 2019 lines, and comparing them to the 2022 and 2023 suggests that CZA has been helpful
# in reducing the level of this pollutants. The lowest was around the 2020 which could have been
# contributed by the covid lockdown. Additionally there was a spike during the covid lockdown. This 
# could suggest that there was a lifting of restriction which lead to more vehicles being used, 
# contributing to the spike.