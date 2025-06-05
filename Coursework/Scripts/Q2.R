#Q2 nitric oxide for specific dates


Q2_NO <- read.csv("Data/NO.csv")

 
Q2_NO_filtered <- NO %>% 
      mutate(Nitric_Oxide = na.approx(Nitric_Oxide, na.rm = FALSE)) 

write.csv(Q2_NO_filtered, "Data/Q2_cleaned.csv", row.names = FALSE)

ggplot(Q2_NO_filtered, aes(x = Time, y = Nitric_Oxide, color = Date, group= Date,
                          text = paste("Date:", Date,
                                       "<br>Time:", Time,
                                       "<br>Nitric Oxide:", Nitric_Oxide))) +
                         geom_line(size = 0.5) +
                         geom_point( size = 1) +
                        labs(title = "Nitric Oxide Levels for selcted dates",
                        x= "Time/Hrs", y = "Nitric Oxide (ugm-3)") +
                        theme(axis.text.x = element_text(angle = 90)) 

ggplotly(tooltip = "text")


# Levels of nitric oxide were lowest on averages in the year 2020. This could be due to 
# the covid lockdown. There were two spikes on 10th November 2020. This could be due to 
# temporary lifting of the restriction.Post CAZ there was lower level of Nitric Oxide levels.
# But surprisingly on the day CAZ came into effect there was a highest peak at 64.83173.The trend
# does suggest that CAZ has been useful in reducing the level of nitric oxide around the Anglesea Road.