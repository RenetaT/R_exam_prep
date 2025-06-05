library(tidyverse)
library(lubridate)
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(plotly)
library(zoo)


df2018 <- read.csv("https://raw.githubusercontent.com/RenetaT/R_exam_prep/refs/heads/main/Coursework/Data/POAR_2018.csv", skip = 4)
df2019 <- read.csv("https://raw.githubusercontent.com/RenetaT/R_exam_prep/refs/heads/main/Coursework/Data/POAR_2019.csv", skip = 4)
df2020 <- read.csv("https://raw.githubusercontent.com/RenetaT/R_exam_prep/refs/heads/main/Coursework/Data/POAR_2020.csv", skip = 4)
df2021 <- read.csv("https://raw.githubusercontent.com/RenetaT/R_exam_prep/refs/heads/main/Coursework/Data/POAR_2021.csv", skip = 4)
df2022 <- read.csv("https://raw.githubusercontent.com/RenetaT/R_exam_prep/refs/heads/main/Coursework/Data/POAR_2022.csv", skip = 4)
df2023 <- read.csv("https://raw.githubusercontent.com/RenetaT/R_exam_prep/refs/heads/main/Coursework/Data/POAR_2023.csv", skip = 4)


df_total <- bind_rows(df2018, df2019, df2020, df2021, df2022, df2023)


df_total <- df_total %>% slice(-c(1, 8762, 17523, 26308, 35069, 43830))


df_total <- df_total %>% 
              rename( 
                Time = time,
                PM10 = PM.sub.10..sub..particulate.matter..Hourly.measured.,
                Status = status,
                Unit = unit,
                Nitric_Oxide = Nitric.oxide,
                Status_1 = status.1,
                Unit_1 = unit.1,
                Nitrogen_Dioxide = Nitrogen.dioxide,
                Status_2 = status.2,
                Unit_2 = unit.2,
                Nitrogen_Oxides_as_Nitrogen_Dioxide = Nitrogen.oxides.as.nitrogen.dioxide,
                Status_3 = status.3,
                Unit_3 = unit.3,
                  )


df_total <- df_total %>% mutate(across(c(Status, Unit,  Status_1, Unit_1, Status_2, Unit_2,
                                         Status_3, Unit_3), ~na_if(., ""))) %>%
                          fill(Status, Unit,  Status_1, Unit_1, Status_2, Unit_2,
                     Status_3, Unit_3, .direction = "downup")

required_dates <- c("20-12-2018", "03-01-2019", "19-03-2020",
                    "26-03-2020", "29-06-2020", "10-11-2020",
                    "20-12-2020", "03-01-2021", "29-11-2021",
                    "25-07-2022", "24-07-2023")

df_required <-  df_total %>% filter(Date %in% required_dates)

PM10 <- df_required %>% select(Date, Time, PM10, Status, Unit)
write.csv(PM10, "Data/PM10.csv", row.names = FALSE)


NO <- df_required %>% select(Date, Time, Nitric_Oxide, Status_1, Unit_1)
write.csv(NO, "Data/NO.csv", row.names = FALSE)

NO2 <- df_required %>% select(Date, Time, Nitrogen_Dioxide, Status_2, Unit_2)
write.csv(NO2, "Data/NO2.csv", row.names = FALSE)

NOx_NO2 <- df_required %>% select(Date, Time, Nitrogen_Oxides_as_Nitrogen_Dioxide, Status_3, Unit_3)
write.csv(NOx_NO2, "Data/NOx_NO2.csv", row.names = FALSE)

Year2018 <- df_total %>% filter(year(dmy(Date)) == 2018)
write.csv(Year2018, "Data/Year2018.csv", row.names = FALSE)

Year2019 <- df_total %>% filter(year(dmy(Date)) == 2019)
write.csv(Year2019, "Data/Year2019.csv", row.names = FALSE)

Year2020 <- df_total %>% filter(year(dmy(Date)) == 2020)
write.csv(Year2020, "Data/Year2020.csv", row.names = FALSE)

Year2021 <- df_total %>% filter(year(dmy(Date)) == 2021)
write.csv(Year2021, "Data/Year2021.csv", row.names = FALSE)

Year2022 <- df_total %>% filter(year(dmy(Date)) == 2022)
write.csv(Year2022, "Data/Year2022.csv", row.names = FALSE)

Year2023 <- df_total %>% filter(year(dmy(Date)) == 2023)
write.csv(Year2023, "Data/Year2023.csv", row.names = FALSE)








