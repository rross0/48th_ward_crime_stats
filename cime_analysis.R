library(RSocrata); library(tidyr)
library(lubridate); library(dplyr); library(ggplot2)

# Read data, munge, and make graph
# Takes about 30 min to get the data from Socrata
crime <- read.socrata(
  paste0(
    "https://data.cityofchicago.org/resource/ijzp-q8t2.json?$",
    "where=Ward = '48'")
)  

gd1 <- crime %>%
  dplyr::mutate(
    year = as.numeric(year)
    , date = as.Date(date)
    , week = lubridate::week(date)
    , arrest = (arrest == "TRUE")
    , domestic = (domestic== "TRUE")
  ) %>%
  # dplyr::filter(primary_type == "MOTOR VEHICLE THEFT") %>%
  dplyr::group_by(week, year) %>%
  dplyr::summarise(
    arrest_rate = mean(arrest)
    , crimes = n()
    , arrests = sum(arrest)
    , date = first(date)
    , domestic = sum(domestic)
  ) %>%
  dplyr::filter(year >= 2021 & arrest_rate <= .75 & crimes >= 10) 

g1 <- gd1 %>%
  ggplot2::ggplot(aes(x = date, y = crimes)) + 
  geom_line( linewidth = 1, linetype = "solid") +
  geom_smooth() +
  labs(title = "Number of crimes in the 48th ward") +
  theme_minimal() +
  xlab("Date (week)") +
  ylab("Crimes") +
  scale_y_continuous(labels = scales::comma_format())
  

g2 <- gd1 %>%
  ggplot2::ggplot(aes(x = date, y = arrest_rate)) +   
  geom_line( linewidth = 1, linetype = "solid") +
  geom_smooth() +
  labs(title = "Arrest rate in the 48th ward"
       , caption = "Data from the City of Chicago Open Data Portal") +
  theme_minimal() +
  xlab("Date (week)") +
  ylab("Arrest rate") +
  scale_y_continuous(labels = scales::percent_format())

g3 <- ggarrange(
  g1, g2, ncol = 2
)

# Save the Plot
jpeg(file=here::here("48th_ward_crime_stats", "plot1.jpeg"), width = 980, height = 760)
g3
dev.off()

rm(g1, g2, g3, crime, gd1)
