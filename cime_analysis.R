library(RSocrata); library(tidyr)
library(lubridate); library(dplyr); library(ggplot2)

# Read data, munge, and make graph
# Takes about 30 min to get the data from Socrata
crime <- read.socrata(
  "https://data.cityofchicago.org/resource/ijzp-q8t2.json?$where=Year between 2009 and 2023",
  CREDS 
)  %>%
  dplyr::mutate(year = as.numeric(year), date = as.Date(date)) %>%
  dplyr::filter(primary_type %in% c("BATTERY", "ASSAULT") & !is.na(date)) %>%
  dplyr::mutate(
    week = lubridate::week(date)
    , arrest = (arrest == "TRUE")
  ) %>%
  dplyr::group_by(week, year) %>%
  dplyr::summarise(
    arrest_rate = mean(arrest)
    , crimes = n()
    , arrests = sum(arrest)
    , date = first(date)
  ) %>%
  ggplot2::ggplot(aes(x = date, y = arrest_rate)) + geom_line() +
  geom_vline(xintercept = as.Date("2014-10-20"), linewidth = 2, linetype = "dashed") +
  geom_text(label = "Laquan murdered", x =  as.Date("2016-06-20"), y = .32) +
  geom_vline(xintercept = as.Date("2020-03-09"), linewidth = 2, linetype = "dashed") +
  geom_text(label = "COVID", x =  as.Date("2021-01-09"), y = .32) +
  theme_minimal() +
  labs(title = "Arrest rate for assault & battery"
       , subtitle = "Chicago, 2009 to June 2023"
       , caption = "Data from the City of Chicago Open Data Portal. Accessed 6/13/2023.") +
  scale_y_continuous(labels = scales::percent_format()) +
  xlab("Date") + ylab("Weekly arrest rate")

