library(tidyverse)
library(geofacet)
library(ggthemes)
options(scipen = 99)

us_state_grid4 <- data.frame(
  row = c(1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 8, 8, 8),
  col = c(11, 1, 11, 10, 6, 2, 6, 10, 7, 5, 3, 9, 4, 1, 10, 6, 5, 2, 9, 7, 1, 8, 11, 4, 3, 1, 3, 10, 6, 9, 5, 4, 2, 8, 7, 2, 5, 4, 3, 7, 8, 6, 9, 7, 8, 5, 6, 4, 9, 1, 4),
  code = c("ME", "AK", "NH", "VT", "WI", "ID", "IL", "MA", "MI", "MN", "MT", "NY", "ND", "WA", "CT", "IN", "IA", "NV", "NJ", "OH", "OR", "PA", "RI", "SD", "WY", "CA", "CO", "DE", "KY", "MD", "MO", "NE", "UT", "VA", "WV", "AZ", "AR", "KS", "NM", "NC", "SC", "TN", "DC", "AL", "GA", "LA", "MS", "OK", "FL", "HI", "TX"),
  name = c("Maine", "Alaska", "New Hampshire", "Vermont", "Wisconsin", "Idaho", "Illinois", "Massachusetts", "Michigan", "Minnesota", "Montana", "New York", "North Dakota", "Washington", "Connecticut", "Indiana", "Iowa", "Nevada", "New Jersey", "Ohio", "Oregon", "Pennsylvania", "Rhode Island", "South Dakota", "Wyoming", "California", "Colorado", "Delaware", "Kentucky", "Maryland", "Missouri", "Nebraska", "Utah", "Virginia", "West Virginia", "Arizona", "Arkansas", "Kansas", "New Mexico", "North Carolina", "South Carolina", "Tennessee", "District of Columbia", "Alabama", "Georgia", "Louisiana", "Mississippi", "Oklahoma", "Florida", "Hawaii", "Texas"),
  stringsAsFactors = FALSE
)

state_rates <- read.csv("https://gist.githubusercontent.com/kanishkamisra/9f7677a7ec05984d060260066eb02d53/raw/64da90856d3ab4f623bbbfdcf94e81b517baefd1/state_mortality")
usa_rates <- read.csv("https://gist.githubusercontent.com/kanishkamisra/a2f49ec4c037751dad94fe8a58dff691/raw/810edc2bc06e4a9a24ec80d5ab729935cee7a9d1/usa_rate.csv")

usa_joining <- usa_rates %>%
  transmute(
    year_id,
    usa_rate = mx,
    usa_avg = mx
  )

usa_vs_state <- state_rates %>%
  transmute(
    location_name,
    year_id,
    state_rate = mx,
    state_avg = mx
  ) %>%
  inner_join(
    usa_joining,
    by = c("year_id")
  ) %>%
  # Little trick to get the geom_ribbon fill color to be the color of the 
  # higher rate (USA avg or state avg) in any given year.
  # That is why I have two columns for the same rate value. 
  # One is used to gather and plot (ymin in the ribbon) 
  # the other is to specify the ribbon ymax value.
  # Might not be the most elegant way to do it in a larger dataset.
  gather(state_rate, usa_rate, key = "metric", value = "value") %>%
  separate(metric,into = c("metric", "extra")) %>%
  select(-extra) %>%
  mutate(
    ribbon_color = case_when(
      state_avg > usa_avg ~ "#f8766d",
      usa_avg > state_avg ~ "#00bfc4"
    ),
    ribbon_value = case_when(
      state_avg > usa_avg ~ state_avg,
      usa_avg > state_avg ~ usa_avg
    )
  )

usa_vs_state %>%
  ggplot(aes(year_id, value, color = metric)) +
  geom_line(size = 1) + 
  geom_ribbon(aes(ymin = value, ymax = ribbon_value, linetype = NA, fill = ribbon_color), alpha = 0.2, show.legend = F) +
  facet_geo(~location_name, grid = us_state_grid4) +
  scale_fill_identity() +
  theme_fivethirtyeight() + 
  theme(legend.position = "top",
        legend.margin = margin(b = -1, unit = "cm")) + 
  labs(
    x = "Year",
    y = "Mortality Rate",
    title = "Mortality Rates in each state vs U.S. average, 1980-2014",
    subtitle = "Deaths per 100,000 people",
    color = "Rate",
    caption = "By Kanishka Misra    Source: IHME"
  )

