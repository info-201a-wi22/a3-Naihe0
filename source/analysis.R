library(dplyr)
library(maps)
library(usdata)
library(ggplot2)
library(plotly)

data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# most recent state with highest ratio of incarceration to total population
state_high_incareration_ratio <- data %>% 
  select(year, state, total_pop, total_jail_pop, total_prison_pop) %>% 
  group_by(state) %>% 
  filter(year == max(year)) %>% 
  summarise(year = year, total_jail_pop = sum(total_jail_pop, na.rm = TRUE), total_pop = sum(total_pop, na.rm = TRUE), total_prison_pop = sum(total_prison_pop, na.rm = TRUE)) %>%
  mutate(ratio = (total_jail_pop + total_prison_pop) / total_pop) %>% unique()
state_high_incareration_ratio[which.max(state_high_incareration_ratio$ratio),]
# most recent ratio of total white population to total population 15to64
white_pop_ratio <- data %>%
  select(year, total_pop_15to64, white_pop_15to64) %>%
  filter(year == max(year)) %>%
  summarise(year = year, total_pop_15to64 = sum(total_pop_15to64, na.rm = TRUE), white_pop_15to64 = sum(white_pop_15to64, na.rm = TRUE)) %>%
  mutate(ratio = white_pop_15to64 / total_pop_15to64) %>% unique()

# most recent ratio of total white jail population to total jail population 15to64
white_jail_pop_ratio <- data %>%
  select(year, total_jail_pop, white_jail_pop) %>%
  filter(year == max(year)) %>%
  summarise(year = year, total_jail_pop = sum(total_jail_pop, na.rm = TRUE), white_jail_pop = sum(white_jail_pop, na.rm = TRUE)) %>%
  mutate(ratio = white_jail_pop / total_jail_pop) %>% unique()

# most recent ratio of total black population to total population 15to64
black_pop_ratio <- data %>%
  select(year, total_pop_15to64, black_pop_15to64) %>%
  filter(year == max(year)) %>%
  summarise(year = year, total_pop_15to64 = sum(total_pop_15to64, na.rm = TRUE), black_pop_15to64 = sum(black_pop_15to64, na.rm = TRUE)) %>%
  mutate(ratio = black_pop_15to64 / total_pop_15to64) %>% unique()

# most recent ratio of total black jail population to total jail population 15to64
black_jail_pop_ratio <- data %>%
  select(year, total_jail_pop, black_jail_pop) %>%
  filter(year == max(year)) %>%
  summarise(year = year, total_jail_pop = sum(total_jail_pop, na.rm = TRUE), black_jail_pop = sum(black_jail_pop, na.rm = TRUE)) %>%
  mutate(ratio = black_jail_pop / total_jail_pop) %>% unique()

# Trends over time chart
white_jail_pop_rate_region <- data %>% 
  select(year, white_jail_pop, total_pop, region) %>%
  filter(year >= 2001) %>%
  group_by(region, year) %>%
  summarise(year = year, region = region, white_jail_pop = sum(white_jail_pop, na.rm = TRUE), total_pop = sum(total_pop, na.rm = TRUE)) %>%
  mutate(ratio = white_jail_pop/total_pop) %>% unique()
trend_over_time <- plot_ly(
  data = white_jail_pop_rate_region,
  x = ~year,
  y = ~ratio,
  color = ~region,
  type = "bar"
) %>% layout(
  title = "White Jail Rate in Regions in since 2001",
  xaxis = list(title = "Year"),
  yaxis = list(title = "White Jail Rate")
)

# Variable comparison chart
black_white_jail_rate <- data %>%
  select(year, black_jail_pop, white_jail_pop, black_pop_15to64, white_pop_15to64) %>% 
  replace(is.na(.), 0) %>%
  filter(year > 1989) %>%
  group_by(year) %>%
  summarise(year = year, black_jail_pop = sum(black_jail_pop), white_jail_pop = sum(white_jail_pop), black_pop_15to64 = sum(black_pop_15to64), white_pop_15to64 = sum(white_pop_15to64)) %>% 
  unique() %>%
  mutate(black_jail_rate = black_jail_pop/black_pop_15to64, white_jail_rate = white_jail_pop/white_pop_15to64)

variable_comparison <- plot_ly(
  data = black_white_jail_rate,
  x = ~year,
  y = ~black_jail_rate,
  name = "Black",
  type = "scatter",
  mode = "line"
) %>% 
  add_trace(
    y = ~white_jail_rate,
    name = "White"
  ) %>% 
  layout(
    title = "Black vs. White Jail Rate since 1990",
    xaxis = list(title = "Year"),
    yaxis = list(title = "Jail Rate")
  )

# Map
state_black_white_jail_ratio <- data %>% 
  select(state, black_jail_pop, white_jail_pop) %>% 
  replace(is.na(.), 0) %>% 
  group_by(state) %>% 
  summarise(black_jail_pop = sum(black_jail_pop), white_jail_pop = sum(white_jail_pop)) %>% 
  mutate(rate = black_jail_pop/white_jail_pop) %>% 
  select(-black_jail_pop, -white_jail_pop) %>% 
  arrange(-rate) %>% 
  slice(-c(1))

state_shape <- map_data("state") %>%
  rename(state = region)

state_shape <- state_shape %>%
  mutate(state = state2abbr(state))

state_shape <- state_shape %>%
  left_join(state_black_white_jail_ratio, by = "state")

map <- ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = rate),
    color = "white",
    size = .1 
  ) +
  coord_map() + 
  scale_fill_continuous(low = "yellow", high = "Red") +
  labs(fill = "Ratio") +
  ggtitle("Black vs. White Jail Ratio") +
  theme_bw()

