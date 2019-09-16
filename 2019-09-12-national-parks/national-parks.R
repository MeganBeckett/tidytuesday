# R-LADIES CAPE TOWN --------------------------------------------------------------------------
# TidyTuesday meetup
# Starter script - Megan Beckett


# LOAD LIBRARIES ------------------------------------------------------------------------------
library(tidyverse)

# Install "scales" package if required - scale functions for visualisation
if(!require(scales)){
  install.packages("scales")
}
library(scales)

# DATA ----------------------------------------------------------------------------------------
data_raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/national_parks.csv")

# Have a quick look at the data
summary(data_raw)
head(data_raw)


# TIDY DATA -----------------------------------------------------------------------------------
data_year <- data_raw %>%
  # Clean column names, if necessary
  janitor::clean_names() %>%
  # Remove observations which have "Total" as the year value
  # These can be calculated later rather, if needed
  filter(year != "Total") %>%
  # Convert year to numerical value and add in date type
  mutate(year = as.numeric(year),
         date = lubridate::ymd(paste(year, 1, 1, sep = "-"))) %>%
  select(year, date, gnis_id:visitors)

# Create a df of the "Totals" for each park
data_total <- data_raw %>%
  janitor::clean_names() %>%
  filter(year == "Total")

summary(data_year)
str(data_year)

# Note: It seems that "unit_name" is more reliable than "park_name" as there are less NAs in "unit_name"


# VISUALISE -----------------------------------------------------------------------------------
# Let's recreate some of the visualisations in the article:
# https://fivethirtyeight.com/features/the-national-parks-have-never-been-more-popular/

# "Annual recreational visits to national parks since 1904"
# Using ALL parks that reported visitation across all years
data_summary <- data_year %>%
  group_by(year) %>%
  summarise(total_visitors_mil = sum(visitors)/10^6)

# 1. Using base R ----------------------------------------------------------------------------
plot(data_summary$year, data_summary$total_visitors_mil, type = "l")

# 2. Using ggplot ----------------------------------------------------------------------------
# Base plot
g <- ggplot(data_summary, aes(x = year, y = total_visitors_mil)) +
  geom_line(colour = "#396D39")
g

# Styling and aesthetics
g <- g +
  geom_area(stat = "identity", fill = "#396D39", alpha = 0.4) +
  scale_x_continuous(breaks = seq(1910, 2010, 10)) +
  scale_y_continuous(breaks = seq(0, 300, 60),
                     labels = scales::unit_format(unit = "M")) +
  labs(title = "U.S. national parks have never been so popular",
       subtitle = "Annual recreational visits to national parks since 1904",
       x = "", y = "") +
  theme(plot.title = element_text(size = 16),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#D4D4D4"),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#F0F0F0"))
g


# 3. Using plotly -----------------------------------------------------------------------------
# Install "plotly" package if required - interactive, web-based graphs
if(!require(plotly)){
  install.packages("plotly")
}
library(plotly)

# Interactive plot with hover text
p <- plot_ly(data_summary,
             x = ~year,
             y = ~total_visitors_mil,
             type = "scatter",
             mode = "lines",
             fill = "tozeroy",
             line = list(color = "#396D39"),
             fillcolor = list(color = "#396D39", alpha = 0.5),
             text = ~paste(year, "<br>", round(total_visitors_mil), "million visitors"),
             hoverinfo = "text") %>%
  layout(title = "U.S. national parks have never been so popular<br>Annual recreational visits to national parks since 1904",
         xaxis = list(title = ""),
         yaxis = list(title = "Millions of visitors"))

p
