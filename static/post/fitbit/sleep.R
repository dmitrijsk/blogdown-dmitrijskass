library(tidyverse)
library(lubridate)
library(png)
library(grid)

Sys.setlocale(category = "LC_ALL", locale = "english")

sleep <- read_delim("sleep.csv", delim = ",", skip = 1, na = "N/A")
sleep

names(sleep) <- names(sleep) %>% 
  str_to_lower() %>% 
  str_replace_all(pattern = " ", replacement = "_")

today <- as.Date("2019-06-16")
target_sleep_hr <- 8
fitbit_cols <- c("#6C6986", "white")

sleep_agg <- sleep %>% 
  select(-start_time) %>% 
  mutate(end_time = as.Date(dmy_hm(end_time)), 
         hours_asleep = minutes_asleep / 60) %>% 
  group_by(end_time) %>% 
  summarise_all(sum, na.rm = TRUE) %>% 
  mutate(target = hours_asleep >= target_sleep_hr)

p <- sleep_agg %>% 
  ggplot(aes(x = end_time, 
             y = hours_asleep, 
             fill = end_time == today)) +
  geom_col() +
  
  geom_hline(yintercept = target_sleep_hr, colour = "white") +
  annotate(geom = "label", 
           x = min(sleep_agg$end_time) - 1, 
           y = target_sleep_hr, 
           label = paste(target_sleep_hr, "hr"),
           label.r = unit(0.3, "lines")) +
  scale_x_date(breaks = "1 day", 
               labels = function(x) if_else(x == today, "TODAY", format(x, "%e"))) +
  scale_y_continuous(labels = function(x) round(x, digits = 0), 
                     breaks = 0:(ceiling(max(sleep_agg$hours_asleep)))) +
  labs(title = "Sleep | Hours slept", 
       x = NULL, 
       y = NULL) +
  scale_fill_manual(values = fitbit_cols) +
  theme(plot.background = element_rect(fill = "#1F1842"), 
        panel.background = element_rect(fill = "#1F1842"), 
        panel.grid = element_blank(), 
        text = element_text(colour = "white"), 
        axis.text.y = element_text(colour = "white"), 
        axis.text.x = element_text(colour = "#6C6986"), 
        legend.position = "none")

p


# source: https://www.freeiconspng.com/img/13227
star_icon <- readPNG("white-star-icon-10.png")
g <- rasterGrob(star_icon, interpolate = TRUE)

sleep_target <- sleep_agg %>% 
  filter(target == TRUE)

p + 
  annotation_custom(grob = g, 
                    xmin = as.Date("2019-06-04"), 
                    xmax = as.Date("2019-06-05"), 
                    ymin = 9.6, 
                    ymax = 10.6)

# expand_limits(x = as.Date("2019-06-01")) +

# target_tib <- tibble(end_time = min(sleep_agg$end_time) - 1, 
#                      hours_asleep = 8.8)

