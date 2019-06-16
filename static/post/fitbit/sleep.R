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

today <- as_datetime("2019-06-16 00:00:00")
target_sleep_hr <- 8.5
cols <- c("#6C6986", "white")

sleep_agg <- sleep %>% 
  mutate(end_date = as.Date(dmy_hm(end_time)),
         hours_asleep = minutes_asleep / 60) %>% 
  select(-start_time, -end_time) %>% 
  group_by(end_date) %>% 
  summarise_all(sum, na.rm = TRUE) %>% 
  mutate(target = hours_asleep >= target_sleep_hr,
         end_time = as_datetime(end_date))

p <- sleep_agg %>% 
  ggplot(aes(x = end_time, 
             y = hours_asleep, 
             fill = end_time == today)) +
  geom_col() +
  geom_hline(yintercept = target_sleep_hr, colour = "white") +
  scale_x_datetime(breaks = "1 day", 
                   labels = function(x) if_else(x == today, "TODAY", format(x, "%e")), 
                   expand = expand_scale(add = 60*60*10)) +
  scale_y_continuous(labels = function(x) round(x, digits = 0), 
                     breaks = 0:(ceiling(max(sleep_agg$hours_asleep)))) +
  labs(title = "Sleep | Hours slept", 
       x = NULL, 
       y = NULL) +
  scale_fill_manual(values = cols) +
  theme(plot.background = element_rect(fill = "#1F1842"), 
        panel.background = element_rect(fill = "#1F1842"), 
        panel.grid = element_blank(), 
        text = element_text(colour = "white"),
        axis.text.y = element_text(colour = "white"), 
        axis.text.x = element_text(colour = if_else(wday(sleep_agg$end_date) == 1, "white", "#6C6986")), # https://stackoverflow.com/questions/38862303/customize-ggplot2-axis-labels-with-different-colors/38862452
        legend.position = "none") +
  coord_cartesian(xlim = c(min(sleep_agg$end_time)-60*60*13, max(sleep_agg$end_time)), clip = "off") +
  annotate(geom = "label", 
           x = min(sleep_agg$end_time)-60*60*21.5, 
           y = target_sleep_hr, 
           size = 3.5,
           label = paste(target_sleep_hr, "hr"),
           label.r = unit(0.3, "lines"))
  
p

# source: https://www.freeiconspng.com/img/13227
star_icon <- readPNG("white-star-icon-10.png")
g <- rasterGrob(star_icon, interpolate = TRUE)

sleep_target <- sleep_agg %>% 
  filter(target == TRUE)

one_day <- 60 * 60 * 7

p_with_stars <- p
for (i in 1:nrow(sleep_target)) {
  p_with_stars <- p_with_stars + 
    annotation_custom(grob = g, 
                      xmin = sleep_target$end_time[i] - one_day,
                      xmax = sleep_target$end_time[i] + one_day, 
                      ymin = sleep_target$hours_asleep[i] - 1.1, 
                      ymax = sleep_target$hours_asleep[i] - 0.1)  
}

p_with_stars


# expand_limits(x = as.Date("2019-06-01")) +

# target_tib <- tibble(end_time = min(sleep_agg$end_time) - 1, 
#                      hours_asleep = 8.8)

