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

today <- as_datetime("2019-06-14 00:00:00")
target_sleep_hr <- 8.5

fitbit_colours <- c("darkpurple" = "#1F1842", "lightpurple" = "#6C6986", "white" = "#FFFFFF")
fitbit_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (fitbit_colours)
  
  fitbit_colours[cols]
}

fitbit_cols
fitbit_cols("lightpurple")

fitbit_palettes <- list("main" = fitbit_cols("lightpurple", "white"))
  
fitbit_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- fitbit_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}  

fitbit_pal("main")(3)

iris %>% 
  ggplot(aes(x = Sepal.Length, y  = Sepal.Width, fill = Species)) +
  geom_col()+
  scale_fill_manual(values = fitbit_pal("main")(3))

# source: https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2
scale_fill_fitbit <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- fitbit_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("fitbit_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

# source: https://github.com/tidyverse/ggplot2/blob/master/R/theme-defaults.r
theme_fitbit <- function(base_size = 11, base_family = "",
                         base_line_size = base_size / 22,
                         base_rect_size = base_size / 22) {
  # Starts with theme_grey and then modify some parts
  theme_grey(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
      theme(
        plot.background = element_rect(fill = fitbit_cols("darkpurple")),
        panel.background = element_rect(fill = fitbit_cols("darkpurple")), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        title = element_text(colour = "white"),
        axis.text.y = element_text(colour = "white"), 
        axis.text.x = element_text(colour = if_else(wday(sleep_agg$end_date) == 1, "white", fitbit_cols("lightpurple"))), # https://stackoverflow.com/questions/38862303/customize-ggplot2-axis-labels-with-different-colors/38862452
        legend.position = "none", 
        complete = TRUE
      )
  }

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
  labs(title = "Sleep | Hours slept") +
  coord_cartesian(xlim = c(min(sleep_agg$end_time)-60*60*13, max(sleep_agg$end_time)), clip = "off") +
  annotate(geom = "label", 
           x = min(sleep_agg$end_time)-60*60*21.5, 
           y = target_sleep_hr, 
           size = 3.5,
           label = paste(target_sleep_hr, "hr"),
           label.r = unit(0.3, "lines"))

p

p_fitbit <- p +
  scale_fill_fitbit() + 
  theme(
    plot.background = element_rect(fill = fitbit_cols("darkpurple")),
    panel.background = element_rect(fill = fitbit_cols("darkpurple")), 
    panel.grid = element_blank(), 
    axis.ticks = element_blank(), 
    axis.title = element_blank(),
    title = element_text(colour = "white"),
    axis.text.y = element_text(colour = "white"), 
    axis.text.x = element_text(colour = if_else(wday(sleep_agg$end_date) == 1, "white", fitbit_cols("lightpurple"))), # https://stackoverflow.com/questions/38862303/customize-ggplot2-axis-labels-with-different-colors/38862452
    legend.position = "none")

p_fitbit <- p +
  scale_fill_fitbit() +
  theme_fitbit()
  
p_fitbit

# source: https://www.freeiconspng.com/img/13227
star_icon <- readPNG("white-star-icon-10.png")
g <- rasterGrob(star_icon, interpolate = TRUE)

sleep_target <- sleep_agg %>% 
  filter(target == TRUE)

half_day <- 60*60*12

# source: https://stackoverflow.com/questions/27637455/display-custom-image-as-geom-point
p_fitbit_with_stars <- p_fitbit +
  mapply(function(x, y) annotation_raster(star_icon, 
                                          xmin = x - half_day * 0.5, 
                                          xmax = x + half_day * 0.5, 
                                          ymin = y - 0.7, 
                                          ymax = y - 0.1), 
         sleep_target$end_time,
         sleep_target$hours_asleep)

p_fitbit_with_stars

ggsave(filename = "fitbit_plot.png", p_fitbit_with_stars, width = 6.5, height = 5.5, units = "in")
