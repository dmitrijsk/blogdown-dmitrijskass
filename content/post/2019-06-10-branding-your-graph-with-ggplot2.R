library(tidyverse)
library(lubridate)

df_orig <- readxl::read_xlsx("SWDchallenge_June19.xlsx", range = "B3:Z7")

cols <- c("iblue" = "#006bb5",
          "iyell" = "#ffe81e")

months <- str_sub(month.abb, start = 1, end = 1)

df <- as_tibble(t(df_orig)) %>% 
  select(is_actual = V1, year = V2, month = V3, sales = V4) %>% 
  fill(is_actual, year, .direction = "down") %>% 
  filter(sales != "SALES") %>% 
  group_by(year) %>% 
  mutate(date = as.Date(str_c(year, row_number(), 1, sep = "-")),
         date_labels = if_else(row_number() == 1, paste(month, year, sep = "\n"), month),
         sales = as.numeric(sales)) %>% 
  ungroup() %>% 
  mutate(year = as.numeric(year))
  
df

points_comments <- read_delim("2019-06-10-poinits-and-comments.csv", delim = ";")

df_with_com <- df %>% 
  left_join(points_comments, by = "date")

# linetype for actual/forecast - DONE
# points - DONE

p1_basic <- df_with_com %>% 
  ggplot(aes(x = date, y = sales, group = 1)) +
  geom_line(data = df_with_com %>% filter(is_actual == "FORECAST"), 
            linetype = "dashed", 
            size = 1) +
  geom_line(data = df_with_com %>% filter(lag(is_actual) %in% c(NA, "ACTUAL")), 
            size = 1,
            colour = cols["iblue"]) +
  geom_point(data = df_with_com %>% filter(date %in% points_comments$date), 
             shape = 21, 
             size = 4, 
             colour = "white",
             fill = "#165386") +
  geom_vline(xintercept = df_with_com %>% filter(!is.na(comment)) %>% pull(date), 
             linetype = "dotted", size = 1)

p1_basic

# Get colours from image: https://imagecolorpicker.com/

# Change non-data elements to match SWD's graph.

# axis names - DONE
# x-axis labels - DONE
# legend - DONE
# title - DONE
# caption - 
# comments - 

p2_default_theme <- p1_basic + 
  scale_y_continuous(limits = c(0, 3),
                     breaks = seq(0, 3, by = 0.5),
                     labels = function(x) scales::dollar(x, accuracy = 0.1)) +
  scale_x_date(breaks = "1 month", 
               minor_breaks = NULL, 
               labels = c("", df_with_com$date_labels, ""), 
               expand = expand_scale(0, add = 15)) +
  labs(x = NULL, 
       y = "SALES ($USD BILLIONS)",
       title = "Market size over time",
       caption = "Makeover in R by Dmitrijs Kass @dmitrijsk for #SWDChallenge")

p2_default_theme

# Branding:

# What are default fonts in `theme_grey()`?
# Do I have fonts for IKEA branding?
# How to change default fonts to IKEA fonts?
# What are other style changes that are necessary to make the graph look like IKEA's own?
             


p3_branded <- p2_default_theme +
  theme_classic() +
  theme(axis.text.x.bottom = element_text(hjust = 0))

p3_branded

# https://guangchuangyu.github.io/2018/04/setting-ggplot2-background-with-ggbackground/
# ggimage::ggbackground(p3_branded, "background-hardwood-smooth-301717.jpg")

# add logo
# https://cran.r-project.org/web/packages/magick/vignettes/intro.html


# super-categity for x-axis
# https://stackoverflow.com/questions/48552671/ggplot2-show-category-and-sub-category-for-x-axis-labels
# https://stackoverflow.com/questions/44247239/grouping-on-the-x-axis-in-ggplot2
# https://stackoverflow.com/questions/20571306/multi-row-x-axis-labels-in-ggplot-line-chart
# https://stackoverflow.com/questions/44616530/axis-labels-on-two-lines-with-nested-x-variables-year-below-months/44616739
# https://github.com/tidyverse/ggplot2/issues/1966


