library(tidyverse)
library(lubridate)
library(scales)

# Import given data.
df_orig <- readxl::read_xlsx("SWDchallenge_June19.xlsx", range = "B3:Z7")

# Import a file comments.
points_comments <- read_delim("2019-06-10-poinits-and-comments.csv", delim = ";")

# Tidy and combine tables.
df <- as_tibble(t(df_orig)) %>% 
  select(is_actual = V1, year = V2, month = V3, sales = V4) %>% 
  fill(is_actual, year, .direction = "down") %>% 
  filter(sales != "SALES") %>% 
  group_by(year) %>% 
  mutate(date = as.Date(str_c(year, row_number(), 1, sep = "-")),
         date_labels = if_else(row_number() == 1, paste(month, year, sep = "\n"), month),
         sales = as.numeric(sales)) %>% 
  ungroup() %>% 
  left_join(points_comments, by = "date") %>% 
  mutate(year = as.numeric(year),
         comment_labels = if_else(!is.na(comment), 
                                  true = paste(comment, dollar(sales, accuracy = 0.1, suffix = "B"), sep = "\n"), 
                                  false = NA_character_),
         sales_labels = if_else(date %in% points_comments$date & is.na(comment), 
                                true = dollar(sales, accuracy = 0.1, suffix = "B"), 
                                false = NA_character_))

# Brand colours.
cols <- c("blue" = "#006bb5", "yellow" = "#ffe81e")

# Extra fonts: https://stackoverflow.com/questions/34522732/changing-fonts-in-ggplot2
extrafont::loadfonts(device="win", quiet = TRUE) # Registers fonts so that they can be used in plots. Must be run once in each R session.

# linetype for actual/forecast - DONE
# points - DONE

p <- df %>% 
  ggplot(aes(x = date, y = sales, group = 1)) +
  geom_segment(data = df %>% filter(!is.na(comment)), 
               mapping = aes(x = date,
                             xend = date,
                             y = 0,
                             yend = 2.5),
               linetype = "dotted", 
               size = 0.4) +
  geom_text(aes(y = 2.8, label = comment_labels), na.rm = TRUE, family = "Raleway") +
  geom_text(aes(label = sales_labels), 
             na.rm = TRUE, 
             family = "Raleway", 
             vjust  = "bottom", 
             nudge_y = 0.2) +
  geom_line(data = df %>% filter(is_actual == "FORECAST"),
            linetype = "dashed",
            size = 1.1,
            colour = cols["blue"]) +
  geom_line(data = df %>% filter(lag(is_actual) %in% c(NA, "ACTUAL")),
            size = 1.1,
            colour = cols["blue"]) +
  geom_point(data = df %>% filter(date %in% points_comments$date), 
             shape = 21, 
             size = 4, 
             colour = "white",
             fill = cols["yellow"]) +
  scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
  scale_x_date(breaks = "1 month", 
               minor_breaks = NULL, 
               labels = c("", df$date_labels, ""), 
               expand = expand_scale(0, add = 15)) +
  labs(x = NULL, 
       y = NULL,
       title = "Market size over time",
       subtitle = "Sales in billions ($USD), monthly",
       caption = "Makeover in R by Dmitrijs Kass @dmitrijsk for #SWDChallenge") +
  theme_classic() +
  theme(axis.text.x.bottom = element_text(hjust = 0),
        axis.line.y = element_blank(),
        text = element_text(family = "Raleway"),
        plot.title = element_text(family = "Open Sans", face = "bold"),
        plot.subtitle = element_text(family = "Playfair Display", margin = margin(b = 3, unit = "lines")),
        plot.caption = element_text(hjust = 0, size = 7), 
        plot.margin = margin(0.5, 0.5, 0.5, 1, "lines")) + 
  coord_cartesian(ylim = c(0, 3), clip = "off") +
  annotate("point", x = max(df$date), y = 3.7, shape = 15, size = 3) +
  annotate("point", x = max(df$date) + 15, y = 3.85, shape = 19, size = 3)

p
ggimage::ggbackground(p, "trees-1915248_1920_greyscale2.jpg")
ggsave(filename = "ikea_split_labels2.png", height = 10, width = 10 * 1.78, units = "cm", dpi = 600)





# extrafont::font_import(pattern="PlayfairDisplay-Regular") # this not sure

# windowsFonts(sans="Raleway") # this helped???

loadfonts(device="postscript") # this not necessary
fonttable() # also helped - see column "FullName"
extrafont::font_import(pattern="PlayfairDisplay-Bold")
# helped: https://blog.revolutionanalytics.com/2012/09/how-to-use-your-favorite-fonts-in-r-charts.html

# Get colours from image: https://imagecolorpicker.com/

# Change non-data elements to match SWD's graph.

# axis names - DONE
# x-axis labels - DONE
# legend - DONE
# title - DONE
# caption - DONE
# comments - DONE

# Branding:

# What are default fonts in `theme_grey()`?
# Do I have fonts for IKEA branding?
# How to change default fonts to IKEA fonts?
# What are other style changes that are necessary to make the graph look like IKEA's own?
             


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

