library(tidyverse)
library(lubridate)

df_orig <- readxl::read_xlsx("SWDchallenge_June19.xlsx", range = "B3:Z7")

cols <- c("iblue" = "#006bb5",
          "iyell" = "#ffe81e")

# https://stackoverflow.com/questions/34522732/changing-fonts-in-ggplot2
extrafont::loadfonts(device="win") # Registers fonts so that they can be used in plots. Must be run once in each R session.

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

p <- df_with_com %>% 
  ggplot(aes(x = date, y = sales, group = 1)) +
  geom_segment(data = df_with_com %>% filter(!is.na(comment)), 
               mapping = aes(x = date,
                             xend = date,
                             y = 0,
                             yend = 2.5),
               linetype = "dashed", 
               size = 0.5) +
  geom_text(aes(y = 2.6, label = comment), na.rm = TRUE, family = "Raleway") +
  annotate("text", 
           label = "Forecast",
           family = "Raleway",
           x = mean(df_with_com[df_with_com$is_actual == "FORECAST", ]$date), 
           y = 2.6) +
  annotate("rect", 
           xmin = min(df_with_com[df_with_com$is_actual == "FORECAST", ]$date), 
           xmax = max(df_with_com[df_with_com$is_actual == "FORECAST", ]$date), 
           ymin = 0, 
           ymax = 3, 
           fill = cols["iyell"],
           alpha = .2) +
  # geom_line(data = df_with_com %>% filter(is_actual == "FORECAST"), 
  #           linetype = "dashed", 
  #           size = 1) +
  # geom_line(data = df_with_com %>% filter(lag(is_actual) %in% c(NA, "ACTUAL")), 
  #           size = 1,
  #           colour = cols["iblue"]) +
  geom_line(size = 1,
            colour = cols["iblue"]) +
  geom_point(data = df_with_com %>% filter(date %in% points_comments$date), 
             shape = 21, 
             size = 4, 
             colour = "white",
             fill = cols["iblue"]) +
  scale_y_continuous(limits = c(0, 3),
                     breaks = seq(0, 3, by = 0.5),
                     labels = function(x) scales::dollar(x, accuracy = 0.1), 
                     expand = c(0,0)) +
  scale_x_date(breaks = "1 month", 
               minor_breaks = NULL, 
               labels = c("", df_with_com$date_labels, ""), 
               expand = expand_scale(0, add = 15)) +
  labs(x = NULL, 
       y = NULL,
       title = "Market size over time",
       subtitle = "Sales in billions ($USD), monthy",
       caption = "Makeover in R by Dmitrijs Kass @dmitrijsk for #SWDChallenge") +
  theme_classic() +
  theme(axis.text.x.bottom = element_text(hjust = 0),
        text = element_text(family = "Raleway"),
        plot.title = element_text(family = "Open Sans", face = "bold"),
        plot.subtitle = element_text(family = "Playfair Display"),
        plot.caption = element_text(hjust = 0), 
        plot.background = element_rect(fill = "#D7C6A8"), 
        panel.background = element_rect(fill = "#D7C6A8"))

p
ggimage::ggbackground(p, "trees-1915248_1920_greyscale2.jpg")
warnings()

ggsave(filename = "ikea_trees-greyscale.png", height = 10, width = 10 * 1.78, units = "cm", dpi = 600)





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
# caption - 
# comments - 

p2_default_theme <- p1_basic + 
  

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

ggsave(filename = "ikea.png", height = 10, width = 10 * 1.78, units = "cm", dpi = 600)


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


p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
p + annotate("text", x = 4, y = 25, label = "Some text")
p + annotate("text", x = 2:5, y = 25, label = "Some text")
p + annotate("rect", xmin = 3, xmax = 4.2, ymin = 12, ymax = 21,
             alpha = .2)
p + annotate("segment", x = 2.5, xend = 4, y = 15, yend = 25,
             colour = "blue")
p + annotate("pointrange", x = 3.5, y = 20, ymin = 12, ymax = 28,
             colour = "red", size = 1.5)



# fonts: https://stackoverflow.com/questions/34522732/changing-fonts-in-ggplot2
library(extrafont)
choose_font(c("BemeboStd", "Garamond", "serif", "Arial Black", "Open Sans"), quiet = TRUE)
font_import()
loadfonts(device = "win")
