
library(dplyr)
library(ggplot2)
library(stringr)

# Bar chart ----

TIMES <- c(2, 4, 3)
set.seed(4)
data <- tibble(group = c(rep(paste("Group", 1:3), times = TIMES)),
               question = LETTERS[1:sum(TIMES)],
               proportion = runif(sum(TIMES)))

openxlsx::write.xlsx(data, file = "data_cols.xlsx")

data %>% 
  ggplot(aes(x = question, y = proportion)) +
  geom_col() +
  facet_grid(~group, scales = "free_x", switch = "x", space = "free_x") +
  labs(title = "Bar chart | Multi-level category labels with facets") +
  theme(strip.placement = "outside", 
        strip.background = element_rect(fill = "white"), 
        axis.title = element_blank())


# Line chart ----

N <- 16
set.seed(4)
data <- tibble(date = seq(as.Date("2018-11-01"), by = "1 month", length.out = N),
               sales = 100 + rnorm(n = N, mean = 10, sd = 10))

data %>% 
  ggplot(aes(x = date, y = sales)) +
  geom_line() +
  scale_x_date(name = NULL,
               breaks = "1 month",
               minor_breaks = NULL, 
               expand = expand_scale(add = 15),
               labels = function(x) stringr::str_sub(strftime(x, format = "%B"), start = 1, end = 1)) +
  facet_grid(~lubridate::year(date), scales = "free_x", switch = "x", space = "free_x") +
  labs(title = "Line chart | Multi-level category labels with facets") +
  coord_cartesian(clip = "off") +
  theme(strip.placement = "outside", 
        strip.background = element_rect(fill = "white"), 
        # panel.spacing = unit(0, units = "cm"),
        strip.text = element_text(hjust = 0),
        axis.title = element_blank())




format_dates <- function(x) {
  
  years <- lubridate::year(x)
  months <- str_sub(str_to_upper(strftime(x, format = "%B")), start = 1, end = 1)
  
  if_else(is.na(lag(years)) | lag(years) != years, 
          true = paste(months, years, sep = "\n"), 
          false = months)
  
}

data %>% 
  ggplot(aes(x = date, y = sales)) +
  geom_line() +
  scale_x_date(name = NULL,
               breaks = "1 month",
               minor_breaks = NULL,
               expand = expand_scale(add = 15),
               labels = format_dates) +
  labs(title = "Line chart | Multi-level category labels") +
  theme(axis.text.x = element_text(hjust = 0))

openxlsx::write.xlsx(data, file = "data_line.xlsx")
