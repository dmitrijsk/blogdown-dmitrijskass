
library(tidyverse)

# https://data1.csb.gov.lv/pxweb/en/iedz/iedz__dzimst/IDG060.px/
# IDG060. Dzīvi dzimušie laulībā un ārpus laulības pēc mātes vecuma
# IDG060. Live births by marital status and age of mother

idg060_orig <- read_delim("content/post/IDG060.csv", delim = ";", skip = 2, na = "...")
idg060_orig

idg060 <- idg060_orig %>% 
  gather(key, value, -1, -2) %>% 
  set_names(c("year", "marital_status_of_month", "age_category", "n"))

idg060 %>% 
  filter(marital_status_of_month != "Total") %>% 
  ggplot(aes(x = year, y = n, colour = age_category)) +
  geom_line()


# https://data1.csb.gov.lv/pxweb/en/iedz/iedz__dzimst/IDG070.px/
# IDG070. Dzīvi dzimušie pēc dzimuma un mātes vecuma
# IDG070. Live births by sex and age of mother

idg070_orig <- read_delim("content/post/IDG070.csv", delim = ";", skip = 2, na = "...")
idg070_orig

# https://data1.csb.gov.lv/pxweb/lv/iedz/iedz__dzimst/IDG080.px
# IDG080. Dzīvi dzimušie pēc mātes un tēva vecuma
# IDG080. Live births by age of parents

idg080_orig <- read_delim("content/post/IDG080.csv", delim = ";", skip = 2, na = "...")
idg080_orig
