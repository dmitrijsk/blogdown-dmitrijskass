
library(tidyverse)

# https://data1.csb.gov.lv/pxweb/en/iedz/iedz__dzimst/IDG060.px/
# IDG060. Dzīvi dzimušie laulībā un ārpus laulības pēc mātes vecuma
# IDG060. Live births by marital status and age of mother

idg060_orig <- read_delim("content/post/IDG060.csv", delim = ";", skip = 2, na = "...")
idg060_orig

idg060 <- idg060_orig %>% 
  gather(key, value, -1, -2) %>% 
  set_names(c("year", "marital_status_of_mother", "age_category", "n"))

idg060 %>% 
  filter(age_category != "Total") %>% 
  ggplot(aes(x = year, y = n, colour = age_category)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1920, 2020, by = 10)) +
  facet_wrap(~marital_status_of_mother, ncol = 1, scales = "free_y")

idg060 %>% 
  filter(age_category == "Total") %>% 
  ggplot(aes(x = year, y = n)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1920, 2020, by = 10)) +
  facet_wrap(~marital_status_of_mother, ncol = 1)


# https://data1.csb.gov.lv/pxweb/en/iedz/iedz__dzimst/IDG070.px/
# IDG070. Dzīvi dzimušie pēc dzimuma un mātes vecuma
# IDG070. Live births by sex and age of mother

idg070_orig <- read_delim("content/post/IDG070.csv", delim = ";", skip = 2, na = c("...", "-"))
idg070_orig

idg070 <- idg070_orig %>% 
  gather(key, n, -1) %>% 
  separate(col = key, into = c("year", "gender"), convert = TRUE) %>% 
  mutate(n = as.integer(n))

names(idg070)[1] <- "age_category"

idg070

idg070 %>% 
  filter(str_detect(age_category, "[-[:alpha:]]") & age_category != "Total") %>% 
  ggplot(aes(x = year, y = n, colour = age_category)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1920, 2020, by = 10)) +
  facet_wrap(~gender, ncol = 1)
 
idg070 %>% 
  filter(str_detect(age_category, "[-[:alpha:]]") & age_category != "Total" & gender %in% c("boys", "girls")) %>% 
  ggplot(aes(x = year, y = n, fill = gender)) +
  geom_col(position = "fill") +
  scale_x_continuous(breaks = seq(1920, 2020, by = 10)) +
  facet_wrap(~age_category, ncol = 1)

idg070 %>% 
  filter(str_detect(age_category, "[-[:alpha:]]") & age_category != "Total" & gender %in% c("boys", "girls")) %>% 
  group_by(year, gender) %>% 
  summarize(n = sum(n)) %>% 
  mutate(p = n / sum(n)) %>% 
  ggplot(aes(x = year, y = n, fill = gender)) +
  geom_area(position = "fill") +
  geom_hline(yintercept = 0.5, linetype = "dashed")

idg070 %>% 
  filter(str_detect(age_category, "[-[:alpha:]]") & age_category != "Total" & gender %in% c("boys", "girls")) %>% 
  group_by(gender) %>% 
  summarize(n = sum(n, na.rm = TRUE)) %>% 
  mutate(p = n / sum(n))

prop.test(259495, 259495 + 245478, p = 0.5, alternative = "greater")


# https://data1.csb.gov.lv/pxweb/en/iedz/iedz__dzimst/IDG080.px
# IDG080. Dzīvi dzimušie pēc mātes un tēva vecuma
# IDG080. Live births by age of parents

idg080_orig <- read_delim("content/post/IDG080.csv", delim = ";", skip = 2, na = "...")
idg080_orig

idg080 <- idg080_orig %>% 
  gather(key, value, -1, -2) %>% 
  set_names(c("year", "age_of_father", "age_of_mother", "n"))

# Age of father.
idg080 %>% 
  filter(age_of_mother == "Total") %>% 
  ggplot(aes(x = year, y = n, colour = age_of_father)) +
  geom_line()

# Age of mother.
idg080 %>% 
  filter(age_of_father == "Total") %>% 
  ggplot(aes(x = year, y = n, colour = age_of_mother)) +
  geom_line()

bind_rows(
  idg080 %>% 
    filter(age_of_mother == "Total" & !str_detect(age_of_father, "not specified|Total")) %>% 
    mutate(who = "father") %>% 
    select(year, age_category = age_of_father, who, n),
  idg080 %>% 
    filter(age_of_father == "Total" & !str_detect(age_of_mother, "not specified|Total")) %>% 
    mutate(who = "mother") %>% 
    select(year, age_category = age_of_mother, who, n)
) %>% 
  mutate(age_category = str_replace_all(age_category, pattern = "[^[:digit:]-]", replacement = "")) %>% 
  ggplot(aes(x = year, y = n, fill = age_category)) +
  geom_area(position = "fill") +
  facet_wrap(~who)



idg080 %>% 
  filter(!(str_detect(age_of_father, "Total|not specified") | str_detect(age_of_mother, "Total|not specified"))) %>% 
  ggplot(aes(x = year, y = n, colour = age_of_mother)) +
  geom_line() +
  facet_wrap(~age_of_father)

idg080 %>% 
  filter(!(str_detect(age_of_father, "Total|not specified") | str_detect(age_of_mother, "Total|not specified"))) %>% 
  ggplot(aes(x = year, y = n, colour = age_of_father)) +
  geom_line() +
  facet_wrap(~age_of_mother)

idg080 %>% 
  filter(!(str_detect(age_of_father, "Total|not specified") | str_detect(age_of_mother, "Total|not specified"))) %>% 
  group_by(year, age_of_father) %>% 
  mutate(p = n / sum(n)) %>% 
  ggplot(aes(x = year, y = p, colour = age_of_mother)) +
  geom_line() +
  facet_wrap(~age_of_father)



# https://data1.csb.gov.lv/pxweb/en/iedz/iedz__dzimst/IDG090.px
# IDG090. Dzīvi dzimušie pēc mātes vecuma un bērna dzimšanas secības
# IDG090. Live births by age of mother and child’s order of birth

idg090_orig <- read_delim("content/post/IDG090.csv", delim = ";", skip = 2, na = "...")
idg090_orig

idg090_orig %>% 
  gather(year, n, -1, -2)


# https://data1.csb.gov.lv/pxweb/en/iedz/iedz__dzimst/IDG100.px
# IDG100. Mātes vidējais vecums pēc izglītības līmeņa un bērna dzimšanas secības
# IDG100. Average age of mother by educational attainment and birth order of child

idg100_orig <- read_delim("content/post/IDG100.csv", delim = ";", skip = 2, na = "...")
idg100_orig

idg100 <- idg100_orig %>% 
  gather(key, age, -1) %>% 
  mutate(age = as.numeric(age))

idg100 %>% 
  filter(key %in% c("first child Total", "second child Total")) %>% 
  ggplot(aes(x = Year, y = age, colour = key)) +
  geom_line() +
  geom_point()


# https://data1.csb.gov.lv/pxweb/en/iedz/iedz__dzimst/IDG120.px
# IDG120. Dzīvi dzimušie pēc tautības
# IDG120. Births by ethnicity

idg120_orig <- read_delim("content/post/IDG120.csv", delim = ";", skip = 2, na = "...")
idg120_orig

idg120 <- idg120_orig %>% 
  gather(year, n, -1, convert = TRUE) %>% 
  mutate(n = as.integer(n))

idg120

idg120 %>% 
  ggplot(aes(x = year, y = n, colour = Ethnicity)) +
  geom_line()

idg120 %>% 
  filter(Ethnicity != "Total") %>% 
  group_by(year) %>% 
  mutate(p = n / sum(n, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = p, colour = Ethnicity)) +
  geom_line()




# EU
# Live births by mother's age and newborn's sex

eu_orig <- read_delim("content/post/demo_fasec_1_Data.csv", delim = ",", na = ":")

eu <- eu_orig %>% 
  select(-SEX, -UNIT, -`Flag and Footnotes`) %>% 
  filter(!str_detect(GEO, "^Euro|Liechtenstein|FRG")) %>% 
  filter(!AGE %in% c("Unknown", "Total")) %>% 
  mutate(Value = as.integer(str_replace_all(Value, "\\s", "")),
         AGE = as.numeric(str_replace_all(AGE, "[^[:digit:]]", "")),
         GEO = if_else(GEO == "Germany including former GDR", "Germany", GEO))

countries_with_missing_values_vec <- unique(eu$GEO[is.na(eu$Value)])

eu <- eu %>% 
  filter(!GEO %in% countries_with_missing_values_vec)
  
eu_mean_age <- eu %>%
  group_by(TIME, GEO) %>% 
  summarise(mean_age = weighted.mean(AGE, w = Value)) %>% 
  ungroup()

eu_mean_age %>% 
  arrange(TIME) %>% 
  group_by(GEO) %>% 
  summarise(age_chg = last(mean_age) - first(mean_age)) %>% 
  arrange(age_chg)

p <- eu_mean_age %>% 
  ggplot(aes(x = TIME, 
             y = mean_age, 
             group = GEO,
             colour = GEO == "Latvia")) +
  geom_line(aes( size = GEO == "Latvia")) +
  geom_text(mapping = aes(label = GEO), 
            data = eu_mean_age %>% filter(TIME == max(eu_mean_age$TIME) & GEO == "Latvia"), 
            hjust = "left", nudge_x = 0.1) +
  scale_x_continuous(breaks = min(eu_mean_age$TIME):max(eu_mean_age$TIME), 
                     minor_breaks = NULL,
                     expand = expand_scale(add = c(0, 0.8))) +
  scale_y_continuous(breaks = floor(min(eu_mean_age$mean_age)):max(eu_mean_age$mean_age), 
                     minor_breaks = NULL) +
  scale_color_manual(values = c("TRUE" = "#9E3039", "FALSE" = "grey80")) +
  scale_size_manual(values = c("TRUE" = 1, "FALSE" = 0.1)) +
  labs(x = NULL) +
  theme_classic() +
  theme(legend.position = "none")
p
plotly::ggplotly(p)

