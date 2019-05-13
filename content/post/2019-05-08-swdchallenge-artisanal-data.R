
library(tidyverse)
library(ggrepel)
library(scales)




# Text import ----

raw_text <- readLines("swd_podcast_episode_14_learning_dataviz.txt", encoding = "ansi")




# Text cleaning ----

# Step 1: Extract interviews from the transcript.

toc <- raw_text[11:25] # toc = table of content.
names(toc) <- str_replace_all(toc, pattern = "(.*)\\|(.*)", replacement = "\\2") %>% str_trim()

content <- raw_text[29:length(raw_text)]
content <- content[content != ""] # Remove empty text elements.

toc_lines <- purrr::map_int(.x = toc, .f = function(x) str_which(string = content, pattern = fixed(x)))
toc_lines <- tibble(interview = names(toc_lines),
                    interview_index = toc_lines)

# First and last paragraph of each interview is intro and outro of the host.
paragraphs_to_ignore <- sapply(c(-1, 0, 1), function(x) toc_lines$interview_index + x) %>% as.vector() %>% sort()

content_df <- tibble(paragraph_id = 1:length(content),
                     paragraph = content) %>% 
  left_join(toc_lines, by = c("paragraph_id" = "interview_index")) %>% 
  fill(interview, .direction = "down") %>% 
  mutate(ignore = interview %in% c("Intro", "Summary", "Updates") | paragraph_id %in% paragraphs_to_ignore) %>% 
  filter(!ignore)


# Step 2. Replace contractions.

# Before applying `qdap::replace_contractions()` we need to replace apostrophes
# from Windows-1252 code page (146) to a ASCII (39).
content_df$paragraph <- str_replace_all(string = content_df$paragraph,
                                        pattern = gtools::chr(146), # https://en.wikipedia.org/wiki/Windows-1252
                                        replacement = gtools::chr(39))
content_df$paragraph <- qdap::replace_contraction(content_df$paragraph)

# Steps 3., 4., 5. Tokenize, lemmatize, mark stop words.

# Define stop words.

# Know your stop words. For example, the SMART set (as documented in Appendix 11
# of http://jmlr.csail.mit.edu/papers/volume5/lewis04a/) has all single letters
# on English alphabet, including "r". In this text, "r" is used to refer to the
# programming language R, so I remove it from a vector of stop words.

names_of_guests <- str_to_lower(names(toc)) %>% str_split(pattern = "\\s") %>% unlist()
stopwords_vec <- setdiff(c(tm::stopwords(kind = "SMART"), names_of_guests, "cole"), "r")

# Tokenize by words, lemmatize and mark stop words.
tokens <- content_df %>% 
  tidytext::unnest_tokens(output = "word", input = paragraph, token = "words") %>% 
  mutate(word_lemma = textstem::lemmatize_words(word),
         stop_word = word_lemma %in% stopwords_vec)

# Transform some words back after lemmatization.
tokens <- tokens %>% 
  mutate(word_lemma = case_when(word_lemma == "numb" ~ "number", 
                                word_lemma == "datum" ~ "data",
                                word_lemma == "infographics" ~ "infographic",
                                TRUE ~ word_lemma))




# Term frequency (tf) ----

# Calculate word frequency.
tokens_freq <- tokens %>% 
  group_by(interview, stop_word, word_lemma) %>% 
  summarise(n = n()) %>% 
  ungroup()

# View stop words.
tokens_freq %>% 
  filter(stop_word) %>% 
  count(word_lemma, wt = n, sort = TRUE)

# Remove stop words.
tokens_freq <- tokens_freq %>% 
  filter(!stop_word)




# Tf-idf ----

# Add TF-IDF.
tokens_tfidf <- tokens_freq %>% 
  tidytext::bind_tf_idf(word_lemma, interview, n)

# View words with zero tf-idf.
tokens_tfidf %>% 
  filter(tf_idf == 0) %>% 
  pull(word_lemma) %>% 
  unique()




# Visualize the effect of idf on tf ----

# Colours.
swd_light_blue <- "#95b3d7"
swd_dark_blue <- "#4f81bd"

# Source for `reverselog_trans()`: https://stackoverflow.com/questions/11053899/how-to-get-a-reversed-log10-scale-in-ggplot2
reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv, 
            log_breaks(base = base), 
            domain = c(1e-100, Inf))
}

# Data frame for viz.
slopes_tf_tfidf <- tokens_tfidf %>% 
  filter(interview == "Andy Cotgreave") %>% 
  mutate(word_tf_rank = row_number(-tf),
         word_tfidf_rank = row_number(-tf_idf)) %>%
  filter(word_tf_rank %in% 1:6 | word_tfidf_rank %in% 1:6) %>% 
  ungroup() %>% 
  select(interview, word_lemma, word_tf_rank, word_tfidf_rank) %>% 
  mutate(rank_inc = word_tf_rank > word_tfidf_rank) %>% 
  gather(key, value, word_tf_rank, word_tfidf_rank)

# Plot titles.
gg_title <- 'tf-idf: the effect of idf on tf.'
gg_subtitle <- paste(str_wrap(
  "As an example, these are some of words used by Andy Cotgreave in the 'learning dataviz' episode of #SWDpodast. Although words such as 'data' and 'learn' are the 1st and the 5th most frequent, they are not important when compared to other 11 interviews on the same podcast episode. This is because all 12 interviews are about learning data visualization.", width = 77),
  "",
  str_wrap("Notice how idf diminishes the weights of words that appear in many interviews (e.g. 'data', 'learn') and promotes other words (e.g. 'copy', 'fun') as more relevant for a particular interview.", width = 77),
  "",
  str_wrap("A word 'copy' is important in the context of recommending Austin Kleon's book Steal Like an Artist. A word 'fun' is important in recommending the listeners to 'get out of your comfort zone and have fun'. 'Have fun. My gosh, you are allowed to have fun!' invites Andy.", width = 77),
  sep = "\n")

gg_caption <- "Source: transcript of #SWD podcast episode 'learning dataviz'.\nIllustration by @DmitrijsKass in R with ggplot2."

# Viz.
set.seed(3) # for `geom_text_repel`.
slopes_tf_tfidf %>% 
  ggplot(aes(x = key, y = value, group = word_lemma, label = word_lemma)) +
  geom_point(colour = swd_light_blue, size = 2) +
  geom_point(data = filter(slopes_tf_tfidf, rank_inc == TRUE), colour = swd_dark_blue, size = 2) +
  geom_line(colour = swd_light_blue) +
  geom_line(data = filter(slopes_tf_tfidf, rank_inc == TRUE), colour = swd_dark_blue, size = 1.05) +
  geom_text_repel(data = filter(slopes_tf_tfidf, key == "word_tf_rank"), 
                  mapping = aes(color = rank_inc),
                  nudge_x = -0.02, 
                  hjust = "outward",
                  show.legend = FALSE) +
  geom_text_repel(data = filter(slopes_tf_tfidf, key != "word_tf_rank"), 
                  mapping = aes(color = rank_inc),
                  nudge_x = +0.02, 
                  hjust = "outward",
                  show.legend = FALSE) +
  scale_color_manual(values = c("TRUE" = swd_dark_blue, "FALSE" = swd_light_blue)) +
  scale_y_continuous(trans = reverselog_trans(10), 
                     sec.axis = dup_axis(), 
                     breaks = c(1:3, 5, 10, 50, 100, 300), 
                     minor_breaks = NULL) +
  scale_x_discrete(labels = c("rank of words\nby tf\n", "rank of words\nby tf-idf\n"), 
                   position = "top", 
                   expand = expand_scale(mult = 0.2, add = 0)) +
  labs(x = NULL,
       y = NULL,
       title = gg_title,
       subtitle = gg_subtitle,
       caption = gg_caption) +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.text.x = element_text(size = 12, colour = "grey30"),
        axis.text.y = element_text(size = 10, colour = "grey30"), 
        plot.title = element_text(size = 15, colour = "grey30", hjust = 0.5),
        plot.subtitle = element_text(size = 9.5, colour = "grey40", margin = margin(b = 15, t = 15)),
        plot.caption = element_text(size = 8, colour = "grey40", hjust = 0.5, margin = margin(t = 15)))

ggsave(filename = "tfidf-the-effect-of-idf-on-tf.png", width = 5.2, height = 7.8, units = "in")
