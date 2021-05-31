
# Packages ----

library(tidyverse)
library(ggrepel)
library(scales)




# Plot the relationship between a number of documents with term t in it and the term’s idf.

p_idf <- tibble(n_docs_with_term = 1:12,
                idf = log(12 / n_docs_with_term)) %>% 
  ggplot(aes(x = n_docs_with_term, y = idf)) +
  geom_point(size = 2) +
  geom_line() +
  labs(x = "Number of documents with term t in it",
       y = "idf(t)") +
  scale_y_continuous(breaks = seq(0, 3, by = 0.5), minor_breaks = NULL, expand = expand_scale(add = 0.05)) +
  scale_x_continuous(breaks = 1:12) +
  theme_classic() +
  theme(panel.grid = element_blank(), 
        axis.text.x = element_text(size = 10, colour = "grey30"),
        axis.text.y = element_text(size = 10, colour = "grey30"), 
        axis.title = element_text(size = 10, colour = "grey30"), 
        plot.title = element_text(size = 11, colour = "grey30", hjust = 0))




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

# Steps 3. Tokenize, Step 4. Lemmatize, Step 5. Mark stop words.

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




# Tf ----

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

# Extract words with zero tf-idf.
zero_tfidf <- tokens_tfidf %>% 
  filter(tf_idf == 0) %>% 
  pull(word_lemma) %>% 
  unique() %>% 
  paste(sep = ",")

# How many documents contain each word.
terms_in_documents <- tokens_tfidf %>% 
  group_by(word_lemma) %>% 
  summarise(documents = n_distinct(interview))

# Data frame for viz.
slopes_tf_tfidf <- tokens_tfidf %>% 
  filter(interview == "Andy Cotgreave") %>% 
  add_count(name = "total_words", wt = n) %>% 
  # arrange(desc(word_lemma)) %>% 
  mutate(word_tf_rank = row_number(-tf),
         word_tfidf_rank = row_number(-tf_idf)) %>%
  filter(word_tf_rank %in% 1:6 | word_tfidf_rank %in% 1:6) %>% 
  ungroup()

# Summary table with most important words by tf and tf-idf.
t_summary <- slopes_tf_tfidf %>% 
  left_join(terms_in_documents, by = "word_lemma") %>% 
  select(word_lemma, freq = n, total_words, tf, documents, idf, tf_idf, tf_rank = word_tf_rank, tfidf_rank = word_tfidf_rank) %>% 
  arrange(desc(freq), desc(tfidf_rank))




# Visualize the effect of idf on tf ----

# Transformations for the plot.
slopes_tf_tfidf_gathered <- slopes_tf_tfidf %>% 
  select(interview, word_lemma, word_tf_rank, word_tfidf_rank) %>% 
  mutate(rank_inc = word_tf_rank > word_tfidf_rank) %>% 
  gather(key, value, word_tf_rank, word_tfidf_rank)

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

# Plot titles.
gg_title <- 'tf-idf: the effect of idf on tf'
gg_subtitle <- "Comparison of two measures of word importance: tf (term frequency) and tf-idf (term frequency-inverse document frequency). Ranks are positions relative to other words; the highest rank is 1, which means the highest importance, the lowest rank is 271."
gg_caption <- "Source: transcript of Andy Cotgreave's interview in the \"learning dataviz\" episode of #SWDpodcast.\nIllustration by @DmitrijsKass in R with ggplot2."

# Viz.
set.seed(1)
p_slopes <- slopes_tf_tfidf_gathered %>% 
  ggplot(aes(x = key, y = value, group = word_lemma, label = word_lemma)) +
  geom_point(colour = swd_light_blue, size = 2) +
  geom_point(data = filter(slopes_tf_tfidf_gathered, rank_inc == TRUE), colour = swd_dark_blue, size = 2) +
  geom_line(colour = swd_light_blue) +
  geom_line(data = filter(slopes_tf_tfidf_gathered, rank_inc == TRUE), colour = swd_dark_blue, size = 1.05) +
  geom_text_repel(data = filter(slopes_tf_tfidf_gathered, key == "word_tf_rank"), 
                  mapping = aes(color = rank_inc),
                  nudge_x = -0.02, 
                  hjust = "outward",
                  segment.colour = "grey20",
                  show.legend = FALSE) +
  geom_text_repel(data = filter(slopes_tf_tfidf_gathered, key != "word_tf_rank"), 
                  mapping = aes(color = rank_inc),
                  nudge_x = +0.02, 
                  hjust = "outward",
                  segment.colour = "grey20",
                  show.legend = FALSE) +
  scale_color_manual(values = c("TRUE" = swd_dark_blue, "FALSE" = swd_light_blue)) +
  scale_y_continuous(trans = reverselog_trans(10), 
                     sec.axis = dup_axis(name = str_wrap("rank of words by tf-idf", width = 8)), 
                     breaks = c(1:3, 5, 10, 50, 100, 271), 
                     minor_breaks = NULL) +
  scale_x_discrete(labels = NULL, 
                   position = "top", 
                   expand = expand_scale(mult = 0.25, add = 0)) +
  labs(x = NULL,
       y = str_wrap("rank of words by tf", width = 8),
       title = gg_title,
       subtitle = str_wrap(gg_subtitle, width = 84),
       caption = gg_caption) +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        axis.title.y.left = element_text(angle = 0, colour = "grey30", vjust = 0.97),
        axis.title.y.right = element_text(angle = 0, colour = "grey30", vjust = 0.97),
        axis.text.x = element_text(size = 12, colour = "grey30"),
        axis.text.y = element_text(size = 10, colour = "grey30"), 
        plot.title = element_text(size = 15, colour = "grey30", hjust = 0.5, margin = margin(b = 0)),
        plot.subtitle = element_text(size = 11.5, colour = "grey50", hjust = 0.5, margin = margin(t = 10, b = 15)),
        plot.caption = element_text(size = 8, colour = "grey40", hjust = 0.5, margin = margin(t = 15)))

ggsave(plot = p_slopes, filename = "tfidf-the-effect-of-idf-on-tf.png", width = 6, height = 7.8, units = "in")
