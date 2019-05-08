library(tidyverse)
library(tidytext)
dark_grey <- "grey30"
light_grey <- "grey80"
swd_blue <- "#4F81BC"

# source: https://docs.google.com/document/d/1S2_63MUbvQs7fxWQrcuCNSl03fmDl1Vr3FjNjnbWK14/edit#
raw_text <- readLines("swd_podcast_episode_14_learning_dataviz.txt", encoding = "ansi")

toc <- raw_text[11:25]
names(toc) <- str_replace_all(toc, pattern = "(.*)\\|(.*)", replacement = "\\2") %>% str_trim()
content <- raw_text[29:length(raw_text)]
content <- content[content != ""]

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
  
# Before applying `qdap::replace_contractions()` we need to replace apostrophes from Windows-1252 code page (146) to a ASCII (39).
# Otherwise, the function does not work.

content_df$paragraph[3]

content_df$paragraph <- str_replace_all(string = content_df$paragraph,
                                        pattern = gtools::chr(146), # https://en.wikipedia.org/wiki/Windows-1252
                                        replacement = gtools::chr(39))

content_df$paragraph[3]

# Replace contractions.
content_df$paragraph <- qdap::replace_contraction(content_df$paragraph)

content_df$paragraph[3]

# Define stopwords.
names_of_guests <- str_to_lower(names(toc)) %>% str_split(pattern = "\\s") %>% unlist()
stopwords_vec <- c(stop_words$word, names_of_guests, "cole")

# TODO: add bi-grams.
# TODO: heatmap.
# TODO: plot network diagram connecting guests by 
# - lines, where thinkness is defned by a number of words in common.
# - lines, one per each word in common (limit to top X)

# Tokenize by words, lemmatize and mark stop words.
tokens <- content_df %>% 
  unnest_tokens(output = "word", input = paragraph, token = "words") %>% 
  mutate(word_lemma = textstem::lemmatize_words(word),
         stop_word = word_lemma %in% stopwords_vec)

# View lemmatized words.
tokens %>% 
  filter(!stop_word & word != word_lemma) %>% 
  count(word, word_lemma, sort = TRUE)

# Transform some words back after lemmatization.
tokens <- tokens %>% 
  mutate(word_lemma = case_when(word_lemma == "numb" ~ "number", 
                                word_lemma == "datum" ~ "data", 
                                TRUE ~ word_lemma))
# Plot stop words.
tokens %>% 
  filter(stop_word) %>% 
  count(word_lemma, sort = TRUE) %>% 
  slice(1:10) %>% 
  mutate(word_lemma = reorder(word_lemma, n)) %>% 
  ggplot(aes(x = word_lemma, y = n, label = n)) +
  geom_col(fill = swd_blue) +
  geom_text(hjust = "right", nudge_y = -10, size = 3.5, colour = "white") +
  coord_flip() +
  labs(title = "Top 10 of stop words in all interviews",
       subtitle = "Words have been lemmatized prior to analysis",
       x = "stop word", 
       y = "word count") +
  theme_minimal()

# Calculate word frequency.
tokens_freq <- tokens %>% 
  group_by(interview, stop_word, word_lemma) %>% 
  summarise(n = n()) %>% 
  ungroup()

tokens_freq

stop_df <- tokens_freq %>% 
  group_by(interview, stop_word) %>% 
  summarize(n_stop_words = sum(n)) %>% 
  ungroup()

stop_df %>% 
  spread(stop_word, n_stop_words) %>% 
  mutate(p_stop_words = `TRUE` / (`TRUE` + `FALSE`)) %>% 
  pull(p_stop_words) %>% 
  range()

stop_df <- stop_df %>% 
  filter(stop_word) %>% 
  select(-stop_word)
  
tokens_freq_groups <- tokens_freq %>% 
  filter(!stop_word) %>% 
  group_by(interview) %>% 
  summarize(n_nonstop_words = sum(n),
            n_distinct_words = n_distinct(word_lemma)) %>% 
  mutate(n_repetitive_words = n_nonstop_words - n_distinct_words) %>% 
  left_join(stop_df, by = "interview") %>% 
  mutate(interview = reorder(interview, n_nonstop_words + n_stop_words)) %>% 
  select(-n_nonstop_words) %>% 
  gather(key, value, -interview) %>% 
  mutate(key = fct_rev(key))
    
tokens_freq_groups %>% 
  ggplot(aes(x = interview, y = value, fill = key, label = value)) +
  geom_col(position = "stack") +
  coord_flip() +
  scale_fill_manual(values = c("n_stop_words" = "grey80", "n_repetitive_words" = "grey60", "n_distinct_words" = swd_blue),
                    labels = c("stop words", "non-stop word repetitions", "unique non-stop words")) +
  labs(title = "Stop words are 60-80% of all words",
       x = NULL, 
       y = "word count", 
       fill = NULL) +
  theme_classic() +
  theme(legend.position = "top", 
        axis.ticks.y = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE))
  

# Remove stop words.
tokens_freq <- tokens_freq %>% 
  filter(!stop_word)


# Add TF-IDF.
tokens_tfidf <- tokens_freq %>% 
  bind_tf_idf(word_lemma, interview, n) %>% 
  # Arrange by facet and word frequency.
  arrange(interview, tf_idf) %>% 
  # Add order column of row numbers
  mutate(order = row_number())

# Words that appear in all interviews.
tokens_tfidf %>% 
  filter(idf == 0) %>% 
  distinct(word_lemma)

# Filter top 10 important words in each interview by TF-IDF.
tokens_top10 <- tokens_tfidf %>% 
  group_by(interview) %>% 
  arrange(desc(tf_idf)) %>% 
  slice(1:10) %>% 
  ungroup() %>% 
  # Arrange by facet and word frequency.
  arrange(interview, tf_idf) %>% 
  # Add order column of row numbers
  mutate(order = row_number())

p <- tokens_top10 %>% 
  ggplot(aes(x = order, y = tf_idf)) +
  geom_col(fill = swd_blue) +
  coord_flip() +
  facet_wrap(~interview, scales = "free") +
  scale_x_continuous(breaks = tokens_top10$order,
                     labels = tokens_top10$word_lemma) +
  theme_minimal()

p

p + 
  labs(x = NULL, 
       y = NULL) +
  theme(strip.background = element_blank(), 
        panel.grid = element_blank(), 
        plot.background = element_rect(fill = "white"), 
        strip.text = element_text(colour = dark_grey, face = "bold", size = 12), 
        axis.text = element_text(colour = dark_grey), 
        title = element_text(colour = dark_grey, size = 15), 
        plot.subtitle = element_text(colour = dark_grey, size = 9.5),
        axis.text.x = element_blank(),
        axis.text.y = element_text(hjust = 0, size = 11), 
        plot.caption = element_text(size = 8, hjust = 0),
        panel.spacing.x = unit(2,"line"), 
        panel.spacing.y = unit(1.5,"line"))
  

  
# Words in common ----

nodes <- tokens_freq %>% 
  distinct(interview) %>% 
  rename(label = interview) %>% 
  rowid_to_column("id")

edges <- left_join(tokens_freq[1:2], tokens_freq[1:2], by = "word_lemma") %>% 
  filter(interview.x != interview.y) %>% # Remove loops.
  count(interview.x, interview.y, name = "weight") %>% 
  left_join(nodes %>% rename(from = id), by = c("interview.x" = "label")) %>% 
  left_join(nodes %>% rename(to = id), by = c("interview.y" = "label")) %>% 
  select(from, to, weight) %>% 
  mutate(dupl = if_else(from < to, paste(from, to), paste(to, from))) %>%
  filter(weight > 50)

edges <- edges %>% 
  filter(duplicated(edges$dupl)) %>% 
  select(-dupl)

edges %>% 
  spread(to, weight)

library(network)

routes_network <- network(edges, vertex.attr = nodes, matrix.type = "edgelist", ignore.eval = FALSE, directed = FALSE, loops = FALSE)
plot(routes_network, vertex.cex = 3, mode = "circle")

library(igraph)
routes_igraph <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)
plot(routes_igraph)

library(ggraph)
ggraph(routes_igraph, layout = layout_igraph_linear(circular = TRUE)) + 
  geom_edge_arc(aes(width = weight), alpha = 0.8) + 
  geom_edge_link(aes(width = weight)) +
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label)) +
  labs(edge_width = "Letters") +
  theme_graph()

