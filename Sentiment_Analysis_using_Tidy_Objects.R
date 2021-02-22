library(readxl)
df1 <- read_excel("C:/Users/Lingi/OneDrive - Education First/MBAN Courses/NLP/Individual Assignment/Files/Whatsapp_reviews.xlsx")
df2 <- read_excel("C:/Users/Lingi/OneDrive - Education First/MBAN Courses/NLP/Individual Assignment/Files/Telegram_reviews.xlsx")
df3 <- read_excel("C:/Users/Lingi/OneDrive - Education First/MBAN Courses/NLP/Individual Assignment/Files/Signal_reviews.xlsx")
df1 <- df1[,c("content","thumbsUpCount")]
df1$platform <- c('Whatsapp')
df2 <- df2[,c("content","thumbsUpCount")]
df2$platform <- c('Telegram')
df3 <- df3[,c("content","thumbsUpCount")]
df3$platform <- c('Signal')

# Getting Revieew number column to the beginning
reviews_df <- rbind(df1,df2,df3)
reviews_df <- reviews_df[c('platform',"content","thumbsUpCount")]

# Renaming the content column name as text
colnames(reviews_df) <- c("platform","text","thumbsUpCount")

# Summarizing all datframe to check whether all the columns are correctly assigned
str(reviews_df)

# Tokenizing the reviews and also find the number of times each token appeared in each platform
library(dplyr)
library(tidytext)
platform_words <- reviews_df %>%
  unnest_tokens(word, text) %>%
  count(platform, word, sort = TRUE) %>%
  ungroup()

# finding the total number of tokens in each platform
total_words <- platform_words %>%
  group_by(platform) %>%
  summarize(total = sum(n))

# Joining both tables
platform_words <- left_join(platform_words, total_words)


library(ggplot2)
ggplot(platform_words, aes(n/total, fill = platform)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.008) +
  facet_wrap(~platform, ncol = 1, scales = "free_y")

#----------------------- Zipf's Law --------------------------------------#

freq_by_rank <- platform_words %>%
  group_by(platform) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)
freq_by_rank

#rank column here tells us the rank of each word within the frequency table
#visualizing Zip's law
freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color = platform)) +
  geom_abline(intercept = -0.62, slope = -1.1, color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()

#--------------------------- TF-IDF --------------------------------------#

#creating TF_IDF
platform_words <- platform_words %>%
  bind_tf_idf(word, platform, n)
platform_words

platform_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))
# visualizing TF_IDF
platform_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(platform) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = platform)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~platform, ncol = 2, scales = "free") +
  coord_flip()

# here we identified some of the top features that are important in each of the platforms



# -------------------------- N - Grams ----------------------------------------#

# Even though we identified that are important for each platform still they dont make senses

# Tokenizing the reviews and also find the number of times each token appeared in each platform
library(dplyr)
library(tidytext)
my_stop_words <- c('а' ,'п','sря','s','в', 'onot',
                  'onot','ря','fря','fря', 'ря', 'zря','є','ш','шЄщ^ш ш','щ шЄщ^ш')
review_bigrams <- reviews_df %>% 
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% my_stop_words) %>%
  filter(!word2 %in% my_stop_words) %>%
  filter(!word1 == "NA") %>%
  filter(!word2 == "NA") %>%
  unite(bigram, word1, word2, sep=" ")

review_bigrams

# priotitizing the ngrams usinf tf_idf
review_bigrams_tf_idf <- review_bigrams %>%
  count(platform, bigram) %>%
  bind_tf_idf(bigram, platform, n) %>%
  arrange(desc(tf_idf))

review_bigrams_tf_idf

# visualizing the bigram
# preparing for chart
review_bigrams_graph <- review_bigrams_tf_idf %>%
  filter(n>2) %>%
  graph_from_data_frame()
review_bigrams_graph

ggraph(review_bigrams_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)



# --------------------------- Trigram -----------------------------------------#


fs_ngrams <- reviews_df %>% 
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>% 
  separate(trigram, c("word1", "word2","word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word1 == "NA") %>%
  filter(!word2 == "NA") %>%
  filter(!word3 == "NA") %>%
  unite(trigram, word1, word2,word3, sep=" ")

fs_ngrams

# priotitizing the ngrams usinf tf_idf
fs_ngrams_tf_idf <- fs_ngrams %>%
  count(platform, trigram) %>%
  bind_tf_idf(trigram, platform, n) %>%
  arrange(desc(tf_idf))

fs_ngrams_tf_idf

# visualizing the bigram
# preparing for chart
fs_ngrams_graph <- fs_ngrams_tf_idf %>%
  filter(n>1) %>%
  graph_from_data_frame()
fs_ngrams_graph

ggraph(fs_ngrams_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)
