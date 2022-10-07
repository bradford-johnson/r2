# load-packages
library(tidyverse) # for working with data
library(tidytext) # for working with text data
library(rtweet) # using the twitter api
library(widyr) # for pairwise correlation 
library(igraph) # for data visualization
library(ggraph) # for visualizing word corr

# rtweet-auth
auth_setup_default()
auth_has_default()
df <- search_tweets("", n = , include_rts = FALSE, lang = "en")

# get-sentiment
db_new <- unnest_tokens(tbl = df, input = full_text, output = word)

stp_wrds <- get_stopwords(source = "smart")

db_new <- anti_join(db_new, stp_wrds, by = "word")

bing <- get_sentiments(lexicon = "bing")

db_bing <- inner_join(db_new, bing, by = "word")

db_bing <- count(db_bing, id_str, display_text_range, retweet_count, favorite_count, word, sentiment)

db_bing <- spread(key = sentiment, value = n, fill = 0, data = db_bing)


db_bing <- mutate(sentiment = positive - negative, .data = db_bing)

mean(db_bing$sentiment, na.rm = TRUE)

#clean-sentiment
fun_color_range <- colorRampPalette(c("red", "green"))
my_colors2 <- fun_color_range(2)

db_sentiment <- db_bing %>% 
  mutate(value = case_when(sentiment > 0 ~ 'positive',
                           sentiment == 0 ~ 'neutral',
                           sentiment < 0 ~ 'negative')) %>%
  filter(value == 'positive' | value == 'negative')

#sentiment-plot
sentiment_plot <- db_sentiment %>%
  group_by(id_str) %>%
  summarise( display_text_range = mean(display_text_range), Sentiment = sum(sentiment)) %>% 
  filter(display_text_range < 280) %>%
  ggplot(mapping = aes(x = display_text_range, y = Sentiment, color = Sentiment)) +
  geom_jitter(alpha = .7) + 
  scale_colour_gradientn(colors = my_colors2) +
  geom_hline(linetype = 1, yintercept = 0, size = .5) +
  geom_vline(linetype = 3, xintercept = 140, size = .5) +
  theme_classic() +
  theme(legend.position = "") + 
  labs(title = "", x = "Number of Characters", y = "Sentiment")

sentiment_plot

#clean-correlation
tweet_words <- df %>% 
  unnest_tokens(output = word, input = text) %>% 
  anti_join(stop_words, by = "word") %>% 
  filter(str_detect(word, "[:alpha:]")) %>% 
  distinct()

#---- remove words ----#
word <- c('')
extra2 <- data.frame(word)

tweet_words <- tweet_words %>%
  anti_join(extra2, by = "word")

#---- number of tweets ----#
tweets_that_mention_word <- tweet_words %>% 
  count(word, name = "number_of_tweets") %>% 
  filter(number_of_tweets >= )

#---- tweet correlations ----#
tweet_correlations <- tweet_words %>% 
  semi_join(tweets_that_mention_word, by = "word") %>% 
  pairwise_cor(item = word, feature = id_str) %>% 
  filter(correlation >= )

# correlation-plot
correlation_plot <- graph_from_data_frame(d = tweet_correlations,
                                         vertices = tweets_that_mention_word %>%
                                           semi_join(tweet_correlations, by = c("word" = "item1"))) %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = correlation)) + 
  geom_node_point() +
  geom_node_text(aes(color = number_of_tweets, label = name), repel = TRUE, check_overlap = TRUE, size = 4) + 
  labs(title = "Psyonix Tweets") +
  scale_colour_gradientn(colours=c("#3e3b92", "#f44369")) +
  theme(panel.background = element_rect(fill = "#eaeaea"),
        plot.background = element_rect(fill = "#ffffff"))

correlation_plot

#tweet-date
df %>%
  select(created_at) %>%
  ggplot(aes(x=created_at)) +
  geom_histogram() +
  theme_classic() +
  labs(title = "Date Tweeted",x="",y="Count")
