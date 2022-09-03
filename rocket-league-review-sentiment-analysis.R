library(dplyr)
library(rvest)
library(tm)


# page 1 

link = "https://www.trustpilot.com/review/rocketleague.com" 

page = read_html(link) 

review = page %>% html_nodes(".styles_reviewContent__0Q2Tg") %>% html_text()  

date = page %>% html_nodes("time") %>% html_text()

user = page %>% html_nodes(".styles_consumerName__dP8Um") %>% html_text()

df = data.frame(date, user, review) 

# page 2 

link = "https://www.trustpilot.com/review/rocketleague.com?page=2" 

page = read_html(link) 

review = page %>% html_nodes(".styles_reviewContent__0Q2Tg") %>% html_text()  

date = page %>% html_nodes("time") %>% html_text()

user = page %>% html_nodes(".styles_consumerName__dP8Um") %>% html_text()

df2 = data.frame(date, user, review) 

# page 3 

link = "https://www.trustpilot.com/review/rocketleague.com?page=3" 

page = read_html(link) 

review = page %>% html_nodes(".styles_reviewContent__0Q2Tg") %>% html_text()  

date = page %>% html_nodes("time") %>% html_text()

user = page %>% html_nodes(".styles_consumerName__dP8Um") %>% html_text()

df3 = data.frame(date, user, review)  

# combine data frames 

df_com <- df %>%
  bind_rows(df2)

df_com <- df_com %>%
  bind_rows(df3)

df_com

# start text analysis

corpus <- iconv(df_com$review)
corpus <- Corpus(VectorSource(corpus))
inspect(corpus)


corpus <- tm_map(corpus, tolower)
inspect(corpus)

corpus <- tm_map(corpus, removePunctuation)
inspect(corpus)

corpus <- tm_map(corpus, removeNumbers)
inspect(corpus)

cleanset <- tm_map(corpus, removeWords, stopwords('english'))
inspect(corpus)

cleanset <- tm_map(cleanset, removeWords, c('aapl', 'apple'))
cleanset <- tm_map(cleanset, gsub,
                   pattern = 'stocks',
                   replacement = 'stock')
cleanset <- tm_map(cleanset, stemDocument)
cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])

# analysis

library(tidyverse)
library(tidytext)
library(RColorBrewer)
db_new <- unnest_tokens(tbl = df_com, input = review, output = word)

stp_wrds <- get_stopwords(source = "smart")

db_new <- anti_join(db_new, stp_wrds, by = "word")

bing <- get_sentiments(lexicon = "bing")

db_bing <- inner_join(db_new, bing, by = "word")

db_bing <- count(db_bing, date, user, word, sentiment)

db_bing <- spread(key = sentiment, value = n, fill = 0, data = db_bing)


db_bing <- mutate(sentiment = positive - negative, .data = db_bing)

mean(db_bing$sentiment, na.rm = TRUE)

ggplot(mapping = aes(x = user, y = sentiment, fill = sentiment)) +
  geom_col() 

sentiment_by_user <- db_bing %>%
  group_by(user) %>%
  summarize(user_sentiment = sum(sentiment)) %>%
  arrange(user_sentiment) %>%
  mutate(Value = case_when(user_sentiment > 0 ~ 'positive',
                           user_sentiment == 0 ~ 'neutral',
                           user_sentiment < 0 ~ 'negative')) %>%
  filter(Value == 'positive' | Value == 'negative')
  
sentiment_by_user %>% summarize(mean(user_sentiment))
    
sentiment_by_user %>% ggplot(aes(x = user, y = user_sentiment, fill = Value)) +
  geom_col() + theme_classic() + geom_hline(yintercept = 0, alpha = .7) + 
  labs(x = "User Reviews", y = "Sentiment Value", title = "Rocket League Reviews Sentiment Analysis") + geom_hline(yintercept = -2.68, linetype = 3)  + geom_label(aes(x = 33, y = -2.68,label = 'Mean Sentiment'), fill = "#f4f1de", show.legend = FALSE) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "right") 



db_bing %>%
  ggplot(aes(x = user, y = sentiment, fill = sentiment)) +
  geom_col(position = "stack") 


library(widyr) # for pairwise correlation 
library(igraph) # for data visualization
library(ggraph) # for data visualization

review_words <- df_com %>% 
  unnest_tokens(output = word, input = review) %>% 
  anti_join(stop_words, by = "word") %>% 
  filter(str_detect(word, "[:alpha:]")) %>% 
  distinct()


reviews_that_mention_word <- review_words %>% 
  count(word, name = "number_of_reviews") %>% 
  filter(number_of_reviews >= 7)


review_correlations <- review_words %>% 
  semi_join(reviews_that_mention_word, by = "word") %>% 
  pairwise_cor(item = word, feature = user) %>% 
  filter(correlation >= 0.01) 


graph_from_data_frame(d = review_correlations,
                      vertices = reviews_that_mention_word %>%
                        semi_join(review_correlations, by = c("word" = "item1"))) %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = correlation)) + 
  geom_node_point() +
  geom_node_text(aes(color = number_of_reviews, label = name), repel = TRUE, check_overlap = TRUE, size = 4) + 
  labs(title = "Rocket League Reviews") +
  scale_colour_gradientn(colours=c("#3e3b92", "#f44369")) +
  theme(panel.background = element_rect(fill = "#eaeaea"),
        plot.background = element_rect(fill = "#ffffff"))

