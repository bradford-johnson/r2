library(tidyverse)
library(tidytext)
library(gt)

seinfeld_df <- read_csv("seinfeld_imdb.csv")
 
db_new <- unnest_tokens(tbl = seinfeld_df, input = desc, output = word)

stp_wrds <- get_stopwords(source = "smart")

db_new <- anti_join(db_new, stp_wrds, by = "word")

bing <- get_sentiments(lexicon = "bing")

db_bing <- inner_join(db_new, bing, by = "word")

db_bing <- count(db_bing, season, episode_num, title, original_air_date, imdb_rating, total_votes, word, sentiment)

db_bing <- spread(key = sentiment, value = n, fill = 0, data = db_bing)


db_bing <- mutate(sentiment = positive - negative, .data = db_bing)

mean(db_bing$sentiment, na.rm = TRUE)

fun_color_range <- colorRampPalette(c("red", "green"))
my_colors2 <- fun_color_range(2)

db_sentiment <- db_bing %>% 
  mutate(value = case_when(sentiment > 0 ~ 'positive',
                           sentiment == 0 ~ 'neutral',
                           sentiment < 0 ~ 'negative')) %>%
  filter(value == 'positive' | value == 'negative')

db_bing %>%
  group_by(title, season) %>%
  summarise(original_air_date = max(original_air_date),Sentiment = sum(sentiment)) %>%
  ggplot(aes(x = title, y = Sentiment, fill = Sentiment)) +
  geom_col() +
  scale_fill_gradientn(colors = my_colors2) +
  facet_wrap(~season)
