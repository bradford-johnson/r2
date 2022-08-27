# load packages
library(tidyverse) # the tidyverse meta package
library(lubridate) # wrangling dates and times
library(rvest) # for webscraping

link = "https://store.steampowered.com/stats/" # link to get data from

page = read_html(link) # read webpage at the above link

game = page %>% html_nodes(".gameLink") %>% html_text() # scrape top 100 games by current players

current_players = page %>% html_nodes("td:nth-child(1) .currentServers") %>% html_text() # scrape number of players for each game 

df = data.frame(current_players, game) # put both game and player data into a data frame


current_date <- as_datetime(Sys.Date()) # get current date

current_time <- timestamp() # create timestamp for when code is run 

df <- df %>% # update data fram with mutated columns that add current_date and current_time
  mutate(date = current_date) %>%
  mutate(time = current_time)

