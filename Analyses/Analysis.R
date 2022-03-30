library(readr)
library(tidyverse)
tweets <- read_csv("C:/Users/wilmd/OneDrive/Desktop/python_env/env/Collegiate_News_and_Sexual_Misconduct/Tweets/tweets_cleaned.csv", 
                           col_types = cols(X1 = col_skip(), `Unnamed: 0` = col_skip(), 
                                            geo = col_skip(), id = col_character(), 
                                            public_metrics = col_skip(), author_id = col_character(), 
                                            conversation_id = col_character(), 
                                            entities = col_skip(), possibly_sensitive = col_skip(), 
                                            created_at = col_date(format = "%m/%d/%Y"), 
                                            context_annotations = col_skip(), 
                                            in_reply_to_user_id = col_character(), 
                                            lang = col_skip(),
                                            attachments = col_skip()))
articles <- read_csv("C:/Users/wilmd/OneDrive/Desktop/python_env/env/Collegiate_News_and_Sexual_Misconduct/Articles/articles_cleaned.csv", 
                            col_types = cols(X1 = col_skip(), 
                                             date = col_date(format = "%m/%d/%Y")))
by_day <- read_csv("C:/Users/wilmd/OneDrive/Desktop/python_env/env/Collegiate_News_and_Sexual_Misconduct/Analyses/counts_by_day.csv", 
                          col_types = cols(date = col_date(format = "%m/%d/%Y")))

View(tweets)
View(articles)
View(by_day)

#########################
#Histograms
#########################
df <- tweets %>%
  filter(location == "Wisconsin")
ggplot(df, aes(x = created_at)) +
  geom_histogram(bins = 50) +
  labs(title="Wisconsin Tweets")

df <- articles %>%
  filter(location == "UMD") %>%
  filter(date > 1990)
ggplot(df, aes(x = date)) +
  geom_histogram(bins = 50) +
  labs(title="UMD Articles")

###########################
#counts by day
###########################

counts <- by_day %>%
  filter(location == "Stanford") %>%
  filter(date > "2010-01-01")

ggplot(counts, aes(date, tweet_count, colour = "Tweets")) + 
  scale_colour_manual("", breaks = c("Tweets", "Articles"),
                      values = c("Tweets" = "blue", "Articles" = "red")) +
  geom_smooth() +
  geom_smooth(data = counts, aes(date, article_count, colour = "Articles"))