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
View(tweets)

df <- tweets %>%
  filter(location == "Stanford")
ggplot(df, aes(x = created_at)) +
  geom_histogram(bins = 50) +
  labs(title="Stanford Tweets")