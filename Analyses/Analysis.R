library(readr)
library(tidyverse)
library(gghighlight)
tweets <- read_csv("C:/Users/Wil/Desktop/GetOldTweets/TwitterSP2022/Collegiate_News_and_Sexual_Misconduct/Tweets/tweets_cleaned.csv", 
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
articles <- read_csv("C:/Users/Wil/Desktop/GetOldTweets/TwitterSP2022/Collegiate_News_and_Sexual_Misconduct/Articles/articles_cleaned.csv", 
                            col_types = cols(X1 = col_skip(), 
                                             date = col_date(format = "%m/%d/%Y")))
by_day <- read_csv("C:/Users/Wil/Desktop/GetOldTweets/TwitterSP2022/Collegiate_News_and_Sexual_Misconduct/Analyses/counts_by_day.csv", 
                          col_types = cols(date = col_date(format = "%m/%d/%Y")))

yearly_averages <- read_csv("C:/Users/Wil/Desktop/GetOldTweets/TwitterSP2022/Collegiate_News_and_Sexual_Misconduct/Analyses/yearly_averages.csv", 
                            col_types = cols(...1 = col_skip(), date = col_date(format = "%Y-%m-%d")))
View(yearly_averages)
View(tweets)
View(articles)
View(by_day)

############Histograms#############

df <- tweets %>%
  filter(location == "Stanford")
ggplot(df, aes(x = created_at)) +
  geom_histogram(bins = 110) +
  ylim(0, 1000) +
  labs(title="Stanford Tweets", y = "Count", x = "Date") +
  theme_classic()

df <- articles %>%
  filter(location == "UMD") %>%
  filter(date > 1990)
ggplot(df, aes(x = date)) +
  geom_histogram(bins = 50) +
  labs(title="UMD Articles")

###########Daily Counts################


counts <- by_day %>%
  filter(location == "IUB") %>%
  filter(date > "2011-01-01")

ggplot(counts, aes(date, tweet_count, colour = "Tweets")) + 
  scale_colour_manual("", breaks = c("Tweets", "Articles"),
                      values = c("Tweets" = "blue", "Articles" = "red")) +
  geom_smooth() +
  geom_smooth(data = counts, aes(date, article_count, colour = "Articles"))+
  labs(title="IUB") +
  ylim(0,NA)


###########Effects Decay Correlations################
effects_decay <- by_day %>%
  filter(location == "UMD") %>%
  filter(date > "2011-01-01")

ggplot(effects_decay, aes(article_coverage, tweet_coverage)) +
  geom_smooth() +
  theme_classic() +
  labs(x = "Collegiate News Coverage (Effects Decay Value)", y = "Tweet Coverage", title = "UMD", caption = "(p-value: 4.467e-05, cor: 0.06686)")

cor.test(effects_decay$article_coverage, effects_decay$tweet_coverage)


###########Seasonality################

averages <- yearly_averages %>%
  filter(location == "IUB") %>%
  mutate(average_tweets = average_tweets*7)
  

ggplot(averages, aes(date, average_tweets, colour = "Tweets")) + 
  scale_colour_manual("", breaks = c("Tweets", "Collegiate News Coverage"),
                      values = c("Tweets" = "red", "Collegiate News Coverage" = "blue")) + 
  geom_smooth() + labs(title="IUB Averages", y = "Averaged Values", x = "Date")+
  geom_smooth(data = averages, aes(date, coverage, colour = "Collegiate News Coverage"))+
  geom_vline(xintercept = as.numeric(as.Date("1999-05-07")), lwd = 1)+
  geom_vline(xintercept = as.numeric(as.Date("1999-08-20")), lwd = 1)+
  theme_classic()+
  scale_x_date(date_labels = "%b")

