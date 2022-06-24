library(readr)
library(tidyverse)
library(gghighlight)
##########Desktop Import##############
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
                          col_types = cols(date = col_date(format = "%Y-%m-%d")))

yearly_averages <- read_csv("C:/Users/Wil/Desktop/GetOldTweets/TwitterSP2022/Collegiate_News_and_Sexual_Misconduct/Analyses/yearly_averages.csv", 
                            col_types = cols(...1 = col_skip(), date = col_date(format = "%Y-%m-%d")))

##########Laptop Import##############
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
                   col_types = cols(date = col_date(format = "%Y-%m-%d")))

yearly_averages <- read_csv("C:/Users/wilmd/OneDrive/Desktop/python_env/env/Collegiate_News_and_Sexual_Misconduct/Analyses/yearly_averages.csv", 
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
require(lubridate)
half_life_articles <- by_day %>%
  .$date %>% 
  unique %>% 
  map_dfr(~ {
    by_day %>% 
      filter(date <= .x) %>% 
      group_by(location) %>% 
      mutate(elapse = as.numeric(.x - date),
             weighted = article_count*exp(1)^-(.05*elapse)) %>% 
      summarise(hl_articles = sum(weighted)) %>%
      mutate(date = .x) %>%
      ungroup
  })

by_day <- merge(half_life_articles, by_day, by = (c("location", "date")))
####
require(lubridate)
half_life_tweets <- by_day %>%
  .$date %>% 
  unique %>% 
  map_dfr(~ {
    by_day %>% 
      filter(date <= .x) %>% 
      group_by(location) %>% 
      mutate(elapse = as.numeric(.x - date),
             weighted = tweet_count*exp(1)^-(.231*elapse)) %>% 
      summarise(hl_tweets = sum(weighted)) %>%
      mutate(date = .x) %>%
      ungroup
  })

by_day <- merge(half_life_tweets, by_day, by = (c("location", "date")))
write.csv(by_day,"C:/Users/Wil/Desktop/GetOldTweets/TwitterSP2022/Collegiate_News_and_Sexual_Misconduct/Analyses/counts_by_day.csv", row.names = FALSE)

####

effects_decay <- by_day %>%
  filter(location == "Wisconsin") %>%
  filter(date > "2011-01-01")

ggplot(effects_decay, aes(hl_articles, tweet_count)) +
  geom_smooth() +
  theme_classic() +
  labs(x = "Collegiate News Coverage (Effects Decay Value)", y = "Twitter Coverage", title = "Wisconsin", caption = "(p-value: 8.631e-16, cor: 0.131)")

cor.test(effects_decay$hl_articles, effects_decay$tweet_count)

###########Seasonality################

averages <- yearly_averages %>%
  filter(location == "Stanford") %>%
  mutate(average_tweets = average_tweets*13) %>%
  mutate(hl_tweets = hl_tweets*5)

###
ggplot(averages, aes(date, average_tweets, colour = "Tweets")) + 
  ###
  scale_colour_manual("", breaks = c("Tweets", "Collegiate News Coverage"),
                      values = c("Tweets" = "red", "Collegiate News Coverage" = "blue")) +
  
  geom_smooth() + labs(title="Stanford Averages", y = "Averaged Values", x = "Date")+
  geom_line()+
  ###
  geom_smooth(data = averages, aes(date, hl_articles, colour = "Collegiate News Coverage"))+
  geom_line(data = averages, aes(date, hl_articles, colour = "Collegiate News Coverage"))+
  ###
  geom_vline(xintercept = as.numeric(as.Date("1999-06-01")), lwd = 1)+
  geom_vline(xintercept = as.numeric(as.Date("1999-09-08")), lwd = 1)+
  theme_classic() +
  scale_x_date(date_labels = "%b")+
  ylim(0, 15)

###########WSS Cluster Visualization################
num_clusters <- c(0.01, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
Sum_of_Squares <- c(0.01, 1021, 1370, 1585, 1745, 1901, 1946, 2008, 2214, 2155, 2348, 2396, 2313, 2503, 2569, 2437, 2650, 2678, 2713, 2763, 2794)
gwss <- data.frame(num_clusters, Sum_of_Squares)

num_clusters <- c(0.01, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
Sum_of_Squares <- c(0.01, 7649, 8052, 8279, 8466, 8581, 8808, 8877, 8874, 8983, 9127, 9094, 9140, 9226, 9261, 9342, 9384, 9372, 9430, 9455, 9491)
twss <- data.frame(num_clusters, Sum_of_Squares)

ggplot(gwss, aes(x = num_clusters, y = Sum_of_Squares)) +
  geom_point()+
  geom_smooth(se = FALSE, method = "gam", formula = y ~ s(log(x))) +
  labs(title = "Google News Pre-trained Model Sum of Squares", x = "Number of Clusters", y = "Sum of Squares")+
  theme_classic()

###########Distortion Cluster Visualization################
num_clusters <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
distortion <- c(3.0379713360664553, 3.0170742255780856, 2.9801884845949043, 2.961491589434526, 2.9472396914592554, 2.9268935978546744, 2.927842481065104, 2.9084443192121374, 2.9132238202398453, 2.9070917848259747, 2.9059254002771073, 2.8799674275734604, 2.8756945973247694, 2.875552147043596, 2.8650273537577755, 2.860946899827552, 2.855750244047214, 2.8530527157471117, 2.8465325259370124, 2.846556450925078)
gdist <- data.frame(num_clusters, distortion)

num_clusters <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
distortion <- c(1.3818723503921926, 1.3230707463690499, 1.160777385497667, 1.127530934748275, 1.1097885676229928, 1.0816233796562393, 1.0712537155780608, 1.0612657653071584, 1.0502491944079542, 1.0437309817714397, 1.0303826728128485, 1.0209542996268537, 1.0135153232955758, 1.0063790883436299, 1.0079600150110013, 0.9847645899185425, 0.983939113515407, 0.9758949738318188, 0.9718707517124919, 0.9620019363638453)
tdist <- data.frame(num_clusters, distortion)

ggplot(gdist, aes(x = num_clusters, y = distortion)) +
  geom_point()+
  geom_smooth(se = FALSE, method = "gam", formula = y ~ s(log(x))) +
  labs(title = "Google News Pre-trained Model Distortion", x = "Number of Clusters", y = "Distortion")+
  theme_classic()

###########Coherence Topic Visualization################
num_topics <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39)
coherence <- c(0.3201158182889358,
               0.3790325209874615,
               0.3710416963143046,
               0.33374569238383345,
               0.3886366105043175,
               0.36453820412799137,
               0.3791220873199293,
               0.3727714668031806,
               0.39832114734486596,
               0.39839163422759183,
               0.3833009113960846,
               0.37289709096124596,
               0.3895807344479638,
               0.37447219440319135,
               0.38517280895149847,
               0.37923962117280074,
               0.3775245456317511,
               0.38245670411852667,
               0.3938980780542207,
               0.3724035519617704,
               0.360581829142865,
               0.38022464229826364,
               0.3717927295253379,
               0.3791448077910565,
               0.38967152753394063,
               0.3840436454085743,
               0.3565104272238512,
               0.3688240014413574,
               0.37116555702081894,
               0.37012673063418744,
               0.37524578020969535,
               0.38077108822202443,
               0.3727897799178545,
               0.36828127253793586,
               0.3943330241410956,
               0.3705832790683772,
               0.3973410374835486,
               0.38100953705536345)
clist <- data.frame(num_topics, coherence)

ggplot(clist, aes(x = num_topics, y = coherence)) +
  geom_point()+
  geom_line(se = FALSE, method = "gam") +
  labs(title = "LDA Num of Topics Coherence", x = "Number of Topics", y = "Coherence")+
  theme_classic()