library(tidyverse)
library(rvest)
library(tidytext)
library(sentimentr)
library(tidytuesdayR)
library(stopwords)


tuesdata <- tt_load(2020, week = 40)

tuesdata


beyonce<- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/beyonce_lyrics.csv')
taylor <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/taylor_swift_lyrics.csv')
sales <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/sales.csv')
charts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/charts.csv')

taylor

get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")


sentiment <- sentiment(taylor$Lyrics)
mean(sentiment$sentiment)


taylor %>% 
  group_by(Album) %>%
  summarise(n = n()) %>%
  ggplot(aes(Album, n, fill = Album)) +
  geom_bar(stat = "identity") +
  ylab("") + xlab("") +
  ggtitle("Number songs per Album") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(size = 18)) +
  geom_text(aes(label = n, y = n + 1), size = 8)



words <- taylor %>%   
  unnest_tokens(input = Lyrics,
                output = word) %>%
  #filter(!word %in% to_remove) %>%
  count(Album, word, sort = TRUE)

words %>% 
  group_by(word) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

table(words$word)


to_remove <- c(stopwords(language = "English"),
               "media",
               "omitted","and","this","that","was","him","he", 
               "deleted", "Message", "https", "www.instagram.com",
               "youtu.be", "i", "you", "the", "we", "to",  "me", "my", 
               "oh", "it", "a", "to")




taylor %>%   
  unnest_tokens(input = Lyrics,
                output = word) %>%
  filter(!word %in% to_remove) %>%
  count(Album, word, sort = TRUE) %>%
  group_by(Album) %>%
  top_n(n = 6, n) %>%
  ggplot(aes(x = reorder_within(word, n, Album), y = n, fill = Album)) +
  geom_col(show.legend = FALSE) +
  ylab("") +
  xlab("") +
  coord_flip() +
  facet_wrap(~Album, ncol = 5, scales = "free_y") +
  scale_x_reordered() +
  ggtitle("Most often used words by album")+
  theme_minimal()


album <- taylor %>%   
  unnest_tokens(input = Lyrics,
                output = word) %>%
  filter(!word %in% to_remove) %>%
  count(Album, word, sort = TRUE)

album_data <- split(album, album$Album)

  am1 <- sentiment(album_data$`1989`$word)
mean(am1$sentiment)

am2 <- sentiment(album_data$Fearless$word)
mean(am2$sentiment)

am3 <- sentiment(album_data$folklore$word)
am4 <- sentiment(album_data$Lover$word)
am5 <- sentiment(album_data$Red$word)
am6 <- sentiment(album_data$reputation$word)
am7 <- sentiment(album_data$`Speak Now`$word)
am8 <- sentiment(album_data$`Taylor Swift`$word)

mean(am1$sentiment)
mean(am2$sentiment)
mean(am3$sentiment)
mean(am4$sentiment)
mean(am5$sentiment)
mean(am6$sentiment)
mean(am7$sentiment)
mean(am8$sentiment)
