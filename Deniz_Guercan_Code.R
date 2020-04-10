##############################
#Individual Assignment - Business Insight Report
#Deniz Guercan 
#MSBA3
##############################

library(tidyverse)
library(tidytext)
library(dplyr)
library(textreadr)
library(stringr)
library(twitteR)
library(tm)
library(wordcloud)
library(scales)
library(wordcloud2)
library(RColorBrewer)
library(reshape2)
library(igraph)
library(ggraph) 
library(plotly)
library(widyr)

#Importing the .txt file with the news, journals and articles (in the following as news)

setwd("/Users/deniz/Desktop/HULT_Stuff/DUAL DEGREE MBAN/SPRING/TEXT ANALYSIS")

#using read document to acces the file and store it as a vector
ndata <- read_document(file="/Users/deniz/Desktop/HULT_Stuff/DUAL DEGREE MBAN/SPRING/TEXT ANALYSIS/Shootings_text_data.txt")

#Transorming to a data frame
ndata <- data.frame(line=1:length(ndata),text=ndata, stringsAsFactors = FALSE)

################
#Accessing Twitter through TwitteR to acces tweets about shool schootings to compare news to social media
################
consumer_key <- 'FMG3XjyXuRkEiOH9C0Q275ZK0'
consumer_secret <- 'kUbLSFvIxNPquviM9dLv8hJR8ss9hxIM2MJ9FX0Y3FghGetkDL'
access_token <- '1217606322838855680-Al87L2vaDuwxLpsBQ9bTkbwTY1ZFQx'
access_secret <- '2PRbCDmmXjgQ8cSDSVdbkoQbg4R7Fo95WWljAjwlwpnu2'

shoosag <- twitteR::searchTwitter('school shooting', lang = 'en', n = 1500, since = '2000-06-01', retryOnRateLimit = 1e3)

View(shoosag)

ss = twitteR::twListToDF(shoosag)
ss <- as.data.frame(ss)

view(ss)
#clean the twitter data
ss$text <- gsub("https\\S*", "", ss$text) 
ss$text <- gsub("@\\S*", "", ss$text) 
ss$text <- gsub("amp", "", ss$text) 
ss$text <- gsub("[\r\n]", "", ss$text)
ss$text <- gsub("[[:punct:]]", "", ss$text)
ss$text <- gsub("http[^[:space:]]*","",  ss$text)
ss$text <- gsub("http[^[:space:]]*","", ss$text) 

#Create own stop_word list 
my_junk_twit <- data_frame(word=c("t.co", "1	o:it", "brianna", "ta", "msnbc", "tony", "cnns","trump","iqykldai5y", "top", "sumoh7", "ii", "iii", "i'm", "y'all'", "celestljade","like", "https", "rt", "shooting","shoot", "school", "im", "hey", "cus" ), lexicon="my_junk_twit")
my_junk_news <- data_frame(word=c("shooting","trump","shoot","na","gun","john", "guns", "school", "shootings"), lexicon= "my_junk_news")

#################
#Tokenizizing both news and twitter data
################

news_tokens <- ndata %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk_news) %>%
  count(word, sort=T) %>%
  ungroup()

view(news_tokens)

ss_tokens <- ss %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk_twit) %>%
  count(word, sort=T) %>%
  ungroup()

view(ss_tokens)

############
#inspect frequencies 
###########

#frequencies news

frequ_tokens_news <- ndata %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk_news) %>%
  count(word, sort=TRUE)
print(frequ_tokens_news)

frequ_tokens_news %>%
  filter(n > 50) %>%
  mutate(word = reorder(word, n, color = "blue")) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#frequencies twitter

frequ_tokens_twit <- ss %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk_twit) %>%
  count(word, sort=TRUE)
print(frequ_tokens_twit)

frequ_tokens_twit %>%
  filter(n > 70) %>%
  mutate(word = reorder(word, n, color = "blue")) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

######################
#Sentiment Analysis
######################

get_sentiments("bing")
get_sentiments("afinn")
get_sentiments("nrc")

#Sentiments - bing
news_senti <- ndata %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk_news) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

news_senti

news_senti %>%
  group_by(sentiment) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

twit_senti <- ss %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk_twit) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

twit_senti

twit_senti %>%
  group_by(sentiment) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

#Sentiment afinn

news_senti_num <- ndata %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk_news) %>%
  inner_join(get_sentiments("afinn")) %>%
  count(word, value, sort=T) %>%
  summarize(mean(value)) %>%
  ungroup()

print(news_senti_num)

twit_senti_num <- ss %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk_twit) %>%
  inner_join(get_sentiments("afinn")) %>%
  count(word, value, sort=T) %>%
  summarize(mean(value)) %>%
  ungroup()

print(twit_senti_num)

#NRC and Comparison cloud

news_senti_nrc <- ndata %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk_news)%>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

#Pizza
news_senti_nrc %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   title.colors=c("red","blue"),
                   max.words=100, fixed.asp=TRUE,
                   scale=c(0.8,0.8), title.size=2, rot.per=0.25)


twit_senti_nrc <- ss %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(my_junk_twit)%>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

twit_senti_nrc %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   title.colors=c("red","blue"),
                   max.words=100, fixed.asp=TRUE,
                   scale=c(0.8,0.8), title.size=2, rot.per=0.25)

bing_and_nrc <- bind_rows(
  news_tokens %>%
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  news_tokens %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", mutate(method = "NRC")) %>% "negative"))) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)


################
#Ngrams
################
news_ngrams <- ndata %>%
  unnest_tokens(bigram, text, token = "ngrams", n=3) %>%
  separate(bigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word1 %in% my_junk_news$word) %>%
  filter(!word2 %in% my_junk_news$word) %>%
  filter(!word3 %in% my_junk_news$word) %>%
  count(word1, word2, word3, sort = TRUE)

ggraph(slice(news_ngrams, 1:300), layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =0.0001, hjust=0.0001)

twit_ngram <- ss %>%
  unnest_tokens(bigram, text, token = "ngrams", n=3) %>%
  separate(bigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word1 %in% my_junk_twit$word) %>%
  filter(!word2 %in% my_junk_twit$word) %>%
  filter(!word3 %in% my_junk_twit$word) %>%
  count(word1, word2, word3, sort = TRUE)

ggraph(slice(twit_ngram, 1:300), layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =0.0001, hjust=0.0001)

#######
#Additional ngrams for testing purposes - not used in analysis
########
twit_ngrams
twit_senti_ngram <- twit_ngram %>%
  inner_join(get_sentiments('afinn'), by=c(word2="word")) %>%
  count(word1, word2, value, sort=TRUE) %>%
  summarise(mean(value)) %>%
  ungroup()

twit_senti_ngram 

set.seed(2017)
ggraph(twit_ngram, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


set.seed(2016)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(twit_ngram, layout = "fr") + 
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) + 
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) + 
  theme_void()

##############
#Trigramm barplots - not used because of NA values
##############
ndata %>% 
  unnest_tokens(word, text, token = "ngrams", n = 3) %>% 
  separate(word, c("word1", "word2", "word3"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word1 %in% my_junk_news$word) %>%
  filter(!word2 %in% my_junk_news$word) %>%
  filter(!word3 %in% my_junk_news$word)%>% 
  unite(word,word1, word2, word3, sep = " ") %>% 
  count(word, sort = TRUE) %>% 
  slice(1:10) %>% 
  mutate(word = reorder(word,n ))%>%
  ggplot() + geom_bar(aes(word, n), stat = "identity", fill = "#3344de") +
  theme_minimal() +
  coord_flip() +
  labs(title = "10 Most Used Trigrams affiliated with School Shootings",
       subtitle = "Based on News & Articles",
       caption = "")

ss %>% 
  unnest_tokens(word, text, token = "ngrams", n = 3) %>% 
  separate(word, c("word1", "word2", "word3"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word1 %in% my_junk_twit$word) %>%
  filter(!word2 %in% my_junk_twit$word) %>%
  filter(!word3 %in% my_junk_twit$word)%>% 
  unite(word,word1, word2, word3, sep = " ") %>% 
  count(word, sort = TRUE) %>% 
  slice(1:10) %>% 
  mutate(word = reorder(word,n ))%>%
  ggplot() + geom_bar(aes(word, n), stat = "identity", fill = "#3344de") +
  theme_minimal() +
  coord_flip() +
  labs(title = "10 Most Used Trigrams affiliated with School Shootings",
       subtitle = "Based on Twitter",
       caption = "")


################
#Wordclouds - not used because of redundance with prior sentiments
################

news_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"))

ss_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"))



###########################
#Correlogram - not used because of a lack of words
############################

frequ <- bind_rows(mutate(news_tokens, author="news"),
                       mutate(ss_tokens, author="twitter")) %>%
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion,`twitter`)%>%
  filter(proportion, 0.0001)


ggplot(frequ, aes(x=proportion, y=`news`, 
                      color = abs(`news`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=0.6, width=0.4, height=0.4)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=.4) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=3)+
  theme(legend.position = "none")+
  labs(y= "news", x=NULL)

