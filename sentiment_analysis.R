#Sentiment Analysis 1

# load required packages
library(tidyverse)
library(syuzhet)
library(tidytext)
library(textdata)

# import text dataset
df <- read.csv("test_File Name_new_new_new_new.csv", header=T)
text.df <- tibble(text = str_to_lower(df$text))

# analyze sentiments using the syuzhet package based on the NRC sentiment dictionary
emotions <- get_nrc_sentiment(text.df$text)
emo_bar <- colSums(emotions)
emo_sum <- data.frame(count=emo_bar, emotion=names(emo_bar))

# create a barplot showing the counts for each of eight different emotions and positive/negative rating
ggplot(emo_sum, aes(x = reorder(emotion, -count), y = count)) + 
  geom_bar(stat = 'identity') 


# sentiment analysis with the tidytext package using the "bing" lexicon
bing_word_counts <- text.df %>% unnest_tokens(output = word, input = text) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)

write.csv(bing_word_counts,"Bing_WordCount.csv", row.names = FALSE)

# select top 10 words by sentiment
bing_top_10_words_by_sentiment <- bing_word_counts %>% 
  group_by(sentiment) %>% 
  slice_max(order_by = n, n = 10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) 
bing_top_10_words_by_sentiment

# create a barplot showing contribution of words to sentiment
bing_top_10_words_by_sentiment %>% 
  ggplot(aes(word, n, fill = sentiment)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = "free_y") + 
  labs(y = "Contribution to sentiment", x = NULL) + 
  coord_flip() 


# sentiment analysis with the tidytext package using the "loughran" lexicon
loughran_word_counts <- text.df %>% unnest_tokens(output = word, input = text) %>%
  inner_join(get_sentiments("loughran")) %>%
  count(word, sentiment, sort = TRUE) 

# select top 10 words by sentiment
loughran_top_10_words_by_sentiment <- loughran_word_counts %>% 
  group_by(sentiment) %>% 
  slice_max(order_by = n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n))
loughran_top_10_words_by_sentiment

# create a barplot showing contribution of words to sentiment
loughran_top_10_words_by_sentiment %>% 
  ggplot(aes(word, n, fill = sentiment)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = "free_y") + 
  labs(y = "Contribution to sentiment", x = NULL) + 
  coord_flip()


# Sentiment Analysis 2

# load required packages
library(tidyverse)
library(syuzhet)
library(tidytext)
library(textdata)
library(ggplot2)
library(dplyr)
library(tm)

# import text dataset
df <- read.csv("test_File Name_new_new_new_new.csv", header=T)
text.df <- tibble(text = str_to_lower(df$text))
Senti_Corpus <- Corpus(VectorSource(text.df))
Senti_DTM <- DocumentTermMatrix(Senti_Corpus)
Senti_td <- tidy(Senti_DTM)

# Access the Lexicons

get_sentiment("afinn") #retrieve the affin lexicon which scoes words from -5 to 5
get_sentiment("bing") #classify words as +ve or -ve
get_sentiment("nrc") #classify words as sadness, anger, fear, etc.

# Examine sentiments using the bing lexicon
Review_sentiments_bing <- Senti_td %>%
  inner_join(get_sentiments("bing"), by = c(term = "word"))

Review_sentiments_affin <- Senti_td %>%
  inner_join(get_sentiments("afinn"), by = c(term = "word"))

names(Senti_td)[2] <- 'word' #rename the term column to word

#Filtering the negative words from the nrc lexicon
nrcneg <- get_sentiments("nrc") %>%
  filter(sentiment == "negative")
Senti_NRC_Neg <- Senti_td %>%
  inner_join(nrcneg) %>%
  count(word, sort = TRUE)

#Filtering the negative words from the nrc lexicon
nrcfear <- get_sentiments("nrc") %>%
  filter(sentiment == "fear")
Senti_NRC_Fear <- Senti_td %>%
  inner_join(nrcneg) %>%
  count(word, sort = TRUE)

#Filtering the positive words from the nrc lexicon
nrcpos <- get_sentiments("nrc") %>%
  filter(sentiment == "positive")
Senti_NRC_Pos <- Senti_td %>%
  inner_join(nrcpos) %>%
  count(word, sort = TRUE)
 
