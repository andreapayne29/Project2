## LOADING IN LIBRARIES
library(topicmodels)
library(tidytext)
library(lsa)
library(tidyr)
library(tidyverse)


### sort_answers_function ###

### Input: original tibble with questions as columns and individual answers as rows (DF)
### Output: a vector of total answers sorted by question

sort_answers_function <- function(df){
  ansVec = c()
  for(j in 1:dim(df)[2]){
    for (i in 1:dim(df)[1]){
      ansVec = c(ansVec, df[i,j])
    }
  }
  return(ansVec)
}


### top_words_function ###

### input: LDA output tibble with beta column
### output: tibble detailing each topic's top 10 words

top_words_function <- function(df){
  return(df %>%
           group_by(topic) %>%
           top_n(10, beta) %>%
           ungroup() %>%
           arrange(topic, -beta))
}

### plot_top_words_function ###
### input: a tibble with each topics top words
### output: horizontal bar plots

plot_top_words_function <- function(df){
  return(df %>% mutate(term = reorder_within(term, beta, topic)) %>%
           ggplot(aes(term, beta, fill = factor(topic))) +
           geom_col(show.legend = FALSE) +
           facet_wrap(~ topic, scales = "free")+
           coord_flip()+
           scale_x_reordered())
}



########### LOADING DATA ##############

#loading in data
survey_data = readxl::read_xlsx() #insert file path here

## selecting only comment box questions
survey_data = survey_data %>% select(c(14, 22, 30, 40, 41, 43, 45, 46, 48))



########## CLEANING DATA ############


## converting a tibble with questions as columns and an individual's answers as rows
## to a tibble with two columns labelled document and text where document is what question
## was asked and text is a response
## document is sorted by appearance on the survey

## vector with each question repeated nrow times
document_vector = c(rep(colnames(survey_data), each = dim(survey_data)[1]))

## a vector with all answers sorted by question
long_survey_answers = sort_answers_function(survey_data)

## converted DF with questions in one col and answers in the other 
long_survey_DF = tibble(document = document_vector, text = long_survey_answers)

## DF with each answer broken down by words and removed NA answers
## a two col matrix with questions in col 1 and individual words in col 2
by_word = long_survey_DF %>%
  unnest_tokens(word, text) %>%
  remove_missing()

## removing filler/stop words

## due to some responses being in french, french stopwords are added from the lsa library
stopwords_updated = stop_words %>% add_row(word = stopwords_fr, lexicon = "LSA")

## counting the presence of non stopwords
word_counts <- by_word %>%
  anti_join(stopwords_updated) %>%
  count(document, word, sort = TRUE) %>%
  ungroup()





########### LDA ANALYSIS ################


## casting to a document term matrix
survey_dtm <- word_counts %>%
  cast_dtm(document, word, n)



## running LDA with 9 topics
survey_lda_9 <- LDA(survey_dtm, k = 9)
## beta tibble
survey_topics_9 <- tidy(survey_lda_9, matrix = "beta")
## finding each topic's top words
survey_topwords_9 = topWords_function(survey_topics_9)
## plotting top words
plot_top_words_function(survey_topwords_9)



## running LDA with 4topics
survey_lda_4 <- LDA(survey_dtm, k = 4)
## beta tibble
survey_topics_4 <- tidy(survey_lda_4, matrix = "beta")
## finding each topic's top words
survey_topwords_4 = topWords_function(survey_topics_4)
## plotting top words
plot_top_words_function(survey_topwords_4)



## running LDA with 14 topics
survey_lda_14 <- LDA(survey_dtm, k = 14)
## beta tibble
survey_topics_14 <- tidy(survey_lda_14, matrix = "beta")
## finding each topic's top words
survey_topwords_14 = topWords_function(survey_topics_14)
## plotting top words
plot_top_words_function(survey_topwords_14)




### finding most different and most similar words using k=9

beta_spread = survey_topics_9 %>%
  mutate(topic = paste0("topic", topic))%>%
  pivot_wider(names_from = topic, values_from = beta) %>%
  filter(eval(parse(text=paste(paste0("topic",1:9,">.0001"),sep="|"))))%>%
  mutate(log_ratio21 = log2(topic2/topic1)) %>%
  mutate(log_ratio31 = log2(topic3/topic1)) %>%
  mutate(log_ratio41 = log2(topic4/topic1)) %>%
  mutate(log_ratio51 = log2(topic5/topic1)) %>%
  mutate(log_ratio61 = log2(topic6/topic1)) %>%
  mutate(log_ratio71 = log2(topic7/topic1)) %>%
  mutate(log_ratio81 = log2(topic8/topic1)) %>%
  mutate(log_ratio91 = log2(topic9/topic1))
#biggest difference
beta_spread %>%
  filter(log_ratio21 > 435 |log_ratio21 < -435) %>%
  mutate(log_ratio21 = sort(log_ratio21)) %>%
  ggplot(aes(term, log_ratio21))+
  geom_col(show.legend = FALSE)+
  coord_flip()+
  scale_x_reordered()
#most similar
beta_spread %>%
  filter(log_ratio21 < 0.40 & log_ratio21 > -0.40) %>%
  mutate(log_ratio21 = sort(log_ratio21)) %>%
  ggplot(aes(term, log_ratio21)) +
  geom_col(show.legend = FALSE) +
  coord_flip()+
  scale_x_reordered()


## word counts per question
word_counts %>% filter(document == document_vector[1]) %>%
  arrange(desc(n))
word_counts %>% filter(document == document_vector[(663*1)+1]) %>%
  arrange(desc(n))
word_counts %>% filter(document == document_vector[(663*2)+1]) %>%
  arrange(desc(n))
word_counts %>% filter(document == document_vector[(663*3)+1]) %>%
  arrange(desc(n))
word_counts %>% filter(document == document_vector[(663*4)+1]) %>%
  arrange(desc(n))
word_counts %>% filter(document == document_vector[(663*5)+1]) %>%
  arrange(desc(n))
word_counts %>% filter(document == document_vector[(663*6)+1]) %>%
  arrange(desc(n))
word_counts %>% filter(document == document_vector[(663*7)+1]) %>%
  arrange(desc(n))
word_counts %>% filter(document == document_vector[(663*8)+1]) %>%
  arrange(desc(n))

