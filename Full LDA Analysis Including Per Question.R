## LOADING IN LIBRARIES
library(topicmodels)
library(tidytext)
library(lsa)
library(tidyr)
library(tidyverse)
library(tm)
library(topicmodels)
library(scales)


#### convert_to_dtm Function ####
## Input: 2x2 dataframe with questions and reponses - including NAs (df)
## Output: a 3 column dataframe with document, word, and n occurances of word in document
split_by_words_function <- function(df){
  
  by_word = df %>%
    unnest_tokens(word, text) %>%
    remove_missing()            
  
  word_counts <- by_word %>%
    anti_join(stop_words) %>%
    count(document, word, sort = TRUE) %>%
    ungroup()
  
  return(word_counts)
}


#### sparsity_correction_function ####
## Input: DTM
##        sparsity percentage with default = 0.95
##        3 col tibble with document, word, n (output from split_by_words_function)
## Output: sparsity corrected DTM
sparsity_correction_function <- function(dtm, sparcity = 0.95, word_counts){
  dtm =survey_dtm
  DTM_sc = removeSparseTerms(dtm, sparcity)
  DTM_sc_matrix = as.matrix(DTM_sc)
  Removethese = which(apply(DTM_sc_matrix, 1, function(x) {sum(x>0)<5|sum(x)<5}))
  
  FullDTM = word_counts %>% filter(!(word_counts$document %in% names(Removethese))) %>%
    cast_dtm(document, word, n)
  
  return(FullDTM)
}






########### LOADING AND CLEANING DATA ##############


#loading in data
survey_data = readxl::read_xlsx() #insert file here

#creating a vector to identify what question is being answered
questionsCol = rep(1:9, len = nrow(survey_data))

#adding question id
survey_data = survey_data %>% mutate(question = paste0("Question ", questionsCol))


#cleaning full survey answers to only have quesiton id (document) and response (text)
cleaned_df = survey_data %>% select(question, english) %>% transmute(document = question, text = english)

#removing NAs and breaking into words
word_counts = split_by_words_function(cleaned_df)

## converting to DTM
survey_dtm <- word_counts %>%
  cast_dtm(document, word, n)

##correcting for sparsity
FullDTM = sparsity_correction_function(survey_dtm, word_counts = word_counts)




##### Testing ideal number of clusters - unclear if necessary as we know the number of questions #####

TestSet = sort(sample(1:nrow(FullDTM), floor(nrow(FullDTM)*.70)))
DTM95Test = FullDTM[TestSet,]
DTM95Train = FullDTM[-TestSet,]

LDAout = NULL
perpl = NULL
for(k in 2:25){
  LDAout[[k]] = LDA(DTM95Train, k)
  perpl = rbind(perpl, c(k, perplexity(LDAout[[k]], newdata = DTM95Test, control = list(seed = 29))))
}

plot(perpl[,1], perpl[,2], main = "Perplexity") 


k = which(perpl == min(perpl[,2]), arr.ind = TRUE)[1] +1
Topics = tidy(LDAout[[k]], matrix = "beta")

Topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic)))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~ topic, scales = "free") + 
  coord_flip()+
  scale_x_reordered()




##### 9 Topic LDA #####
## All gamma visualization and analysis code comes from Chapter 6 of the textbook "Test Mining with R: A Tidy Approach" by Julia Silge and David Robinson
## https://www.tidytextmining.com/index.html


## Analysis ##

#running LDA
LDA_9_topics = LDA(FullDTM, k = 9, control = list(seed = 299))

#creating beta matrix for total document analysis
Topics = tidy(LDA_9_topics, matrix = "beta")
#creating gamma matrix for individual document analysis
LDA_gamma = tidy(LDA_9_topics, matrix = "gamma")

## determining which question assigns to what topic

## 3 col tibble with document, topic, and gamma value
question_classification <- LDA_gamma %>%
  group_by(document) %>%
  slice_max(gamma) %>%
  ungroup()

## 2 col tibble with docuement and topic
question_topics <- question_classification%>%
  count(document, topic) %>%
  group_by(document) %>% 
  slice_max(n, n= 1) %>%
  ungroup() %>%
  transmute(consensus = document, topic)






## Visualization


## top words per topic plot
Topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic)))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~ topic, scales = "free") + 
  coord_flip()+
  scale_x_reordered()
# related questions to topic for reference
print(question_topics)


## gamma correspondence with topic to question
LDA_gamma %>%
  mutate(title = reorder(document, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ document) +
  labs(x = "topic", y = expression(gamma))

## topics that were misclassified
question_classification %>%
  inner_join(question_topics, by = "topic") %>%
  filter(document != consensus)

## determining which words were assigned to which topic
assignments <- augment(LDA_9_topics, data = FullDTM)

## adding actual correspondence
assignments = assignments %>%
  inner_join(question_topics, by = c(".topic" = "topic"))


## a different way of visualizing misclassifications
assignments %>%
  count(document, consensus, wt = count) %>%
  mutate(across(c(document, consensus), ~str_wrap(., 20))) %>%
  group_by(document) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(consensus, document, fill = percent)) +
  geom_tile() +
  scale_fill_gradient2(high = "darkred", label = percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Question words were assigned to",
       y = "Question words came from",
       fill = "% of assignments")

## words that were misclassified and where
wrong_words <- assignments %>%
  filter(document != consensus)
wrong_words


#removing NAs and breaking into words
word_counts1 = split_by_words_function(question1)
word_counts2 = split_by_words_function(question2)
word_counts3 = split_by_words_function(question3)
word_counts4 = split_by_words_function(question4)
word_counts5 = split_by_words_function(question5)
word_counts6 = split_by_words_function(question6)
word_counts7 = split_by_words_function(question7)
word_counts8 = split_by_words_function(question8)
word_counts9 = split_by_words_function(question9)


## converting to DTM
survey_dtm1 <- word_counts1 %>%
  cast_dtm(document, word, n)
survey_dtm2 <- word_counts2 %>%
  cast_dtm(document, word, n)
survey_dtm3 <- word_counts3 %>%
  cast_dtm(document, word, n)
survey_dtm4 <- word_counts4 %>%
  cast_dtm(document, word, n)
survey_dtm5 <- word_counts5 %>%
  cast_dtm(document, word, n)
survey_dtm6 <- word_counts6 %>%
  cast_dtm(document, word, n)
survey_dtm7 <- word_counts7 %>%
  cast_dtm(document, word, n)
survey_dtm8 <- word_counts8 %>%
  cast_dtm(document, word, n)
survey_dtm9 <- word_counts9 %>%
  cast_dtm(document, word, n)

##correcting for sparsity
FullDTM1 = sparsity_correction_function(survey_dtm1, word_counts = word_counts1)
FullDTM2 = sparsity_correction_function(survey_dtm2, word_counts = word_counts2)
FullDTM3 = sparsity_correction_function(survey_dtm3, word_counts = word_counts3)
FullDTM4 = sparsity_correction_function(survey_dtm4, word_counts = word_counts4)
FullDTM5 = sparsity_correction_function(survey_dtm5, word_counts = word_counts5)
FullDTM6 = sparsity_correction_function(survey_dtm6, word_counts = word_counts6)
FullDTM7 = sparsity_correction_function(survey_dtm7, word_counts = word_counts7)
FullDTM8 = sparsity_correction_function(survey_dtm8, word_counts = word_counts8)
FullDTM9 = sparsity_correction_function(survey_dtm9, word_counts = word_counts9)


## LDA Analysis
lda1 = LDA(FullDTM1, k = 6, control = list(seed = 29))
lda2 = LDA(FullDTM2, k = 6, control = list(seed = 29))
lda3 = LDA(FullDTM3, k = 6, control = list(seed = 29))
lda4 = LDA(FullDTM4, k = 6, control = list(seed = 29))
lda5 = LDA(FullDTM5, k = 6, control = list(seed = 29))
lda6 = LDA(FullDTM6, k = 6, control = list(seed = 29))
lda7 = LDA(FullDTM7, k = 6, control = list(seed = 29))
lda8 = LDA(FullDTM8, k = 6, control = list(seed = 29))
lda9 = LDA(FullDTM9, k = 6, control = list(seed = 29))

## Beta Matrix
Topics1 = tidy(lda1, matrix = "beta")
Topics2 = tidy(lda2, matrix = "beta")
Topics3 = tidy(lda3, matrix = "beta")
Topics4 = tidy(lda4, matrix = "beta")
Topics5 = tidy(lda5, matrix = "beta")
Topics6 = tidy(lda6, matrix = "beta")
Topics7 = tidy(lda7, matrix = "beta")
Topics8 = tidy(lda8, matrix = "beta")
Topics9 = tidy(lda9, matrix = "beta")




Topics1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic)))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~ topic, scales = "free") + 
  coord_flip()+
  scale_x_reordered() +
  ggtitle("Question 1")

Topics2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic)))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~ topic, scales = "free") + 
  coord_flip()+
  scale_x_reordered() +
  ggtitle("Question 2")

Topics3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic)))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~ topic, scales = "free") + 
  coord_flip()+
  scale_x_reordered() +
  ggtitle("Question 3")

Topics4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic)))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~ topic, scales = "free") + 
  coord_flip()+
  scale_x_reordered() +
  ggtitle("Question 4")

Topics5 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic)))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~ topic, scales = "free") + 
  coord_flip()+
  scale_x_reordered() +
  ggtitle("Question 5")

Topics6 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic)))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~ topic, scales = "free") + 
  coord_flip()+
  scale_x_reordered() +
  ggtitle("Question 6")

Topics7 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic)))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~ topic, scales = "free") + 
  coord_flip()+
  scale_x_reordered() +
  ggtitle("Question 7")

Topics8 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic)))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~ topic, scales = "free") + 
  coord_flip()+
  scale_x_reordered() +
  ggtitle("Question 8")

Topics9 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic)))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~ topic, scales = "free") + 
  coord_flip()+
  scale_x_reordered() +
  ggtitle("Question 9")


