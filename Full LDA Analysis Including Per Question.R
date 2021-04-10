## LOADING IN LIBRARIES
library(topicmodels)
library(tidytext)
library(lsa)
library(tidyr)
library(tidyverse)
library(tm)
library(topicmodels)
library(scales)


########### FUNCTIONS ##############

#### cleaning_function ####
## input: full response data
## output: responses to a single question and uniquely numbered
cleaning_function <- function(data){
  data_cleaned = data %>% select(question, english) %>% 
    transmute(document = question, text = english) %>% 
    mutate(document = 1:663)
}


#### split_by_words Function ####
## Input: 2x2 dataframe with questions and responses - including NAs (df)
## Output: a 2 column dataframe with document, word, and n occurrences of word in document
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
  DTM_sc = dtm #was removeSparseTerms(dtm, sparcity) but that messes up any data
  DTM_sc_matrix = as.matrix(DTM_sc)
  Removethese = which(apply(DTM_sc_matrix, 1, function(x) {sum(x>0)<5|sum(x)<5}))
  
  FullDTM = word_counts %>% filter(!(word_counts$document %in% names(Removethese))) %>%
    cast_dtm(document, word, n)
  
  return(FullDTM)
}

#### LDA_prep_function ####
## prepares a DTM for LDA analysis
## Input: cleaned dataframe with two cols: question and response
## Output: a DTM of fully cleaned responses
## Nested funtions: split_by_words and sparsity_correction_function
LDA_prep_function <- function(cleaned){
  #removing NAs and breaking into words
  word_counts = split_by_words_function(cleaned)
  
  ## converting to DTM
  dtm = word_counts %>% cast_dtm(document, word, n)
  
  ##correcting for sparsity
  FullDTM = sparsity_correction_function(dtm, word_counts = word_counts)
  
  return(FullDTM)
}



##### LDA_function ######
## Input: topic values to test from kmin to kmax, testing and training DTMs 
## Output: a plot of perplexity and returns the LDA analysis at ideal k
LDA_function <- function(kmin=2, kmax, testingDTM, trainingDTM, questionNumber){
  LDAout = NULL
  perpl = NULL
  for(k in 1:kmax){
    LDAout[[k]] = LDA(trainingDTM, k+1)
    perpl = rbind(perpl, c(k+1, perplexity(LDAout[[k]], newdata = testingDTM, control = list(seed = 29))))
  }
  plot(perpl[,1], perpl[,2], main = paste0("Perplexity, ", questionNumber))
  idealK = which.min(perpl[,2])
  return(LDAout[[idealK]])
  
}

#### topic_plotting_function ####
## input: a beta matrix from LDA analysis
## output: topics plot
topic_plotting_function <- function(betaMatrix, n){
  betaMatrix %>%
    group_by(topic) %>%
    top_n(n, beta) %>%
    ungroup() %>%
    arrange(topic, -beta) %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(term, beta, fill = factor(topic)))+
    geom_col(show.legend = FALSE)+
    facet_wrap(~ topic, scales = "free") + 
    coord_flip()+
    scale_x_reordered()
}


##### individual_question_LDA #####
## Input: which question number
##        original data, 
##        min for LDA analysis, max for LDA analysis, 
##        testing/training split, 
##        number of terms to plot
## Output: LDA perplexity plot and topic plot from ideal k LDA analysis

individual_question_LDA <- function(questionNumber, original_data = survey_data, kmin=2, kmax=10, testingSplit=0.9, n=10){
  # aquiring data
  data = original_data %>% filter(question == paste0("Question ", questionNumber))
  # cleaning
  cleaned = cleaning_function(data)
  # convert to DTM
  FullDTM = LDA_prep_function(cleaned)
  
  ## finding ideal number of topics
  # determining test/training data
  TestSet = sort(sample(1:nrow(FullDTM), floor(nrow(FullDTM)*testingSplit)))
  DTMTest = FullDTM[TestSet,]
  DTMTrain = FullDTM[-TestSet,]
  
  # running LDA
  LDA = LDA_function(kmin = kmin, kmax = kmax, testingDTM = DTMTest, trainingDTM = DTMTrain, questionNumber = questionNumber)
  # creating beta matrix
  LDATopics = tidy(LDA, matrix = "beta")
  # plotting
  topic_plotting_function(LDATopics, n = n)
  
}





########### LOADING AND CLEANING DATA ##############


# loading in data
survey_data = readxl::read_xlsx() #insert file path here
# creating a vector to identify what question is being answered
questionsCol = rep(1:9, len = nrow(survey_data))
# adding question id
survey_data = survey_data %>% mutate(question = paste0("Question ", questionsCol))




#### Total LDA Analysis ####

#cleaning full survey answers to only have question id (document) and response (text)
cleaned_df = survey_data %>% select(question, english) %>% transmute(document = question, text = english)
#removing NAs and breaking into words
FullDTM = LDA_prep_function(cleaned_df)


##### 'Ideal' Number of Topics #####
TestSet = sort(sample(1:nrow(FullDTM), floor(nrow(FullDTM)*.70)))
DTMTest = FullDTM[TestSet,]
DTMTrain = FullDTM[-TestSet,]
LDA = LDA_function(kmin = 2, kmax = 25, testingDTM = DTMTest, trainingDTM = DTMTrain, questionNumber = "Total")
# creating beta matrix
LDATopics = tidy(LDA, matrix = "beta")
# plotting
topic_plotting_function(LDATopics, n = 10)



##### 9 Topic LDA #####


## All gamma visualization and analysis code comes from Chapter 6 of the textbook "Test Mining with R: A Tidy Approach" by Julia Silge and David Robinson
## https://www.tidytextmining.com/index.html

#running LDA
LDA_9_topics = LDA(FullDTM, k = 9, control = list(seed = 29))

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

## 2 col tibble with document and topic
question_topics <- question_classification %>%
  count(document, topic) %>%
  group_by(document) %>% 
  slice_max(n, n= 1) %>%
  ungroup() %>%
  transmute(consensus = document, topic)


## Visualization
## top words per topic plot
topic_plotting_function(Topics, n = 10)
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






##### Individual Quesiton LDA #####

individual_question_LDA(1)
individual_question_LDA(2)
individual_question_LDA(3)
individual_question_LDA(4)
individual_question_LDA(5)
individual_question_LDA(6)
individual_question_LDA(7)
individual_question_LDA(8)
individual_question_LDA(9)

