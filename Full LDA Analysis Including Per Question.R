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
  data_cleaned = data %>% select(c_1, question, english) %>% 
    transmute(document = c_1, text = english)
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
  #Removethese = which(apply(DTM_sc_matrix, 1, function(x) {sum(x>0)<5|sum(x)<5}))
  
  FullDTM = word_counts %>% #filter(!(word_counts$document %in% names(Removethese))) %>%
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
  plot(perpl[,1], perpl[,2], main = paste0("Perplexity, Question ", questionNumber))
  idealK = which.min(perpl[,2])
  return(idealK+1)
  
}

#### topic_plotting_function ####
## input: a beta matrix from LDA analysis
## output: topics plot
topic_plotting_function <- function(LDA, n, questionNumber){
  betaMatrix = tidy(LDA, matrix = "beta")
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
    scale_x_reordered()+
    ggtitle(paste0("Topics for Question ", questionNumber))
}


##### individual_question_LDA #####
## Input: which question number
##        original data, 
##        min for LDA analysis, max for LDA analysis, 
##        testing/training split, 
##        number of terms to plot
## Output: LDA perplexity plot and topic plot from ideal k LDA analysis

individual_question_LDA <- function(questionNumber, original_data = survey_data, kmin=2, kmax=10, testingSplit=0.9){
  # acquiring data
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
  k = LDA_function(kmin = kmin, kmax = kmax, testingDTM = DTMTest, trainingDTM = DTMTrain, questionNumber = questionNumber)
  LDA = LDA(FullDTM, k)
  return(LDA)
}


##### classify_responses #####
### Input: LDA
### Output: 3 col tibble with response ID, associated topic, and gamma value
classify_responses <- function(LDA){
  #creating a gamma matrix
  LDA_gamma = tidy(LDA, matrix = "gamma")
  ## determining which answer assigns to what topic
  question_classification <- LDA_gamma %>%
    group_by(document) %>%
    slice_max(gamma) %>%
    ungroup()
  
  return(question_classification)
}

##### determine_missing_ans #####
#### Due to cleaning, some responses are lost. This is to manually check the missing answers
#### to see if any answer of value is lost (i.e. anything more informative that "no"/"n/a" etc.)
## Input: question classification tibble, question number, and cleaned survey_data
## Output: response IDs that are not present in topic analysis
determine_missing_ans <- function(question_classification, questionNumber, survey_data){
  question_filter = as.character(paste0("Question ", questionNumber))
  final_responses = question_classification$document
  actual_responses = survey_data  %>%filter(question == question_filter)
  missing_values = setdiff(actual_responses$c_1, final_responses)
  return(missing_values)                                  
}



########### LOADING AND CLEANING DATA ##############


# loading in data
survey_data = readxl::read_xlsx("masterResponse.xlsx")
# creating a vector to identify what question is being answered
questionsCol = rep(1:9, len = nrow(survey_data))
# adding question id
survey_data = survey_data %>% mutate(question = paste0("Question ", questionsCol))





##### Individual Question LDA #####

question1 = individual_question_LDA(1)
topic_plotting_function(question1, n = 10, questionNumber = 1)
question1 = classify_responses(question1)

question2 = individual_question_LDA(2)
topic_plotting_function(question2, n = 10, questionNumber = 2)
question2 = classify_responses(question2)

question3 = individual_question_LDA(3)
topic_plotting_function(question3, n = 10, questionNumber = 3)
question3 = classify_responses(question3)

question4 = individual_question_LDA(4)
topic_plotting_function(question4, n = 10, questionNumber = 4)
question4 = classify_responses(question4)

question5 = individual_question_LDA(5)
topic_plotting_function(question5, n = 10, questionNumber = 5)
question5 = classify_responses(question5)

question6 = individual_question_LDA(6)
topic_plotting_function(question6, n = 10, questionNumber = 6)
question6 = classify_responses(question6)

question7 = individual_question_LDA(7)
topic_plotting_function(question7, n = 10, questionNumber = 7)
question7 = classify_responses(question7)

question8 = individual_question_LDA(8)
topic_plotting_function(question8, n = 10, questionNumber = 8)
question8 = classify_responses(question8)

question9 = individual_question_LDA(9)
topic_plotting_function(question9, n = 10, questionNumber = 9)
question9 = classify_responses(question9)


## writing CSVs for further analysis with response ID/topic association
write.csv(question1,file="Question 1 Response Topics LDA.csv", row.names = FALSE)
write.csv(question2,file="Question 2 Response Topics LDA.csv", row.names = FALSE)
write.csv(question3,file="Question 3 Response Topics LDA.csv", row.names = FALSE)
write.csv(question4,file="Question 4 Response Topics LDA.csv", row.names = FALSE)
write.csv(question5,file="Question 5 Response Topics LDA.csv", row.names = FALSE)
write.csv(question6,file="Question 6 Response Topics LDA.csv", row.names = FALSE)
write.csv(question7,file="Question 7 Response Topics LDA.csv", row.names = FALSE)
write.csv(question8,file="Question 8 Response Topics LDA.csv", row.names = FALSE)
write.csv(question9,file="Question 9 Response Topics LDA.csv", row.names = FALSE)



#removing NAs from initial dataset
removed_missing_sd = survey_data %>% remove_missing(vars = "english")

#finding answers that were lost to cleaning (ex. no, n/a, nope, etc.)
missing1 = determine_missing_ans(question1, 1, removed_missing_sd)
missing2 = determine_missing_ans(question2, 2, removed_missing_sd)
missing3 = determine_missing_ans(question3, 3, removed_missing_sd)
missing4 = determine_missing_ans(question4, 4, removed_missing_sd)
missing5 = determine_missing_ans(question5, 5, removed_missing_sd)
missing6 = determine_missing_ans(question6, 6, removed_missing_sd)
missing7 = determine_missing_ans(question7, 7, removed_missing_sd)
missing8 = determine_missing_ans(question8, 8, removed_missing_sd)
missing9 = determine_missing_ans(question9, 9, removed_missing_sd)

write.csv(missing1,file="Question 1 Missing Responses.csv", row.names = FALSE)
write.csv(missing2,file="Question 2 Missing Responses.csv", row.names = FALSE)
write.csv(missing3,file="Question 3 Missing Responses.csv", row.names = FALSE)
write.csv(missing4,file="Question 4 Missing Responses.csv", row.names = FALSE)
write.csv(missing5,file="Question 5 Missing Responses.csv", row.names = FALSE)
write.csv(missing6,file="Question 6 Missing Responses.csv", row.names = FALSE)
write.csv(missing7,file="Question 7 Missing Responses.csv", row.names = FALSE)
write.csv(missing8,file="Question 8 Missing Responses.csv", row.names = FALSE)
write.csv(missing9,file="Question 9 Missing Responses.csv", row.names = FALSE)


