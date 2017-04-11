###This script defines the stupid back-off algorithm and function for predicting the next word in a sentence############

#Load the required libraries###
library(data.table)
library(stringr)
library(tidytext)
library(dplyr)
library(tm)
library(ggplot2)

###Stupid-Backoff# ############

# the stupid back-off algorithm gives a score to the frequencies of words or n-grams and develop relative scores####


#setwd("C:/Modeling and Simulation/DataScienceCapstone/Shiny App")

# read in the unigram, bigram, trigram, quadragram and pentagram files
unigram<-fread("unigrams.csv")
bigram<-fread("bigrams.csv")
trigram<-fread("trigrams.csv")
quadragram<-fread("quadragrams.csv")
pentagram<-fread("pentagrams.csv")

##Define the function for calculating a score and a back-off score
stupid_backoff <- function (sentence) {
    token <- strsplit(sentence, split=" ")[[1]]##split the sentence into  a vector of words
    token<-tail(token, n=5)##choose the last 5 words in the sentence
    ##the if else function defines a value for variable k depending on the length of the partial sentence
    ifelse(length(token)>=5, k<-4,
                  ifelse(length(token)==4, k<-3,
                         ifelse(length(token)==3, k<-2,
                                ifelse(length(token)==2, k<-1,k<-0))))
    
    ifelse(k>0,val <- paste(token[(length(token) - k + 1): (length(token))], collapse= " "),val<-token)# if more than one word
    ##in a sentence then val is a subset of token based on the k value, if not val is equal to token
    
    numerator <- pentagram$n[pentagram$start==val]## select the frequencies from pentagram (pentagram$n) where the 4-word sentence matches the start
    
    if (length(numerator)>0) { #if matching pentagram found
        denominator <- quadragram$n[quadragram$quadragram==val]
        res <- numerator / denominator
        names<-pentagram$end[pentagram$start==val]
    }
    else {#back-off one step and search for a quadragram 
    
       token <- tail(token, n=4)# get last 4 elements of the token vector
       ifelse(length(token)>=5, k<-4,
              ifelse(length(token)==4, k<-3,
                     ifelse(length(token)==3, k<-2,
                            ifelse(length(token)==2, k<-1,k<-0))))
       ifelse(k>0,val <- paste(token[(length(token) - k + 1): (length(token))], collapse= " "),val<-token)# take the last 4 words in the sentence
       numerator <- quadragram$n[quadragram$start==val]
       if (length(numerator)>0) { #if matching quadragram found
           denominator <- trigram$n[trigram$trigram==val]
           if (length(token)>=5){
           res <- numerator*0.4 / denominator}#(this applies if 5 or more words were entered and not found in pentagram)
           if (length(token)==4){
               res <- numerator/ denominator}#(this applies if 4 words were entered and found in the quadragram)
           names<-quadragram$end[quadragram$start==val]}
      
       else {#back-off one step and search for a trigram
           token <- tail(token, n=3)# get last 3 elements of the token vector
           ifelse(length(token)>=5, k<-4,
                  ifelse(length(token)==4, k<-3,
                         ifelse(length(token)==3, k<-2,
                                ifelse(length(token)==2, k<-1,k<-0))))
           
           ifelse(k>0,val <- paste(token[(length(token) - k + 1): (length(token))], collapse= " "),val<-token)# take the last 4 words in the sentence
           numerator <- trigram$n[trigram$start==val]
           if (length(numerator)>0) { #if matching trigram found
               denominator <- bigram$n[bigram$bigram==val]
               if (length(token)>=5){
                   res <- numerator*0.4**2 / denominator}#(this applies if 5 or more words were entered and not found in pentagram and quadragram
               if (length(token)==4){
                   res <- numerator*0.4/ denominator}#(this applies if 4 words were entered and not found in the quadragram)
               if (length(token)==3){
                   res <- numerator/ denominator}##(this applies if 3 words were entered and found in the trigram)
               names<-trigram$end[trigram$start==val]
           }
           else{#back-off one step and search for a bigram
               token <- tail(token, n=2)# get last 2 elements of the token vector
               ifelse(length(token)>=5, k<-4,
                      ifelse(length(token)==4, k<-3,
                             ifelse(length(token)==3, k<-2,
                                    ifelse(length(token)==2, k<-1,k<-0))))
               ifelse(k>0,val <- paste(token[(length(token) - k + 1): (length(token))], collapse= " "),val<-token)# take the last 4 words in the sentence
               numerator <- bigram$n[bigram$start==val]
               if (length(numerator)>0) { #if matching bigram found
                   denominator <- unigram$n[unigram$words==val]
                   if (length(token)>=5){
                       res <- numerator*0.4**3 / denominator}#(this applies if 5 or more words were entered and not found in pentagram,quadragram, trigram
                   if (length(token)==4){
                       res <- numerator*0.4**2/ denominator}#(this applies if 4 words were entered and not found in the quadragram and trigram)
                   if (length(token)==3){
                       res <- numerator*0.4/ denominator}
                   if (length(token)<=2){
                       res <- numerator/ denominator}
                   names<-bigram$end[bigram$start==val]}
               
               else{# this is if the entry is blank i.e no word entry for the sentence
                   res<-0
                   names<-"no words found"}
               }}}###closes all elses
    
    return(list(name=names, count = res))##the output is a list of 2 vectors with names and counts
}

##########Define the predict word function which predicts the top 5 likely words and returns a dataframe with two columns
########### of the predicted word and the score#########################################################################

predict_word<-function(partial_sentence){
    ##clean the input sentence
    trim <- function (x) gsub("^\\s+|\\s+$", "", x)# remove leading and trailing white spaces
    partial_sentence <- trim(partial_sentence)# apply the trim function to the partial sentence
    partial_sentence<-tolower(partial_sentence)# convert the sentence to lower case
    partial_sentence<-gsub('[0-9]+', '', partial_sentence)# removes numbers from a string
    partial_sentence<-gsub("[^[:alnum:][:blank:]+?&/\\-]", "", partial_sentence)# remove all non alphanumeric characters
    out1<-stupid_backoff(sentence=partial_sentence)#apply the stupid back off function to partial_sentence. The results are stored in out1
    out1<-as.data.frame(cbind(as.character(out1[[1]]),signif(as.numeric(as.character(out1[[2]])),digits=3)))# convert the output to a dataframe
    names(out1)<-c("PredictedWord", "Score")## rename the columns of the dataframe
    return(out1)
}



