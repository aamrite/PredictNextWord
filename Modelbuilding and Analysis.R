
#This script defines the preparation of uni, bi, tri, quadra and pentagram datasets from a sample
# of a large corpus. The corpus is prepared using a large input text files containing samples from twitter, news and blogs
# the script describes downloading and copying the files and cleaning the database and constructing a corpus. 
# the corpus is then cleaned and broken down to sonstruct uni, bi, tri, quadra, and pentagrams and their frequencies


#Load the required libraries
require(tm)
require(SnowballC)
require(RWeka)
require(data.table)
require(stringr)
library(tidytext)
library(dplyr)
library(data.table)
setwd("C:\\Modeling and Simulation\\DataScienceCapstone\\All Dsets")

####read in the datasets###############################
fp<-("H:/Personal/Continuing Education/Data Scientist Specialization/Data Science Capstone/en_US")

####Read in the three datasets#############################################################################################

## Because of special characters the readLines function does not read the news dataset correctly. To handle this
## the datasets are read in using the binary mode


conn <- file(paste(fp,"/","en_US.blogs.txt", sep=""), open = "rb")
blogs <- readLines(conn, encoding = "UTF-8")
close(conn)


conn <- file(paste(fp,"/","en_US.news.txt", sep=""), open = "rb")
news <- readLines(conn, encoding = "UTF-8")
close(conn)


conn <- file(paste(fp,"/","en_US.twitter.txt", sep=""), open = "rb")
twit <- readLines(conn, encoding = "UTF-8")
close(conn)

#################################################################################################################################



##A function to sample lines from a text file
sample_files <- function(x) {
    x <- sample(x, length(x)*0.1)# 10% of the lines are sampled
}

####Functions to remove special characters
detectNonAsciiChar <- function(x) iconv(x, from="UTF-8", to="ASCII",sub="X")##detects a nonASCII character and substitutesit with "X"
removeNonAsciiWord <- function(x) gsub("[a-z]*X+[a-z]*", " ", x)##removes the detected nonASCII characters
removeHTTPS <- function(x) gsub("https://(.*)[.][a-z]+|https://[a-z]+", " ", x)
removeHTTP  <- function(x) gsub("http://(.*)[.][a-z]+|https://[a-z]+", " ", x)
removeFTP <- function(x) gsub("ftp://(.*)[.][a-z]+|https://[a-z]+", " ", x)
removeWWW <- function(x) gsub("www(.*)[.][a-z]+|www.", " ", x)
removeHashTag <- function(x) gsub("#[a-z0-9]+", " ", x)
removeTwitterRT <- function(x) gsub("^rt |^rt:", " ", x)
removeCharRepetition <- function(x) {
    a <- gsub("[a-z]*aaa[a-z]*", " ", x)
    a <- gsub("[a-z]*bbb[a-z]*", " ", a)
    a <- gsub("[a-z]*ccc[a-z]*", " ", a)
    a <- gsub("[a-z]*ddd[a-z]*", " ", a)
    a <- gsub("[a-z]*eee[a-z]*", " ", a)
    a <- gsub("[a-z]*fff[a-z]*", " ", a)
    a <- gsub("[a-z]*ggg[a-z]*", " ", a)
    a <- gsub("[a-z]*hhh[a-z]*", " ", a)
    a <- gsub("[a-z]*iii[a-z]*", " ", a)
    a <- gsub("[a-z]*jjj[a-z]*", " ", a)
    a <- gsub("[a-z]*kkk[a-z]*", " ", a)
    a <- gsub("[a-z]*lll[a-z]*", " ", a)
    a <- gsub("[a-z]*mmm[a-z]*", " ", a)
    a <- gsub("[a-z]*nnn[a-z]*", " ", a)
    a <- gsub("[a-z]*ooo[a-z]*", " ", a)
    a <- gsub("[a-z]*ppp[a-z]*", " ", a)
    a <- gsub("[a-z]*qqq[a-z]*", " ", a)
    a <- gsub("[a-z]*rrr[a-z]*", " ", a)
    a <- gsub("[a-z]*sss[a-z]*", " ", a)
    a <- gsub("[a-z]*ttt[a-z]*", " ", a)
    a <- gsub("[a-z]*uuu[a-z]*", " ", a)
    a <- gsub("[a-z]*vvv[a-z]*", " ", a)
    a <- gsub("[a-z]*www[a-z]*", " ", a)
    a <- gsub("[a-z]*xxx[a-z]*", " ", a)
    a <- gsub("[a-z]*yyy[a-z]*", " ", a)
    gsub("[a-z]*zzz[a-z]*", " ", a)
}




####GEt a sample of all the three datasets##########
set.seed(12345)
blogSamp <- sample_files(blogs)#using the sample_files function defined above to sample 10% of the file
newsSamp <- sample_files(news)
twitSamp <- sample_files(twit)
rm(blogs,news,twit)##removes the original datasets to free up memory

bad_words<-readLines("bad-words.txt", encoding="UTF-8", skipNul = TRUE)##this is a text file for bad/offensive words
bad_words<-bad_words[2:1384]

##combine all three sampled files and remove the individual sample files

allSamp<-c(blogSamp,newsSamp,twitSamp)
rm(blogSamp)
rm(newsSamp)
rm(twitSamp)

write.table(allSamp, file="C:\\Modeling and Simulation\\DataScienceCapstone\\All Dsets\\Samples\\allSamples.txt", row.names = F)

#####Create a corpus#########################################################################
allSamp<-VCorpus(DirSource(directory="C:\\Modeling and Simulation\\DataScienceCapstone\\All Dsets\\Samples",encoding="UTF-8"),readerControl = list(language="us"))

###Clean the combined file

allSamp<- tm_map(allSamp, removeNumbers)
allSamp <- tm_map(allSamp, content_transformer(tolower))
allSamp <- tm_map(allSamp, content_transformer(detectNonAsciiChar))
allSamp <- tm_map(allSamp, content_transformer(removeNonAsciiWord))
allSamp<- tm_map(allSamp, content_transformer(removeHTTPS))
allSamp <- tm_map(allSamp, content_transformer(removeHTTP))
allSamp <- tm_map(allSamp, content_transformer(removeFTP))
allSamp <- tm_map(allSamp, content_transformer(removeWWW))
allSamp <- tm_map(allSamp, content_transformer(removeHashTag))
allSamp <- tm_map(allSamp, content_transformer(removeTwitterRT))
allSamp<- tm_map(allSamp, content_transformer(removeCharRepetition))
allSamp <- tm_map(allSamp, removePunctuation)
allSamp <- tm_map(allSamp , stripWhitespace)
allSamp <- tm_map(allSamp , removeWords,bad_words)

####Preparing a tidy text document
tidy_docs<-tidy(allSamp)
###Generating frquencies
############uni grams##################################################
token_uni<-tidy_docs %>% unnest_tokens(words,text)

freq_uni<-token_uni %>% count(words, sort=TRUE)
uiL<-str_split(freq_uni$words," ")
freq_uni$start<-sapply(uiL,FUN=function(x) x[1])
freq_uni$end<-sapply(uiL,FUN=function(x) x[1])

rm(token_uni, uiL)


######################bi grams#####################################################################################
token_bi<-tidy_docs %>% unnest_tokens(bigram,text,token="ngrams", n=2)

freq_bi<-token_bi %>% count(bigram, sort=TRUE)
biL<-str_split(freq_bi$bigram," ")
freq_bi$start<-sapply(biL,FUN=function(x) x[1])
freq_bi$end<-sapply(biL,FUN=function(x) x[2])

rm(token_bi, biL)

######################tri grams#####################################################################################
token_tri<-tidy_docs %>% unnest_tokens(trigram,text,token="ngrams", n=3)

freq_tri<-token_tri %>% count(trigram, sort=TRUE)
triL<-str_split(freq_tri$trigram," ")
freq_tri$start<-sapply(triL,FUN=function(x) paste(x[1],x[2]))
freq_tri$end<-sapply(triL,FUN=function(x) x[3])

rm(token_tri, triL)

######################quadra grams#####################################################################################
token_quadi<-tidy_docs %>% unnest_tokens(quadragram,text,token="ngrams", n=4)

freq_quadi<-token_quadi %>% count(quadragram, sort=TRUE)
quadiL<-str_split(freq_quadi$quadragram," ")
freq_quadi$start<-sapply(quadiL,FUN=function(x) paste(x[1],x[2],x[3]))
freq_quadi$end<-sapply(quadiL,FUN=function(x) x[4])

rm(token_quadi, quadiL)

######################penta grams#####################################################################################
token_penta<-tidy_docs %>% unnest_tokens(pentagram,text,token="ngrams", n=5)

freq_penta<-token_penta %>% count(pentagram, sort=TRUE)
pentaL<-str_split(freq_penta$pentagram," ")
freq_penta$start<-sapply(pentaL,FUN=function(x) paste(x[1],x[2],x[3],x[4]))
freq_penta$end<-sapply(pentaL,FUN=function(x) x[5])

rm(token_penta, pentaL)




rm(allSamp)

########################################################################################################################
##Clean up lower frequency terms to reduce size of datasets####################################################################################
freq_bi<-subset(freq_bi, freq_bi$n>=2)
freq_tri<-subset(freq_tri, freq_tri$n>=2)
freq_quadi<-subset(freq_quadi, freq_quadi$n>=2)
freq_penta<-subset(freq_penta, freq_penta$n>=2)

########################Write the frequency datasets to file; to be used later for app upload#########################

write.csv(freq_uni, file="unigrams.csv", row.names = F)
write.csv(freq_bi, file="bigrams.csv", row.names = F)
write.csv(freq_tri, file="trigrams.csv", row.names = F)
write.csv(freq_quadi, file="quadragrams.csv", row.names = F)
write.csv(freq_penta, file="pentagrams.csv", row.names = F)