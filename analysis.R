#Complete Analysis with all the techniques

require(topicmodels)# topic modeling
require(tm)# text mining
require(tidytext)#tidytext format
require(ggplot2)#create visualizations 
require(dplyr)#Manipulate data

# library("SnowballC")# for text stemming
library("wordcloud")# word-cloud generator
library("RColorBrewer")# color palettes


require(igraph)# lib to identify the graphing relationship
require(tidyr)# tidy text data
require(ggraph)# to create network graph

# import text dataset
new_test_papers<-read.csv("test_File Name3.csv",header=T)

document1<-Corpus(VectorSource(new_test_papers$doc2))
# document1 <- tm_map(document1, removeWords, c("will","can"))

# # # # # # # # # # # # # # # # # # # # # # # # 
###WORD CLOUD & BAR PLOT based on FREQUENCY ###
# # # # # # # # # # # # # # # # # # # # # # # #

DTM<-DocumentTermMatrix(document1)
#raw.sum=apply(DTM,1,FUN=sum) #sum by raw each raw of the table
#DTM=DTM[raw.sum!=0,]

m<-as.matrix(DTM)
v<-sort(rowSums(m), decreasing = TRUE)
d<-data.frame(word=names(v), frequency=v)
head(d,10)

findFreqTerms(DTM, lowfreq = 100)

heritage_assoc <- findAssocs(DTM, terms = "heritage", corlimit = 0.25)

write.csv(heritage_assoc,"Heritage_Assoc.csv", row.names = FALSE)

#set.seed(1234)
wordcloud(words = findFreqTerms(DTM, lowfreq = 100), freq = d$freq, min.freq = 1,
          max.words=1000, random.order=FALSE, rot.per=0.30, 
          colors=brewer.pal(8, "Dark2"))

barplot(d[1:15,]$freq, las = 3.5, names.arg = findFreqTerms(DTM, lowfreq = 500)[1:15], col ="lightblue", main ="Most frequent words",       
        ylab = "Word frequencies", ylim=c(0,1200))


##########################
### LDA TOPIC MODELING ###
##########################

Model_lda<-LDA(DTM, k=5)
#Model_lda<-LDA(DTM, k=8,control=list(seed=1234))

beta_topics<-tidy(Model_lda, matrix ="beta")

#Grouping the terms by topic
beta_top_terms <- beta_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

#Display the grouped terms on the charts 
beta_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

tidy(DTM)%>%
  filter(document==3)%>%
  arrange

##### GAMMA #####

gamma_documents<-tidy(Model_lda, matrix ="gamma")

#create a data.frame with the gamma results 
doc_gamma.df<-data.frame(gamma_documents)
doc_gamma.df$chapter<-rep(1:dim(DTM)[1],5)
#!!!!! CHANGE ,5 INTO NO. OF TOPICS !!!!!!!!

#plot Gamma results
ggplot(data=doc_gamma.df, aes(x=chapter, y= gamma,
                              group= factor(topic),color=factor(topic)))+
  geom_line()+facet_wrap(~factor(topic),ncol=1)


#############################
### Topic Coherence   #######
#############################

library(topicdoc)
library(topicmodels)

terms(Model_lda, 10)

topic_diagnostics(Model_lda, DTM)

topic_coherence(Model_lda, DTM)

########################
#loess regression curve#
########################

x <- 1:5
y <- topic_coherence(Model_lda, DTM)

plot(x,y) #plot without line

curve_values <- loess(y ~ x)    # Apply loess function

plot(x, y)                      # Plot with line
lines(predict(curve_values),
      col = "red",
      lwd = 3, f = 2/3)

#############################
## # # # # # # # # # # # # ##
######## TF-IDF #############
## # # # # # # # # # # # # ##
#############################

# Loading Packages
require(pdftools)# read and load PDF documents
library(tm) # For text mining and data cleaning activities
library(textstem) # For stemming of text
library(SnowballC) # For stemming of text
library(readxl) # For reading excel files
library(dplyr) # For filtering and selection of data
library(wordcloud) # Reading the dataset
library(RColorBrewer) # For colorful word cloud

#Creating a term document matrix with tf-idf weight
tdm_tfidf <- TermDocumentMatrix(document1,control = list(weighting = weightTfIdf)) #using td-idf method

#Other weightings available in tm package weightTf, weightTfIdf,weightBin, and weightSMART
# Link: https://cran.r-project.org/web/packages/tm/tm.pdf - page 41
m_tfidf <- as.matrix(tdm_tfidf)
headmatrix_tfidf<-head.matrix(m_tfidf,10)
#CorpusData$content[1]
#CorpusData$content[2]
v_tfidf <- sort(rowSums(m_tfidf),decreasing=TRUE)
head(v_tfidf, 100)
d_tfidf <- data.frame(word = names(v_tfidf),freq=v_tfidf)
head(d_tfidf, 10)
#row.names(d)

#Without tf-idf method

tdm_normal <- TermDocumentMatrix(document1)
m_normal <- as.matrix(tdm_normal)
headmatrix_normal<-head.matrix(m_normal,10)
v_normal <- sort(rowSums(m_normal),decreasing=TRUE)
head(v_normal, 10)
d_normal <- data.frame(word = names(v_normal),freq=v_normal)
head(d_normal, 10)
#row.names(d)

# Using bar graph

par(mfrow=c(1,2))

bp_tfidf<-barplot(d_tfidf[1:15,]$freq, las = 2, names.arg = d_tfidf[1:15,]$word,
                  col ="#2d4dd2", main ="Most important terms
                  tf id weights",
                  ylab = "tf-idf weight",xlab="terms",ylim=c(0,75))
text(x=bp_tfidf, y=d_tfidf[1:15,]$freq, labels=round(d_tfidf[1:15,]$freq,0), pos=1,family = "sans",col="#ffffff",font=1,cex=1)

bp_normal<-barplot(d_normal[1:15,]$freq, las = 2, names.arg = d_normal[1:15,]$word,
                   col ="#2d4dd2", main ="Most frequent terms
                   without weights",
                   ylab = "term frequency",xlab="terms",ylim=c(0,2500))
text(x=bp_normal, y=d_normal[1:15,]$freq, labels=round(d_normal[1:15,]$freq,0), pos=1,family = "sans",col="#ffffff",font=1,cex=1)

par(mfrow=c(1,1))
# # # # # # # # # # # # # # # # #
######### LDA 2 #################
# # # # # # # # # # # # # # # # #

doc1<-new_test_papers$doc2

library(tm)

stop_words <- stopwords("SMART")

# pre-processing:
doc1 <- gsub("'", "", doc1)  # remove apostrophes
doc1 <- gsub("[[:punct:]]", " ", doc1)  # replace punctuation with space
doc1 <- gsub("[[:cntrl:]]", " ", doc1)  # replace control characters with space
doc1 <- gsub("^[[:space:]]+", "", doc1) # remove whitespace at beginning of documents
doc1 <- gsub("[[:space:]]+$", "", doc1) # remove whitespace at end of documents
doc1 <- tolower(doc1)  # force to lowercase

# tokenize on space and output as a list:

doc.list <- strsplit(doc1, "[[:space:]]+")

########### NEW K ###############

library(textmineR)
library(dplyr)
library(ggplot2)

dtm <- CreateDtm(doc.list, 
                 ngram_window = c(1, 2))#explore the basic frequency
tf <- TermDocFreq(dtm = dtm)
original_tf <- tf %>% select(term, term_freq,doc_freq)
rownames(original_tf) <- 1:nrow(original_tf)# Eliminate words appearing less than 2 times or in more than half of the
# documents
vocabulary <- tf$term[ tf$term_freq > 1 & tf$doc_freq < nrow(dtm) / 2 ]
dtm = dtm

###
# K List - Simulation
###

k_list <- seq(1, 10, by = 1)
model_dir <- paste0("models_", digest::digest(vocabulary, algo = "sha1"))
if (!dir.exists(model_dir)) dir.create(model_dir)

model_list <- TmParallelApply(X = k_list, FUN = function(k){
  filename = file.path(model_dir, paste0(k, "_topics.rda"))
  
  if (!file.exists(filename)) {
    m <- FitLdaModel(dtm = dtm, k = k, iterations = 50)
    m$k <- k
    m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5)
    save(m, file = filename)
  } else {
    load(filename)
  }
  
  m
}, export=c("dtm", "model_dir")) # export only needed for Windows machines#model tuning
#choosing the best model
coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                            coherence = sapply(model_list, function(x) mean(x$coherence)), 
                            stringsAsFactors = FALSE)
ggplot(coherence_mat, aes(x = k, y = coherence)) +
  geom_point() +
  geom_line(group = 1)+
  ggtitle("Best Topic by Coherence Score") + theme_minimal() +
  scale_x_continuous(breaks = seq(1,10,1)) + ylab("Coherence")

###
# Execute the Best Model (K with highest coherence value) #
###

model <- model_list[which.max(coherence_mat$coherence)][[ 1 ]]
model$top_terms <- GetTopTerms(phi = model$phi, M = 20)
top20_wide <- as.data.frame(model$top_terms)
write.csv(top20_wide,"Topic_Modelling_2_result.csv", row.names = FALSE)
###
# Cluster Analysis of the topics #
###

model$topic_linguistic_dist <- CalcHellingerDist(model$phi)
model$hclust <- hclust(as.dist(model$topic_linguistic_dist), "ward.D")
model$hclust$labels <- paste(model$hclust$labels, model$labels[ , 1])
plot(model$hclust)

#################################
# # # # # # # # # # # # # # # # #
######### LDA 3 #################
# # # # # # # # # # # # # # # # #
#################################

library(lsa)
library(tm)
library(slam)
library(LDAvis)
library(topicmodels)

# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)

# remove terms that are stop words or occur fewer than 5 times:
del <- names(term.table) %in% stop_words | term.table < 5
term.table <- term.table[!del]
vocab <- names(term.table)

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents_ldavis <- lapply(doc.list, get.terms)

#####

# Compute some statistics related to the data set:
D <- length(documents_ldavis)  # number of documents (2,000)
W <- length(vocab)  # number of terms in the vocab (14,568)
doc.length <- sapply(documents_ldavis, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
N <- sum(doc.length)  # total number of tokens in the data (546,827)
term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus [8939, 5544, 2411, 2410, 2143, ...]

# MCMC and model tuning parameters:
K <- 4
G <- 5000
alpha <- 0.02
eta <- 0.02

# Fit the model:
library(lda)
#set.seed(357)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents_ldavis, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2 - t1  # about 24 minutes on laptop

###

theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

###

LetterReviews <- list(phi = phi,
                      theta = theta,
                      doc.length = doc.length,
                      vocab = vocab,
                      term.frequency = term.frequency)

###

library(LDAvis)

# create the JSON object to feed the visualization:
json <- createJSON(phi = LetterReviews$phi, 
                   theta = LetterReviews$theta, 
                   doc.length = LetterReviews$doc.length, 
                   vocab = LetterReviews$vocab, 
                   term.frequency = LetterReviews$term.frequency)
###

library(servr)

serVis(json, out.dir = 'vis', open.browser = TRUE)


######################################
######################################
# # # # # # # LSA  # # # # # # # # # #
######################################
######################################

require(pdftools) # pdf docs manupulations
require(tm) # text mining
require(lsa) # for simple LSA functions
require(LSAfun) # Analytics Fucntions

# Create term-document matrix
TDM <- TermDocumentMatrix(document1)

#Perform Latent Semantic Analysis
#Looking at the relationship between documents and terms
#Specifying that the dimension is five since we are looking 
#for five factors (based on five topics)

lsaTdmSpace = lsa(TDM, dims = 5)

#viewing the matrix by transforming it
as.textmatrix(lsaTdmSpace)

#Results showing how the terms concur across each document
#The relationship of the terms to the documents

#first range -> no. of words to display
#second range -> no. of documents
lsaTdmSpace$tk[1:10,1:5]
# lsaTdmSpace$tk[8:18,1:5]

# Considering the document context 
# This analysis on dk is focused on the documents and not terms
# SVD is similar to factor analysis and this is what is being done here

lsaTdmSpace$dk[1:5,1:5]

#Show the relationship between two documents and all others
lsaTdmSpace$dk[1:2,]

tk <- lsaTdmSpace$tk
dk <- lsaTdmSpace$dk

# Dimensions of $tk, marking the dots are red dots.
# The second line superimposes term names.

plot(tk[,1], y= tk[,2], col="red", cex=.50, main="TK Plot")
text(tk[,1], y= tk[,2], labels=rownames(tk), cex=.70)

#This is to plot the documents. Parameter 'cex' determines
plot(dk[,1], y= dk[,2], col="blue", pch="+", main="DK Plot")
text(dk[,1], y= dk[,2], labels=rownames(dk), cex=.70)

# View Neighbours
postLsaMatrix=lsaTdmSpace$tk%*% diag(lsaTdmSpace$sk) %*% t(lsaTdmSpace$dk)

plot_neighbors("heritage", n=50, tvectors = postLsaMatrix)
plot_neighbors("heritage", n=50, tvectors = postLsaMatrix, connect.lines = 0, start.lines = T,
               method = "PCA", dims = 3, axes = F, box = F, cex = 1, alpha = 0.5, col = "BLACK",
               breakdown = FALSE)


############################
#### N GRAM ANALYIS  #######
############################

PDFdataframe<-data.frame(text = sapply(document1, as.character),
                         stringsAsfactors = FALSE)

#Create BiGrams

New_Bigrams <- PDFdataframe%>%
  unnest_tokens(bigram,text, token = "ngrams", n = 2)
# New_Bigrams #All bigrams (all words coupled together front & back)

#Create TriGrams

New_Trigrams <- PDFdataframe%>%
  unnest_tokens(trigram,text, token = "ngrams", n = 3)

#Creating Bigram frequency

BigramCount <- New_Bigrams %>%
  count(bigram, sort = TRUE)

#Creating Trigram frequency

TrigramCount <- New_Trigrams %>%
  count(trigram, sort = TRUE)

#To seperate Bigrams and remove Stop Words
bigrams_separated <- New_Bigrams %>%
  separate(bigram, c("word1","word2"), sep = " ")

#To seperate Trigrams and remove Stop Words
trigrams_separated <- New_Trigrams %>%
  separate(trigram, c("word1","word2","word3"), sep = " ")

stop_words <- as.data.frame(stop_words) 

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

trigrams_filtered <- trigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)%>%
  filter(!word3 %in% stop_words$word)

#New Bigram Counts:

bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
bigram_counts
write.csv(bigram_counts,"BiGram_Count.csv", row.names = FALSE)

#New Trigram Counts:

trigram_counts <- trigrams_filtered %>%
  count(word1, word2, word3, sort = TRUE)
trigram_counts
write.csv(trigram_counts,"TriGram_Count.csv", row.names = FALSE)

#filtering for specific words

word1heritage <- bigrams_filtered %>%
  filter(word1 == "heritage") %>%
  count(word2, sort = TRUE)
write.csv(word1heritage,"word1heritage.csv", row.names = FALSE)

word2heritage <- bigrams_filtered %>%
  filter(word2 == "heritage") %>%
  count(word1, sort = TRUE)
write.csv(word2heritage,"word2heritage.csv", row.names = FALSE)

#Creating Bigraph
#Using word with count more than five to find relationship

bigram_graph <- bigram_counts %>%
  filter(n > 25) %>%
  graph_from_data_frame()

bigram_graph # To show relationships among words

#Creates the graph based on word relationships

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

#Creating Trigraph
#Using word with count more than twenty five to find relationship

trigram_graph <- trigram_counts %>%
  filter(n > 10) %>%
  graph_from_data_frame()

trigram_graph # To show relationships among words

#Creates the graph based on word relationships

ggraph(trigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

########################################################
########################################################
#Co-occurrence network - Representation of Collocation #
########################################################
########################################################

require(quanteda)
require(quanteda.textstats)
require(quanteda.textplots)
require(tidyverse)
require(tidytext)
require(pdftools)
require(tm)
require(ggcorrplot)

#Finding Collocations

social_sentences <- document1 %>%
  tolower() %>%
  paste0(collapse= " ") %>%
  stringr::str_split(fixed(".")) %>%
  unlist() %>%
  tm::removePunctuation() %>%
  stringr::str_squish()

#Create a Token Object

social_tokens <- quanteda::tokens(social_sentences, remove_punct = TRUE) %>%
  tokens_remove(stopwords("english"))

#Extract Collocations
social_coll <- textstat_collocations(social_tokens, size = 2, min_count = 20)

#Create a Document-Feature Matrix

social_dfm <- social_sentences %>%
  quanteda::dfm(remove = stopwords('english'), remove_punc = TRUE) %>%
  quanteda::dfm_trim(min_termfreq = 10, verbose = FALSE)

#To load function for Co-occurance calculation

source("https://slcladal.github.io/rscripts/calculateCoocStatistics.R")

# Define Term
coocTerm <- "heritage"

#Calculate Co=occurance Statistics

coocs <- calculateCoocStatistics(coocTerm, social_dfm, measure="LOGLIK")

# Inspect results

coocs[1:20]

#To reduce the document-feature matrix to contain only the top 20 collocates

redux_dfm <- dfm_select(social_dfm, pattern = c(names(coocs)[1:20], "heritage"))

#To transform the document-feature matrix into a feature-co-occurance matrix

tag_fcm <- fcm(redux_dfm)

#To create graph

textplot_network(tag_fcm,
                 min_freq = 1,
                 edge_alpha = 0.2,
                 edge_size = 5,
                 edge_colour = "purple",
                 vertex_labelsize = 3)

#############
##### CA ####
#############

require(quanteda.textmodels)

toks_irish <- tokens(doc1, remove_punct = TRUE)
dfmat_irish <- dfm(toks_irish) %>% 
  dfm_remove(pattern = stopwords("en"))

tmod_ca <- textmodel_ca(dfmat_irish)
textplot_scale1d(tmod_ca)

dat_ca <- data.frame(dim1 = coef(tmod_ca, doc_dim = 1)$coef_document, 
                     dim2 = coef(tmod_ca, doc_dim = 2)$coef_document)
head(dat_ca)

plot(1, xlim = c(-2, 2), ylim = c(-2, 2), type = "n", xlab = "Dimension 1", ylab = "Dimension 2")
grid()
text(dat_ca$dim1, dat_ca$dim2, labels = rownames(dat_ca), cex = 0.8, col = rgb(0, 0, 0, 0.7))

##############
## WORD-FISH##
##############


tmod_wf <- textmodel_wordfish(dfmat_irish, dir = c(6, 5))
summary(tmod_wf)

textplot_scale1d(tmod_wf)
#textplot_scale1d(tmod_wf, groups = dfmat_irish$text)

textplot_scale1d(tmod_wf, margin = "features", 
                 highlighted = c("concerned", "object", "irreparable",
                                 "destroy", "impact", "great", 
                                 "great", "destruction", "important"))

# textplot_scale1d(tmod_wf, margin = "features", 
                 highlighted = c("government", "protest", "delay", 
                                 "cutcover", "economy", "tunnel", "bored",
                                 "inflation", "money", "concerned", "object", "irreparable",
                                 "destroy", "impact", "loss", "great", "destruction", "damage",
                                 "traffic", "important", "noise", "never", "increase", "horrified")
