#PDF Scraper

require(pdftools)# read and load PDF documents
require(tm)# text mining
require(tidytext)#tidytext format
require(dplyr)#Manipulate data
require(tidyr)# tidy text data

# loads all PDF files
All_Files<- list.files(pattern = "pdf$")
All_opinions <- lapply(All_Files, pdf_text)

document<-Corpus(VectorSource(All_opinions))#create corpus

#Convert all text to lower case
document<-tm_map(document, content_transformer(tolower))
#Remove numbers from the text 
document<-tm_map(document, removeNumbers)
#Remove stopwords in English
document<-tm_map(document, removeWords, stopwords("english"))
#Remove punctuation
#document<-tm_map(document, removePunctuation, preserve_intra_word_dashes = TRUE)
document<-tm_map(document, removePunctuation)
#Remove white Spaces 
document<-tm_map(document, stripWhitespace)#remove white spaces
                   
test_dataframe <- data.frame(text=sapply(document, identity), stringsAsFactors=F)
                   
write.csv(test_dataframe,"test_File Name.csv", row.names = FALSE)