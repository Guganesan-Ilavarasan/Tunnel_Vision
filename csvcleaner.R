#CSV Cleaner (To Clean the CSV)

require(tm)# text mining
# require(tidytext)#tidytext format

#####################
#### Cleaning 1 #####
#####################

textdf = data.frame()

new_test_papers<-read.csv("new_cleaned_csv.csv",header=T)

doc1<-new_test_papers$review

# pre-processing:
doc1 <- gsub("'", "", doc1)  # remove apostrophes
doc1 <- gsub("[[:punct:]]", " ", doc1)  # replace punctuation with space
doc1 <- gsub("[[:cntrl:]]", " ", doc1)  # replace control characters with space
doc1 <- gsub("^[[:space:]]+", "", doc1) # remove whitespace at beginning of documents
doc1 <- gsub("[[:space:]]+$", "", doc1) # remove whitespace at end of documents
doc1 <- tolower(doc1)  # force to lowercase

tdf = rbind(textdf, data.frame(doc1, stringsAsFactors = FALSE))

write.csv(tdf,"test_File Name.csv", row.names = FALSE)

#####################
#### Cleaning 2 #####
#####################

All_opinions<-read.csv("test_File Name.csv",header=T)
document<-All_opinions$doc1

document<-Corpus(VectorSource(document))#create corpus

document<-tm_map(document, removeNumbers)

document<-tm_map(document, removeWords, stopwords("english"))


document <- tm_map(document1, removeWords, c("will","can", "sites", "site", "stonehenge", "stones", "stone", "henge", "nov", "oct", "jan", "dec", "monuments", "monument",
                                               "amesbury", "professor", "tunnels", "tunnel","roads", "road","letters", "letter", "archaeological", "oxford",
                                               "archaeology", "archaeologists", "archaeologist", "nthe", "university", "emeritus", "salisbury", "dr", "mr",
                                               "blick", "mead", "schemes", "scheme", "carriageways", "carriageway", "dual", "london", "bc", "director",
                                               "sir", "editor", "gale", "copyright", "times", "dear","madame", "madam", "s"))

document<-tm_map(document, stripWhitespace)

test_dataframe <- data.frame(text=sapply(document, identity), stringsAsFactors=F)

new_test_dfc<-gsub("[^A-Za-z]", " ", test_dataframe$text)

write.csv(new_test_dfc,"test_File NameA.csv", row.names = FALSE)

#####################
#### Cleaning 3 #####
#####################

ultra_new_test_papers<-read.csv("test_File NameA.csv",header=T)

ultra_new_test_crude<-ultra_new_test_papers$x

toks <- strsplit(ultra_new_test_crude, "\\s+")
lens <- sapply(toks, nchar) 

nultra_new_test_crude<-mapply(function(a, b) {
  paste(a[b > 2], collapse = " ")
}, toks, lens)

write.csv(test_dataframe,"test_File Name2.csv", row.names = FALSE)

#####################
#### Cleaning 4 #####
#####################

new_test_papers2<-read.csv("test_File Name2.csv",header=T)
textdf2 = data.frame()

doc2<-new_test_papers2$text
doc2 <- gsub("^[[:space:]]+", "", doc2) # remove whitespace at beginning of documents
doc2 <- gsub("[[:space:]]+$", "", doc2) # remove whitespace at end of documents

tdf2 = rbind(textdf2, data.frame(doc2, stringsAsFactors = FALSE))

write.csv(tdf2,"test_File Name3.csv", row.names = FALSE)
