# Clear the environment

rm(list=ls())

# Set the working Directory

setwd("C:\\Users\\Ramnath.Dhadage\\Documents\\TextMining and Document Classification")

# Load the libraries

library(tidyverse) 

library(qdapRegex)

library(tm)
library("wordcloud")

library("wordcloud2")

library(RColorBrewer)

library(lsa)

library(caret)

# Load Fitted model to predict lables of score data.

load("Logistic_Regression model.rda")
# write the functions

# Set options

Sys.setlocale('LC_ALL','C')

# Reading 'tm_case_score_data' and replacing column names as per tm package requirement.

Score_Data  <- read_csv("tm_case_score_data.csv") 

colnames(Score_Data) <- c("doc_id","text") #first 2 columns must be 'doc_id' & 'text'     

#### Data Preprocessing ####
# Step1: Tocanization: Tocanization is building of Text Mining and Natural Language processing.
txtCorpus <- VCorpus(DataframeSource(Score_Data))

#Step2: Normalization:
# Why Normalixation?
# Normalization puts all words on equal footing(in simkple word convert tokens in std format), and allows processing to proceed uniformly.
#step 2.1: Removing Punctuations, white spaces, numbers and stopwords and converting all tokens to lowercase, 
# because they are no use to classify documents. 

txtCorpus <-txtCorpus %>% 
  tm_map(content_transformer(qdapRegex::rm_url))%>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removeNumbers) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map( removeWords, c(stopwords('SMART'), 'lol', 'smh', 'amp','RT'))

# remove Emoticons
f <- content_transformer(function(x, pattern) gsub(pattern, "", x))
txtCorpus <-tm_map(txtCorpus,f,"[^\x01-\x7F]")

# Step 2.2: Stemming: remove suffixed, prefixes, infixes, circumfixes from each token tol get original steam. 

txtCorpus <- txtCorpus %>%
  tm_map(stemDocument)


# Need to plain text Saves time on large corpora
df <- data.frame(text = unlist(sapply(txtCorpus, `[`, "content")),
                 stringsAsFactors=F)

####End of preprocessing of data ####

#### Stage the data ####

# Term Document Matrix

tdm <- TermDocumentMatrix(txtCorpus) 

#### End of Stagging ####

# tf-idf:
tfidf <- weightTfIdf(tdm)

# As we can see there are total 6516. to reduce dimention and to extract concept from term we use latent segmatic analysis.
lsa_tfidf <-lsa(tfidf, dim =20) # we have converted all terms into 20 concepts.

# convert to data frame
words_df <- lsa_tfidf$dk %>% as.matrix() %>% as.data.frame() 




pred <- predict(fit_Training_LogReg,newdata = words_df, type = 'response') 

pred_label <- ifelse(pred>0.5, 1,0)
Score_Data$label_predicted <- pred_label

write.csv(Score_Data,"tm_case_score_data_predicted_label.csv")
