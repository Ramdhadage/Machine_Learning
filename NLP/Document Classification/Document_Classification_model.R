# Clear the environment

rm(list=ls())

# Set the working Directory

setwd("C:\\Users\\Ramnath.Dhadage\\Documents\\TextMining and Document Classification")

# Install required packeges and load them 
packages <- c("tidyverse" # for r code ease
              , "qdapRegex" # for preprocessing
              , "tm"  # for preprocession 
              , "wordcloud"  # for plotting word cloud
              , "wordcloud2" # other way to plot wrd cloud
              , "RColorBrewer" # for visualizaion color
              ,  "lsa" # for dimention reduction
)
for (i in 1:length(packages)) {
  
  # installl only those package which are no install in your machine
  
  if (!packages[i] %in% rownames(installed.packages())) {
    
    install.packages(packages[i], dependencies = TRUE)
  }
  #Load all library
  
  library(packages[i], character.only = TRUE)
}

# write the functions

# Set options

Sys.setlocale('LC_ALL','C')

# Reading 'tm_case_training_data' 

Training_Data <- read_csv("tm_case_training_data.csv") 

# replacing column names as per tm package requirement.

colnames(Training_Data)[-3] <- c("doc_id","text") #first 2 columns must be 'doc_id' & 'text'     

#### Data Preprocessing ####
# Step1: Tocanization: Tocanization is building of Text Mining and Natural Language processing.
# Why Tocanization?
# Tokenization  convert raw data to tokens. this tokans used for vocabulary(set of unique tokens) pepration. 
# Vocabulary used for modeling the text data. ultimately tokanization is first step of building text data modeling.
# Refer https://www.analyticsvidhya.com/blog/2020/05/what-is-tokenization-nlp/ for more details 

txtCorpus <- VCorpus(DataframeSource(Training_Data))

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

#### Data Exploration ####

# frequent words in decreasing order

freq <- tdm %>%
  as.matrix %>%
  rowSums() %>%
  sort(decreasing = TRUE)

# Convert frequency table to detaframe to plot the frequent words

freq_df <- data.frame(words = names(freq),freqs=freq, row.names = NULL)

# plot frequent words

freq_words_bar_chart <- ggplot(subset(freq_df,freq>20),aes(x=reorder(words, -freqs),y=freqs))+
  geom_bar(stat = "identity",fill="steelblue") + 
  scale_x_discrete(name ="Most frequent words")+
  scale_y_discrete(name ="Fequency of words")+
  theme(axis.text.x=element_text(angle=45, hjust=1))

freq_words_bar_chart

png(file = "freq_words_bar_chart.png", width = 1920, height = 1920, res = 180)

freq_words_bar_chart

dev.off()

## Word Clouds

set.seed(100) # setting the seed so results will be regeneratable   

wordcloud <- wordcloud(names(freq), freq, min.freq=20,colors=brewer.pal(8, "Dark2"),scale=c(2,0.5)) 

wordcloud
png(file = "wordcloud.png", width = 1920, height = 1920, res = 180)

wordcloud

dev.off()

wordcloud_2ndoption <- wordcloud2(data=freq_df, color='random-dark')  

wordcloud_2ndoption

png(file = "wordcloud_2ndoption.png", width = 1920, height = 1920, res = 180)

wordcloud_2ndoption

dev.off()

# Worlds like people, protest ,Covid, people, trupms etc. are important.
# Plot the distribution of label column(that is target vector)

label_distribution_bar_chart <- Training_Data %>% 
  count(label) %>%
  ggplot(aes(x = factor(label), y = n)) +
  geom_bar(stat="identity", width=0.7, fill="steelblue") +
  geom_text(aes(label = n), vjust = 1)+
  # scale_x_discrete(name ="labels")+
  scale_x_discrete(name ="labels",labels=c("Non-Political", "Political"))+
  scale_y_discrete(name ="Frequency")+
  theme_minimal()

# There are more non- political social media posts.  
label_distribution_bar_chart

png(file = "label_distribution_bar_chart.png", width = 1920, height = 1920, res = 180)

label_distribution_bar_chart

dev.off()

# word length in each document

word_length <- str_count(df$text, '\\s+')+1

# Average Word Length for political postes

political_word_length <- str_count(df$text[Training_Data$label == 1], '\\s+')+1

(political_word_length_average <- mean(political_word_length))

# Average Word Length for political post  8.776224

non_political_word_length <- str_count(df$text[Training_Data$label == 0], '\\s+')+1

(non_political_word_length_average <- mean(non_political_word_length))

# Average Word Length for non-political post  8.647759

# Average Word Length for political as well as non political postes

(avgrage_word_length <- mean(c(political_word_length_average,non_political_word_length_average)))

# Average word length is 8.711991.

#####End of Data Exploratrion####
# Term documet matrix tell importance of word for perticular documents, this is noo longere useful hence using tf- idf
# tf-idf: calculate impotance of term in  all documents.

tfidf <- weightTfIdf(tdm)
nrow(tfidf)
# As we can see there are total 6516. to reduce dimention and to extract concept from term we use latent segmatic analysis.
lsa_tfidf <-lsa(tfidf, dim =20) # we have converted all terms into 20 concepts.

# convert to data frame
words_df <- lsa_tfidf$dk %>% as.matrix() %>% as.data.frame() 

#-----------Data Spliting into Training Data and Test Data------------
# created spliting 80% - train and 20 - test 
Train <- sample(1:nrow(words_df),0.8*nrow(words_df)) 

# Train Data 
Train_Data <- cbind(label = Training_Data$label[Train], 
                       words_df[Train,])
# Testing Data
Test_Data <- cbind(label = Training_Data$label[-Train], 
                   words_df[-Train,])

# Run Logistic Regression on Trainging

fit_Training_LogReg <- glm(label ~ .,
                           data = Train_Data, family = binomial)
# Save the model for future use and score data

save(fit_Training_LogReg, file = "Logistic Regression Model.rda")

# Predict label on Train Data to compute accuracy

pred_train <- predict(fit_Training_LogReg,newdata = Train_Data, type = 'response')

# Predict label on Test Data Data to compute accurac

pred_test <- predict(fit_Training_LogReg,newdata = Test_Data, type = 'response') 

# confusion matrix for computing accuracy  on train data(training set)

confusionMatrix(factor(ifelse(pred_train>0.5,1,0)), factor(Train_Data$label))

# Accuracy :  0.8769 
# confusion matrix for computing accuracy  on test data(Validation set)
confusionMatrix(factor(ifelse(pred_test>0.5,1,0)), factor(Test_Data$label))
# Accuracy : 0.8825 
# Now lets go to 
rstudioapi::navigateToFile("Predictions.R")

