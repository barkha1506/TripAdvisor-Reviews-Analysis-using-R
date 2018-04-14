library(rvest)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
setwd("C:/Studies/Fall/Live case")

url <- "https://www.tripadvisor.com/Attraction_Review-g42881-d109972-Reviews-Mall_of_America-Bloomington_Minnesota.html"

webpage <- read_html(url)

#Using CSS selectors to scrap the rankings section
review_data_html <- html_nodes(webpage,'.partial_entry')

#Converting the ranking data to text
review_data <- html_text(review_data_html)

#Let's have a look at the rankings
head(review_data)

for ( i in 1:437){
  url<-paste("https://www.tripadvisor.com/Attraction_Review-g42881-d109972-Reviews-or",(i*10), "-Mall_of_America-Bloomington_Minnesota.html",sep="")
  webpage <- read_html(url)
  review_data_html <- html_nodes(webpage,'.partial_entry')
  review_data_int <- html_text(review_data_html)
  review_data<-append(review_data, review_data_int, after = length(review_data))

}

saveRDS(review_data,"moa_reviews")


docs <- Corpus(VectorSource(review_data))
inspect(docs)


# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Remove punctuations
docs <- tm_map(docs, removePunctuation)

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("the","this","ride","rides","mall","america","of","moa")) 


# Text stemming
# docs <- tm_map(docs, stemDocument)

#Wordcloud
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=600, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


findFreqTerms(dtm, lowfreq = 80)
findAssocs(dtm, terms = "long", corlimit = 0.3)
#We see that the word 'long' comes more than 80 times in all the reviews and 46% of the time it is 
#associated with lines indicating people compalaining about long queues

findAssocs(dtm, terms = "park", corlimit = 0.1)
#36% of the time staff is associated with friendly and helpful but there are cases where 16% they are associated
#with being unprofessional

findAssocs(dtm, terms = "one", corlimit = 0.1)
#One with operator 18%

findAssocs(dtm, terms = "workers", corlimit = 0.1)
#Workers are 39% correlated with rude

findAssocs(dtm, terms = "rude", corlimit = 0.1)

findAssocs(dtm, terms = "operator", corlimit = 0.3)

#LDA

# Convert all documents to a term frequency matrix. 
tfm <- DocumentTermMatrix(docs)

results <- LDA(tfm, k = 3, method = "Gibbs",control = list(seed = 0))


# Obtain the top w words (i.e., the w most probable words) for each topic, with the
#optional requirement that their probability is greater than thresh

#After trying multiple combinations of of word limit and threshold, the below value
#gave decent understanding of some of the topics
w=5 
thresh = 0.010
Terms <- terms(results, w,thresh) 

#Lets view all the words under a topic so that we can understand what each topic
#actually is about
Terms


# Obtain the most likely t topic assignments for each document. 
t=1 
Topic <- topics(results,t)

# Get the posterior probability for each document over each topic 
posterior <- posterior(results)[[2]]

# look at the posterior topic distribution for the dth document and plot it visually 
d = 215
posterior[d,]
barplot(posterior[d,])

# Examine the main topic for document d 
Terms[which.max(posterior[d,])]

# Compare the keyword of document d to the terms. keywords$query[d]
docs[[d]]$meta$id


docs[[1592]]$content
d=1592
posterior[d,]
Terms[which.max(posterior[d,])]
docs[[d]]$meta$id 
