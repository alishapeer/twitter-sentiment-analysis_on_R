# CS688 - Web Analytics Term Project
# Alisha Peermohamed | Spring 2020

# Twitter Sentiment Analysis

# ReadMe: For my term project, I decided to perform a Twitter Sentiment Analysis on the 
# leaders of different countries. Given the current coronavirus pandemic, I wanted to know what
# the public's general sentiment and opinion is on each leader and how well the population
# thinks the leader is handling the crisis. 

# I decided to select 3 leaders of countries that seem to be handling the crisis well and 3
# leaders from countries where the pandemic is still growing rapidly. I decided to focus on 
# the following countries: United States, India, China, Germany, Japan, and Britain and searched
# for tweets on their leaders: 
# Donald Trump, Narendra Modi, Xi Jinping, Angela Merkel, Shinzo Abe, and Boris Johnson. 

library(twitteR)
library(ROAuth)
library("RCurl"); library("bitops"); library("rjson")
library(tm)
library(SnowballC)
library(tidytext)
library(stringr)
library(wordcloud)
library(ggplot2)
install.packages('gridExtra')
library(gridExtra)

consumer_key <- '7TZej7KM8M6j7bFAUR4ad49Hu'
consumer_secret <- 'SDO9V566eFyXVR79OytPLBstS4f9rIrYCCoxeeCTs9aBdPoPXX'
AccessToken <- '1192863433575686144-KEru54FegpvekfV1hPFKt474gioWMh'
AccessTokenSecret <- 'ILnUus2XgYEGb3PC51hjZ7kM34odhmHe8PJD9WLF2S6Xj'

# Establishing a connection to Twitter API
setup_twitter_oauth(consumer_key, consumer_secret, AccessToken, AccessTokenSecret)

# list of search terms
trump = 'Donald Trump' # American President
modi = 'Modi' # Indian Prime Minister
xi = 'Xi Jinping' # Chinese President
angela = 'Angela Merkel' # German Chancellor
shinzo = 'Shinzo Abe' # Japanese President
boris = 'Boris Johnson' #British Prime Minister

keyterms <- c(trump, modi, xi, angela, shinzo, boris)

# Fetching tweets for all keyterms- excluding retweets:
tweets_list <- list()
#print(noquote('Fetching tweets. This may take a few minutes...'))
for (i in 1:6) {
  if (i == 1) {print(noquote('Fetching tweets. This may take a few minutes...'))}
  tweets_list[[i]] <- searchTwitter(paste(keyterms[i], '-filter:retweets'), n = 500, lang = 'en')}


# getting the tweet message from tweet for all keyterms
tweet_texts <- list()
for (i in 1:6) {tweet_texts[[i]] <- lapply(unlist(tweets_list[i]), function(t){t$getText()})}


# Sample tweets for each leader:
display_sample <- function(i) {
  a <- noquote(paste('Sample tweets from', '"', keyterms[i], '"', 'search:'))
  print(a) ; print (unlist(tweet_texts[i])[1:3])}

display_sample(1)
display_sample(2)
display_sample(3)
display_sample(4)
display_sample(5)
display_sample(6)

## Tweet preprocessing
clean_up <- function(text) {
  clean <- gsub('http\\S+\\s*',"", text) # removing imbedded URLs ('https:..')
  clean <- gsub('@\\w+', "", clean) # removing twitter handles
  clean <- gsub("[^\x01-\x7F]", "", clean) # removing emoticons
  clean <- gsub('#[A-Za-z0-9]+', "", clean) # removing hashtags
  clean <- gsub('[[:punct:]]', " ", clean)
  clean <- gsub('[[:digit:]]', '', clean)
  clean <- gsub('\\d+', '', clean)
  clean <- gsub('\n', " ", clean)
  clean <- tolower(clean)
  clean <- str_squish(clean)
  #clean <- stem_words(clean, language = 'english')
  return(clean)}

# cleaning tweets for all keyterms
clean_tweets_list <- list()
for (i in 1:6) {
  clean_tweets <- c(); for (t in tweet_texts[i]) {clean_tweets <- c(clean_tweets, clean_up(t))}
  clean_tweets_list[[i]] <- clean_tweets}

# Sample clean tweets for each leader:
display_sample_clean <- function(i) {
  a <- noquote(paste('Sample clean tweets from', '"', keyterms[i], '"', 'search:'))
  print(a) ; print (unlist(clean_tweets_list[i])[1:3])}

for (i in 1:6) {display_sample_clean(i); print(noquote(rep('_', 40)))}

# Part 1: Text Analysis

# creating the text corpus for each search term
corpus_list <- list()
for (i in 1:6) {
  corpus_list[[i]] <- Corpus(VectorSource(unlist(clean_tweets_list[i])))}

# inspecting corpus for Donald Trump (as sample)
inspect(corpus_list[[1]][1:3])
meta(corpus_list[[1]][[1]])
content(corpus_list[[1]][[1]])

# removing stop words and other insignificant words: 
new_stopwords <- c('will', 'via', 'say', 'says', 'can', 'just', 'get', 'also',
                   'like', 'said', 'did', 'amp', 'may', 'dont', 'one', 'news', 'now',
                   'govt', 'government', 'sir', 'per', 'sign', 'think', 'going', 'time',
                   'yet', 'people', 's', 'lol', 'new', 'called')

remove_words_list <- list(c(tolower(trump), 'trump', 'donald', 'america', 'united', 'states', 'president', 'prime minister', 'trumps'), 
                          c(tolower(modi), 'narendra', 'modi', 'india', 'indian', 'president', 'prime minister'),
                          c(tolower(xi), 'xi', 'jinping', 'china', 'chinas' , 'chinese', 'president', 'prime minister'), 
                          c(tolower(angela), 'angela', 'merkel', 'merkels', 'germany', 'chancellor', 'president', 'prime minister'), 
                          c(tolower(shinzo), 'shinzo', 'abe', 'japan' , 'japanese', 'president', 'prime minister'), 
                          c(tolower(boris), 'boris', 'johnson', 'president', 'britain', 'prime minister'))
for (i in 1:6) {
  corpus.trans <- tm_map(corpus_list[[i]], removeWords, c(stopwords('english'), new_stopwords, unlist(remove_words_list[i])))
  corpus_list[[i]] <- corpus.trans }

# Creating a term document matrix for each search term
tdm_list <- list()
for (i in 1:6) {tdm_list[[i]] <- TermDocumentMatrix(corpus_list[[i]])}

# Examining part of the tdm for donald trump
print(noquote('Examining part of the Term Document Matrix for Donald Trump:'))
inspect(tdm_list[[1]][150:160, 50:60])

# Calculating the frequency of words
term_freq_list <- list()
for (i in 1:6) {term_freq_list[[i]] <- rowSums(as.matrix(tdm_list[[i]]))}

# Examining part of the Term Frequencies for Modi (as example)
cbind(term_freq_list[[2]][150:160])

# frequent terms 
for (i in 1:6) {print(noquote(paste('Frequent Terms in tweets with "', keyterms[i], ':')));
  print(findFreqTerms(tdm_list[[i]], lowfreq=15)); print(noquote(rep('_', 40)))}

# Associations
for (i in 1:6) {print(noquote(paste('Associations with "economy" in tweets with "', keyterms[i], ':')));
  print(findAssocs(tdm_list[[i]], 'economy', 0.4)); print(noquote(rep('_', 40))) }

# Sorting the words by descending order of frequency
for (i in 1:6) {term_freq_list[[i]] <- sort(term_freq_list[[i]], decreasing=TRUE)}

# Function to return the top n most frequent words:
top_n <- function(i, n) {
  term_freq = unlist(term_freq_list[i])
  x <- data.frame(names(term_freq[1:n]), term_freq[1:n])
  rownames(x) <- NULL
  colnames(x) <- c('Term', 'Frequency')
  a <- noquote(paste('Top', n, 'terms occuring in tweets with', '"', keyterms[i], '"', ':'))
  print(a); print (x)}

# top 10 words:
for (i in 1:6) {top_n(i,10); print(noquote(rep('_', 25)))}

# plotting the most frequent words by leader: 
plot_list <- list()
colors <- c('blue', 'orange', 'red', '#046307', 'pink', 'purple') 
for (i in 1:6) {
  plot_list[[i]] <- ggplot(top_n(i, 15), aes(x = reorder(Term, Frequency), y = Frequency)) +
    geom_bar(stat = 'identity', width = 0.8, fill = colors[i]) + coord_flip() +
    ggtitle(paste(keyterms[i], ':')) + 
    xlab('Term') + theme(plot.title = element_text(size=10))}

grided <- gridExtra::grid.arrange(grobs = plot_list, top = 'Top 15 Most Frequent terms by Leader' , ncol = 3, nrow = 2)

# Term with Maximum appearance:

most_freq <- function(i) {
  term_freq = unlist(term_freq_list[i]); tdm = tdm_list[[i]]
  a <- findFreqTerms(tdm,max(term_freq),max(term_freq))
  print(noquote(paste('Most frequent term in tweets with', '"',keyterms[i],'"', ':', a)))}

for (i in 1:6) {most_freq(i)}

# Percentage of top 20% of words that make up the full text:
get_perc_text <- function(i) {
  term_freq <- term_freq_list[[i]]
  ix <- floor(length(term_freq)*0.2)
  perc <- round(sum(term_freq[1:ix])/sum(term_freq)*100,2)
  return(noquote(paste(perc, '%')))}

perc_list <- list(); for (i in 1:6) {perc_list[[i]] <- get_perc_text(i)}
for (i in 1:1) {
a <- noquote('Percent of top 20% of text that make up the full text:')
perc_tweets <- data.frame(Leader = keyterms, Percent = unlist(perc_list))
print(a); print(perc_tweets)}

# Word Cloud Analysis
par(mfrow = c(1,1))

palette <- brewer.pal(8,"Dark2")

set.seed(137)
noquote(paste('WordCloud of Top terms in', trump, 'tweets --->'))
wordcloud(words=names(term_freq_list[[1]]), freq=term_freq_list[[1]],
          min.freq=5, random.order=F, colors=palette)

noquote(paste('WordCloud of Top terms in', modi, 'tweets --->'))
set.seed(137)
wordcloud(words=names(term_freq_list[[2]]), freq=term_freq_list[[2]],
          min.freq=5, random.order=F,colors=palette)

noquote(paste('WordCloud of Top terms in', xi, 'tweets --->'))
set.seed(137)
wordcloud(words=names(term_freq_list[[3]]), freq=term_freq_list[[3]],
          min.freq=5, random.order=F,colors=palette)

noquote(paste('WordCloud of Top terms in', angela, 'tweets --->'))
set.seed(137)
wordcloud(words=names(term_freq_list[[4]]), freq=term_freq_list[[4]],
          min.freq=5, random.order=F,colors=palette)

noquote(paste('WordCloud of Top terms in', shinzo, 'tweets --->'))
set.seed(137)
wordcloud(words=names(term_freq_list[[5]]), freq=term_freq_list[[5]],
          min.freq=5, random.order=F,colors=palette)

noquote(paste('WordCloud of Top terms in', boris, 'tweets --->'))
set.seed(137)
wordcloud(words=names(term_freq_list[[6]]), freq=term_freq_list[[6]],
          min.freq=5, random.order=F,colors=palette)

## Part 2: Sentiment Analysis

# Establishing Lexicons:
pos.words = scan('positive-words.txt',
                 what='character',
                 comment.char=';')

neg.words = scan('negative-words.txt',  
                 what='character', 
                 comment.char=';')

# Calculating the sentiment score for a given text
sentiment <- function(text, i) {
  # split the text into a vector of words
  words <- strsplit(text, '\\s+')
  words <- unlist(words)
  words <- words[!words %in% stopwords('en')]
  words <- words[!words %in% c(new_stopwords, remove_words_list[[i]])]
  # find which words are positive
  pos.matches <- match(words, pos.words)
  pos.matches <- !is.na(pos.matches)
  # find which words are negative
  neg.matches <- match(words, neg.words)
  neg.matches <- !is.na(neg.matches)
  # calculate the sentiment score
  p <- sum(pos.matches)
  n <- sum(neg.matches)
  if (p==0 & n==0)
    return(NA)
  else
    return(p-n)}

# calculating sentiment analysis scores for individual tweets
twitter_sentiments_list <- list()
for (i in 1:6) {
  sent_scores <- sapply(clean_tweets_list[[i]], sentiment, i = i)
  vector <- sapply(clean_tweets_list[[i]], function (t) {(t)})
  twitter_sentiments_list[[i]] <- data.frame(Score=sent_scores, Tweet= vector)}

# sample sentiment score on select few tweets:
sample_scores <- function(i) {
  a <- noquote(paste('Sample Sentiment Score for Tweets on', '"', keyterms[i],'":'))
  print(a); print(twitter_sentiments_list[[i]][1:5,]); print(noquote(rep('_', 50)))}

for(i in 1:6) {sample_scores(i)}

# distribution of sentiment scores
sentiment_dist <- function(i) {
  a <- noquote(paste('Distribution of Sentiment Scores for Tweets on', '"', keyterms[i],'":'))
  b <- twitter_sentiments_list[[i]]$Score
  print(a); print(table(b))}

for (i in 1:6) {sentiment_dist(i)}

# barplots of sentiment distributions: 
par(mfrow = c(2,3), oma = c(0, 0, 4, 0))

barplot_list <- list()
for (i in 1:6) {
  barplot_list[[i]] <- barplot(table(twitter_sentiments_list[[i]]$Score), xlab = 'Score',
                               ylab = 'Count', col = colors[i], main = keyterms[i])}
title(main=print(("Distribution of Sentiment Scores by Leader")),outer=T, cex = 1.5)

# removing NA scores 
scores_list <- list()
for (i in 1:6) {
  scores_list[[i]] <- twitter_sentiments_list[[i]]$Score[!is.na(twitter_sentiments_list[[i]]$Score)]}

# Average sentiment score by leader
avg.score_list <- c()
for (i in 1:6) {avg.score_list[i] <- round((sum(scores_list[[i]])/length(scores_list[[i]])),3)}

# boxplot of scores
a <- cbind(unlist(scores_list[1]), unlist(scores_list[2]), 
           unlist(scores_list[3]), unlist(scores_list[4]), 
           unlist(scores_list[5]), unlist(scores_list[6]))
colnames(a) <- c(trump, modi, xi, angela, shinzo, boris)

#Graphical depiction of State vs. Points received data

par(mfrow = c(1,1), oma = c(0, 0, 0, 0))

boxplot(a, main = 'Twitter Sentiment Scores by Leader:', 
        col = colors, cex.axis = 0.75, ylab = 'Score')

for (i in 1:1) {
  text(x=1,y=(fivenum(scores_list[[1]])[3]+0.1), paste('avg:',avg.score_list[[1]]), pos=3, offset=.2, cex=.8, col = 'white', font = 2)
  text(x=2,y=(fivenum(scores_list[[2]])[3]+0.1), paste('avg:',avg.score_list[[2]]), pos=3, offset=.2, cex=.8, col = 'white', font = 2)
  text(x=3,y=(fivenum(scores_list[[3]])[3]+0.1), paste('avg:',avg.score_list[[3]]), pos=3, offset=.2, cex=.8, col = 'white', font = 2)
  text(x=4,y=(fivenum(scores_list[[4]])[3]-0.5), paste('avg:',avg.score_list[[4]]), pos=3, offset=.2, cex=.8, col = 'white', font = 2)
  text(x=5,y=(fivenum(scores_list[[5]])[3]+0.1), paste('avg:',avg.score_list[[5]]), pos=3, offset=.2, cex=.8, col = 'black', font = 2)
  text(x=6,y=(fivenum(scores_list[[6]])[3]+0.1), paste('avg:',avg.score_list[[6]]), pos=3, offset=.2, cex=.8, col = 'white', font = 2)
}

for (i in 1:1){
a <- noquote('Average Sentiment Score by Leader (Sorted in decreasing order):')
b <- data.frame(Leader = keyterms, Average.Score = avg.score_list)
b <- b[order(b$Average.Score, decreasing = TRUE),]
print(a); print(b)}

for (i in 1:1) {
  best <- b[1,1] ; best_score <- b[1,2];  worst <- b[nrow(b),1]; worst_score <- b[nrow(b),2]
  print(noquote(paste('**' , best , 'had the highest sentiment score of' , best_score , 'indicating that people are generally speaking positive thing about them and people are generally content with how they have handled the coronavirus pandemic.')))
  print(noquote(paste('**' , worst , 'had the highest sentiment score of' , worst_score , 'indicating that people are generally speaking negative this about them and people are generally unhappy with how they have handled the coronavirus pandemic.')))
}
