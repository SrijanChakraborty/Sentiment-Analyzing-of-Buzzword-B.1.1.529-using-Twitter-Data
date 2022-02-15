# Sentiment-Analyzing-of-Buzzword-B.1.1.529-using-Twitter-Data
Today microblogging has become a very common platform for exchanging opinion  among us. Many users exchange their thoughts on a various aspect of their activity.  Consequently, microblogging websites are the substantial origin of information for  sentiment analysis and opinion mining. Twitter is a famous microblogging website  where 500 million tweets are posted every day. Corona Virus or COVID-19 first  appeared in December, 2019 in Wuhan, China. People tweeted aggressively on  twitter at that time. All tweets are categorized into 3 categories (Positive,  NegativeandNeutral). On 26 November 2021, WHO designated the variant B.1.1.5 29 a variant of concern (VOC), following advice fromthe WHO’s TechnicalAdvis ory Group on Virus Evolution.
#Code in R
install.packages("twitteR")
install.packages("RCurl")
require(twitteR)
require(RCurl)
# User API Key
api_key 🡨 "XX" 
# User API Secret
api_secret 🡨 "XX" 
"# User Access Token
access_token 🡨 "XX
# User Access Token Secret
access_token_secret 🡨 "XX"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
s <- read.csv("file name")
x <- list(s[,4])
# Remove at people mentioned preceding by ‘#’ or ‘@’
x = gsub("#\\w+", "", s[,4])
x = gsub("@\\w+", "", x)
# Replace any non UTF-8 by space
x=iconv(x, "UTF-8", "UTF-8",sub='')
# Remove punctuation
x = gsub("[[:punct:]]", "", x)
#Remove alphanumeric character
x = gsub("[^[:alnum:]]", " ",x)
# Remove numbers
x = gsub("[[:digit:]]", "", x)
# Remove html links
x = gsub("http\\w+", "", x)
# Remove unnecessary spaces
x = gsub("[ \t]{2,}", "", x)
x = gsub("^\\s+|\\s+$", "", x)
#Convert text to tolower
x=tolower(x)
# Write x to csv file
write.csv(x, "output file name")
library(syuzhet)
 library(plotly)
 library(tm)
 library(wordcloud)
# Read the file name
s <- read.csv("file name")
 Timestamp <- list(s[,1])
 Account <- list(s[,2])
 Tweets <- list(s[,4])
 Retweet <- list(s[,9])
 Tweets = gsub("#\\w+", "", s[,4])
 Tweets = gsub("@\\w+", "", Tweets)
 # Replace any non UTF-8 by space
 Tweets = iconv(Tweets, "UTF-8", "UTF-8",sub='')
 # Remove punctuation
 Tweets = gsub("[[:punct:]]", "", Tweets)
 #Remove alphanumeric character
 Tweets = gsub("[^[:alnum:]]", " ", Tweets)
 # Remove numbers
 Tweets = gsub("[[:digit:]]", "", Tweets)
 # Remove html links
 Tweets = gsub("http\\w+", "", Tweets)
 # Remove unnecessary spaces
 Tweets = gsub("[ \t]{2,}", "", Tweets)
 Tweets = gsub("^\\s+|\\s+$", "", Tweets)
 #Convert text to tolower
 Tweets = tolower(Tweets)
 Outfile <- list(Timestamp, Account, Tweets, Retweet)
 write.csv(Outfile, "Output preprocess file name")
 l <- as.Date(as.character(s[,1]), format = "%m/%d/%Y")
 m <- l[!duplicated(l)]
 syuzhet <- get_sentiment(Tweets, method="syuzhet")
 bing <- get_sentiment(Tweets, method="bing")
 afinn <- get_sentiment(Tweets, method="afinn")
 nrc <- get_sentiment(Tweets, method="nrc")
 sentiments <- data.frame(syuzhet, bing, afinn, nrc, l)
 write.csv(sentiments, "Output sentiment file name")
 #get the emotions using the NRC dictionary
 emotions <- get_nrc_sentiment(Tweets)
emo_bar = colSums(emotions)
 emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
 emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing 
= TRUE)])
 # plot the different sentiments from different methods
 plot_ly(sentiments, x=~l, y=~syuzhet, type="scatter", mode="jitter", name="syuzhet") %>%
 add_trace(y=~bing, mode="lines", name="bing") %>%
 add_trace(y=~afinn, mode="lines", name="afinn") %>%
 add_trace(y=~nrc, mode="lines", name="nrc") %>%
 layout(title="Recent sentiments of Vijay Rupani in India",
 yaxis=list(title="score"), xaxis=list(title="date"))
 # Visualize the emotions from NRC sentiments
 plot_ly(emo_sum, x=~emotion, y=~count, type="bar", color=~emotion) %>%
 layout(xaxis=list(title=""), showlegend=FALSE,
 title="Distribution of emotion categories for Vijay Rupani (21 Nov - 1st Dec 2017)")
 # Comparison word cloud
 all = c(
 paste(Tweets[emotions$anger > 0], collapse=" "),
 paste(Tweets[emotions$anticipation > 0], collapse=" "),
 paste(Tweets[emotions$disgust > 0], collapse=" "),
 paste(Tweets[emotions$fear > 0], collapse=" "),
 paste(Tweets[emotions$joy > 0], collapse=" "),
 paste(Tweets[emotions$sadness > 0], collapse=" "),
 paste(Tweets[emotions$surprise > 0], collapse=" "),
 paste(Tweets[emotions$trust > 0], collapse=" ")
 )
 all <- removeWords(all, stopwords("english"))
 # create corpus
 corpus = Corpus(VectorSource(all))
 # create term-document matrix
 tdm = TermDocumentMatrix(corpus)
 # convert as matrix
 tdm = as.matrix(tdm)
 tdm1 <- tdm[nchar(rownames(tdm)) < 11,]
 # add column names
 colnames(tdm) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
 colnames(tdm1) <- colnames(tdm)
 comparison.cloud(tdm1, random.order=FALSE,
 colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
 title.size=1, max.words=250, scale=c(2.5, 0.4),rot.per=0.4)
