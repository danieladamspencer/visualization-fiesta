##### Word cloud fun
# install.packages("XML")
require(XML)
# install.packages("tm")
require(tm)
# install.packages("wordcloud")
require(wordcloud)
require(RColorBrewer)

##### Trying my own example using text message data
Lisa <- read.csv("lisa_word_cloud/Lisa.csv",stringsAsFactors = FALSE) #Note: Replace with your own .csv
# Jo <- Lisa$message
# Jo.corpus <- Corpus(DataframeSource(data.frame(as.character(Jo))))
Jo <- data.frame(doc_id = rownames(Lisa),
                 text = Lisa$message)
Jo.corpus <- Corpus(DataframeSource(Jo))
Jo.corpus <- tm_map(Jo.corpus, removePunctuation)
Jo.corpus <- tm_map(Jo.corpus, content_transformer(tolower))
Jo.corpus <- tm_map(Jo.corpus, removeWords, stopwords("english"))
Jo.tdm <- TermDocumentMatrix(Jo.corpus)
Jo.m <- as.matrix(Jo.tdm)
Jo.v <- sort(rowSums(Jo.m),decreasing=TRUE)
Jo.d <- data.frame(word = names(Jo.v),freq = Jo.v)
table(Jo.d$freq)
pal2 <- brewer.pal(8,"Dark2")
windows(width = 12, height = 12)
jpeg(filename = 'LisaWordCloud.jpg')
wordcloud(Jo.d$word,Jo.d$freq,scale = c(8,.2),min.freq = 3,random.order = FALSE, rot.per = .15, colors = pal2)
dev.off()