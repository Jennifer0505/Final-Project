#The reason that I wish to do this topic is becuase recently more and more school shootings begin
#to exist. It happened without any preparation and many students and faculties have been killed
#by the shooter. From my perspective, it is an important social issue that everyone need to pay
#attention to in order to have a stable society in the future. 
#analyze what users say about school shootings(positive, negative)
#create word cloud
#visualize which location has happened school shootings. hard to happen, collecet new data about where has happened school shootings
#comparison different comments in different year, frequent used words. 
#time series of number of everyday users watching ,through data data frame only enough
#people who have like is talking about what 
#which channel report the most 

#word cloud for real time data & historical data, compare, nothing change, always about the gun control, need more attention
#barplot/time series for word frequency compare, real&historical2017,2016,2015,2014
#sentiment analysis compare
#find a data can get location, did geo graph
#statistics analysis, gun control related with school shootings, whether there is a change(look at assignment 1) 
#gun!!, find gun's relationship with school shootings. 
library(tuber)
library(dplyr)
library(tidytext)
library(stringr)
library(lubridate)
library(dplyr)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(plotrix)
library(reshape)
clientID <- "979385012952-iive5jqvebq2emfhrnld66iflt1rq1t8.apps.googleusercontent.com"
client_secrete <- "F5n5Sxt2PXkeeUnnI89mJGo5"
yt_oauth(clientID, client_secrete, token = '')
#Collecting Real Time Data for comments
#Selecting abc, CNN, NBC, fox and CBS channel's school shootings' news with the most click.
#comments_abc <- get_all_comments(video_id = c('xgIJosk0pnA'))
#comments_cnn <- get_all_comments(video_id = c('yW61tS8H66E'))
#comments_NBC <-get_all_comments(video_id = c('3F_vdCHlDds'))
#comments_fox <-get_all_comments(video_id = c('ZQcaYYHqrUY'))
#comments_cbs <-get_all_comments(video_id = c('npnH0_natfs'))
#all_comments <- rbind(comments_abc, comments_cnn, comments_NBC, comments_fox, comments_cbs)
#real_time_data <- data.frame(all_comments$authorDisplayName,
#                   all_comments$textOriginal,
#                   all_comments$likeCount,
#                   all_comments$publishedAt)
#clean real_time_data
#real_time_data[,4]<-str_split_fixed(real_time_data$all_comments.publishedAt, "T", 2)[,1]
#real_time_data[,4] <- ymd(real_time_data[,4])
#For real time data purpose, only collect data after 2018/04/011
#real <- real_time_data %>%
#  filter(all_comments.publishedAt >= as.Date("2018-04-1") )
#real<- data.frame(real$all_comments.textOriginal)
#write.csv(real, file = "real.csv")
#real_time_data <- real_time_data %>%select(all_comments.authorDisplayName, 
#                         all_comments.likeCount, 
#                         all_comments.textOriginal,
#                         all_comments.publishedAt) %>%
#                         filter(all_comments.publishedAt >= as.Date("2018-04-1"))
#write.csv(real_time_data, file="real_time_data.csv")
#Word Cloud
real_time_data <- read.csv("real_time_data.csv", header=TRUE, stringsAsFactors = FALSE, fileEncoding = "latin1")
real_time_data_comments <- Corpus(VectorSource(real_time_data$all_comments.textOriginal))
#text cleaning
#real_time_data_comments[[1]][1], test the text cleaning
real_time_data_comments <- tm_map(real_time_data_comments, removeNumbers)
real_time_data_comments <- tm_map(real_time_data_comments, removeWords, stopwords("english"))
real_time_data_comments <- tm_map(real_time_data_comments, removePunctuation )
real_time_data_comments <- tm_map(real_time_data_comments, removeNumbers)
real_time_data_comments <- tm_map(real_time_data_comments, content_transformer(tolower))
real_time_data_comments <- tm_map(real_time_data_comments, removeWords, c("this","so", "will", "can", 
                                                                          "why","said","get","really",
                                                                          "just", "like","the","one", 
                                                                          "even", "you", "know","say",
                                                                          "think", "dont", "people", 
                                                                          "school", "guns"))
real_time_data_comments <- tm_map(real_time_data_comments, stripWhitespace)
real_time_data_comments <- TermDocumentMatrix(real_time_data_comments)
real_time_data_comments <- as.matrix(real_time_data_comments)
real_time_data_comments <- sort(rowSums(real_time_data_comments), decreasing=TRUE)
real_time_data_comments <- data.frame(word=names(real_time_data_comments), freq=real_time_data_comments)
wordcloud(real_time_data_comments$word, real_time_data_comments$freq, random.order = FALSE, 
          rot.per = 0.3, scale = c(4,.5), max.words=800, colors = rainbow(5),vfont= c ( "sans serif" , "plain" ))

#According to this word cloud, people basically commented through three aspects. 
#The main perspective is an intense emotion toward the shooting, which they talk about the words like "gun" , "shooting" and " kill" a lot. 
# gun. Besides that,they also express the sympathy to the children with words like "bad" and "sad". 
# The last aspect is judgemental emotions towards the shooter and their attitude toward these tragedies, which they talked about "judge", "police", and "shooter".
# This frequency of words is predictable becuase expect the video that is the news, the video with most clikc in the CNN is parents and teachers
#push for change after school shooting, so that is why the comments metioned a lot of gun and gun control

# show the top 10 words in the word cloud and their frequency
real_high_frequency <- head(real_time_data_comments, 10)
barplot(real_high_frequency$freq, width=1,space= 0.5,
        names.arg = real_high_frequency$word,
        col =rainbow(20), main ="Most frequent words",
        ylab = "Word frequencies",cex.names = 0.8) 
#In order for more clear undersing about the word cloud, I did a barplot to directly show the frequency of the top 10 words. 
#From this bar plot we can notice that besides any emotional words, the gun has been mentioned the most
#which indicates that people are begin to pay attention on the gun using. 
#Sentiment Analysis
RTD_analysis <- str_split(real_time_data$all_comments.textOriginal, pattern = "\\s+")
RTD_analysis <-unlist(RTD_analysis)
RTD_analysis <- RTD_analysis %>% 
  unnest_tokens(word, text)
RTD_analysis <- data_frame(line = 1:45251, text = RTD_analysis)
RTD_analysis <- RTD_analysis %>%
  unnest_tokens(word, text)
bing_positive <- get_sentiments("bing") %>%
  filter(sentiment == "positive")
bing_negative <- get_sentiments("bing") %>%
  filter(sentiment == "negative")
#positive and negative & anger, anticipation and joy
positive <- RTD_analysis %>%
  inner_join(bing_positive)%>%
  count(word, sentiment,sort = TRUE)
negative <- RTD_analysis %>%
  inner_join(bing_negative)%>%
  count(word, sentiment, sort = TRUE)

anger <-get_sentiments("nrc") %>%
  filter(sentiment == "anger")
anticipation <-get_sentiments("nrc") %>%
  filter(sentiment == "anticipation")
joy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")
RTD_anger<-RTD_analysis %>%
  inner_join(nrc_anger) %>%
  count(word, sentiment, sort = TRUE)
RTD_anticipation<-RTD_analysis %>%
  inner_join(nrc_anticipation) %>%
  count(word, sentiment,sort = TRUE)
RTD_joy<-RTD_analysis %>%
  inner_join(nrc_joy) %>%
  count(word, sentiment, sort = TRUE)

#Bar Plot for sentimental analysis
slices <- c(anticipation_total <- sum(RTD_anticipation$n), anger_total <- sum(RTD_anger$n)
            , joy_total <- sum(RTD_joy$n))
lbls <- c("anticipation", "anger", "joy")
pie3D(slices,labels=lbls,explode=0.1,
      main="Real Time: Pie Chart of sentiment analysis")

#although the postive word "like" has been mentioned 200 times, but "like" in this content
#doesn't really have any meaning. we could conclude that, as we expected, most comments are 
#negative and intense. We could also conclude from the pie chart that anger has occupy a large proption of the comments
#However, one thing that needs to notice is that people's anticipations 
#or suggestions on the current situation have not been mentioned a lot in the comment
#but this might attribute to that the word "gun", which implies the gun control is include in the anger sentiment. 




#Historical Data: also collect the comments from abc, CNN, NBC and CBS channel's school shoting news with the most clike
#however, since the comments are not enough, I also collect comments from other school shooting video with high clike rate. 
#his_comments_abc <- get_all_comments(video_id = c('aRHcbJ9DHEg'))
#his_comments_abc2 <- get_all_comments(video_id = c('6X7cVDxYd6A')), if not enough, add
#his_comments_cnn <- get_all_comments(video_id = c('nPpEpNk519U'))
#his_comments_nbc <- get_all_comments(video_id = c('icVShYaYxEM'))
#his_comments_fox <- get_all_comments(video_id = c('-6RhIU5Dreo'))
#his_comments_cbs <- get_all_comments(video_id = c('-9XGNIKW-uM'))
#his_comments_random <- get_all_comments(video_id = c('UeO5QTAryNE'))

#his_all_comments <- rbind(his_comments_abc, his_comments_cnn, his_comments_nbc, 
#                          his_comments_fox, his_comments_cbs,his_comments_random)
#his_data <- data.frame(his_all_comments$authorDisplayName,
#                       his_all_comments$textOriginal,
#                       his_all_comments$likeCount,
#                       his_all_comments$publishedAt)
#clean historical data
#his_data[,4]<-str_split_fixed(his_data$all_comments.publishedAt, "T", 2)[,1]
#his_data[,4] <- ymd(his_data[,4])

#For historical data purpose, only collect data between 2012/01/1 and 2017/12/31
#his_data <- his_data %>% 
#  filter(his_all_comments.publishedAt >= as.Date("2012-01-1") & 
#           his_all_comments.publishedAt<= as.Date("2017-12-31") )
#his_data_2012 <- his_data %>%
#  filter(his_all_comments.publishedAt >= as.Date("2012-01-1") & 
#           his_all_comments.publishedAt<= as.Date("2012-12-31") )
#his_data_2012<- data.frame(his_data_2012$his_all_comments.textOriginal)
#his_data_2013 <- his_data %>%
#  filter(his_all_comments.publishedAt >= as.Date("2013-01-1") & 
#           his_all_comments.publishedAt<= as.Date("2013-12-31") )
#his_data_2013<- data.frame(his_data_2013$his_all_comments.textOriginal)
#his_data_2014 <- his_data %>%
#  filter(his_all_comments.publishedAt >= as.Date("2014-01-1") & 
#           his_all_comments.publishedAt<= as.Date("2014-12-31") )
#his_data_2014<- data.frame(his_data_2014$his_all_comments.textOriginal)
#his_data_2015 <- his_data %>%
#  filter(his_all_comments.publishedAt >= as.Date("2015-01-1") & 
#           his_all_comments.publishedAt<= as.Date("2015-12-31") )
#his_data_2015<- data.frame(his_data_2015$his_all_comments.textOriginal)
#his_data_2016 <- his_data %>%
#  filter(his_all_comments.publishedAt >= as.Date("2016-01-1") & 
#           his_all_comments.publishedAt<= as.Date("2016-12-31") )
#his_data_2016<- data.frame(his_data_2016$his_all_comments.textOriginal)
#his_data_2017 <- his_data %>%
#  filter(his_all_comments.publishedAt >= as.Date("2017-01-1") & 
#           his_all_comments.publishedAt<= as.Date("2017-12-31") )
#his_data_2017<- data.frame(his_data_2017$his_all_comments.textOriginal)

#write.csv(his_data, file="his_data.csv")
#write.csv(his_data_2012, file="his_data_2012.csv")
#write.csv(his_data_2013, file="his_data_2013.csv")
#write.csv(his_data_2014, file="his_data_2014.csv")
#write.csv(his_data_2015, file="his_data_2015.csv")
#write.csv(his_data_2016, file="his_data_2016.csv")
#write.csv(his_data_2017, file="his_data_2017.csv")
real_time <- read.csv("real.csv", header=TRUE, stringsAsFactors = FALSE, fileEncoding = "latin1")
real_time_comments <- Corpus(VectorSource(real_time))
his_data_2012 <- read.csv("his_data_2012.csv", header=TRUE, stringsAsFactors = FALSE, fileEncoding = "latin1")
his_data_comments_2012 <-Corpus(VectorSource(his_data_2012))
his_data_2013 <- read.csv("his_data_2013.csv", header=TRUE, stringsAsFactors = FALSE, fileEncoding = "latin1")
his_data_comments_2013 <-Corpus(VectorSource(his_data_2013))
his_data_2014 <- read.csv("his_data_2014.csv", header=TRUE, stringsAsFactors = FALSE, fileEncoding = "latin1")
his_data_comments_2014 <-Corpus(VectorSource(his_data_2014))
his_data_2015 <- read.csv("his_data_2015.csv", header=TRUE, stringsAsFactors = FALSE, fileEncoding = "latin1")
his_data_comments_2015 <-Corpus(VectorSource(his_data_2015))
his_data_2016 <- read.csv("his_data_2016.csv", header=TRUE, stringsAsFactors = FALSE, fileEncoding = "latin1")
his_data_comments_2016 <-Corpus(VectorSource(his_data_2016))
his_data_2017 <- read.csv("his_data_2017.csv", header=TRUE, stringsAsFactors = FALSE, fileEncoding = "latin1")
his_data_comments_2017 <-Corpus(VectorSource(his_data_2017))

#text cleaning
clean.text = function(x)
{x <- tm_map(x, removeNumbers)
x <- tm_map(x, removeWords, stopwords("english"))
x <- tm_map(x, removePunctuation )
x <- tm_map(x, removeNumbers)
x <- tm_map(x, content_transformer(tolower))
x <- tm_map(x, removeWords, c("this","so", "will", "can", "why","said",
                              "get","really","just", "like","the","one", 
                              "even", "you", "know","say",
                              "think", "dont", "people","guns"))

x <- tm_map(x, stripWhitespace)
}

# clean texts
clean_real = clean.text(real_time_comments)
clean_2012 = clean.text(his_data_comments_2012)
clean_2013 = clean.text(his_data_comments_2013)
clean_2014 = clean.text(his_data_comments_2014)
clean_2015 = clean.text(his_data_comments_2015)
clean_2016 = clean.text(his_data_comments_2016)
clean_2017 = clean.text(his_data_comments_2017)

comments_real = paste(clean_real, collapse=" ")
comments_2012 = paste(clean_2012, collapse=" ")
comments_2013 = paste(clean_2013, collapse=" ")
comments_2014 = paste(clean_2014, collapse=" ")
comments_2015 = paste(clean_2015, collapse=" ")
comments_2016 = paste(clean_2016, collapse=" ")
comments_2017 = paste(clean_2017, collapse=" ")

# put everything in a single vector
all = c(comments_real, comments_2012, comments_2013,comments_2014,comments_2015,comments_2016,comments_2017)
c(stopwords("english"),"comments_real", "comments_2012", "comments_2013", "comments_2014", "comments_2015",
  "comments_2016", "comments_2017")
corpus = Corpus(VectorSource(all))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = c("comments_real","comments_2012","comments_2013","comments_2014","comments_2015","comments_2016","comments_2017")
#Comparison Word Cloud

comparison.cloud(tdm, random.order=FALSE, 
                 colors = c("pink", "#00B2FF", "red", "#FF0099", "#6600CC","blue","green"),
                 title.size=1)
#according to the word cloud we could notice that comments in the 2017 is the most, despite 
#that we only include part of the comments in 2018. We could conjecutred that school shootings
#become more and more serious and importnat among people around 2017, so there are more comments
#begin from 2017. However, we still should addmitted that 2017 has the most comments might also 
#due to other circumstances. Most words in the word cloud are negative words, although they 
#contain the same emotions, we could detect that the main topic toward shool shootings
#among different years are slight different. real time daya is more focus on the judgement
#toward the shooter, with words like judge, penalty. 2017's comments are mainly around school
#Moreover, 2014's comments have mentioned about words like goverment and crime.


# show the top 10 words in the word cloud and their frequency
# frequency function
frequency.text = function(x){
  x<-TermDocumentMatrix(x)
  x<- as.matrix(x)
  x<- sort(rowSums(x), decreasing=TRUE)
  x<- data.frame(word=names(x), freq=x)
  x<- head(x, 10)
}
frequency_real = frequency.text(clean_real)
frequency_2012 = frequency.text(clean_2012)
frequency_2013 = frequency.text(clean_2013)
frequency_2014 = frequency.text(clean_2014)
frequency_2015 = frequency.text(clean_2015)
frequency_2016 = frequency.text(clean_2016)
frequency_2017 = frequency.text(clean_2017)
structure <- par(mfrow=c(3,3))
barplot(frequency_real$freq, width=1,space= 0.5,
        names.arg = frequency_real$word,ylim=c(0,2500),
        col =rainbow(20), main ="Most frequent words_real",
        ylab = "Word frequencies",cex.names = 0.6) 
barplot(frequency_2012$freq, width=1,space= 0.5,
        names.arg = frequency_2012$word,ylim=c(0,2500),
        col =rainbow(20), main ="Most frequent words_2012",
        ylab = "Word frequencies",cex.names = 0.6) 
barplot(frequency_2013$freq, width=1,space= 0.5,
        names.arg = frequency_2013$word,ylim=c(0,2500),
        col =rainbow(20), main ="Most frequent words_2013",
        ylab = "Word frequencies",cex.names = 0.6) 
barplot(frequency_2014$freq, width=1,space= 0.5,
        names.arg = frequency_2014$word,ylim=c(0,2500),
        col =rainbow(20), main ="Most frequent words_2014",
        ylab = "Word frequencies",cex.names = 0.6) 
barplot(frequency_2015$freq, width=1,space= 0.5,
        names.arg = frequency_2015$word,ylim=c(0,2500),
        col =rainbow(20), main ="Most frequent words_2015",
        ylab = "Word frequencies",cex.names = 0.6) 
barplot(frequency_2016$freq, width=1,space= 0.5,
        names.arg = frequency_2016$word,ylim=c(0,2500),
        col =rainbow(20), main ="Most frequent words_2016",
        ylab = "Word frequencies",cex.names = 0.6) 
barplot(frequency_2017$freq, width=1,space= 0.5,
        names.arg = frequency_2017$word,ylim=c(0,2500),
        col =rainbow(20), main ="Most frequent words_2017",
        ylab = "Word frequencies",cex.names = 0.6) 
dev.off()

#According to these graph, although different years are having different most frequent words, 
#we could clearly see that the word "gun" has been highly mentioned in the 2012, 2013, 2014, 2015, and 
#2018, which is the real time data. Although in 2016 and 2017 the word "gun" has not been mentioned
#as the most frequent word, it is still in the rank of the top ten mentioned words. This indicates
#that as the school shooting happening, the most comments, besides the expression of sympathy to 
#the children, would be the eagserness of the gun control or things related to gun. It is a 
#continuous topic from 2012 to 2018 that people keep dicussing and debating about. 

#the real time data is only from 04/01 till 04/22, so it has smaller amount of words compare to the rest. 
#2012 also has small amount of words because it is long time ago and people might not be that active online 
# to comment during 2012 


#Time Series on video related with school shootings
#Real Time Data for how many reporting about school shootings/time series something like that 
news <- yt_search("school shootings", lang="en", published_after =  "2018-04-10T00:00:00Z")
new <- yt_search("school shooting", lang ="en",published_after =  "2018-04-10T00:00:00Z")
News <- rbind(news,new)
News <- data.frame(News$publishedAt,
                   News$title)
#Eliminating the same title
News <- News[!duplicated(News$News.title),]
#2017 
news_2017 <- yt_search("school shootings", lang="en", published_after =  "2017-01-1T00:00:00Z", published_before = "2017-12-31T00:00:00Z")
new_2017 <- yt_search("school shooting", lang ="en",published_after =  "2017-01-1T00:00:00Z", published_before = "2017-12-31T00:00:00Z")
News_2017 <- rbind(news_2017,new_2017)
News_2017 <- data.frame(News_2017$publishedAt,
                   News_2017$title)
News_2017 <- News_2017[!duplicated(News_2017$News_2017.title),]
#2016
news_2016 <- yt_search("school shootings", lang="en", published_after =  "2016-01-1T00:00:00Z", published_before = "2016-12-31T00:00:00Z")
new_2016 <- yt_search("school shooting", lang ="en",published_after =  "2016-01-1T00:00:00Z", published_before = "2016-12-31T00:00:00Z")
News_2016 <- rbind(news_2016,new_2016)
News_2016 <- data.frame(News_2016$publishedAt,
                        News_2016$title)
News_2016 <- News_2016[!duplicated(News_2016$News_2016.title),]

#2015
news_2015 <- yt_search("school shootings", lang="en", published_after =  "2015-01-1T00:00:00Z", published_before = "2015-12-31T00:00:00Z")
new_2015 <- yt_search("school shooting", lang ="en",published_after =  "2015-01-1T00:00:00Z", published_before = "2015-12-31T00:00:00Z")
News_2015 <- rbind(news_2015,new_2015)
News_2015 <- data.frame(News_2015$publishedAt,
                        News_2015$title)
News_2015 <- News_2015[!duplicated(News_2015$News_2015.title),]
#2014
news_2014 <- yt_search("school shootings", lang="en", published_after =  "2014-01-1T00:00:00Z", published_before = "2014-12-31T00:00:00Z")
new_2014 <- yt_search("school shooting", lang ="en",published_after =  "2014-01-1T00:00:00Z", published_before = "2014-12-31T00:00:00Z")
News_2014 <- rbind(news_2014,new_2014)
News_2014 <- data.frame(News_2014$publishedAt,
                        News_2014$title)
News_2014 <- News_2014[!duplicated(News_2014$News_2014.title),]
#2013
news_2013 <- yt_search("school shootings", lang="en", published_after =  "2013-01-1T00:00:00Z", published_before = "2016-12-31T00:00:00Z")
new_2013 <- yt_search("school shooting", lang ="en",published_after =  "2013-01-1T00:00:00Z", published_before = "2016-12-31T00:00:00Z")
News_2013 <- rbind(news_2013,new_2013)
News_2013 <- data.frame(News_2013$publishedAt,
                        News_2013$title)
News_2013 <- News_2013[!duplicated(News_2013$News_2013.title),]
#2012
news_2012 <- yt_search("school shootings", lang="en", published_after =  "2012-01-1T00:00:00Z", published_before = "2012-12-31T00:00:00Z")
new_2012 <- yt_search("school shooting", lang ="en",published_after =  "2012-01-1T00:00:00Z", published_before = "2012-12-31T00:00:00Z")
News_2012 <- rbind(news_2012,new_2012)
News_2012 <- data.frame(News_2012$publishedAt,
                        News_2012$title)
News_2012 <- News_2012[!duplicated(News_2012$News_2012.title),]

#time series for related school shooting video from 2012 to 2018
ts_shooting <- ts(c(nrow(News_2012), nrow(News_2013), nrow(News_2014), 
                    nrow(News_2015), nrow(News_2016), nrow(News_2017),
                    nrow(News) ), start=c(2012), end=c(2018))
plot(ts_shooting)

#we can see from the graph that although 

#Anova table of the relationship between gun and like?








