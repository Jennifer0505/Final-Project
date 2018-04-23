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

clientID <- "979385012952-iive5jqvebq2emfhrnld66iflt1rq1t8.apps.googleusercontent.com"
client_secrete <- "F5n5Sxt2PXkeeUnnI89mJGo5"
yt_oauth(clientID, client_secrete, token = '')
#Collecting Real Time Data for comments
#Selecting abc, CNN, NBC, fox and CBS channel's school shootings' news with the most click.
comments_abc <- get_all_comments(video_id = c('xgIJosk0pnA'))
comments_cnn <- get_all_comments(video_id = c('yW61tS8H66E'))
comments_NBC <-get_all_comments(video_id = c('3F_vdCHlDds'))
comments_fox <-get_all_comments(video_id = c('ZQcaYYHqrUY'))
comments_cbs <-get_all_comments(video_id = c('npnH0_natfs'))
all_comments <- rbind(comments_abc, comments_cnn, comments_NBC, comments_fox, comments_cbs)
real_time_data <- data.frame(all_comments$authorDisplayName,
                   all_comments$textOriginal,
                   all_comments$likeCount,
                   all_comments$publishedAt)
#clean real_time_data
real_time_data[,4]<-str_split_fixed(real_time_data$all_comments.publishedAt, "T", 2)[,1]
real_time_data[,4] <- ymd(real_time_data[,4])
#For real time data purpose, only collect data after 2018/04/1
real_time_data <- real_time_data %>%select(all_comments.authorDisplayName, 
                         all_comments.likeCount, 
                         all_comments.textOriginal,
                         all_comments.publishedAt) %>%
                         filter(all_comments.publishedAt >= as.Date("2018-04-1"))
#write.csv(real_time_data, file="real_time_data.csv")
#Word Cloud
real_time_data <- read.csv("real_time_data.csv", header=TRUE, stringsAsFactors = FALSE, fileEncoding = "latin1")
real_time_data_comments <- Corpus(VectorSource(real_time_data$all_comments.textOriginal))
#text cleaning
#real_time_data_comments[[1]][1], test the text cleaning
real_time_data_comments <- tm_map(real_time_data_comments, removeNumbers)
real_time_data_comments  <- tm_map(real_time_data_comments , removeWords, stopwords("english"))
real_time_data_comments <- tm_map(real_time_data_comments ,removePunctuation )
real_time_data_comments <- tm_map(real_time_data_comments, removeNumbers)
real_time_data_comments <- tm_map(real_time_data_comments, content_transformer(tolower))
real_time_data_comments <- tm_map(real_time_data_comments, removeWords, c("this","so", "will", "can", 
                                                                          "why","said","get","really",
                                                                          "just", "like","the","one", 
                                                                          "even", "you", "know","say",
                                                                          "think", "dont", "people", "school"))
real_time_data_comments <- tm_map(real_time_data_comments, stripWhitespace)
real_time_data_comments <- TermDocumentMatrix(real_time_data_comments)
real_time_data_comments <- as.matrix(real_time_data_comments)
real_time_data_comments <- sort(rowSums(real_time_data_comments), decreasing=TRUE)
real_time_data_comments <- data.frame(word=names(real_time_data_comments), freq=real_time_data_comments)
wordcloud(real_time_data_comments$word, real_time_data_comments$freq, random.order = FALSE, 
          rot.per = 0.3, scale = c(4,.5), max.words=800, colors = rainbow(800),vfont= c ( "sans serif" , "plain" ))

#according to this word cloud we can see that most people express intense emotions, and the word express the most is 
# gun.Besides that, they also express the sympathy to the children with words like "bad" and "sad". So basiclly two emotions, one is intense
#another is sympathy, they also talks about shooter, judge, bad, this is their jugment towards the shooter
#death and dead and students also mentions a lot, which indicates the life is precoise. However, this 
#frequency of words is predictable becuase expect the video that is the news, the video with most clikc in the CNN is parents and teachers
#push for change after school shooting, so that is why the comments metioned a lot of gun and gun control
#

#Historical Data 

#Sentiment Analysis




#Real Time Data for how many reporting about school shootings
news <- yt_search("school shootings", lang="en", published_after =  "2018-04-10T00:00:00Z")
new <- yt_search("school shooting", lang ="en",published_after =  "2018-04-10T00:00:00Z")
News <- rbind(news,new)
News <- data.frame(News$publishedAt,
                   News$title)
#Eliminating the same title
News <- News[!duplicated(News$News.title),]
#Clean the data

#Sentiment Analysis
tidy_data <- data_frame(line=1:24421, text=data)

tidy_real_data<- data %>%
  unnest_tokens(word, text)

#location of users?



