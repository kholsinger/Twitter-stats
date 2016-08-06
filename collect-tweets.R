library(ggplot2)
library(plyr)
library(twitteR)
library(RSQLite)

rm(list=ls())

## get these from Twitter
##
consumer_key <- ""
consumer_secret <- ""
access_token <- ""
access_secret <- ""
##
## base name for files
##
base.name <- "Botany-2016"
hashtag <- "botany2016"

## You shouldn't need to modify anything after this except for the dates
##


##
## filenames
##
sqlite <- paste(base.name,".sqlite", sep="")
tweets <- paste(base.name, "-tweets.png", sep="")
sentiment <- paste(base.name, "-sentiment.png", sep="")
tweeters.cumulative <- paste(base.name, "-tweeters-cumulative.png", sep="")
tweeters <- paste(base.name, "-tweeters.png", sep="")
impact <- paste(base.name, "-impact.png", sep="")
likes <- paste(base.name, "-likes.png", sep="")

## copied directly from
## http://www.inside-r.org/howto/mining-twitter-airline-consumer-sentiment
##
## Author: Jeffrey Breen
##
## N.B.: Page seems to have disappeared as of 6 August 2016
##
score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)

  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {

    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    ## 2016-07-02 KEH: tryCatch() added
    sentence = tryCatch(tolower(sentence), error=function(err) {return(0)})

    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)

    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)

    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)

    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)

    return(score)
  }, pos.words, neg.words, .progress=.progress )

  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

positive_words <- scan("positive-words.txt",
                       what="character",
                       comment.char=";")
negative_words <- scan("negative-words.txt",
                       what="character",
                       comment.char=";")

setup_twitter_oauth(consumer_key=consumer_key,
                    consumer_secret=consumer_secret,
                    access_token=access_token,
                    access_secret=access_secret)

register_sqlite_backend(sqlite)
search_twitter_and_store(hashtag)

con <- dbConnect(SQLite(), sqlite)
tweets.df <- dbGetQuery(con, "SELECT * FROM tweets")
dbDisconnect(con)
tweets.df <- tweets.df[order(tweets.df$created),]
tweets.df$created <- as.POSIXct(tweets.df$created,
                                origin="1970-01-01",
                                tz="America/New_York")
tweets.df$date <- strftime(tweets.df$created,
                           format="%Y-%m-%d",
                           usetz=FALSE)
tweets.df$sentiment <- score.sentiment(tweets.df$text, positive_words, negative_words)$score

tweets.df <- subset(tweets.df, tweets.df$created < as.POSIXct("2016-08-06 00:00:00"))

for.plot <- ddply(tweets.df, c("date"), summarise,
                  Count=length(created), Sentiment=mean(sentiment))
p <- ggplot(for.plot, aes(x=as.Date(date), y=Count)) +
  geom_point(size=2) +
  stat_smooth(method="loess", span=0.2, se=TRUE) +
  xlab("Date") +
  ylab("Number of Tweets") +
  theme_bw() +
  ggtitle("Number of tweets per day")
print(p)
ggsave(tweets, width=5, height=4, dpi=300)

p <- ggplot(for.plot, aes(x=as.Date(date), y=Sentiment)) +
  geom_point(size=2) +
  stat_smooth(method="loess", span=0.2, se=TRUE) +
  xlab("Date") +
  geom_hline(yintercept=0, linetype="dashed", alpha=0.4) +
  theme_bw() +
  ggtitle("Average daily sentiment")
print(p)
ggsave(sentiment, width=5, height=4, dpi=300)

for.plot <- ddply(tweets.df, c("screenName"), summarise,
                  Count=length(created))
for.plot <- for.plot[order(-for.plot$Count),]
for.plot <- for.plot[1:10,]
for.plot$screenName <- reorder(for.plot$screenName, for.plot$Count, FUN=identity)
p <- ggplot(for.plot, aes(x=screenName, y=Count)) +
  geom_bar(stat="identity", width=0.25, color="blue", fill="blue") +
  xlab("Screen name") +
  coord_flip() +
  theme_bw() +
  ggtitle("Top 10 Tweeters (cumulative)")
print(p)
ggsave(tweeters.cumulative, width=5, height=4, dpi=300)

tweets.df <- subset(tweets.df, tweets.df$created > as.POSIXct("2016-07-31 00:00:00"))

for.plot <- ddply(tweets.df, c("screenName"), summarise,
                  Count=length(created))
for.plot <- for.plot[order(-for.plot$Count),]
for.plot <- for.plot[1:10,]
for.plot$screenName <- reorder(for.plot$screenName, for.plot$Count, FUN=identity)
p <- ggplot(for.plot, aes(x=screenName, y=Count)) +
  geom_bar(stat="identity", width=0.25, color="blue", fill="blue") +
  xlab("Screen name") +
  coord_flip() +
  theme_bw() +
  ggtitle("Top 10 Tweeters (since 30 July 2016)")
print(p)
ggsave(tweeters, width=5, height=4, dpi=300)

## measuring impact by
## 1) excluding retweets
## 2) weighting tweets by # of retweets
## 3) weighting tweets by # of likes
for.plot <- ddply(subset(tweets.df, isRetweet==0),
                  c("screenName"), summarise,
                  Impact=sum(retweetCount),
                  Liked=sum(favoriteCount))
for.plot <- for.plot[order(-for.plot$Impact),]
for.plot <- for.plot[1:10,]
for.plot$screenName <- reorder(for.plot$screenName, for.plot$Impact, FUN=identity)
p <- ggplot(for.plot, aes(x=screenName, y=Impact)) +
  geom_bar(stat="identity", width=0.25, color="blue", fill="blue") +
  xlab("Screen name") +
  coord_flip() +
  theme_bw() +
  ggtitle("Top 10 Tweeters by impact (# of retweets)")
print(p)
ggsave(impact, width=5, height=4, dpi=300)

for.plot <- for.plot[order(-for.plot$Liked),]
for.plot <- for.plot[1:10,]
for.plot$screenName <- reorder(for.plot$screenName, for.plot$Liked, FUN=identity)
p <- ggplot(for.plot, aes(x=screenName, y=Liked)) +
  geom_bar(stat="identity", width=0.25, color="blue", fill="blue") +
  xlab("Screen name") +
  coord_flip() +
  theme_bw() +
  ggtitle("Top 10 Tweeters by impact (# of likes)")
print(p)
ggsave(likes, width=5, height=4, dpi=300)
