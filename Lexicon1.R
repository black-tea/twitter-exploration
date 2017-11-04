library(syuzhet)
library(rtweet)

## name of app you created (replace rtweet_token with name of your app)
appname <- "twitter-sentiment8553"

## api key (example below is not a real key)
key <- "nsVVHL7L5xU35268dAsXy7Ygu"

## api secret (example below is not a real key)
secret <- "yxuOWAnmyQU5odqTV5y9TA3C7E9T3bFj3hLYbgqNUJLghy9ari"

## create a token, storing it as object 'twitter_token'
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret
)

saveRDS(twitter_token, "~/.rtweet-oauth.rds")

# ideally put this in ~/.Renviron
Sys.setenv(TWITTER_PAT=path.expand("~/.rtweet-oauth.rds"))


##### Analysis

# Grab tweets
vz_tweet <- search_tweets("visionzerola", n = 1000, token = twitter_token)

# Glimplse data set
dplyr::glimpse(rt)

# Clean data
rt$text <- sapply(rt$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
vz_tweet$text <- sapply(vz_tweet$text,function(row) iconv(row, "latin1", "ASCII", sub=""))

dot_sentiment <- get_nrc_sentiment(rt$text)
vz_sentiment <- get_nrc_sentiment(vz_tweet$text)

dplyr::glimpse(dot_sentiment)

# Calculate Sentiment Totals and Plot
sentimentTotals <- data.frame(colSums(vz_sentiment))
names(sentimentTotals) <- "count"
sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
rownames(sentimentTotals) <- NULL
ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for All Tweets")
