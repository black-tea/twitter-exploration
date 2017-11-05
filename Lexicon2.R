library(syuzhet)
library(ggplot2)
library(dplyr)
library(lubridate) # with_tz function
library(reshape2) # with melt() function
library(scales) # with date_breaks() function

### Data Prep
# Load Data
dot_df <- read.csv('C:/Users/Tim/Documents/GitHub/twitter-exploration/twitterscraper_data.csv')

# Clean data, Calculate Sentiment, & Add columns
dot_df$text <- sapply(dot_df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
dot_sentiment <- get_nrc_sentiment(dot_df$text)
tweets <- cbind(dot_df, dot_sentiment)

### Sentiment Totals
# Calculate Sentiment Totals and Plot
sentimentTotals <- data.frame(colSums(dot_sentiment))
names(sentimentTotals) <- "count"
sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
rownames(sentimentTotals) <- NULL

# Plot Sentiment Totals
ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for All Tweets")

### Sentiment Over Time
# Calculate Sentiment over Time
tweets$timestamp <- with_tz(ymd_hms(tweets$timestamp), "America/Chicago")
monthlysentiment <- tweets %>% 
  # Group by Month
  group_by(timestamp = cut(timestamp, breaks="2 weeks")) %>%
  # For each month, sum totals of each emotion
  summarise(negative = sum(negative),
            positive = sum(positive)) %>%#,
            #anger = sum(anger),
            #anticipation = sum(anticipation),
            #disgust = sum(disgust),
            #fear = sum(fear),
            #joy = sum(joy),
            #sadness = sum(sadness),
            #surprise = sum(surprise),
            #trust = sum(trust)) %>% 
  # remove first/last (incomplete) months
  slice(3:n()-2) %>% 
  # then Use melt to flatten table
  melt
names(monthlysentiment) <- c("month", "sentiment", "count")

# Plot Sentiment over Time
ggplot(data = monthlysentiment, aes(x = as.Date(month), y = count, group = sentiment)) +
  geom_line(size = 1, alpha = 0.7, aes(color = sentiment)) +
  #geom_point(size = 0.5) +
  ylim(0, NA) +
  theme(legend.title=element_blank(), axis.title.x = element_blank()) +
  scale_x_date(breaks = date_breaks("3 months"), 
               labels = date_format("%Y-%b")) +
  # VdM Restriped
  geom_vline(aes(xintercept = as.numeric(as.Date("2017-05-26"))), 
             linetype=4, col = "black") +
  annotate("text",x = as.Date("2017-05-26"),y=300, label= "VdM Restriped\n", size=3, angle=90) +
  # VdM Reversal Announced
  geom_vline(aes(xintercept = as.numeric(as.Date("2017-07-26"))), 
             linetype=4, col = "black") +
  annotate("text",x = as.Date("2017-07-26"),y=100, label= "VdM Reversal Announced\n", size=3, angle=90) +
  # Full Reversal Announced w/ Garcetti
  geom_vline(aes(xintercept = as.numeric(as.Date("2017-10-18"))), 
             linetype=4, col = "black") +
  annotate("text",x = as.Date("2017-10-18"),y=425, label= "Full PdR Reversal Announced\n", size=3, angle=90) +
  # Culver / Jefferson Westbound Travel Lane Reversal Announced
  geom_vline(aes(xintercept = as.numeric(as.Date("2017-10-2"))), 
             linetype=4, col = "black") +
  annotate("text",x = as.Date("2017-10-2"),y=425, label= "Culver/Jefferson WBound Reversal\n", size=3, angle=90) +  
  ylab("Sentiment Count") + 
  ggtitle("Tweet Sentiments Involving 'LADOT'")
