#Important libraries needed for analysis, to be updated as we go...
library(readtext)
library(quanteda)
library(ggplot2)
library(reshape2)

#set your working directory here
setwd("C:/Users/joshua/Downloads/BC2406 Analytics I/BC2406 AY21 Team Assignment and Project/Datasets Cleaned/Sentiment Analysis")

# Increase to max available from default 2 threads to speed up computations.
quanteda_options("threads" = 4)


# Import text files from a folder
argps.data <- readtext("*.txt",
                     docvarsfrom = "filenames",
                     docvarnames = c("Year", "Country", "PoliticalOutlook"),
                     dvsep = "_",
                     encoding = "UTF-8")

# Create a Corpus
argps.corpus <- corpus(argps.data)
summary(argps.corpus)
## Metadata attributes Year, Country, PoliticalOutlook can be used to subset Corpus

argps.tokens1 <- tokens(argps.corpus, remove_punct = T, remove_numbers = T)
sum(ntoken(argps.tokens1))


# Apply word stemming ------------------------
argps.tokens2 <- tokens_wordstem(argps.tokens1)
sum(ntoken(argps.tokens2))

# All lower case ----------------------------
argps.tokens3 <- tokens_tolower(argps.tokens2)
sum(ntoken(argps.tokens3))


# Apply Lexicoder Dictionary and adjust for negated words ---------------------

dfm.lsd <- dfm(argps.tokens3, dictionary = data_dictionary_LSD2015)
argps.lsd.df <- convert(dfm.lsd, to = "data.frame")

# Adjusting the negatives due to negated words
argps.lsd.df$adj.negative <- argps.lsd.df$negative + argps.lsd.df$neg_positive - argps.lsd.df$neg_negative

# Adjusting the positives due to negated words
argps.lsd.df$adj.positive <- argps.lsd.df$positive + argps.lsd.df$neg_negative - argps.lsd.df$neg_positive

argps.lsd.df$sentiment <- argps.lsd.df$adj.positive - argps.lsd.df$adj.negative

argps.lsd.df
#-------------------------------------------------------------------------------

# Visualise Sentiment Trends ---------------------------------------------------

sentiment.df <- data.frame(Year = argps.data$Year, Positive = argps.lsd.df$adj.positive, Negative = argps.lsd.df$adj.negative, stringsAsFactors = F)

sentiment.long <- melt(sentiment.df, id = "Year")
colnames(sentiment.long)[2] <-"Sentiment"
colnames(sentiment.long)[3] <-"Score"

ggplot(data = sentiment.long, aes(x = Year, y = Score, colour = Sentiment)) + 
  geom_line() +
  labs(title = "Argentina Sentiment Scores of Political Outlook (2014-2020)",
       subtitle = "Adjusted for negated words",
       caption = "Sentiment Analysis on Political Stability of Argentina")

#By Conducting Sentiment Analysis on the political outlook of Argentina
#We can see that there is a general downward trend in positive sentiment.
#Negative sentiment hit a high in 2016, but subsequently started falling 
#The fall in positive sentiment is similar to that of the negative sentiment.
#Overall sentiment is still negative.
