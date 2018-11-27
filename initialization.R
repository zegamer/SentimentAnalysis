library(ROAuth)
library(twitteR)

# Authorization
# Get twitter tokens and keys by registering your app
# at https://developer.twitter.com/en/apps
consumer_key = ""
consumer_secret = ""
access_token = ""
access_secret = ""
setup_twitter_oauth(consumer_key ,consumer_secret, access_token,  access_secret )
cred = OAuthFactory$new(consumerKey = consumer_key, consumerSecret = consumer_secret,requestURL='https://api.twitter.com/oauth/request_token',accessURL='https://api.twitter.com/oauth/access_token',authURL='https://api.twitter.com/oauth/authorize')
# Obtain cacert.pem from https://curl.haxx.se/ca/cacert.pem
# cred$handshake(cainfo="assets/cacert.pem")


############################################################################
#Getting textual parts of tweets


# srh = readline(prompt = "*********** Enter search query *********** : ")
search.tweets = searchTwitter("Donald+Trump", n=135)

sample = NULL #Initialising
for (tweet in search.tweets)
  sample = c(sample,tweet$getText())

#converts to data frame
df = do.call("rbind", lapply(search.tweets, as.data.frame))
#remove odd characters
df$text = sapply(df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
df$text = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", df$text)
sample = df$text

############################################################################
#Building positive and negative word database
pos = scan('assets/positive.txt', what='character', comment.char=';')
neg = scan('assets/negative.txt', what='character', comment.char=';')