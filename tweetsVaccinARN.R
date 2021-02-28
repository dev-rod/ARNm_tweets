# https://github.com/ropensci/rtweet
# https://cran.r-project.org/web/packages/rtweet/readme/README.html
# https://rtweet.info/articles/auth.html
# https://www.infoworld.com/article/3515712/how-to-search-twitter-with-rtweet-and-r.html
if("httpuv" %in% rownames(installed.packages()) == FALSE) {install.packages("httpuv")};library(httpuv)
if("rtweet" %in% rownames(installed.packages()) == FALSE) {install.packages("rtweet")};library(rtweet)
if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")};library(tidyverse)
if("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")};library(lubridate)
if("plyr" %in% rownames(installed.packages()) == FALSE) {install.packages("plyr")};library(plyr)
if("rgexf" %in% rownames(installed.packages()) == FALSE) {install.packages("rgexf")};library(rgexf)

# generate tweeter token for session
create_token(
    app = Sys.getenv("TWEETER_APP_NAME"),
    Sys.getenv("TWEETER_CONSUMER_KEY"),
    Sys.getenv("TWEETER_CONSUMER_SECRET"),
    access_token = Sys.getenv("TWEETER_ACCESS_TOKEN"),
    access_secret = Sys.getenv("TWEETER_ACCESS_SECRET"),
    set_renv = TRUE
)


# ymd_hms(now())
# format(Sys.Date(), format="%Y%m%d%H%M%S")
# format(now(), format="%Y%m%d%H%M%S")

## search for tweets
# https://www.rdocumentation.org/packages/rtweet/versions/0.7.0/topics/search_tweets
tweets <- search_tweets2(
    c("\"arn messager\" OR ARNm", "vaccin AND (pfizer OR biontech OR moderna OR ARNm)", "vaccin AND (fiabilité OR fiable OR efficace OR inefficacité OR inefficace OR efficacité OR inopérant OR faible OR inutile OR sûreté OR sureté OR sûr)"), n=18000, type="recent", include_rts=TRUE, lang="fr", retryonratelimit = TRUE
)
# save tweets in csv file
write_as_csv(tweets, str_c("csv/ARNmTweets_", format(now(), format="%Y%m%d%H%M%S"), ".csv"), prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")


##########################################
tweets <- search_tweets2(
    c("\"arn messager\" OR ARNm", "vaccin AND (pfizer OR biontech OR moderna OR ARNm)", "vaccin AND (fiabilité OR fiable OR efficace OR inefficacité OR inefficace OR efficacité OR inopérant OR faible OR inutile OR sûreté OR sureté OR sûr)"), n=18000, type="recent", include_rts=TRUE, lang="fr", retryonratelimit = FALSE
)
#colnames(tweets)
data_tweet <- tweets[,c(1:5,8:9,11:12,17,30,33:35,39,48,54,73:75,78,79,83:85,91)]
#write_as_csv(data_tweet, str_c("csv/data_tweet_", format(now(), format="%Y%m%d%H%M%S"), ".csv"), prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")


followers_counts+friends_count

arn_query_twt <- data_tweet[which(data_tweet$query=="\"arn messager\" OR ARNm") ,]
hahstags <- data_tweet[which(data_tweet$is_retweet==TRUE) ,]
retweets <- data_tweet[which(data_tweet$is_retweet==TRUE) ,]

# user_id, retweet_user_id, "mentions_user_id"
gexf_nodes <- distinct(retweets[,c(1,4)])
gexf_edges <- retweets[,c(17,1)]
gexf_edgesId <- retweets[,c(2)]
gexf_edgesWeight <- retweets[,c(2)]
nodesAtt

str(gexf_nodes)
str(gexf_edges)
# generate 
write.gexf(nodes=gexf_nodes, edges=gexf_edges, output=str_c("gexf/ARNmTweets_", format(now(), format="%Y%m%d%H%M%S"), ".gexf"))
##########################################

mydir = "csv"
myfiles = list.files(path=mydir, pattern="*.csv", full.names=TRUE)
myfiles
all_tweets = ldply(myfiles, read_csv)
summary(all_tweets)

uniq_all_tweets <- all_tweets %>% distinct(user_id, status_id, created_at, screen_name, text, reply_to_status_id, is_quote, is_retweet, retweet_count, quote_count, hashtags, mentions_user_id, retweet_status_id, retweet_user_id, geo_coords, coords_coords, followers_count, friends_count )
write_as_csv(uniq_all_tweets, str_c("csv/complete/completeARNmTweets_", format(now(), format="%Y%m%d%H%M%S"), ".csv"), prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")

sapply (uniq_all_tweets, function(x) length(unique(x)))
# environ 60 000 user pour 180 000 tweets et 100 000 retweet et 60 000 ???

uniq_all_tweets %>%
    ts_plot("3 hours") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
    ggplot2::labs(
        x = NULL, y = NULL,
        title = "Frequency of arn messager Twitter statuses from past 9 days",
        subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
        caption = "\nSource: Data collected from Twitter's REST API via rtweet"
    )

