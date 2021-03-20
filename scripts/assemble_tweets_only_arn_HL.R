# https://github.com/ropensci/rtweet
# https://cran.r-project.org/web/packages/rtweet/readme/README.html
# https://rtweet.info/articles/auth.html
# https://www.infoworld.com/article/3515712/how-to-search-twitter-with-rtweet-and-r.html
if("httpuv" %in% rownames(installed.packages()) == FALSE) {install.packages("httpuv")};library(httpuv)
if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")};library(tidyverse)
if("zip" %in% rownames(installed.packages()) == FALSE) {install.packages("zip")};library(zip)
if("syuzhet" %in% rownames(installed.packages()) == FALSE) {install.packages("syuzhet")};library(syuzhet)
if("plyr" %in% rownames(installed.packages()) == FALSE) {install.packages("plyr")};library(plyr)
if("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")};library(lubridate)
if("rtweet" %in% rownames(installed.packages()) == FALSE) {install.packages("rtweet")};library(rtweet)
if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")};library(rtweet)
tweetsCsvFiles <- list.files(path="csv", pattern="*.zip", full.names=TRUE)
#tweetsCsvFiles <- "csv/ARNmTweets_20210131095600.zip"

# ("user_id", "status_id", "created_at", "screen_name", "text", "reply_to_status_id", "reply_to_user_id",
#  "is_quote", "is_retweet", "hashtags", "mentions_user_id", "quoted_status_id", "quoted_text", "quoted_created_at"
#  "quoted_user_id", "retweet_status_id", "retweet_user_id", "name", "location", "description", "followers_count"
#  "friends_count", "account_created_at", "verified", "profile_url", "query")

get_score <- function(text) {
    char_v <- syuzhet::get_sentences(text)
    text_values <- syuzhet::get_sentiment(char_v, method="nrc", language="french")
    #text_values2 <- syuzhet::get_nrc_sentiment(char_v, language=lang)
    #message(sum(text_values))
    #message(text_values2)
    return(sum(text_values))
}

# pour chacun des fichiers csv zippés récupérés
readTweetFile <- function(filename)
{
    tweets <- read_csv(filename)
    
    #message(colnames(tweets))
    
    message("------------------------------")
    file_date <- format(now(), format="%Y-%m-%d %H:%M:%S")
    message(file_date)
    message(filename)
    #message(str(tweets$query))
    message(length(colnames(tweets)))
    
    # on retire les colonnes en trop
    tweets <- tweets[,c(1:5,8:9,12,14,17,30,48,54,73:75,78,79,83,85,91)]
    tweets<-dplyr::filter(tweets, grepl("arn messager",query))
    uniq_tweets <- tweets %>%
        distinct_at(vars(user_id, status_id, created_at, screen_name, text, reply_to_status_id, reply_to_user_id, is_retweet, retweet_count, hashtags, mentions_user_id, retweet_status_id, retweet_user_id, name, location, description, followers_count, friends_count))
    uniq_tweets <- uniq_tweets %>%
        group_by(user_id, status_id, created_at, screen_name, text, reply_to_status_id, reply_to_user_id, is_retweet, retweet_count, hashtags, mentions_user_id, retweet_status_id, retweet_user_id, name, location, description, followers_count, friends_count) %>%
        dplyr::mutate(sentiment_score = get_score(text))
    
    file_date <- format(now(), format="%Y-%m-%d %H:%M:%S")
    message(file_date)
    message("------------------------------")
    # uniq_tweets <- tweets
    # %>% distinct_at(vars(-query))
    
    return(uniq_tweets)
}

all_tweets <- ldply(.data = tweetsCsvFiles, .fun = readTweetFile, .parallel = FALSE)

all_tweets_sorted <- all_tweets %>%
    arrange(status_id, followers_count, friends_count) %>%
    distinct_at(vars(user_id, status_id, created_at, screen_name, text, reply_to_status_id, reply_to_user_id, is_retweet, retweet_count, hashtags, mentions_user_id, retweet_status_id, retweet_user_id, name, location, description, followers_count, friends_count, sentiment_score))

# De manière générale on garde les infos les plus récentes en fonction du nombre de followers 
# qui a plus tendance à augmenter qu'a diminuer

all_tweets_mutated <- all_tweets_sorted %>%
    group_by(user_id, status_id, created_at, screen_name, text, reply_to_status_id, reply_to_user_id, is_retweet, hashtags, mentions_user_id, retweet_status_id, retweet_user_id, sentiment_score) %>%
    dplyr::mutate(name = last(name)) %>%
    dplyr::mutate(location = last(location)) %>%
    dplyr::mutate(description = last(description))

all_tweets_summarised <- all_tweets_mutated %>%
    group_by(user_id, status_id, created_at, screen_name, text, reply_to_status_id, reply_to_user_id, is_retweet, hashtags, mentions_user_id, retweet_status_id, retweet_user_id, name, location, description, sentiment_score) %>%
    summarise_at(vars(followers_count, friends_count, retweet_count), funs(max))

# Si on a encore des doublons, on regarde sur quels tweets et quels champs il subsiste des doublons pour analyse
all_tweets_distincted <- all_tweets_summarised %>%
    distinct(status_id)
if(nrow(all_tweets_summarised) != nrow(all_tweets_distincted)) {
    nb_occurence_by_tweet_distincted <- data.frame(table(all_tweets_distincted$status_id))
    doublon_anormal <- nb_occurence_by_tweet_distincted[nb_occurence_by_tweet_distincted$Freq>1,]
    tweets_again_duplicated <- all_tweets_summarised %>%
        filter(status_id %in% doublon_anormal$Var1)
    view(tweets_again_duplicated)
}


# On sauvegarde tous les tweets récupérés dans un fichier .RData pour gagner du temps
save(all_tweets_summarised, file = "data/all_tweets_summarised_onlyARN.RData")
# On sauvegarde aussi tous les tweets récupérés dans un fichier csv zippé pour pouvoir le traiter dans d'autres outils comme tableau 
filename_csv  <- str_c("csv/complete/ARNmTweets_full_onlyARN", ".csv")
filename_zip  <- str_c("csv/complete/ARNmTweets_full_onlyARN", ".zip")
write_as_csv(all_tweets_summarised, filename_csv, prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
zipr(filename_zip, filename_csv)