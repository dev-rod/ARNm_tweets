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
if("zip" %in% rownames(installed.packages()) == FALSE) {install.packages("zip")};library(zip)
if("wordcloud2" %in% rownames(installed.packages()) == FALSE) {install.packages("wordcloud2")};library(wordcloud2)
if("webshot" %in% rownames(installed.packages()) == FALSE) {install.packages("webshot")};library(webshot)
if("htmlwidgets" %in% rownames(installed.packages()) == FALSE) {install.packages("htmlwidgets")};library(htmlwidgets)
#if("doFuture" %in% rownames(installed.packages()) == FALSE) {install.packages("doFuture")};library(doFuture)

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
# ("user_id", "status_id", "created_at", "screen_name", "text", "reply_to_status_id", "reply_to_user_id",
#  "is_quote", "is_retweet", "hashtags", "mentions_user_id", "quoted_status_id", "quoted_text", "quoted_created_at"
#  "quoted_user_id", "retweet_status_id", "retweet_user_id", "name", "location", "description", "followers_count"
#  "friends_count", "account_created_at", "verified", "profile_url", "query")

## On récupère les tweets traitant des vaccins à arn messager sur les 7 derniers jours
# https://www.rdocumentation.org/packages/rtweet/versions/0.7.0/topics/search_tweets
tweets <- search_tweets2(
    c("\"arn messager\" OR ARNm", "vaccin AND (pfizer OR biontech OR moderna OR ARNm)", "vaccin AND (fiabilité OR fiable OR efficace OR inefficacité OR inefficace OR efficacité OR inopérant OR faible OR inutile OR sûreté OR sureté OR sûr)"), n=18000, type="recent", include_rts=TRUE, lang="fr", retryonratelimit = TRUE
)

# On sauvegarde les derniers tweets récupérés dans un fichier csv zippé
file_date <- format(now(), format="%Y%m%d%H%M%S")
filename_csv  <- str_c("csv/ARNmTweets_", file_date, ".csv")
filename_zip  <- str_c("csv/ARNmTweets_", file_date, ".zip")
write_as_csv(tweets, filename_csv, prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
zipr(filename_zip, filename_csv)

# Pour regénérer le fichier complet des tweets
## On passe en mode parallèle avec les coeurs du processeur disponible sur cette session R
#registerDoFuture()
#plan(multisession)

# pour chacun des fichiers csv zippés récupérés
readTweetFile <- function(filename)
{
    tweets <- read_csv(filename)
    
    #message(colnames(tweets))
    
    message("------------------------------")
    message(filename)
    #message(str(tweets$query))
    message(length(colnames(tweets)))
    message("------------------------------")
    
    # on retire les colonnes en trop
    tweets <- tweets[,c(1:5,8:9,12,17,30,48,54,73:75,78,79,83,85)]
    uniq_tweets <- tweets %>%
        arrange(status_id, followers_count, friends_count) %>%
        distinct_at(vars(user_id, status_id, created_at, screen_name, text, reply_to_status_id, reply_to_user_id, is_retweet, hashtags, mentions_user_id, retweet_status_id, retweet_user_id, name, location, description, followers_count, friends_count))
    
    # uniq_tweets <- tweets %>%
    #     distinct_at(vars(-query))
    
    return(uniq_tweets)
}

tweetsCsvFiles <- list.files(path="csv", pattern="*.zip", full.names=TRUE)
#tweetsCsvFiles <- "csv/ARNmTweets_20210131095600.zip"
all_tweets <- ldply(.data = tweetsCsvFiles, .fun = readTweetFile, .parallel = TRUE)

# On repasse en mode séquentiel
#plan(sequential)

all_tweets_sorted <- all_tweets %>%
    arrange(status_id, followers_count, friends_count) %>%
    distinct_at(vars(user_id, status_id, created_at, screen_name, text, reply_to_status_id, reply_to_user_id, is_retweet, hashtags, mentions_user_id, retweet_status_id, retweet_user_id, name, location, description, followers_count, friends_count))

# de manière générale on garde les infos les plus récentes en fonction du nombre de followers 
# qui a plus tendance à augmenter qu'a diminuer

all_tweets_mutated <- all_tweets_sorted %>%
    group_by(user_id, status_id, created_at, screen_name, text, reply_to_status_id, reply_to_user_id, is_retweet, hashtags, mentions_user_id, retweet_status_id, retweet_user_id) %>%
    dplyr::mutate(name = last(name)) %>%
    dplyr::mutate(location = last(location)) %>%
    dplyr::mutate(description = last(description))

all_tweets_summarised <- all_tweets_mutated %>%
    group_by(user_id, status_id, created_at, screen_name, text, reply_to_status_id, reply_to_user_id, is_retweet, hashtags, mentions_user_id, retweet_status_id, retweet_user_id, name, location, description) %>%
    summarise_at(vars(followers_count, friends_count), funs(max))

save(all_tweets_summarised, file = "data/all_tweets_summarised.RData")

# si on a encore des doublons, on regarde sur quels tweets et quels champs il subsiste des doublons pour analyse
all_tweets_distincted <- all_tweets_summarised %>%
    distinct(status_id)
if(nrow(all_tweets_summarised) != nrow(all_tweets_distincted)) {
    nb_occurence_by_tweet_distincted <- data.frame(table(all_tweets_distincted$status_id))
    doublon_anormal <- nb_occurence_by_tweet_distincted[nb_occurence_by_tweet_distincted$Freq>1,]
    tweets_again_duplicated <- all_tweets_summarised %>%
        filter(status_id %in% doublon_anormal$Var1)
    view(tweets_again_duplicated)
}

# On sauvegarde tous les tweets récupérés dans un fichier csv zippé
filename_csv  <- str_c("csv/complete/ARNmTweets_full", ".csv")
filename_zip  <- str_c("csv/complete/ARNmTweets_full", ".zip")
write_as_csv(all_tweets_summarised, filename_csv, prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
zipr(filename_zip, filename_csv)