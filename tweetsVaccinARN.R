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

load("data/all_tweets_summarised.RData")

big_followers_tweets <- all_tweets_summarised %>%
    filter(followers_count>5000)

# On génère le fichier gephi pour analyse le graphe des retweet
retweets <- all_tweets_summarised[which(all_tweets_summarised$is_retweet==TRUE) ,]
sample <- retweets
#sample <- retweets[1:1000,]

# user_id, screen_name
# pour une raison que j'ignore le data.frame initial ne plait pas a rgexf, on recaste en data.frame snif
gexf_nodes <- data.frame(distinct(sample[,c(1,4)]) %>%
                             dplyr::rename(
                                 id = user_id,
                                 label = screen_name
                             ))

# user_id, retweet_user_id
gexf_edges <- data.frame(sample[,c(12,1)] %>%
                             dplyr::rename(
                                 source = user_id,
                                 target = retweet_user_id
                             ))

# status_id
gexf_edgesLabel <- data.frame(sample[,2])
# status_id
gexf_edgesId <- data.frame(sample[,2])
# status_id, created_at, text, hashtags, mentions_user_id
gexf_edgesAtt <- data.frame(sample[,c(2,3)])
# followers_count
gexf_edgesWeight <- sample$followers_count
# user_id, screen_name, name, location, description, followers_count, friends_count
#gexf_nodesAtt <- data.frame(distinct(sample[,c(1,4,13:17)]))
gexf_nodesAtt <- data.frame(distinct(sample[,c(1,4)]))

# generate 
filename <- str_c("gexf/ARNmTweets_", format(now(), format="%Y%m%d%H%M%S"), ".gexf")
# write.gexf(nodes = gexf_nodes,
#            edges = gexf_edges,
#            edgesLabel = gexf_edgesLabel,
#            edgesId = gexf_edgesId,
#            #edgesAtt = gexf_edgesAtt,
#            edgesWeight = gexf_edgesWeight,
#            nodesAtt  = gexf_nodesAtt,
#            output = filename)

# https://gvegayon.github.io/rgexf/
write.gexf(gexf_nodes, gexf_edges, output = filename)


# On génère le fichier gephi pour analyse le graphe des hashtags
#hashtags <- all_tweets_distincted[which(!is.na(all_tweets_distincted$hashtags)) ,]
# # user_id, retweet_user_id, "hashtags"
# gexf_nodes <- distinct(retweets[,c(1,4)])
# gexf_edges <- retweets[,c(17,1)]
# gexf(nodes=gexf_nodes, edges=gexf_edges, output=str_c("gexf/ARNmTweets_", format(now(), format="%Y%m%d%H%M%S"), ".gexf"))



# on génère un nuage de mots


# nuage de mots sur les hashtags
hashtag_counts <- table(all_tweets_summarised$hashtags)
# sort
sorted_hashtag_counts <- sort(hashtag_counts, decreasing=TRUE)
set.seed(1234)
webshot::install_phantomjs()
wc <- wordcloud2(sorted_hashtag_counts)
saveWidget(wc, "tmp.html", selfcontained = F)
fileName = "img/hashtags.png"
webshot::webshot("tmp.html", fileName, delay =3)


# nuage de mots sur les textes

# text mining
if("tm" %in% rownames(installed.packages()) == FALSE) {install.packages("tm")};library(tm)
if("spacyr" %in% rownames(installed.packages()) == FALSE) {install.packages("spacyr")};library(spacyr)
text <- big_followers_tweets$text
corpus <- Corpus(VectorSource(text))
# tolower words
tm_map(corpus, content_transformer(tolower))

# remove special chars
(kill_chars <- content_transformer (function(x, pattern) gsub(pattern, " ", x)))
corpus <- tm_map (corpus, kill_chars, "…")
corpus <- tm_map (corpus, kill_chars, "’")
corpus <- tm_map (corpus, kill_chars, "\"je")
corpus <- tm_map (corpus, kill_chars, '"je')
corpus <- tm_map (corpus, kill_chars, 'les')

# remove stopwords
corpus <- tm_map(corpus, removeWords, stopwords("french"))
# remove ponctuations
corpus <- tm_map(corpus, removePunctuation)
# remove numbers
corpus <- tm_map(corpus, removeNumbers)
# remove extra spaces
corpus <- tm_map(corpus, stripWhitespace)

# Most frequent terms by document
# contains everything that have minimum word length >= 1
DTM <- TermDocumentMatrix(corpus, control = list(minWordLength=c(1,Inf)))
# words appearing at least 1000 times
print(findFreqTerms(DTM, 1000))
# remove terms with a sparse factor of more than 0,98
DTM2 <- removeSparseTerms(DTM, sparse = 0.991)
DTM2
matrix <- as.matrix(DTM2)
freq <- sort(rowSums(matrix), decreasing=TRUE)
df <- data.frame(word = names(freq),freq=freq)
set.seed(1234)
fileName = "img/Global_wordcloud_with_tm.png"
global_wc <- wordcloud2(df[1:100,])
saveWidget(global_wc, "tmp.html", selfcontained = F)
webshot::webshot("tmp.html", fileName, delay =3)




#sapply (all_tweets_distincted, function(x) length(unique(x)))

# all_tweets_summarised %>%
#     ts_plot("3 hours") +
#     ggplot2::theme_minimal() +
#     ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
#     ggplot2::labs(
#         x = NULL, y = NULL,
#         title = "Frequency of arn messager Twitter statuses from past 9 days",
#         subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
#         caption = "\nSource: Data collected from Twitter's REST API via rtweet"
#     )


