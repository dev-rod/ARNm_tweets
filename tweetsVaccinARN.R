# https://github.com/ropensci/rtweet
# https://cran.r-project.org/web/packages/rtweet/readme/README.html
# https://rtweet.info/articles/auth.html
# https://www.infoworld.com/article/3515712/how-to-search-twitter-with-rtweet-and-r.html
# if("httpuv" %in% rownames(installed.packages()) == FALSE) {install.packages("httpuv")};library(httpuv)
# if("plyr" %in% rownames(installed.packages()) == FALSE) {install.packages("plyr")};library(plyr)
# if("zip" %in% rownames(installed.packages()) == FALSE) {install.packages("zip")};library(zip)
# if("textdata" %in% rownames(installed.packages()) == FALSE) {install.packages("textdata")};library(textdata)
# https://cran.r-project.org/web/packages/syuzhet/vignettes/syuzhet-vignette.html
# if("tidytext" %in% rownames(installed.packages()) == FALSE) {install.packages("tidytext")};library(tidytext)

if("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")};library(lubridate)
if("rgexf" %in% rownames(installed.packages()) == FALSE) {install.packages("rgexf")};library(rgexf)
if("syuzhet" %in% rownames(installed.packages()) == FALSE) {install.packages("syuzhet")};library(syuzhet)
if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")};library(tidyverse)
if("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2")};library(ggplot2)
if("rtweet" %in% rownames(installed.packages()) == FALSE) {install.packages("rtweet")};library(rtweet)
source("scripts/tweet_stats.R")

# 298856 tweets récupérés entre le 31 janvier et le 03 mars
#load("data/all_tweets_summarised.RData")
# LH
#load("data/all_tweets_summarised_onlyARN.RData")
# RB
load("data/all_tweets_arnm_only.RData")

all_tweets_summarised <- all_tweets_summarised %>%
    arrange(created_at)

# stats sur tous les tweets
(get_tweet_stat(all_tweets_summarised, "ARNmTweets_tweet"))

# graphe du volume de récupération des tweets sur le mois de février, par jour ou semaine
# TODO
# cf histogramme dans powerbi ou tableau

# contrôle visuel des scores de sentiments
#head(all_tweets_summarised[,c(5,18)])



# La fonction get_dct_transform est similaire à la fonction get_transformed_values,
# mais elle applique la transformation cosinus discrète plus simple (DCT) à la place de la transformée de Fourier rapide.
# Son principal avantage réside dans sa meilleure représentation des valeurs de bord dans la version lissée du vecteur de sentiment.
# !!!!!!!!!!!!!!!!! peut être très long sur tous les tweets !!!!!!!!!!!!!!!!
dct_values <- get_dct_transform(
    all_tweets_summarised$sentiment_score,
    low_pass_size = 5,
    x_reverse_len = 100,
    scale_vals = F,
    scale_range = T
)
plot(
    dct_values,
    type ="l",
    main ="évolution des emotions sur les tweets sur le mois de février",
    xlab = "Narrative Time",
    ylab = "Emotional Valence",
    col = "red"
)
# Ici on voit une evolution d'abord négative des tweets sur les vaccins à arn messager au fur et a mesure du mois de février,
# un regain de confiance puis de nouveau une tendance à la négativité, pourquoi ?

# tweet négatifs
tweet_negative <- all_tweets_summarised %>%
    filter(sentiment_score < 0)
# génération des stats
(get_tweet_stat(tweet_negative, "ARNmTweets_tweet_negative"))

# tweet très négatifs
tweet_very_negative <- tweet_negative %>%
    filter(sentiment_score < -3)

# %>%
#     filter(follower_count >= 225)
# génération des stats
(get_tweet_stat(tweet_very_negative, "ARNmTweets_tweet_very_negative"))

# On génère le fichier .gexf pour analyser les sources de ces tweets dans gephi
(by_user_gephi_file(tweet_very_negative, "ARNmTweets_tweet_very_negative"))


no_retweet_very_negative <- tweet_very_negative %>%
    filter(is_retweet == FALSE)

no_retweet_very_negative_sorted <- no_retweet_very_negative %>%
    arrange(location, desc(sentiment_score), followers_count) %>%
    distinct_at(vars(user_id, screen_name, location, followers_count, sentiment_score))

no_retweet_very_negative_mutated <- no_retweet_very_negative_sorted %>%
    group_by(user_id) %>%
    dplyr::mutate(screen_name = last(screen_name)) %>%
    dplyr::mutate(location = last(location)) %>%
    dplyr::mutate(sentiment_score = sum(sentiment_score)) %>%
    dplyr::mutate(followers_count = max(followers_count))

retweet_very_negative <- tweet_very_negative %>%
    filter(is_retweet == TRUE)

csv_nodes <- distinct(no_retweet_very_negative_mutated[,c(1,4,14,16,17)]) %>%
    dplyr::rename(
        id = user_id,
        label = screen_name
    )
csv_edges <- retweet_very_negative[,c(1,12)] %>%
    dplyr::rename(
        source = user_id,
        target = retweet_user_id
    )
filename_csv  <- str_c("gexf/negative/nodes.csv")
write_as_csv(csv_nodes, filename_csv, prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
filename_csv  <- str_c("gexf/negative/edges.csv")
write_as_csv(csv_edges, filename_csv, prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")

# tentative d'isoler des ensemble de rumeurs ou sujets de tendances négatives


# tweet positifs
tweet_positive <- all_tweets_summarised %>%
    filter(sentiment_score >= 0)
# génération des stats
(get_tweet_stat(tweet_positive, "ARNmTweets_tweet_positive"))
# On génère le fichier .gexf pour analyser les sources de ces tweets dans gephi

# tweet très positifs
tweet_very_positive <- tweet_positive %>%
    filter(sentiment_score > 2)

# génération des stats
(get_tweet_stat(tweet_very_positive, "ARNmTweets_tweet_very_positive"))

no_retweet_very_positive <- tweet_very_positive %>%
    filter(is_retweet == FALSE)

no_retweet_very_positive_sorted <- no_retweet_very_positive %>%
    arrange(location, desc(sentiment_score), followers_count) %>%
    distinct_at(vars(user_id, screen_name, location, followers_count, sentiment_score))

no_retweet_very_positive_mutated <- no_retweet_very_positive_sorted %>%
    group_by(user_id) %>%
    dplyr::mutate(screen_name = last(screen_name)) %>%
    dplyr::mutate(location = last(location)) %>%
    dplyr::mutate(sentiment_score = sum(sentiment_score)) %>%
    dplyr::mutate(followers_count = max(followers_count))

retweet_very_positive <- tweet_very_positive %>%
    filter(is_retweet == TRUE)

csv_nodes <- distinct(no_retweet_very_positive_mutated[,c(1,4,14,16,17)]) %>%
    dplyr::rename(
        id = user_id,
        label = screen_name
    )
csv_edges <- retweet_very_positive[,c(1,12)] %>%
    dplyr::rename(
        source = user_id,
        target = retweet_user_id
    )
filename_csv  <- str_c("gexf/positive/nodes.csv")
write_as_csv(csv_nodes, filename_csv, prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
filename_csv  <- str_c("gexf/positive/edges.csv")
write_as_csv(csv_edges, filename_csv, prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
# tentative d'isoler des ensemble de rumeurs ou sujets de tendances positives




# tous les tweet
no_retweet <- all_tweets_summarised %>%
    filter(is_retweet == FALSE)

no_retweet_sorted <- no_retweet %>%
    arrange(location, desc(sentiment_score), followers_count) %>%
    distinct_at(vars(user_id, screen_name, location, followers_count, sentiment_score))

no_retweet_mutated <- no_retweet_sorted %>%
    group_by(user_id) %>%
    dplyr::mutate(screen_name = last(screen_name)) %>%
    dplyr::mutate(location = last(location)) %>%
    dplyr::mutate(sentiment_score = sum(sentiment_score)) %>%
    dplyr::mutate(followers_count = max(followers_count))

retweet <- all_tweets_summarised %>%
    filter(is_retweet == TRUE)

csv_nodes <- distinct(no_retweet_mutated[,c(1,4,14,16,17)]) %>%
    dplyr::rename(
        id = user_id,
        label = screen_name
    )
csv_edges <- retweet[,c(1,12)] %>%
    dplyr::rename(
        source = user_id,
        target = retweet_user_id
    )
filename_csv  <- str_c("gexf/nodes.csv")
write_as_csv(csv_nodes, filename_csv, prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
filename_csv  <- str_c("gexf/edges.csv")
write_as_csv(csv_edges, filename_csv, prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")


































##################################" todo a finir #####################"
library(dplyr)
library(tidyr)
hashtags <- tweet_negative %>% 
    mutate(hashtags = strsplit(as.character(hashtags), " ")) %>%
    unnest(hashtags)

# nuage de mots sur les hashtags
hashtag_counts <- table(tweet_negative$hashtags)
# sort
sorted_hashtag_counts <- sort(hashtag_counts, decreasing=TRUE)
set.seed(1234)
wc <- wordcloud2(sorted_hashtag_counts)
saveWidget(wc, "tmp.html", selfcontained = F)
fileName = str_c("img/test_hashtags.png")
webshot::webshot("tmp.html", fileName, delay =6)
print(wc)

# tweets des auteurs les plus suivis hors retweet ie avec plus de 343 followers (dernier décile)
# 7252 tweets de ces auteurs
# tweets_negative_from_big_authors <- tweet_negative %>%
#     arrange(desc(followers_count)) %>%
#     filter(is_retweet==FALSE) %>%
#     filter(followers_count>343)

# 4166 auteurs obtenus soit environ ? % des auteurs
# tweets_negative_from_big_authors_summarised <- tweets_negative_from_big_authors %>%
#     group_by(user_id) %>%
#     summarise_at(vars(followers_count), funs(max))

# On ne retient que les 300 premiers sur 4166
# first_big_authors <- tweets_negative_from_big_authors_summarised[1:300,]

# On obtient 10954 tweets et retweet soit ~1,4% de tous les tweets 
# tweets_retweets_relative_big_authors <- all_tweets_summarised %>%
#     filter(user_id %in% first_big_authors$user_id | retweet_user_id %in% first_big_authors$user_id )