# https://github.com/ropensci/rtweet
# https://cran.r-project.org/web/packages/rtweet/readme/README.html
# https://rtweet.info/articles/auth.html
# https://www.infoworld.com/article/3515712/how-to-search-twitter-with-rtweet-and-r.html
if("httpuv" %in% rownames(installed.packages()) == FALSE) {install.packages("httpuv")};library(httpuv)
if("plyr" %in% rownames(installed.packages()) == FALSE) {install.packages("plyr")};library(plyr)
if("zip" %in% rownames(installed.packages()) == FALSE) {install.packages("zip")};library(zip)
if("wordcloud2" %in% rownames(installed.packages()) == FALSE) {install.packages("wordcloud2")};library(wordcloud2)
if("webshot" %in% rownames(installed.packages()) == FALSE) {install.packages("webshot")};library(webshot)
if("htmlwidgets" %in% rownames(installed.packages()) == FALSE) {install.packages("htmlwidgets")};library(htmlwidgets)
if("textdata" %in% rownames(installed.packages()) == FALSE) {install.packages("textdata")};library(textdata)
# https://cran.r-project.org/web/packages/syuzhet/vignettes/syuzhet-vignette.html
if("tidytext" %in% rownames(installed.packages()) == FALSE) {install.packages("tidytext")};library(tidytext)

if("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")};library(lubridate)
if("rgexf" %in% rownames(installed.packages()) == FALSE) {install.packages("rgexf")};library(rgexf)
if("syuzhet" %in% rownames(installed.packages()) == FALSE) {install.packages("syuzhet")};library(syuzhet)
if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")};library(tidyverse)
if("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2")};library(ggplot2)
source("scripts/tweet_stats.R")

# 298856 tweets récupérés entre le 31 janvier et le 03 mars
load("data/all_tweets_summarised.RData")

# stats sur tous les tweets
(get_tweet_stat(all_tweets_summarised))

# graphe du volume de récupération des tweets sur le mois de février, par jour ou semaine
# TODO
# cf histogramme dans powerbi ou tableau

# contrôle visuel des scores de sentiments
#head(all_tweets_summarised[,c(5,18)])



# La fonction get_dct_transform est similaire à la fonction get_transformed_values,
# mais elle applique la transformation cosinus discrète plus simple (DCT) à la place de la transformée de Fourier rapide.
# Son principal avantage réside dans sa meilleure représentation des valeurs de bord dans la version lissée du vecteur de sentiment.
# !!!!!!!!!!!!!!!!! peut être très long sur tous les tweets !!!!!!!!!!!!!!!!
# dct_values <- get_dct_transform(
#     all_tweets_summarised$sentiment_score,
#     low_pass_size = 5,
#     x_reverse_len = 100,
#     scale_vals = F,
#     scale_range = T
# )
# plot(
#     dct_values,
#     type ="l",
#     main ="évolution des emotions sur les tweets sur le mois de février",
#     xlab = "Narrative Time",
#     ylab = "Emotional Valence",
#     col = "red"
# )
# Ici on voit une evolution d'abord négative des tweets sur les vaccins à arn messager au fur et a mesure du mois de février,
# un regain de confiance puis de nouveau une tendance à la négativité, pourquoi ?

tweet_negative <- all_tweets_summarised %>%
    filter(sentiment_score < 0)

(get_tweet_stat(tweet_negative))


# tweets des auteurs les plus suivis hors retweet ie avec plus de 343 followers (dernier décile)
# 7252 tweets de ces auteurs
tweets_negative_from_big_authors <- tweet_negative %>%
    arrange(desc(followers_count)) %>%
    filter(is_retweet==FALSE) %>%
    filter(followers_count>343)

# 4166 auteurs obtenus soit environ ? % des auteurs
tweets_negative_from_big_authors_summarised <- tweets_negative_from_big_authors %>%
    group_by(user_id) %>%
    summarise_at(vars(followers_count), funs(max))

# On ne retient que les 300 premiers sur 4166
first_big_authors <- tweets_negative_from_big_authors_summarised[1:300,]

# On obtient 10954 tweets et retweet soit ~1,4% de tous les tweets 
tweets_retweets_relative_big_authors <- all_tweets_summarised %>%
    filter(user_id %in% first_big_authors$user_id | retweet_user_id %in% first_big_authors$user_id )

# On génère le fichier gephi pour analyser les sources de ces tweets
tweet_negative_gexf_nodes <- data.frame(distinct(tweets_retweets_relative_big_authors[,c(1,4)]) %>%
                             dplyr::rename(
                                 id = user_id,
                                 label = screen_name
                             ))
tweet_negative_gexf_edges <- data.frame(tweets_retweets_relative_big_authors[,c(1,12)] %>%
                             dplyr::rename(
                                 source = user_id,
                                 target = retweet_user_id
                             ))
filename <- str_c("gexf/ARNmTweets_tweet_negative_", format(now(), format="%Y%m%d%H%M%S"), ".gexf")
write.gexf(tweet_negative_gexf_nodes, tweet_negative_gexf_edges, output = filename)

tweet_positive <- all_tweets_summarised %>%
    filter(sentiment_score >= 0)

(get_tweet_stat(tweet_positive))


# On génère le fichier gephi pour analyser le graphe des retweet
retweets <- all_tweets_summarised[which(all_tweets_summarised$is_retweet==TRUE) ,]
sample <- retweets
sample <- tweets_retweets_relative_big_authors
#sample <- retweets[1:1000,]

# user_id, screen_name
gexf_nodes <- data.frame(distinct(sample[,c(1,4)]) %>%
                             dplyr::rename(
                                 id = user_id,
                                 label = screen_name
                             ))

# user_id, retweet_user_id
gexf_edges <- data.frame(sample[,c(1,12)] %>%
                             dplyr::rename(
                                 source = user_id,
                                 target = retweet_user_id
                             ))
filename <- str_c("gexf/ARNmTweets_big_authors_", format(now(), format="%Y%m%d%H%M%S"), ".gexf")

# https://gvegayon.github.io/rgexf/
write.gexf(gexf_nodes, gexf_edges, output = filename)

# On génère le fichier gephi pour analyse le graphe des hashtags
#hashtags <- all_tweets_distincted[which(!is.na(all_tweets_distincted$hashtags)) ,]
# # user_id, retweet_user_id, "hashtags"
# gexf_nodes <- distinct(retweets[,c(1,4)])
# gexf_edges <- retweets[,c(17,1)]
# gexf(nodes=gexf_nodes, edges=gexf_edges, output=str_c("gexf/ARNmTweets_", format(now(), format="%Y%m%d%H%M%S"), ".gexf"))


# isolation des auteurs et cluster les plus negatifs
# sortie d"un nuage de mots
# tentative d'isoler des ensemble de rumeurs ou sujets de tendances négatives

# isolation des auteurs et cluster les plus positifs
# sortie d"un nuage de mots
# tentative d'isoler des ensemble de rumeurs ou sujets de tendances positives



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