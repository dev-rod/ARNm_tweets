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
if("rgexf" %in% rownames(installed.packages()) == FALSE) {install.packages("rgexf")};library(rgexf)
if("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")};library(lubridate)
if("textdata" %in% rownames(installed.packages()) == FALSE) {install.packages("textdata")};library(textdata)
if("rtweet" %in% rownames(installed.packages()) == FALSE) {install.packages("rtweet")};library(rtweet)
# https://cran.r-project.org/web/packages/syuzhet/vignettes/syuzhet-vignette.html
if("syuzhet" %in% rownames(installed.packages()) == FALSE) {install.packages("syuzhet")};library(syuzhet)
if("tidytext" %in% rownames(installed.packages()) == FALSE) {install.packages("tidytext")};library(tidytext)

if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")};library(tidyverse)
if("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2")};library(ggplot2)

# 298856 tweets récupérés entre le 31 janvier et le 03 mars
load("data/all_tweets_summarised.RData")

# nombre tweets total : 300 000 environ
nrow(all_tweets_summarised)

# nombre auteur total : 80 000 environ
authors <- distinct(all_tweets_summarised[,1])
nrow(authors)

# deciles de la répartition des auteurs par rapport à leur nombre de followers
followers_count_by_authors <- all_tweets_summarised %>%
    group_by(user_id) %>%
    summarise_at(vars(followers_count), funs(max))
quantile(followers_count_by_authors$followers_count, prob = seq(0, 1, length = 11))

# graphe de la répartition des auteurs par rapport à leur nombre de followers
get_decile <- function(x) ceiling(10*rank(-x, ties.method="random") / length(x))
followers_count_by_authors$decile <- get_decile(followers_count_by_authors$followers_count)
ggplot(followers_count_by_authors, aes(x=decile, y=followers_count)) + 
    stat_summary(fun=mean, geom="line") +
    scale_x_reverse(breaks=1:10)

# deciles de la répartition des tweets par rapport à leur score de sentiment
quantile(all_tweets_summarised$sentiment_score, prob = seq(0, 1, length = 11))

# graphe de la répartition des tweets par rapport à leur score de sentiment
all_tweets_summarised$sentiment_score_decile <- get_decile(all_tweets_summarised$sentiment_score)
ggplot(all_tweets_summarised, aes(x=sentiment_score_decile, y=sentiment_score)) + 
    stat_summary_bin(fun=median, bins=100, geom="line") +
    scale_x_reverse(breaks=1:10)

# tweets des auteurs les plus suivis hors retweet ie avec plus de 2420 followers (dernier décile)
# 14895 tweets de ces auteurs
tweets_from_big_authors <- all_tweets_summarised %>%
    arrange(desc(followers_count)) %>%
    filter(is_retweet==FALSE) %>%
    filter(followers_count>2420)

# 4321 auteurs obtenus soit environ 5,4 % des auteurs
tweets_from_big_authors_summarised <- tweets_from_big_authors %>%
    group_by(user_id) %>%
    summarise_at(vars(followers_count), funs(max))

# On ne retient que les 100 premiers sur 4321
first_big_authors <- tweets_from_big_authors_summarised[1:100,]

# On obtient 4132 tweets et retweet soit ~1,4% de tous les tweets 
tweets_retweets_relative_big_authors <- all_tweets_summarised %>%
    filter(user_id %in% first_big_authors$user_id | retweet_user_id %in% first_big_authors$user_id )

# graphe du volume de récupération des tweets sur le mois de février, par jour ou semaine
# TODO


view(tweets_sentiments[,c(5,18)])

# on reclasse les tweets par date
tweets_sentiments <- tweets_sentiments %>%
    arrange(created_at)

# 5696
sum(tweets_sentiments$sentiment_score)
# Le résultat, 5696 est positif, un fait qui peut indiquer que dans l'ensemble, le texte n'est pas décevant.
# Comme alternative, nous pouvons souhaiter comprendre la tendance centrale, la valence émotionnelle moyenne.

# 1.378509
mean(tweets_sentiments$sentiment_score)
# Cette moyenne de 1.378509 est bien supérieure à zéro.
# Cette statistique récapitulative et d'autres similaires peuvent offrir une meilleure idée de la façon dont les émotions
# dans le passage sont distribuées. Nous pouvons utiliser la fonction de résumé pour avoir une idée générale de la
# répartition des sentiments dans le texte.

summary(tweets_sentiments$sentiment_score)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# -6.0000  0.0000  1.0000  1.379  4.0000  8.0000
# Bien que ces mesures globales du sentiment puissent être informatives,
# elles nous en disent très peu sur la façon dont le récit est structuré et comment ces sentiments positifs et négatifs
# sont activés dans le texte. Vous pouvez donc trouver utile de tracer les valeurs dans un graphique
# où l'axe des x représente le passage du temps du début à la fin du texte et l'axe des y mesure les degrés de sentiment positif
# et négatif.

# Le get_dct_transformest similaire à la get_transformed_values fonction,
# mais il applique la transformation cosinus discrète plus simple (DCT) à la place de la transformée de Fourier rapide.
# Son principal avantage réside dans sa meilleure représentation des valeurs de bord dans la version lissée
# du vecteur de sentiment.
dct_values <- get_dct_transform(
    tweets_sentiments$sentiment_score,
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
# ici on voit une evolution d'abord négative des tweets sur les vaccins à arn messager au fur et a mesure du mois de février,
# un regain de confiance puis de nouveau une tendance à la négativité, pourquoi ?

tweet_negative <- tweets_sentiments %>%
    filter(sentiment_score < 0)

tweet_positive <- tweets_sentiments %>%
    filter(sentiment_score >= 0)

# On génère le fichier gephi pour analyser les sources de ces tweets
tweet_negative_gexf_nodes <- data.frame(distinct(tweet_negative[,c(1,4)]) %>%
                             dplyr::rename(
                                 id = user_id,
                                 label = screen_name
                             ))
tweet_negative_gexf_edges <- data.frame(tweet_negative[,c(1,12)] %>%
                             dplyr::rename(
                                 source = user_id,
                                 target = retweet_user_id
                             ))
filename <- str_c("gexf/ARNmTweets_tweet_negative_", format(now(), format="%Y%m%d%H%M%S"), ".gexf")
write.gexf(tweet_negative_gexf_nodes, tweet_negative_gexf_edges, output = filename)




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