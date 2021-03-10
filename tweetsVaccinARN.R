# https://github.com/ropensci/rtweet
# https://cran.r-project.org/web/packages/rtweet/readme/README.html
# https://rtweet.info/articles/auth.html
# https://www.infoworld.com/article/3515712/how-to-search-twitter-with-rtweet-and-r.html
if("httpuv" %in% rownames(installed.packages()) == FALSE) {install.packages("httpuv")};library(httpuv)
if("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")};library(lubridate)
if("plyr" %in% rownames(installed.packages()) == FALSE) {install.packages("plyr")};library(plyr)
if("rgexf" %in% rownames(installed.packages()) == FALSE) {install.packages("rgexf")};library(rgexf)
if("zip" %in% rownames(installed.packages()) == FALSE) {install.packages("zip")};library(zip)
if("wordcloud2" %in% rownames(installed.packages()) == FALSE) {install.packages("wordcloud2")};library(wordcloud2)
if("webshot" %in% rownames(installed.packages()) == FALSE) {install.packages("webshot")};library(webshot)
if("htmlwidgets" %in% rownames(installed.packages()) == FALSE) {install.packages("htmlwidgets")};library(htmlwidgets)

if("textdata" %in% rownames(installed.packages()) == FALSE) {install.packages("textdata")};library(textdata)
if("tidytext" %in% rownames(installed.packages()) == FALSE) {install.packages("tidytext")};library(tidytext)
if("rtweet" %in% rownames(installed.packages()) == FALSE) {install.packages("rtweet")};library(rtweet)
if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")};library(tidyverse)

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

# graphe de la répartition
get_decile <- function(x) ceiling(10*rank(-x, ties.method="random") / length(x))
followers_count_by_authors$decile <- get_decile(followers_count_by_authors$followers_count)
library(ggplot2)
ggplot(followers_count_by_authors, aes(x=decile, y=followers_count)) + 
    stat_summary(fun=mean, geom="line") +
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

# tweets_retweets_relative_big_authors$date <- substr(tweets_retweets_relative_big_authors$created_at,1,10)
# tweets_retweets_relative_big_authors_bis <- tweets_retweets_relative_big_authors[tweets_retweets_relative_big_authors$date == '2021-02-02',]
# 
# tweets_retweets_relative_big_authors_bis %>%
#     rtweet::ts_plot("weeks") +
#     ggplot2::theme_minimal() +
#     ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
#     ggplot2::labs(
#         x = 'Date',
#         y = 'Count',
#         title = "Frequency of arn messager Twitter statuses on february",
#         subtitle = "Twitter status (tweet) counts aggregated using days intervals",
#         caption = "\nSource: Data collected from Twitter's REST API via rtweet"
#     )
# 
# 
# tweets_retweets_relative_big_authors$datetime <- substr(tweets_retweets_relative_big_authors$created_at,1,10)
# tweets_retweets_relative_big_authors$datetime <- tweets_retweets_relative_big_authors$created_at
# by <- "days"
# figure <- tweets_retweets_relative_big_authors_bis %>% 
#     rtweet::ts_plot(by = by, trim = 1) +
#     geom_point() +
#     ggplot2::theme_minimal() +
#     ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
#     ggplot2::labs(
#         title = paste0('Tweets mentioning "', "arn messager",'" by ', by),
#         x = 'Date',
#         y = 'Count',
#         caption = 'Source: Twitter API'
#     )
# figure
# 
# #save
# ggsave('tweet_volume.png', last_plot())

tbl_stem_sent <- tweets_retweets_relative_big_authors %>%
    inner_join(get_sentiments("nrc")) %>%
    group_by(sentiment) %>%
    count(word, sort=TRUE)


# https://cran.r-project.org/web/packages/syuzhet/vignettes/syuzhet-vignette.html
if("syuzhet" %in% rownames(installed.packages()) == FALSE) {install.packages("syuzhet")};library(syuzhet)
char_v <- get_sentences(tweets_retweets_relative_big_authors$text)
poa_word_v <- get_tokens(tweets_retweets_relative_big_authors$text, pattern = "\\W")
head(char_v)
head(poa_word_v)
method <- "nrc"
lang <- "french"
my_text_values <- get_sentiment(char_v, method=method, language=lang)
head(my_text_values)

my_token_values <- get_sentiment(poa_word_v, method=method, language=lang)
head(my_token_values)

# bing_vector <- get_sentiment(char_v, method = "bing")
# head(bing_vector)


# 5696
sum(my_text_values)
# Le résultat, 5696 est positif, un fait qui peut indiquer que dans l'ensemble, le texte n'est pas décevant.
# Comme alternative, nous pouvons souhaiter comprendre la tendance centrale, la valence émotionnelle moyenne.

# 0.6381358
mean(my_text_values)
# Cette moyenne de 0.6381358 est légèrement supérieure à zéro.
# Cette statistique récapitulative et d'autres similaires peuvent offrir une meilleure idée de la façon dont les émotions
# dans le passage sont distribuées. Nous pouvons utiliser la fonction de résumé pour avoir une idée générale de la
# répartition des sentiments dans le texte.

summary(my_text_values)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -6.0000  0.0000  0.0000  0.6381  1.0000  8.0000
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
    my_text_values, 
    low_pass_size = 5, 
    x_reverse_len = 100,
    scale_vals = F,
    scale_range = T
)
plot(
    dct_values, 
    type ="l", 
    main ="tweets emotions using Transformed Values", 
    xlab = "Narrative Time", 
    ylab = "Emotional Valence", 
    col = "red"
)
# ici on voit une evolution positive du vaccin au fur et a mesure de la lecture du tableau de tweets, pourquoi ?
# il serait interessant de les reclasser par date

nrc_data <- get_nrc_sentiment(char_v)
head(nrc_data)
angry_items <- which(nrc_data$anger > 0)
char_v[angry_items]
joy_items <- which(nrc_data$joy > 0)
char_v[joy_items]

require(parallel)
cl <- makeCluster(2) # or detect_cores() - 1
clusterExport(cl = cl, c("get_sentiment", "get_sent_values", "get_nrc_sentiment", "get_nrc_values", "parLapply"))
nrc_data_token <- get_nrc_sentiment(poa_word_v, cl=cl)
head(nrc_data_token)
anger_items_token <- which(nrc_data_token$anger > 0)
poa_word_v[anger_items_token]
stopCluster(cl)

#simple de visualiser toutes les émotions et leurs valeurs:
pander::pandoc.table(nrc_data[, 1:8], split.table = Inf)

#examiner uniquement la valence positive et négative:
pander::pandoc.table(nrc_data[, 9:10])

#Ces deux dernières colonnes sont celles utilisées par la nrcméthode dans la get_sentimentfonction décrite ci-dessus. Pour calculer une valeur unique de valence positive ou négative pour chaque phrase, les valeurs de la colonne négative sont converties en nombres négatifs, puis ajoutées aux valeurs de la colonne positive, comme ceci.

valence <- (nrc_data[, 9]*-1) + nrc_data[, 10]
valence
##  [1]  1 -1 -1  1  1  0  0 -2  0  0  1  1

# le pourcentage de chaque émotion dans le texte peut être tracé sous forme de graphique à barres:
barplot(
    sort(colSums(prop.table(nrc_data[, 1:8]))),
    horiz = TRUE,
    cex.names = 0.7,
    las = 1,
    main = "Emotions in arn messager tweets from 100 best followed authors", xlab="Percentage"
)


# On génère le fichier gephi pour analyser le graphe des retweet
retweets <- all_tweets_summarised[which(all_tweets_summarised$is_retweet==TRUE) ,]
sample <- retweets
sample <- tweets_retweets_relative_big_authors
#sample <- retweets[1:1000,]

# user_id, screen_name
# pour une raison que j'ignore le data.frame initial ne plait pas a rgexf, on recaste en data.frame
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

# status_id
#gexf_edgesLabel <- data.frame(sample[,2])
# status_id
#gexf_edgesId <- data.frame(sample[,2])
# status_id, created_at, text, hashtags, mentions_user_id
#gexf_edgesAtt <- data.frame(sample[,c(2,3)])
# followers_count
#gexf_edgesWeight <- sample$followers_count
# user_id, screen_name, name, location, description, followers_count, friends_count
#gexf_nodesAtt <- data.frame(distinct(sample[,c(1,4,13:17)]))
#gexf_nodesAtt <- data.frame(distinct(sample[,c(1,4)]))

# generate 
filename <- str_c("gexf/ARNmTweets_big_authors_", format(now(), format="%Y%m%d%H%M%S"), ".gexf")
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




