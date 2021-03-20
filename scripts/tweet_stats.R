if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")};library(tidyverse)
if("tm" %in% rownames(installed.packages()) == FALSE) {install.packages("tm")};library(tm)
if("spacyr" %in% rownames(installed.packages()) == FALSE) {install.packages("spacyr")};library(spacyr)
if("wordcloud2" %in% rownames(installed.packages()) == FALSE) {install.packages("wordcloud2")};library(wordcloud2)
if("webshot" %in% rownames(installed.packages()) == FALSE) {install.packages("webshot")};library(webshot)
if("htmlwidgets" %in% rownames(installed.packages()) == FALSE) {install.packages("htmlwidgets")};library(htmlwidgets)
#webshot::install_phantomjs(force = TRUE)

get_decile <- function(x){
    ceiling(10*rank(-x, ties.method="random") / length(x))
}

get_tweet_stat <- function (df_tweets, tag){
    # nombre tweets total : 300 000 environ
    nb_tweets <- nrow(df_tweets)
    message(sprintf("nombre tweets total: %s", nb_tweets))
    
    # nombre auteur total : 80 000 environ
    authors <- distinct(df_tweets[,1])
    nb_authors <- nrow(authors)
    message(sprintf("nombre auteurs total: %s", nb_authors))
    
    # on reclasse les tweets par date
    df_tweets <- df_tweets %>%
        arrange(created_at)
    
    # somme du sentiment_score (tendance générale négative ou positive ?)
    somme_sentiment <- sum(df_tweets$sentiment_score)
    message(sprintf("Somme du sentiment_score (tendance générale négative ou positive ?): %s", somme_sentiment))
    
    # moyenne du sentiment_score (tendance générale négative ou positive ?)
    moyenne_sentiment <- mean(df_tweets$sentiment_score)
    message(sprintf("Moyenne du sentiment_score : %s", moyenne_sentiment))
    
    # quartiles des valeurs du sentiment_score (tendance générale négative ou positive ?)
    resume_sentiment <- summary(df_tweets$sentiment_score)
    message(sprintf("Résumé du sentiment_score :"))
    print(resume_sentiment)
    
    # deciles de la répartition des tweets par rapport à leur score de sentiment
    sentiment_quantiles <- quantile(df_tweets$sentiment_score, prob = seq(0, 1, length = 11))
    message(sprintf("quantiles sur sentiment_score:"))
    print(sentiment_quantiles)
    
    # graphe de la répartition des tweets par rapport à leur score de sentiment
    df_tweets$sentiment_score_decile <- get_decile(df_tweets$sentiment_score)
    plot_sentiments <- ggplot(df_tweets, aes(x=sentiment_score_decile, y=sentiment_score)) + 
        stat_summary_bin(fun=median, bins=100, geom="line") +
        scale_x_reverse(breaks=1:10)
    print(plot_sentiments)
    
    # deciles de la répartition des auteurs par rapport à leur nombre de followers
    followers_count_by_authors <- df_tweets %>%
        group_by(user_id) %>%
        summarise_at(vars(followers_count), funs(max))
    followers_quantiles <- quantile(followers_count_by_authors$followers_count, prob = seq(0, 1, length = 11))
    message(sprintf("quantiles sur followers_count:"))
    print(followers_quantiles)
    
    # graphe de la répartition des auteurs par rapport à leur nombre de followers
    followers_count_by_authors$decile <- get_decile(followers_count_by_authors$followers_count)
    plot_authors <- ggplot(followers_count_by_authors, aes(x=decile, y=followers_count)) + 
        stat_summary(fun=mean, geom="line") +
        scale_x_reverse(breaks=1:10)
    
    # nuage de mots sur les hashtags

    hashtag_counts <- table(df_tweets$hashtags)
    # sort
    sorted_hashtag_counts <- sort(hashtag_counts, decreasing=TRUE)
    set.seed(1234)
    wc <- wordcloud2(sorted_hashtag_counts)
    saveWidget(wc, "tmp.html", selfcontained = F)
    fileName = str_c("img/", tag, "_hashtags.png")
    webshot::webshot("tmp.html", fileName, delay =6)
    print(wc)
    
    # # nuage de mots sur les textes
    # # text mining
    # text <- df_tweets$text
    # corpus <- Corpus(VectorSource(text))
    # # tolower words
    # tm_map(corpus, content_transformer(tolower))
    # 
    # # remove special chars
    # (kill_chars <- content_transformer (function(x, pattern) gsub(pattern, " ", x)))
    # corpus <- tm_map (corpus, kill_chars, "…")
    # corpus <- tm_map (corpus, kill_chars, "’")
    # corpus <- tm_map (corpus, kill_chars, "\"je")
    # corpus <- tm_map (corpus, kill_chars, '"je')
    # corpus <- tm_map (corpus, kill_chars, 'les')
    # 
    # # remove stopwords
    # corpus <- tm_map(corpus, removeWords, stopwords("french"))
    # # remove ponctuations
    # corpus <- tm_map(corpus, removePunctuation)
    # # remove numbers
    # corpus <- tm_map(corpus, removeNumbers)
    # # remove extra spaces
    # corpus <- tm_map(corpus, stripWhitespace)
    # 
    # # Most frequent terms by document
    # # contains everything that have minimum word length >= 1
    # DTM <- TermDocumentMatrix(corpus, control = list(minWordLength=c(1,Inf)))
    # # words appearing at least 1000 times
    # print(findFreqTerms(DTM, 1000))
    # # remove terms with a sparse factor of more than 0,98
    # DTM2 <- removeSparseTerms(DTM, sparse = 0.991)
    # matrix <- as.matrix(DTM2)
    # freq <- sort(rowSums(matrix), decreasing=TRUE)
    # df <- data.frame(word = names(freq),freq=freq)
    # set.seed(1234)
    # wc <- wordcloud2(df[1:100,])
    # print(wc)
    # fileName = str_c("img/", tag, "_text.png")
    # saveWidget(global_wc, "tmp.html", selfcontained = F)
    # webshot::webshot("tmp.html", fileName, delay =3)
    
    return(plot_authors)
}

# https://gvegayon.github.io/rgexf/
by_user_gephi_file <- function (df_tweets, tag) {
    gexf_nodes <- data.frame(distinct(df_tweets[,c(1,4)]) %>%
                                 dplyr::rename(
                                     id = user_id,
                                     label = screen_name
                                     ))
    gexf_edges <- data.frame(df_tweets[,c(1,12)] %>%
                                 dplyr::rename(
                                     source = user_id,
                                     target = retweet_user_id
                                     ))
    filename <- str_c("gexf/", tag, "_", format(now(), format="%Y%m%d%H%M%S"), ".gexf")
    write.gexf(gexf_nodes, gexf_edges, output = filename)
    
    
    csv_nodes <- df_tweets[,c(1,4)]
    csv_edges <- df_tweets[,c(1,12)]
    filename_csv  <- str_c("csv/complete/ARNmTweetsOnly_full", ".csv")
    write_as_csv(all_tweets_summarised, filename_csv, prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
    
    
    
    return(TRUE)
}

# by_status_gephi_file <- function (df_tweets, tag) {
#     gexf_nodes <- data.frame(distinct(df_tweets[,c(1,4)]) %>%
#                                  dplyr::rename(
#                                      id = user_id,
#                                      label = screen_name
#                                  ))
#     gexf_edges <- data.frame(df_tweets[,c(1,12)] %>%
#                                  dplyr::rename(
#                                      source = user_id,
#                                      target = retweet_user_id
#                                  ))
#     filename <- str_c("gexf/", tag, "_", format(now(), format="%Y%m%d%H%M%S"), ".gexf")
#     write.gexf(gexf_nodes, gexf_edges, output = filename)
#     return(TRUE)
# }

# On génère le fichier gephi pour analyse le graphe des hashtags
#hashtags <- all_tweets_distincted[which(!is.na(all_tweets_distincted$hashtags)) ,]
# # user_id, retweet_user_id, "hashtags"
# gexf_nodes <- distinct(retweets[,c(1,4)])
# gexf_edges <- retweets[,c(17,1)]
# gexf(nodes=gexf_nodes, edges=gexf_edges, output=str_c("gexf/ARNmTweets_", format(now(), format="%Y%m%d%H%M%S"), ".gexf"))