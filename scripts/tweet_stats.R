if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")};library(tidyverse)

get_decile <- function(x){
    ceiling(10*rank(-x, ties.method="random") / length(x))
}

get_tweet_stat <- function (df_tweets){
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
    
    return(plot_authors)
}