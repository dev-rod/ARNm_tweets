install.packages("data.table")
library(data.table)

#setwd("C:/Users/Louis/Desktop/RS_tweeter/ARNm_tweets")
#files = list.files(pattern="*.csv")
#df = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE,encoding = "UTF-8")))
load("data/all_tweets_summarised.RData") # 298856 tweets récupérés entre le 31 janvier et le 03 mars
df <- all_tweets_summarised


#numéro des messages qui sont des retweets
idRetweets <- which(df$is_retweet)
#vecteur du compteur de retweet
#pour les messages retweetés
nombre_retweets <- df$retweet_count[idRetweets]
#index de tri décroissant selon le nombre
index <- order(nombre_retweets,decreasing=TRUE)
#20 premiers messages avec des auteurs et des identifiants différents
print(df[df$is_retweet,][index[1:20],c('screen_name','user_id','retweet_count')])
print(df[df$is_retweet,][index[1:20],c('text')])
#on va enlever les doublons
#récupération du data.frame trié selon le nombre de retweets
#on ne travaille que sur les retweets (df$isRetweet)
dfRetweet <- df[df$is_retweet,][index,]
#première occurrence de chaque exemplaire de tweet
first <- !duplicated(dfRetweet$text)
#affichage des $2$ premiers éléments
print(dfRetweet$text[first][1:20])
#premiere analyse avec ce top retweet
#thèmes et individus
#les premières occurrences sont récupérées
dfUnique <- df[!duplicated(df$text),]
messages <- dfUnique$text
#retrait du saut de ligne \n
msgClean <- gsub("\n"," ",messages)
#retrait des URL
msgClean <- gsub('http\\S+\\s*',"",msgClean)
#retrait des espaces en trop
msgClean <- gsub("\\s+"," ",msgClean)
#retrait des "\"
msgClean <- gsub("[\\]","",msgClean)
#retrait des espaces en fin de texte
msgClean <- gsub("\\s*$","",msgClean)
#harmonisation de la casse - tout mettre en minuscule
msgClean <- tolower(msgClean)
#retrait des accents
msgClean <- gsub("[éèê]","e",msgClean)
msgClean <- gsub("[àâ]","a",msgClean)
msgClean <- gsub("[ùû]","u",msgClean)
#retrait de l'indicateur de retweet
msgClean <- gsub("rt ","",msgClean)
#enlever les doublons
msgClean <- msgClean[!duplicated(msgClean)]
#récupérer l'ensemble des mots délimtés par des ESPACE
all_mots <- unlist(strsplit(msgClean," "))
#un mot est un hashtag s'il débute par #
signature_hashtag <- regexpr("^#[[:alnum:]_]*",all_mots)
#récupérer l'ensemble des thèmes désignés par un "#" dans les messages
liste_hashtags <- regmatches(all_mots,signature_hashtag)
#nombre d'apparition de chaque hashtag
nb_hashtags <- table(liste_hashtags)
#tri selon la fréquence décroissante
tri_nb_hashtags <- sort(nb_hashtags,decreasing=TRUE)
#affichage des 50 hastags les plus fréquents
print(tri_nb_hashtags[1:50])
#un mot désigne un individu (pseudo) s'il débute par @
signature_individu <- regexpr("^@[[:alnum:]_]*",all_mots)
#récupérer l'ensemble des thèmes désignés par un "#" dans les messages
liste_individus <- regmatches(all_mots,signature_individu)
#nombre d'apparition des individus
nb_individus <- table(liste_individus)
#tri selon la fréquence décroissante
tri_nb_individus <- sort(nb_individus,decreasing=TRUE)
#affichage des 10 auteurs les plus fréquents
print(tri_nb_individus[1:10])
#Text mining
#retrait des hashtags
msgCleanBis <- gsub("#[[:alnum:]_]*( |:|$)","",msgClean)
#retrait des pseudos
msgCleanBis <- gsub("@[[:alnum:]_]*( |:|$)","",msgCleanBis)
install.packages("tm")
library(tm)
#transformation de la liste des tweets en un format interne
corpus <- Corpus(VectorSource(msgCleanBis))
#retrait des ponctuations
corpus <- tm_map(corpus,removePunctuation)
#retrait des nombres
corpus <- tm_map(corpus,removeNumbers)
#retrait des stopwords (mots outils)
corpus <- tm_map(corpus,removeWords,stopwords("french"))
#retirer les espaces en trop (s'il en reste encore)
corpus <- tm_map(corpus,stripWhitespace)
#création de la MDT à partir du corpus
mdt <- DocumentTermMatrix(corpus,control=list(weighting=weightBin))
#termes apparaissant au moins 1000 fois
print(findFreqTerms(mdt,1000))
#transformation en matrice pleine plus facile a manipuler
memory.limit() 
memory.limit(size =70000)
m <- as.matrix(mdt)
print(dim(m))
#frequence des mots
freqMots <- colSums(m)
#tri par ordre décroissant
freqMots <- sort(freqMots,decreasing=TRUE)
#affichage des 100 mots les plus fréquents
print(freqMots[1:100])
#ne conserver que les termes apparaissant plus de 100 fois dans la matrice
mClean <- m[,colSums(m) > 100]
freqMots2 <- colSums(mClean)
print(freqMots2[1:100])
#Extraction des règles d'association
install.packages("matrix")
install.packages("arules")
library(arules)
#paramètres de l'extraction des itemsets
parametres <- list(supp=0.01,minlen=2, maxlen=3,target="frequent itemsets")
#extraction des itemsets
itemsets <- apriori(mClean,parameter=parametres)
#affichage
inspect(itemsets)