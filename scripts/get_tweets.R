# https://github.com/ropensci/rtweet
# https://cran.r-project.org/web/packages/rtweet/readme/README.html
# https://rtweet.info/articles/auth.html
# https://www.infoworld.com/article/3515712/how-to-search-twitter-with-rtweet-and-r.html
if("httpuv" %in% rownames(installed.packages()) == FALSE) {install.packages("httpuv")};library(httpuv)
if("rtweet" %in% rownames(installed.packages()) == FALSE) {install.packages("rtweet")};library(rtweet)
if("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")};library(lubridate)
if("zip" %in% rownames(installed.packages()) == FALSE) {install.packages("zip")};library(zip)
if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")};library(tidyverse)

# generate tweeter token for session
create_token(
    app = Sys.getenv("TWEETER_APP_NAME"),
    Sys.getenv("TWEETER_CONSUMER_KEY"),
    Sys.getenv("TWEETER_CONSUMER_SECRET"),
    access_token = Sys.getenv("TWEETER_ACCESS_TOKEN"),
    access_secret = Sys.getenv("TWEETER_ACCESS_SECRET"),
    set_renv = TRUE
)

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
