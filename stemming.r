rm(list=ls())
library(tm)
library(quanteda)
library(SnowballC)

setwd("D:/D/Administrative/R repo/TextMining/email")
email <- read.csv("email.csv",header=TRUE,sep=";",encoding= "utf-8")


email_text <- data.frame(doc_id=seq(1:nrow(email)),text=email[,ncol(email)])



#Using tm package
email_corpus <- Corpus(DataframeSource(email_text))


email_corpus1 <- tm_map(email_corpus, stripWhitespace)
email_corpus1 <- tm_map(email_corpus1, content_transformer(tolower))
email_corpus1 <- tm_map(email_corpus1, removeWords, stopwords("hungarian"))
email_corpus2 <- tm_map(email_corpus1, stemDocument, language="hungarian")

inspect(email_corpus2[[1]])






#Using quanteda package
dcorpus <- corpus(train$item_description)


data.frame(sapply(email_corpus2, function(x) x[[1]]))
