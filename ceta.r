
#https://www.r-bloggers.com/an-overview-of-text-mining-visualisations-possibilities-with-r-on-the-ceta-trade-agreement/?utm_source=feedburner&utm_medium=feed&utm_campaign=Feed%3A+RBloggers+%28R+bloggers%29
#https://github.com/jwijffels/ceta-txtmining/blob/master/01_import_ceta_treaty_pos_tagging.R

#Words per minute (WPM, max 5 characters)
#characters per minute (CPM) --> 20 WPM are roughly equal to 80-100 CPM

library(pdftools)
library(data.table)
library(pattern.nlp)

######################################################################################
## Data preparation
##
######################################################################################
download.file(url = "http://trade.ec.europa.eu/doclib/docs/2014/september/tradoc_152806.pdf", destfile = "tradoc_152806.pdf")

## Read in the CETA treaty as a character vector of length 1
txt <- pdf_text("tradoc_152806.pdf")
txt <- unlist(txt)
txt <- paste(txt, collapse = " ")

## Identify articles and make a dataset containing 1 row per article of the CETA treaty
ceta <- data.frame(txt = strsplit(txt, split = "\n")[[1]], 
                   stringsAsFactors = FALSE)
ceta$startofarticle <- FALSE
ceta$startofarticle[grep("^article[[:digit:]]+(.[[:digit:]]+)*(.[[:digit:]]+)*(.[[:digit:]]+)*$", gsub("[[:space:]]", "", tolower(ceta$txt)))] <- TRUE
ceta$document <- cumsum(ceta$startofarticle)
ceta <- subset(ceta, document >= 1)
ceta <- as.data.table(ceta)
ceta <- ceta[, list(article = head(txt, 1), 
                    article.title = tail(head(txt, 2), 1),
                    txt = paste(txt[-c(1, 2)], collapse = " ")), by = list(document)]
ceta$article <- gsub("^ +|\r", "", ceta$article)
ceta$article.title <- gsub("^ +|\r", "", ceta$article.title)
ceta$article.id <- seq_len(nrow(ceta))

## Natural Language Processing: POS tagging
ceta_tagged <- mapply(article.id = ceta$article.id, content = ceta$txt, FUN=function(article.id, content){
  out <- pattern_pos(x = content, language = "english", core = TRUE)
  out$article.id <- rep(article.id, times = nrow(out))
  out
}, SIMPLIFY = FALSE)  
ceta_tagged <- rbindlist(ceta_tagged)

## Limit ourselves to nouns only and terms with more than 2 characters
ceta_nouns <- subset(ceta_tagged, word.type %in% c("NN") & nchar(word.lemma) > 2)

ceta <- list(ceta_txt = txt,
             ceta = ceta,
             ceta_tagged = ceta_tagged, 
             ceta_nouns = ceta_nouns)
save(ceta, file = "ceta.RData")