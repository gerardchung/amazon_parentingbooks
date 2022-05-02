# Scrapped parenting books

rm(list=ls())

pacman::p_load(dplyr, tidyverse, janitor, stringr, quanteda, tidyr, tidytext)

# LOAD FILE ####
getwd()
load(file = "an_data/amazon_parentingbooks_all.RData") # these are all books first 50 pages
rm(title_all, pub_type_all)

load(file = "an_data/amazon_parentingbooks_subset.RData") # these are all books first 50 pages
rm(title_all, pub_type_all,date_all) # these are books in first 20 pages with dates



glimpse(title_tibble_all)
glimpse(title_tibble_subset)


title_tibble_all <- 
    title_tibble_all %>% 
    filter(title != "Purposeful Retirement: How to Bring Happiness and Meaning to Your Retirement (Volunteer Work, Retirement Planning, Retirement Gift)")

title_tibble_subset <- 
    title_tibble_subset %>% 
    filter(title != "Purposeful Retirement: How to Bring Happiness and Meaning to Your Retirement (Volunteer Work, Retirement Planning, Retirement Gift)")


# CREATE ID ####

data_all <- 
    title_tibble_all %>% 
    mutate(id = row_number(), .before = title)

data_sub <- 
    title_tibble_subset %>% 
    mutate(id = row_number(), .before = title)

# Date conversion ####
data_sub <- data_sub %>% mutate(date1 = as.Date(date, "%d %B %Y"))

# Duplicates 
    # Remove one book ####

dup <- data_all %>% duplicated()
tabyl(dup)

dup <- data_all[duplicated(data_all$title), ] 
NROW(dup) # 51 duplicates in title
data_all <- data_all[!duplicated(data_all$title), ] # 850 - 51 = 799 


dup2 <- data_sub[duplicated(data_sub$title), ] 
NROW(dup2) # 0 duplicates in title
data_sub <- data_sub[!duplicated(data_sub$title), ] # no change since no dups


# Corpus for data_all ####
corp <- corpus(data_all,
               docid_field = "id",
               text_field = "title")

#mystopwords = c("parent", "parents", "parenting") 
    # since i wil tokens_tolower before doing tokens_select, 
    # i do not need to include caps variants

tok <- 
    corp %>% 
    tokens(remove_numbers = T,
           remove_punct = T,
           remove_separators = T,
           remove_symbols = T,
           remove_url = T,
           include_docvars = T) %>% 
    tokens_tolower() %>% 
    tokens_select(pattern = c(stopwords("en")
                            #  ,
                            #  mystopwords
                              ),
                  selection = "remove") %>% 
   # tokens_wordstem() %>% 
    tokens_ngrams(n = 1:2) 


dfm <-
    tok %>% dfm() #%>% dfm_trim( min_termfreq = 5, min_docfreq = 2) 
# words appear at least 5 times across documents and in 10 of document

library(quanteda.textstats)

topfeatures(dfm, n = 30,scheme = "docfreq")
topfeatures(dfm, n = 30,scheme = "count")
str_view_all(data_all$title, regex(pattern = "\\beditl\\b", ignore_case = T), match = T)
str_view_all(data_all$title, regex(pattern = "edit", ignore_case = T), match = T)


summary(summary(corp))
ndoc(dfm)
nfeat(dfm)

# convert  to tidy
library(tidytext)
data_tidy <- tidy(dfm)

# Quanteda network plot
library(quanteda.textplots)
set.seed(100)
fcmat <- fcm(tok, context = "window", tri = FALSE)
feat <- names(topfeatures(fcmat, 30))
fcm_select(fcmat, pattern = feat) %>%
    textplot_network(min_freq = 0.2)

    

