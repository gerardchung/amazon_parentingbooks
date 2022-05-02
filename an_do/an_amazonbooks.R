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

# Freq plot ####

library(ggtext)
library(scales)
library(showtext)

#install.packages("geomtextpath")
library(geomtextpath)

#font_add_google("roboto condensed")
#font_add_google("poppins")
font_add_google("Dosis", "Dosis")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)


#font <- "roboto condensed"
#label_font <- "roboto condensed"
#
#font <- "poppins"
#label_font <- "poppins"

font <- "Dosis"
label_font <- "Dosis"

bcolor <- "#7EC8E3"
fontcolor <- "#000000"

nus_blue = "#003D7C"
nus_orange = "#EF7C00"

tar_blue = "#7BAFD4"
yellow = "#FFCC66"


theme_set(theme_classic( base_family = font))
library("MetBrewer")
# https://github.com/BlakeRMills/MetBrewer#functions


word_freq <-
    data_tidy %>% 
    group_by(term) %>% 
    summarize(word_n = sum(count)) %>% 
    ungroup()

word_freq %>% 
    filter(term != "parent" & term != "parents" & term != "parenting") %>% 
    slice_max(word_n, n = 50) %>% 
    ggplot(aes(x = word_n, y = reorder(term,word_n))) +
    geom_point(size = 3, color = nus_blue) +    
    geom_textsegment(aes(yend=term, xend=-0, label = term),  
                 alpha = .9, 
                 size = 5, 
                 linewidth = 1.5,
                 linecolor = "black",
                 textcolor = nus_blue, fontface = 7, family = font
              ) +
    labs(title = "Parenting books on Amazon.sg",
         subtitle = "Most used words in book titles",
         x = "# times word appeared",
         y = "Word",
         caption = "GERARDCHUNG.COM"
    ) + 
    theme(#legend.position = "none",
        plot.title = element_text(size=22, face="bold"),
        plot.subtitle = element_text(size=18),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_blank(),
       # axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=15),
       axis.ticks.y = element_blank()
    ) + 
    scale_x_continuous(limits = c(0, 190), breaks = seq(0, 200, by = 20), expand = c(0, 0)) 

#scale_fill_manual(values=met.brewer("Degas", 6)) +

    
getwd()
ggsave(file = "results/word_freq.png",
       plot =   last_plot(),
       #  width = 40,
       # height = 40,
       units = "cm",
       dpi = 300, 
       type = "cairo"
)

    

