# TEXT-MING SOCIAL WORK NEWS 
## STM has been done
## I now look at the post-stm analyses in Tidy format to explore the topic model
##  https://juliasilge.com/blog/evaluating-stm/

rm(list = ls())

# LOAD PACKAGES ####
pacman::p_load(dplyr,stringr, ggplot2, janitor, tidyverse, tidytext, stm) 

# GGPLOT SETTINGS ####
library(ggtext)
library(scales)
library(showtext)

#install.packages("geomtextpath")
library(geomtextpath)

font_add_google("roboto condensed")
font_add_google("poppins")
font_add_google("Dosis", "Dosis")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

font <- "roboto condensed"
label_font <- "roboto condensed"

font <- "poppins"
label_font <- "poppins"

#font <- "Dosis"
#label_font <- "Dosis"

bcolor <- "#7EC8E3"

fontcolor <- "#000000"

nus_blue = "#003D7C"
nus_orange = "#EF7C00"

tar_blue = "#7BAFD4"
yellow = "#FFCC66"


theme_set(theme_classic( base_family = font))


# LOAD DATASET ####
getwd()
load(file = "an_data/stm/post_stm_analyses/post_stm__spline.RData")


# Explore the FREX words ####

words <- (labelTopics(stm_k_final))
words_df <- tibble(words)


# Explore the topic model ####
## Convert the stm_final_model to tidy format =====
# https://juliasilge.com/blog/evaluating-stm/
# https://juliasilge.github.io/tidytext/reference/stm_tidiers.html
# beta is the (per-term-per-topic, default) or gamma/theta (per-document-per-topic) matrix. 
# The stm package calls this the theta matrix, but other topic modeling packages call this gamma.

## Probabilities that each document is generated from each topic, that gamma matrix =====

### beta (word-topics_)
td_beta <- tidy(stm_k_final, matrix = "beta") # the default is matric =  beta 
td_beta

### Gamma 
td_gamma <- tidy(stm_k_final, matrix = "gamma",
                 document_names = rownames(meta)) 
# DONT USE out$documents; USE meta
# Because documents were removed in the pre-processing

td_gamma <- td_gamma %>% mutate(document = as.integer(document))

## Plot topic prevalence with key terms
library(ggthemes)
library(scales)

top_terms <- 
    td_beta %>%
    arrange(beta) %>%
    group_by(topic) %>%
    top_n(6, beta) %>%
    arrange(-beta) %>%
    select(topic, term) %>%
    summarise(terms = list(term)) %>%
    mutate(terms = map(terms, paste, collapse = ", ")) %>% 
    unnest()

gamma_terms <- 
    td_gamma %>%
    group_by(topic) %>%
    summarise(gamma = mean(gamma)) %>%
    arrange(desc(gamma)) %>%
    left_join(top_terms, by = "topic") %>%
    mutate(topic = paste0("Topic ", topic),
           topic = reorder(topic, gamma))

library(wesanderson)

## https://www.datanovia.com/en/blog/easy-way-to-expand-color-palettes-in-r/
#library(RColorBrewer)
## Define the number of colors you want
#nb.cols <- 29
#mycolors <- colorRampPalette(brewer.pal(8, "Pastel2"))(nb.cols)

library("MetBrewer")
# https://github.com/BlakeRMills/MetBrewer#functions

gamma_terms %>%
    top_n(26, gamma) %>%
    ggplot(aes(topic, gamma, label = terms, fill = topic)) +
    geom_col(show.legend = FALSE) +
    geom_text(hjust = 0, nudge_y = 0.0005, size = 3, family = font ) +
    coord_flip() +
    scale_y_continuous(expand = c(0,0),
                       limits = c(0, 0.15),
                       labels = percent_format()) +
    #   theme_tufte( ticks = T, base_family=font) +
    theme(plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 13)) +
    labs(x = NULL, y = expression(gamma),
         title = "What are the topics of parenting books sold on Amazon.sg",
         subtitle = "With the top words that contribute to each topic") +
    # viridis::scale_fill_viridis(discrete = TRUE, option = "D") +
    scale_fill_manual(values=met.brewer("Tiepolo", 26))

getwd()
ggsave(file = "results/stm/topicsprevalence_words.png",
       plot =   last_plot(),
       #  width = 40,
       # height = 40,
       units = "cm",
       dpi = 300, 
       type = "cairo",
)

plot(stm_k_final, 
     n = 10, 
     text.cex = .8)

summary(stm_k_final)

### Using Kable 
gamma_terms %>%
    select(topic, gamma, terms) %>%
    knitr::kable(digits = 3, 
                 col.names = c("Topic", "Expected topic proportion", "Top 7 terms"))


# Look at word topics again using td_beta above 

## Plot 
td_beta
td_beta %>%
    group_by(topic) %>%
    slice_max(beta, n = 3) %>%
    ungroup() %>%
    filter(topic < 5) %>% 
    mutate(topic = paste("Topic", topic)) %>%
    ggplot(aes(beta, reorder_within(term, beta, topic), fill = topic)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(vars(topic), scales = "free_y") +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_reordered() +
    labs(x = expression(beta), y = NULL) 



# GET THE TOP DOCUMENTS ASSOCIATED WITH EACH TOPIC ####
glimpse(td_gamma)

## Get top 3 documents associated with each topic 

topdocs <- 
    td_gamma %>% 
    group_by(topic) %>% 
    slice_max(gamma, n=5) %>% 
    ungroup() %>% 
    rename(id =document )

## Merge in the data with the topdocs
names(data_all)
merge_topdocs <- 
    topdocs %>% 
    left_join(data_all) 
    
merge_topdocs <- merge_topdocs %>% filter(!is.na(title))

merge_topdocs %>% knitr::kable(digits = 3, 
             #col.names = c("Topic", "Expected topic proportion", "Top 7 terms")
             )
