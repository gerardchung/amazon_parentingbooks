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


# KEEP ONLY TITLES WITH MOTHERS AND FATHERS WORDS
str_view_all(data_all$title, regex(pattern = "dad|mum|mother|father|dads|mums|mothers|fathers", ignore_case = T), match =T)

data_all <- 
    data_all %>% 
    mutate(mumdad = str_detect(title, regex(pattern = "dad|mum|mother|father|dads|mums|mothers|fathers|mom|mama", ignore_case = T)))

data_all %>% tabyl(mumdad)

dadmum <- 
    data_all %>% 
    filter(mumdad == T) %>% 
    mutate(dad = str_detect(title, regex(pattern = "dad|father|dads|fathers", ignore_case = T)),
           mum = str_detect(title, regex(pattern = "mum|mother|mums|mothers|mom|mama", ignore_case = T)))

dadmum %>% tabyl(dad, mum)
dadmum %>% tabyl(dad)
dadmum %>% tabyl(mum)

# Plot freq for mum and dad 

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

font <- "Dosis"
label_font <- "Dosis"

bcolor <- "#7EC8E3"
fontcolor <- "#000000"
nus_blue = "#003D7C"
nus_orange = "#EF7C00"

tar_blue = "#7BAFD4"
yellow = "#FFCC66"

theme_set(theme_classic( base_family = font))


dadmum_bar <- 
    dadmum %>% 
    summarize(n_dad = sum(dad),
              n_mum = sum(mum)) %>% 
    pivot_longer(cols = c(n_dad,n_mum)) %>% 
    add_row(name = "n_all", value = 1049) # this is for neither mum nor dad
    
dadmum_bar <- 
    dadmum_bar %>% mutate(ToHighlight = ifelse( name == "n_all", "yes", "no" ) )


dadmum_bar %>% 
    ggplot(aes(y = name, x = value, fill = ToHighlight )) +
    geom_bar(stat = "identity") +
    scale_y_discrete(labels = c("'No reference made'",'"Dad"','"Mum"' )) +
    geom_text(
        aes(label = value),
        colour = "white", size = 5, family = font,
        hjust = 1.2, vjust = 1.5, position = position_dodge(.9)
    ) + 
    theme(#legend.position = "none",
        plot.title = element_text(size=22, face="bold"),
        plot.subtitle = element_text(size=18),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size=15),
        axis.text.x = element_text(size=13)
    ) +
    labs(title = "Number of Books on Amazon.sg with 'mum' or 'dad' in titles", 
       #  subtitle = "# of books",
         x = "# of books",
         #y = "Books titles with 'Mum' or 'Dad'"
         ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_fill_manual( values = c( "yes"=tar_blue, "no"="gray" ), guide = FALSE )
        
    
getwd()
ggsave(file = "results/mumdad_occurrences.png",
       plot =   last_plot(),
       #  width = 40,
       # height = 40,
       units = "cm",
       dpi = 300, 
       type = "cairo"
)


# Differentiate tokens between dads and mums
library(tidytext)

glimpse(dadmum)

both <- dadmum %>% filter(dad == TRUE & mum == TRUE) 
NROW(both) # 2 books had both mum and dad -> filter them out

word_dadmum <- 
    dadmum %>% 
    select(-mumdad) %>% 
    mutate(mumdad = case_when(dad == TRUE ~ "dad",
                              mum == TRUE ~ "mum")) %>% 
    filter(!(dad == TRUE & mum == TRUE)) %>% 
    unnest_tokens(word, title)  %>% 
    anti_join(stop_words)

word_dadmum_count <- 
    word_dadmum %>% 
    group_by(mumdad, word) %>% 
    summarize(n_mumdad = n()) %>% 
    ungroup() %>% 
    filter(!str_detect(word, regex(pattern = "dad|mum|mother|father|dads|mums|mothers|fathers|mom|moms|mama|parents|parenting|book", ignore_case = T)))

# Plot words by mum or dads
library(forcats)
word_dadmum_count <- word_dadmum_count %>% mutate(word = reorder_within(word, n_mumdad, mumdad)) 

word_dadmum_count %>% 
    group_by(mumdad) %>%
    slice_max(n_mumdad, n = 5) %>%
    ungroup() %>%
    ggplot(aes(n_mumdad, word, fill = mumdad)) +
    geom_col(aes(fill = mumdad), show.legend = FALSE) +
    facet_wrap(~mumdad, ncol = 2, scales = "free_y") +
    labs(title = "Book Titles with Word 'Dad' or 'Mum'",
         subtitle = "What words are often used?",
        x = "# occurrences", y = NULL) + 
    scale_y_reordered() +
    scale_x_continuous(expand = c(0,0)) +
   # theme(panel.spacing.x = unit(-4, "lines")) +
#    theme(panel.spacing = unit(-5, "lines"))
    theme(#legend.position = "none",
        plot.title = element_text(size=22, face="bold"),
        plot.subtitle = element_text(size=18),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size=15),
        axis.text.x = element_text(size=13),
        panel.spacing = unit(-14, "lines") # for spacing btw facet_wrap
    ) +
    scale_fill_manual(values = c(tar_blue, yellow))


