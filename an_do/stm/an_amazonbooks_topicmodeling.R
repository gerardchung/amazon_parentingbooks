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

# Drop id 899 because it will be dropped ater st preprocessing (empty title)
data_all <- data_all %>%  filter(id!=899)

# Corpus for data_all ####
corp <- corpus(data_all,
               docid_field = "id",
               text_field = "title")

mystopwords = c("parent", "parents", "parenting") 
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
    tokens_select(pattern = c(stopwords("en"),
                              mystopwords
                              ),
                  selection = "remove") %>% 
    tokens_wordstem() %>% 
    tokens_ngrams(n = 1:2) 


dfm <-
    tok %>% dfm() #%>% dfm_trim( min_termfreq = 5, min_docfreq = 2) 
# words appear at least 5 times across documents and in 10 of document

library(quanteda.textstats)

topfeatures(dfm, n = 30,scheme = "docfreq")
topfeatures(dfm, n = 30,scheme = "count")
str_view_all(data_all$title, regex(pattern = "\\beditl\\b", ignore_case = T), match = T)
str_view_all(data_all$title, regex(pattern = "edit", ignore_case = T), match = T)



# STM ####
library(stm)
## leeMimno ====

# prepDocuments can take the object out of convert -> then this can plugged in and k=0 will work
stm_input_stm <-
    dfm  %>%  quanteda::convert(to = "stm") 
    # id_bysentence 899 were dropped because they are empty doc after the pre-processing
    # RECORDE THE ID OF DOCUMENTS REMOVED

plotRemoved(stm_input_stm$documents, lower.thresh = seq(1, 100, by = 1))

out <- prepDocuments(stm_input_stm$documents, stm_input_stm$vocab, stm_input_stm$meta,
                     lower.thresh = 0)
# the default is to remove docs with only 1 word/token 
# this can be change by specifying the threshold
plotRemoved(stm_input_stm$documents, lower.thresh = seq(1, 100, by = 1))
docs <- out$documents
vocab <- out$vocab
meta <- out$meta


#processed <- textProcessor(data_stm$f16_end, metadata = data_stm) 
#out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
#docs <- out$documents
#vocab <- out$vocab
#meta <- out$meta

# Run Lee Mimno algorithm =====
#system.time(
#    stm_mod_leeMimno <- stm(documents = out$documents,
#                            vocab = out$vocab,
#                            K = 0,
#                            init.type = "Spectral",
#                           # prevalence = ~  s(pub_year),
#                            #  content = ~ gentreat_bas,
#                            #  prevalence = ~ gentreat_bas*prepost,
#                            #content = ~ Rating,
#                            # data = out$meta,
#                            data = meta,
#                            seed = 8458159)
#    
#)
#
#system("say Gerard, finished!")
#
#
#getwd()  
#save(stm_mod_leeMimno, file = "an_data/stm/stm_mod_leeMimno_spline.RData") 
load(file = "an_data/stm/stm_mod_leeMimno_spline.RData") 


### Plot leeMimno and estimate effect =====
plot(stm_mod_leeMimno, 
     n = 10, 
     text.cex = .8)
# k=35


# SEARCHK ####

system.time(
    storage <- searchK(documents = out$documents, vocab = out$vocab, 
                       K = c(5,10,15,20,25,30,40,50,60,70,80,90,100,120), 
                    #   K = c(5,10,15,20,22,23,24,25,26,27,28,29,30,40), 
                       #K = c(5,6,7), 
                      # prevalence = ~ s(pub_year), 
                       #content = ~ gentreat_bas, # content dont hv coherence
                       # max.em.its = 75, 
                       data = meta, 
                       init.type = "Spectral",  
                       seed = 8458159)
)

system("say Gerard, finished!")


#system.time(
#    storage2 <- searchK(documents = out$documents, vocab = out$vocab, 
#                        K = c(20,21,22,23,24,25,26,27,28,29,30,31,32), 
#                        prevalence = ~ gentreat_bas, 
#                        # max.em.its = 75, 
#                        data = meta, 
#                        init.type = "Spectral",  
#                        seed = 8458159)
#)


save(storage, file = "an_data/stm/stm_mod_searchk_spline.RData") 
load(file = "an_data/stm/stm_mod_searchk_spline.RData")
plot(storage)
#plot(storage2)

## Plot storage in GGPLOT  ======
class(storage$results)
k_results <- as.data.frame(storage$results)

### Convert this dataframe of lists to a dataframe of vectors ======
# https://stackoverflow.com/questions/29674661/r-list-of-lists-to-data-frame
library(jsonlite)
library(purrr)
library(data.table)

k_results_df <- map(k_results, as.data.table)
k_results_df <- rbindlist(k_results_df, fill = TRUE, idcol = T)
pacman::p_load(dplyr,stringr, ggplot2, janitor, tidyverse, quanteda, tidytext) 
# need to rerun this again because functions of dplyr will not work;
# I believe due to data.table library package
# for e.g. ,when i rename below, it will not work.
# if the code is run, dont need to do dplyr:: below
names(k_results_df)

k_results_df <-  k_results_df %>%  dplyr::rename(name = .id)

k_results_df <-  as.data.frame(t(k_results_df))

k_results_df <- 
    k_results_df  %>% 
    row_to_names(row_number = 1) 

names(k_results_df)
k_results_df <- k_results_df %>% mutate(K = as.integer(K),
                                        exclus = as.numeric(exclus),
                                        semcoh = as.numeric(semcoh),
                                        heldout = as.numeric(heldout),
                                        residual = as.numeric(residual),
                                        bound = as.numeric(bound),
                                        lbound = as.numeric(lbound),
                                        em.its = as.numeric(em.its))

k_results_df_long <- 
    k_results_df %>%
    select(-em.its, -bound) %>% 
    pivot_longer(cols = c(exclus:lbound)) %>% 
    rename(metric = name)

### Plot the metrics for searchK ======
k_results_df_long %>% 
    filter(metric != "exclus") %>% 
    ggplot( aes(y = value, x = K, color = metric)) +
    geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
    geom_point(size = 2, alpha = 0.7, show.legend = FALSE) +
    facet_wrap(vars(metric), scales = "free_y") +
    labs(x = "K (number of topics)",
         y = NULL,
         title = "Model diagnostics by number of topics",
         subtitle = "These diagnostics indicate that a good number of topics would be around 25") +
    theme_minimal() +
    scale_x_continuous( breaks = seq(0, 60, by = 5)) +
    ggrepel::geom_text_repel(aes(label = K), nudge_x = 2, color = "black", size = 3)

### Plot exclusivity and coherence (different methods) =====

# Method 1: STM functions
topicQuality(model=k20, documents=out$documents)


# Method 2: https://cschwem2er.github.io/stminsights/reference/get_diag.html
#  this method uses get_diag function from stminsights
#  it churns out the mean and median value for exclus and semcoh for
#  all the topics within each K value. 
#  I find this more useful than to churn values for all topics for each K value 

## Do each model for each value of K

k15 <- stm(documents = out$documents,
           vocab = out$vocab,
           K = 15,
          # prevalence = ~ s(pub_year), 
           # data = out$meta,
           data = meta,
           init.type = "Spectral",  
           seed = 8458159)

k20 <- stm(documents = out$documents,
           vocab = out$vocab,
           K = 20,
          # prevalence = ~ s(pub_year), 
           # data = out$meta,
           data = meta,
           init.type = "Spectral",  
           seed = 8458159)


k22 <- stm(documents = out$documents,
           vocab = out$vocab,
           K = 22,
          # prevalence = ~ s(pub_year), 
           # data = out$meta,
           data = meta,
           init.type = "Spectral",  
           seed = 8458159)


k24 <- stm(documents = out$documents,
           vocab = out$vocab,
           K = 24,
         #  prevalence = ~ s(pub_year), 
           # data = out$meta,
           data = meta,
           init.type = "Spectral",  
           seed = 8458159)


k25 <- stm(documents = out$documents,
           vocab = out$vocab,
           K = 25,
         #  prevalence = ~ s(pub_year), 
           # data = out$meta,
           data = meta,
           init.type = "Spectral",  
           seed = 8458159)

k26 <- stm(documents = out$documents,
           vocab = out$vocab,
           K = 26,
       #    prevalence = ~ s(pub_year), 
           # data = out$meta,
           data = meta,
           init.type = "Spectral",  
           seed = 8458159)

k27 <- stm(documents = out$documents,
           vocab = out$vocab,
           K = 27,
        #   prevalence = ~ s(pub_year), 
           # data = out$meta,
           data = meta,
           init.type = "Spectral",  
           seed = 8458159)

#k28 <- stm(documents = out$documents,
#           vocab = out$vocab,
#           K = 28,
#           prevalence = ~ s(pub_year), 
#           # data = out$meta,
#           data = meta,
#           init.type = "Spectral",  
#           seed = 8458159)

#k29 <- stm(documents = out$documents,
#           vocab = out$vocab,
#           K = 29,
#           prevalence = ~ s(pub_year), 
#           # data = out$meta,
#           data = meta,
#           init.type = "Spectral",  
#           seed = 8458159)
#
#k30 <- stm(documents = out$documents,
#           vocab = out$vocab,
#           K = 30,
#           prevalence = ~ s(pub_year), 
#           # data = out$meta,
#           data = meta,
#           init.type = "Spectral",  
#           seed = 8458159)
#
#
#k31 <- stm(documents = out$documents,
#           vocab = out$vocab,
#           K = 31,
#           prevalence = ~ s(pub_year), 
#           # data = out$meta,
#           data = meta,
#           init.type = "Spectral",  
#           seed = 8458159)
#
#k32 <- stm(documents = out$documents,
#           vocab = out$vocab,
#           K = 32,
#           prevalence = ~ s(pub_year), 
#           # data = out$meta,
#           data = meta,
#           init.type = "Spectral",  
#           seed = 8458159)
#
#k33 <- stm(documents = out$documents,
#           vocab = out$vocab,
#           K = 33,
#           prevalence = ~ s(pub_year), 
#           # data = out$meta,
#           data = meta,
#           init.type = "Spectral",  
#           seed = 8458159)
#
#
#k34 <- stm(documents = out$documents,
#           vocab = out$vocab,
#           K = 34,
#           prevalence = ~ s(pub_year), 
#           # data = out$meta,
#           data = meta,
#           init.type = "Spectral",  
#           seed = 8458159)
#
#k35 <- stm(documents = out$documents,
#           vocab = out$vocab,
#           K = 35,
#           prevalence = ~ s(pub_year), 
#           # data = out$meta,
#           data = meta,
#           init.type = "Spectral",  
#           seed = 8458159)

save(k15,k20,k22,k24,k25,k26,k27,
     file = "an_data/stm/differentK_ExclusSem_spline.RData") 
load(file = "an_data/stm/differentK_ExclusSem_spline.RData")

### get diagnostics using STM insights function get_diag
library(stminsights)
diag <- get_diag(models = list(
    k15 = k15,
    k20 = k20,
    k22 = k22,
    k24 = k24,
    k25 = k25,
    k26 = k26,
    k27 = k27
   # k28 = k28,
   # k29 = k29,
   # k30 = k30,
   # k31 = k31,
   # k32 = k32,
   # k33 = k33,
   # k34 = k34,
   # k35 = k35
),
outobj = out)

diag %>%
    ggplot(aes(x = coherence, y = exclusivity, color = statistic))  +
    geom_text(aes(label = name), nudge_x = 1) + geom_point() +
    labs(x = 'Semantic Coherence', y = 'Exclusivity') + theme_light()

# Method 3: https://francescocaberlin.blog/2019/06/26/messing-around-with-stm-part-iiia-model-selection/
# the above method get the mean across all the topics for each K values
# the below method will churn the exclus and semcoh for every topic in every K value
# i dont find it very helpful to have every single value -> hard to evaluate
#M10ExSem<-as.data.frame(cbind(c(1:20),exclusivity(k20), semanticCoherence(model=k20, docs), "Mod20"))
M15ExSem<-as.data.frame(cbind(c(1:15),exclusivity(k15), semanticCoherence(model=k15, docs), "Mod15"))
M26ExSem<-as.data.frame(cbind(c(1:26),exclusivity(k26), semanticCoherence(model=k26, docs), "Mod26"))


ModsExSem <- rbind(#M10ExSem, 
                   M15ExSem, 
                   M26ExSem)
colnames(ModsExSem) <- c("K","Exclusivity", "SemanticCoherence", "Model")


ModsExSem$Exclusivity <- as.numeric(as.character(ModsExSem$Exclusivity))
ModsExSem$SemanticCoherence<-as.numeric(as.character(ModsExSem$SemanticCoherence))

ggplot(ModsExSem, aes(SemanticCoherence, Exclusivity, color = Model)) + 
    geom_point(size = 2, alpha = 0.7) + 
    geom_text(aes(label=K), nudge_x=.001, nudge_y=.001)+
    labs(x = "Semantic coherence",
         y = "Exclusivity",
         title = "Comparing exclusivity and semantic coherence")


# FINAL MODEL ######

## Choose k= 26 
system.time(
    stm_k_final <- stm(documents = out$documents,
                       vocab = out$vocab,
                       K = 26,
                      # prevalence = ~ s(pub_year), 
                       #content = ~ gentreat_bas,
                       # data = out$meta,
                       data = meta,
                       init.type = "Spectral",  
                       seed = 8458159)
)


system("say Gerard, finished!")

#getwd()  
save(stm_k_final, file = "an_data/stm/stm_k_final_spline.RData") 
load(file = "an_data/stm/stm_k_final_spline.RData") 




### Plot [Final model stm_k_final]  =====

# Normal STM function
plot(stm_k_final, 
     n = 5, 
     text.cex = .8)

plot(stm_k_final, 
     n = 10, 
     text.cex = .8)

summary(stm_k_final)

# SAVE STM MODEL AND TD_GAMMA FOR ANALYSIS ELSEWHERE ####
save(stm_k_final, data_all,  diag, dfm, out, docs, meta, stm_input_stm, k_results_df, 
     file = "an_data/stm/post_stm_analyses/post_stm__spline.RData")



