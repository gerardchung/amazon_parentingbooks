# https://www.pluralsight.com/guides/exploring-web-scraping-with-r
# https://qa.ostack.cn/qa/?qa=287828/

rm(list=ls())

#install.packages("rvest")
library(rvest)

library(stringr)
library(tidyverse)
library(xml2)

# search parenting in "books" in amazon.sg ####


## Scrap  web pages =====
    ## https://stackoverflow.com/questions/45550820/scraping-amazon-web-using-r

# https://www.amazon.sg/s?k=parenting&i=stripbooks&rh=p_36%3A100-&s=relevancerank&page=2&crid=3DGF4OTW5NWUS&qid=1650974729&rnid=6469114051&sprefix=parenting%2Cstripbooks%2C219&ref=sr_pg_2

url_prefix1 <- "https://www.amazon.sg/s?k=parenting&i=stripbooks&rh=p_36%3A100-&s=relevancerank&page="
url_prefix2 <- "&crid=3DGF4OTW5NWUS&qid=1650974729&rnid=6469114051&sprefix=parenting%2Cstripbooks%2C219&ref=sr_pg_"
    # need to break the url into two because the page numbers appear twice in the url

#title_all <- vector()
#pub_type_all <- vector()
#price_all <- vector()
#date_all <- vector()

### Create the vectors for the  data scrapped interative for each web page ===== 
title_all <- tibble(title = character())
pub_type_all <- tibble(pub_type = character())
date_all <- tibble(date = character())
price_all <- tibble(price = character())

#### first 19 pags because these pages have no missing data for publication date ====
for(i in 1:19){
    url_n <- paste0(url_prefix1, i, url_prefix2,i)
    page <- read_html(url_n)
    Sys.sleep(sample(47, 1) * .4)
    title <- page %>%
        html_nodes(".s-line-clamp-2") %>%
        html_text(trim = TRUE) %>% 
        tibble(title = .) 

    pub_type <- page %>%
        html_nodes(".a-spacing-mini.a-color-base .a-text-bold") %>%
        html_text(trim = TRUE) %>% 
        tibble(pub_type = .) 
    
    date <- page %>%
        html_nodes(".a-color-secondary.a-text-normal") %>%
        html_text(trim = TRUE) %>% 
        tibble(date = .) 

    price <- page %>%
        html_nodes(".s-price-instructions-style .a-price-whole") %>%
        html_text(trim = TRUE) %>% 
        tibble(price = .) 
    
    title_all <- bind_rows(title_all,title)
    date_all <- bind_rows(date_all,date)
    price_all <- bind_rows(price_all,price)
    pub_type_all <- bind_rows(pub_type_all,pub_type)
   # title_all <- append(title_all, title)
   # pub_type_all <- append(pub_type_all, pub_type)
   # date_all <- append(date_all, date)
   # price_all <- append(price_all, price)
}
    # only the date_all, title_all, and pub_type_all will no missing data

title_tibble_subset <- tibble(title_all, date_all, pub_type_all)
getwd()
save(title_tibble_subset,
     title_all, date_all, pub_type_all, file  =  "an_data/amazon_parentingbooks_subset.RData")

# Entire 75 pages

title_all <- tibble(title = character())
pub_type_all <- tibble(pub_type = character())
date_all <- tibble(date = character())
price_all <- tibble(price = character())


for(i in 1:75){
    url_n <- paste0(url_prefix1, i, url_prefix2,i)
    page <- read_html(url_n)
    Sys.sleep(sample(55, 1) * .9)
    title <- page %>%
        html_nodes(".s-line-clamp-2") %>%
        html_text(trim = TRUE) %>% 
        tibble(title = .) 
    
    pub_type <- page %>%
        html_nodes(".a-spacing-mini.a-color-base .a-text-bold") %>%
        html_text(trim = TRUE) %>% 
        tibble(pub_type = .) 
    
   # date <- page %>%
   #     html_nodes(".a-color-secondary.a-text-normal") %>%
   #     html_text(trim = TRUE) %>% 
   #     tibble(date = .) 
   # 
   # 
   # price <- page %>%
   #     html_nodes(".s-price-instructions-style .a-price-whole") %>%
   #     html_text(trim = TRUE) %>% 
   #     tibble(price = .) 
    
    title_all <- bind_rows(title_all,title)
 #   date_all <- bind_rows(date_all,date)
 #   price_all <- bind_rows(price_all,price)
    pub_type_all <- bind_rows(pub_type_all,pub_type)
    # title_all <- append(title_all, title)
    # pub_type_all <- append(pub_type_all, pub_type)
    # date_all <- append(date_all, date)
    # price_all <- append(price_all, price)
    
    #  
    
}


title_tibble_all <- tibble(title_all, pub_type_all)
getwd()
save(title_tibble_all,
     title_all, pub_type_all, file  =  "an_data/amazon_parentingbooks_all.RData")


