# https://www.pluralsight.com/guides/exploring-web-scraping-with-r
# https://qa.ostack.cn/qa/?qa=287828/

rm(list=ls())

#install.packages("rvest")
library(rvest)

library(stringr)
library(tidyverse)
library(xml2)

#install.packages("robotstxt")
#library(robotstxt)
#paths_allowed(
#    paths = c("https://www.amazon.sg/s?k=parenting&i=stripbooks&crid=3DGF4OTW5NWUS&sprefix#=parenting%2Cstripbooks%2C219&ref=nb_sb_noss_1")
#)

## parenting and relationships -> paperbacks only



## parenting and relationships -> parenting section 
### search parenting in "books"

url <- "https://www.amazon.sg/s?k=parenting&i=stripbooks&crid=3DGF4OTW5NWUS&sprefix=parenting%2Cstripbooks%2C219&ref=nb_sb_noss_1"


webdata = read_html(url)
print(webdata)

# Num pages
## https://stackoverflow.com/questions/45550820/scraping-amazon-web-using-r 
#pages<-c(1,2,3,4,5)
#pages<-1:3

##getting the url of the 5 pages
#urls<-data.table::rbindlist(lapply(pages,function(x){
#    url<-paste("https://www.amazon.sg/s?k=parenting&i=stripbooks&crid=3DGF4OTW5NWUS&sprefix=parenting%2Cstripbooks%2C219&ref#=nb_sb_noss_",x,sep="")
#    data.frame(url)
#}),fill=TRUE)


numPages <- webdata %>%
    html_node('.zg_pagination') %>%
    html_nodes('li') %>%
    length

# titles
webdata %>% 
    html_nodes(".s-line-clamp-2") %>% html_text()


# Pub_type: Paperback/hardback
webdata %>% 
    html_nodes(".a-spacing-mini.a-color-base .a-text-bold") %>% html_text() %>%  tibble(pub = .) 

pub_tibble <- webdata %>% 
    html_nodes(".a-spacing-mini.a-color-base .a-text-bold") %>% html_text() %>%  tibble(pub = .) 

#test <- tibble(pub = character())
#
#test1 <- bind_rows(test, pub_tibble)


# Price (dollars/whole)

webdata %>% 
    html_nodes(".s-price-instructions-style .a-price-whole") %>% html_text()


# Price (cents)

webdata %>% 
    html_nodes(".s-price-instructions-style .a-price-fraction") %>% html_text()

# Date  
webdata %>% 
    html_nodes(".a-color-secondary.a-text-normal") %>% html_text()


## One missing value because on book did not have ratings/reviews 

## # reviews
#webdata %>% 
#    html_nodes(".s-link-style .s-underline-text") %>% html_text()
#
#
## rating
#webdata %>% 
#    html_nodes(".aok-align-bottom") %>% html_text()
#


# Images 
#webdata %>% 
#    html_nodes(".s-image") %>% html_text()


# Scrap all web pages
## https://stackoverflow.com/questions/45550820/scraping-amazon-web-using-r

# https://www.amazon.sg/s?k=parenting&i=stripbooks&rh=p_36%3A100-&s=relevancerank&page=2&crid=3DGF4OTW5NWUS&qid=1650974729&rnid=6469114051&sprefix=parenting%2Cstripbooks%2C219&ref=sr_pg_2
items <- vector()

url_prefix1 <- "https://www.amazon.sg/s?k=parenting&i=stripbooks&rh=p_36%3A100-&s=relevancerank&page="
url_prefix2 <- "&crid=3DGF4OTW5NWUS&qid=1650974729&rnid=6469114051&sprefix=parenting%2Cstripbooks%2C219&ref=sr_pg_"

#title_all <- vector()
#pub_type_all <- vector()
#price_all <- vector()
#date_all <- vector()

# for(i in 1:75){
title_all <- tibble(title = character())
pub_type_all <- tibble(pub_type = character())
date_all <- tibble(date = character())
price_all <- tibble(price = character())

# 20 
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
    
  #  

}


title_tibble_subset <- tibble(title_all, date_all, pub_type_all)
getwd()
save(title_tibble_subset,
     title_all, date_all, pub_type_all, file  =  "an_data/amazon_parentingbooks_subset.RData")

# Entire 

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


#data <- bind_cols(title_all, pub_type_all,  date_all)

date_tibble <- tibble(date_all)

#data <- bind_cols(title_all, pub_type_all)





url_n <- paste0(url_prefix1, 1, url_prefix2,1)
page <- read_html(url_n)

title <- page %>%
    html_nodes(".s-line-clamp-2") %>%
    html_text(trim = TRUE)

pub_type <- page %>%
    html_nodes(".a-spacing-mini.a-color-base .a-text-bold") %>%
    html_text(trim = TRUE)

price <- page %>%
    html_nodes(".s-price-instructions-style .a-price-whole") %>%
    html_text(trim = TRUE)



items <- bind_cols(title, pub_type, price)

class(items)
data <- tibble(items)
