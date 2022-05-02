# https://www.pluralsight.com/guides/exploring-web-scraping-with-r
# https://qa.ostack.cn/qa/?qa=287828/

#install.packages("rvest")
library(rvest)

library(stringr)
library(xml2)

install.packages("robotstxt")
library(robotstxt)
paths_allowed(
    paths = c("https://www.imdb.com/")
)

## parenting and relationships -> paperbacks only



## parenting and relationships -> parenting section -> paperbacks only

url = "https://www.amazon.com/s?k=parenting&i=stripbooks&rh=n%3A283155%2Cn%3A20%2Cn%3A11401&dc&page=2&crid=38U91IX0Y4YP0&qid=1650812081&rnid=283155&sprefix=%2Cstripbooks%2C301&ref=sr_pg_1"


webdata = read_html(url)
print(webdata)

## Note that sponsered are excluded!

# titles
webdata %>% 
    html_nodes(".a-size-base-plus") %>% html_text()



# Paperback/hardback
webdata %>% 
    html_nodes(".a-spacing-mini.a-color-base .a-text-bold") %>% html_text()





# Price (dollars/whole)

webdata %>% 
    html_nodes(".s-price-instructions-style .a-price-whole") %>% html_text()


# Price (cents)

webdata %>% 
    html_nodes(".s-price-instructions-style .a-price-fraction") %>% html_text()

# Date  
webdata %>% 
    html_nodes(".a-color-secondary.a-text-normal") %>% html_text()

# # reviews
webdata %>% 
    html_nodes(".s-link-style .s-underline-text") %>% html_text()


# rating
webdata %>% 
    html_nodes(".aok-align-bottom") %>% html_text()



