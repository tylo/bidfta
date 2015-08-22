require(rvest)
url <- "http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords=camden+gray+linen+sofa"

item_1 <- url %>%
    html %>%
    html_nodes(".s-item-container") %>%
    .[[1]]

img_src <- item_1 %>% html_nodes("img") %>%
    html_attr("src")

price <- item_1 %>% html_node(".s-price") %>% html_text
