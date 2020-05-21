library(tidyverse)
library(rvest)
library

url<- "https://www.beckershospitalreview.com/rankings-and-ratings/states-ranked-by-hospital-beds-per-1-000-population.html"

#convert a website into an XML object using read_html()
url_beds <- read_html(url)
url_beds
#extract the relevant nodes from the xml object usinb html_nodes()
nodes_bed <- html_nodes(url_beds,xpath = "//*[@id=\"left-column\"]/div[1]/p[5]")
nodes_bed2 <- nodes_bed <- html_nodes(url_beds,xpath = "//*[@id=\"left-column\"]/div[1]/p[6]")

#extract the tagged data by applying html_text()
nodes_bed %>% html_text()

#wrap a function to extract states ranked by beds 
state_beds <- function(x, index) {
   xpath = str_c("//*[@id=\"left-column\"]/div[1]/p[",as.character(index),"]")# %>% as.character()
   x %>% html_nodes(xpath=xpath) %>% html_text()
}

state_beds(url_beds,index=5)
state_beds(url_beds,index=10)
#extract all paragraphs with the wanted data
beds <- 5:52 %>% purrr::map_chr(.f = function(t) state_beds(url_beds,t))
#extract state names with regexp
states = c()
for (i in 1:length(beds)) {
  states[i] <- str_extract_all(beds[i],"([A-Z|a-z])\\w+")
}
#extract beds numbers with regexp
bedsper1000 <- c()
for (i in 1:48) {
  bedsper1000[i] <- str_extract_all(beds[i],"\\s[0-9].[0-9]|\\s[0-9]")
}

#adjust state names with more than one word
for (i in c(1,2,3,5,21,27,28,36,37,38,46)) {
  states[[i]] <- states[[i]] %>% paste(collapse = " ")
}
#simply states and beds
states <- states %>% unlist() 
bedsper1000 <- bedsper1000 %>% unlist() %>% as.numeric()

#make a tibble
states_ranked_by_bedsper1000 <- tibble(State=states,
                                       beds = bedsper1000)
#save as a csv file
write_csv(states_ranked_by_bedsper1000,"state_by_beds.csv")










