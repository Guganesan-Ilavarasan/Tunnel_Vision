#Synopsis Scraper (To scrape the opinion)

library(rvest)
library(dplyr)

movies = data.frame()

for (page_result in seq(from = 1, to = 48, by = 1)) {
  link = paste0("https://infrastructure.planninginspectorate.gov.uk/projects/south-west/a303-stonehenge/?ipcsection=relreps&ipcpagesizesubmit=Apply&ipcsearch=&ipcpagesize=50&ipcpage=", 
                page_result, "#ipcprevnext")
  
  page = read_html(link)
  
  # name = page %>% html_nodes("#repstable strong") %>% html_text()
  # movie_links = page %>% html_nodes("td") %>%
  #   html_attr("href") %>% paste("https://infrastructure.planninginspectorate.gov.uk/projects/south-west/a303-stonehenge/", ., sep="")
  synopsis = page %>% html_nodes(".ipcrelrep") %>% html_text()
  # date = sapply(movie_links, FUN = get_date, USE.NAMES = FALSE)
  
  movies = rbind(movies, data.frame(synopsis, stringsAsFactors = FALSE))
  
  print(paste("Page:", page_result))  
}

write.csv(movies,"movies.csv")
# =IF(ISERROR(SEARCH(A2,B2,1)),B2,RIGHT(B2,LEN(B2)-LEN(A2)))


#Names Scraper (To scrape the names)

#library(rvest)
#library(dplyr)

names = data.frame()

for (page_result in seq(from = 1, to = 48, by = 1)) {
  link = paste0("https://infrastructure.planninginspectorate.gov.uk/projects/south-west/a303-stonehenge/?ipcsection=relreps&ipcpagesizesubmit=Apply&ipcsearch=&ipcpagesize=50&ipcpage=", 
                page_result, "#ipcprevnext")
  
  page = read_html(link)
  
  name = page %>% html_nodes("#repstable strong") %>% html_text()
  # movie_links = page %>% html_nodes("td") %>%
  #   html_attr("href") %>% paste("https://infrastructure.planninginspectorate.gov.uk/projects/south-west/a303-stonehenge/", ., sep="")
  # synopsis = page %>% html_nodes(".ipcrelrep") %>% html_text()
  
  names = rbind(names, data.frame(name, stringsAsFactors = FALSE))
  
  print(paste("Page:", page_result))  
}

write.csv(names,"names.csv")
# =IF(ISERROR(SEARCH(A2,B2,1)),B2,RIGHT(B2,LEN(B2)-LEN(A2)))

