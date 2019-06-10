
library(pacman)
p_load(fst,data.table,dplyr,stringr)
p_load(tidytext,RTextTools)

#read.fst("/home/tyHuang/eco_io/eco_raw.fst",columns = c("au.key","eid")) -> keyword.raw

#read.fst("/home/tyHuang/eco_io/eco_raw.fst",from = 1,to = 2) %>% as_tibble() -> sample

fread("G:\\eco_io\\eco_raw.csv",select = c("au.key","eid")) %>% as_tibble -> keyword.raw

keyword.raw %>%
  unnest_tokens(word,au.key,token = stringr::str_split, pattern = ";") -> keyword.tidy

keyword.tidy %>%
  mutate(word = str_squish(word)) %>% as_tibble() -> key.clean

key.clean %>%
  mutate(stem = wordStem(word,language = "english",warnTested = FALSE)) -> compare.table

compare.table %>%
  as.data.table() %>%
  .[,.(stem,word.n = .N),by = .(word)] %>%
  .[,.(word,word.n,stem.n = .N),by = .(stem)] %>%
  unique() %>%
  #.[word != stem,,] %>%
  as_tibble() %>%
  filter(word.n < stem.n) %>%
  arrange(desc(stem.n)) %>%
  filter(str_detect(word,"forest")) %>%
  print(n = 100)

compare.table %>%
  as.data.table() %>%
  .[,.N,by = .(word)]

compare.table %>%
  filter(word != stem) %>%
  as.data.table() %>%
  unique(.,by = c("word","stem")) %>%
  .[order(-stem)]



compare.table %>%
  distinct(stem)
