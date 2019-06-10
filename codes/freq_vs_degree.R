
library(igraph)
library(ggraph)
library(tidygraph)
library(tidyverse)
library(widyr)
library(data.table)


setwd("G:\\生态学知识图谱构建可行性分析\\数据源")

read.csv("us_states.csv") %>% pull(State) %>% tolower -> states
readLines("country.csv") %>% tolower() -> country
c(states,country) -> place
fread("tidy.csv") %>% as_tibble -> tidy
fread("eco.csv") %>% as_tibble -> eco

tidy %>%
  count(word) %>%
  rename(freq=n) -> tidy.freq

tidy %>%
  pairwise_count(word,doi,upper=F)  %>%
  graph_from_data_frame(directed = F)%>%
  as_tbl_graph(directed=F) %>%
  activate(edges) %>%
  rename(weight=n) %>%
  activate(nodes) %>%
  rename(word=name) %>%
  mutate(degree = centrality_degree()) %>%
  as.tibble() %>%
  inner_join(tidy.freq) -> tidy.table


tidy.table %>%
  mutate(degree_rank=min_rank(desc(degree))) %>%
  mutate(freq_rank=min_rank(desc(freq))) %>%
  mutate(delta_rank=freq_rank-degree_rank) %>%
  filter(degree_rank <=1000|freq_rank<=1000) %>%
  arrange(delta_rank) %>%
  slice(1:50) -> over_est

tidy.table %>%
  mutate(degree_rank=min_rank(desc(degree))) %>%
  mutate(freq_rank=min_rank(desc(freq))) %>%
  mutate(delta_rank=freq_rank-degree_rank) %>%
  filter(degree_rank <=1000|freq_rank<=1000) %>%
  arrange(desc(delta_rank)) %>%
  slice(1:50) -> under_est

over_est %>% head(20) -> over_table 
over_table %>% write.csv("over_table.csv")

under_est %>% 
  filter(!word %in% place & !str_detect(word,"british columbia")) %>% 
  head(20) -> under_table
under_table %>% write.csv("under_table.csv")

eco %>% filter(str_detect(keyword,"historical ecology")) %>% pull(journal) %>% table

over_table %>% pull(word) -> over_word
under_table %>% pull(word) -> under_word

eco %>% 
  tidytext::unnest_tokens(word, keyword, token = str_split, pattern = ";") %>% 
  mutate(word=str_trim(word)) %>% 
  filter(word != "") %>%
  distinct()-> tidy2

tidy2 %>% filter(word %in% over_word) %>% pull(journal) %>% table %>% sort
tidy2 %>% filter(word %in% under_word) %>% pull(journal) %>% table %>% sort

