
library(tidyverse)
library(igraph)
library(ggraph)
library(tidygraph)
library(widyr)

setwd("G:\\ecology_journals_raw")


dir() -> filename
tibble() -> eco_raw

for(i in seq_along(filename))
{
 read_csv(filename[[i]]) %>% select('DOI','Authors with affiliations','Author Keywords') -> a
 eco_raw %>% bind_rows(a) -> eco_raw
}

eco_raw %>% 
 rename(doi='DOI',awa='Authors with affiliations',keyword='Author Keywords')  -> eco


eco %>%
  filter(str_detect(awa,"Wu, J.") & str_detect(awa,"Beijing Normal University") |
           str_detect(awa,"Forman, R.T.T.") & str_detect(awa,"Harvard University")) -> case

case %>%
  tidytext::unnest_tokens(word, keyword, token = stringr::str_split, pattern = ";") %>%
  na.omit %>%
  mutate(word=str_trim(word)) -> case.full

case.full%>%
  mutate(is.wu=str_detect(awa,"Wu, J.") & str_detect(awa,"Beijing Normal University")) %>%
  mutate(is.forman= str_detect(awa,"Forman, R.T.T.") & str_detect(awa,"Harvard University")) %>%
  group_by(word) %>%
  summarise(is.wu=sum(is.wu),is.forman=sum(is.forman)) %>%
  mutate(is.both=is.wu*is.forman) %>%
  mutate(author=case_when(
    is.both != 0 ~ "both",
    is.both ==0 & is.wu !=0 ~ "Wu",
    is.both ==0 & is.wu ==0 ~ "Forman"
  )) %>%
  mutate(sum=is.wu+is.forman)-> case.wordlist

case.full%>% 
  pairwise_count(word,doi,upper=F) %>%
  graph_from_data_frame(directed = F) %>%
  as_tbl_graph(directed=F) %>%
  rename(word=name) %>%
  left_join(case.wordlist)-> case.net

case.net %>%
  ggraph(layout = "kk") +               #linear 'fr', 'kk', 'lgl', 'graphopt'
  geom_edge_link(aes(edge_width = n), edge_colour = "skyblue3",edge_alpha = 0.5) +
  geom_node_point(aes(size = sum,colour=author)) +
  geom_node_text(aes(label = word,colour=author), repel = TRUE, 
                 fontface="bold",size=4,
                 point.padding = unit(0.2, "lines")) +
  theme_void() 


