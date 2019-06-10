

library(igraph)
library(ggraph)
library(tidygraph)
library(tidyverse)
library(widyr)
library(PAFit)

setwd("G:\\生态学知识图谱构建可行性分析\\数据源")

read_csv("tidy_au.csv") -> tidy


tidy %>% filter(year<2015) -> tidy27

tidy27 %>%
  count(word) %>%
  rename(freq.27=n) -> tidy27.freq

tidy27 %>%
  pairwise_count(word,link,upper=F)  %>%
  graph_from_data_frame(directed = F)%>%
  as_tbl_graph(directed=F) %>%
  activate(edges) %>%
  rename(weight=n) %>%
  activate(nodes) %>%
  rename(word=name) %>%
  mutate(degree.27 = centrality_degree()) %>%
  as.tibble() %>%
  left_join(tidy27.freq) -> tidy27.table

tidy %>%
  count(word) %>%
  rename(freq=n) -> tidy.freq

tidy %>%
  pairwise_count(word,link,upper=F)  %>%
  graph_from_data_frame(directed = F)%>%
  as_tbl_graph(directed=F) %>%
  activate(edges) %>%
  rename(weight=n) %>%
  activate(nodes) %>%
  rename(word=name) %>%
  mutate(degree = centrality_degree()) %>%
  as.tibble() %>%
  left_join(tidy.freq) -> tidy.table

inner_join(tidy27.table,tidy.table) %>%
  mutate(delta.degree=degree-degree.27) %>%
  mutate(delta.freq=freq-freq.27) %>%
  select(-degree,-freq) -> wait_for_pafit

#################################################

tidy27 %>%
  mutate(word.id=as.numeric(as.factor(word))) ->tidy.1

tidy.1 %>%
  distinct(word,word.id) -> word_id

tidy.1 %>% 
  group_by(year) %>%
  pairwise_count(word.id,link,upper=F) %>%
  select(item1,item2,year) %>%
  as.matrix() %>%
  as.PAFit_net(type="undirected") -> PA.net

PA.net%>%
  get_statistics() -> stat.net

joint_estimate(PA.net,stat.net) -> full_result

summary(full_result)

full_result$estimate_result$alpha %>% as.numeric() -> alpha

full_result$estimate_result$f %>% 
  enframe() %>%
  mutate(name=as.numeric(name)) %>%
  rename(word.id=name,fitness=value)-> fit

word_id %>%
  select(word,word.id) %>%
  right_join(fit) %>%
  select(-word.id) %>%
  inner_join(wait_for_pafit) %>%
  mutate(Ak=degree.27^alpha) %>%
  mutate(P=Ak*fitness) -> full.compare

full.compare %>% write_csv("full_compare.csv")

#read_csv("full_compare.csv") -> full.compare

full.compare %>%
  select(-word) %>%
  select(delta.freq,delta.degree,everything()) %>%
  cor()

##########增加的比例
