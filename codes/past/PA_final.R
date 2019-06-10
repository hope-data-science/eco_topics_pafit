
library(igraph)
library(ggraph)
library(tidygraph)
library(tidyverse)
library(widyr)
library(PAFit)

setwd("G:\\生态学知识图谱构建可行性分析\\数据源")

read_csv("tidy_au.csv") -> tidy

tidy %>%
  pairwise_count(word,link,upper=F)  %>%
  graph_from_data_frame(directed = F)%>%
  as_tbl_graph(directed=F) %>%
  activate(edges) %>%
  rename(weight=n) %>%
  activate(nodes) %>%
  rename(word=name) %>%
  mutate(degree = centrality_degree()) %>%
  as.tibble()  -> tidy.table

tidy %>%
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
  inner_join(tidy.table) %>%
  mutate(Ak=degree^alpha) %>%
  mutate(P=Ak*fitness) -> Pa.fit

#四种评价体系下的生态学热点排序表格

tidy %>%
  count(word) %>%
  rename(freq=n)-> freq.table

Pa.fit %>%
  left_join(freq.table) %>%
  arrange(desc(P)) %>%
  write_csv("P_sorted.csv")

Pa.fit %>%
  left_join(freq.table) %>%
  arrange(desc(fitness))%>%
  write_csv("fit_sorted.csv")

Pa.fit %>%
  arrange(desc(degree))%>%
  write_csv("degree_sorted.csv")

freq.table %>%
  arrange(desc(freq)) %>%
  write_csv("freq_sorted.csv")
















