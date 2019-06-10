
library(igraph)
library(ggraph)
library(tidygraph)
library(tidyverse)
library(widyr)


setwd("G:\\生态学知识图谱构建可行性分析\\数据源")

read_csv("tidy_au.csv") -> tidy

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

#freq-degree图的绘制
tidy.table %>% 
  filter(freq<=500) %>%
  ggplot(aes(x=freq,y=degree))+
  geom_boxplot(aes(group=cut_width(freq,10))) + 
  theme_bw() +
  xlab("frequency")

#相关性分析
tidy.table %>%
  group_by(freq) %>%
  summarize(mean=mean(degree),sd=sd(degree)) -> frde


 cor.test(frde$freq,frde$mean)

 cor.test(frde$freq,frde$sd)


#基于两种准则筛选关键词构造网络图对比

 tidy.table %>%
   count(freq) %>%
   print(n=100)
 
 tidy %>%
   pairwise_count(word,link,upper=F)  %>%
   graph_from_data_frame(directed = F)%>%
   as_tbl_graph(directed=F) %>%
   activate(edges) %>%
   rename(weight=n) %>%
   activate(nodes) %>%
   rename(word=name) %>%
   mutate(degree = centrality_degree()) %>%
   left_join(tidy.freq)-> tidy.net

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
  #filter(degree_rank <= 1000) %>%
  #filter(freq>=40) %>%
  arrange(desc(delta_rank)) %>%
  slice(1:50) -> under_est

write_csv(over_est,"over_est.csv")
write_csv(under_est,"under_est.csv")


