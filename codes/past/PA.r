
library(igraph)
library(ggraph)
library(tidygraph)
library(tidyverse)
library(widyr)
library(PAFit)

setwd("G:\\生态学知识图谱构建可行性分析\\数据源")

read_csv("tidy_au.csv") -> tidy

#n=1998;m=2000;k=2001;

get_cor=function(n,m,k){
  tidy %>%
    filter(year>=n,year<=m) %>%
    pairwise_count(word,link,upper=F) %>%
    graph_from_data_frame(directed = F)%>%
    as_tbl_graph(directed=F) %>%
    activate(nodes) %>%
    rename(word=name) %>%
    mutate(degree = centrality_degree()) %>%
    as.tibble() %>%
    rename(degree.nm=degree)-> net.nm
  
  tidy %>%
    filter(year>=n,year<=k) %>%
    pairwise_count(word,link,upper=F) %>%
    graph_from_data_frame(directed = F)%>%
    as_tbl_graph(directed=F) %>%
    activate(nodes) %>%
    rename(word=name) %>%
    mutate(degree = centrality_degree()) %>%
    as.tibble() %>%
    rename(degree.nk=degree)-> net.nk
  
  inner_join(net.nm,net.nk) %>%
    mutate(delta.degree=degree.nk-degree.nm) %>%
    select(word,delta.degree) -> delta_degree
  ##
  tidy %>%
    filter(year>=n,year<=m) %>%
    mutate(word.id=as.numeric(as.factor(word))) -> tidy.1
  
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
  
  full_result$estimate_result$alpha %>% as.numeric() -> alpha
  
  full_result$estimate_result$f %>% 
    enframe() %>%
    mutate(name=as.numeric(name)) %>%
    rename(word.id=name,fitness=value)-> fit
  
  word_id %>%
    select(word,word.id) %>%
    right_join(fit) %>%
    select(-word.id) %>%
    inner_join(net.nm) %>%
    mutate(Ak=degree.nm^alpha) %>%
    mutate(P=Ak*fitness) -> Pa.fit
  
  Pa.fit %>%
    select(word,P) %>%
    inner_join(delta_degree)-> final.table
  
  cor(final.table$P,final.table$delta.degree)
}

crossing(1988:2017,1988:2017,1988:2017) %>%
  select(n=1,m=2,k=3) %>%
  filter(m-n>=2,k>m) ->  greed_table

#greed_table %>% slice(1:3) -> sub

greed_table %>%
  rowwise() %>%
  mutate(cor=get_cor(n,m,k)) -> full_table

write_csv(full_table,"full_table.csv")

