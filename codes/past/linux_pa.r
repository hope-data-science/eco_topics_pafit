
library(igraph)
library(ggraph)
library(tidygraph)
library(tidyverse)
library(widyr)
library(PAFit)
library(BBmisc)
library(foreach)
library(doParallel)

setwd("/home/tyHuang/pafit")

read_csv("tidy_au.csv") -> tidy

#n=1998;m=2000;k=2001;


get_cor=function(x){
  tidy %>%
    filter(year>=x[[1]],year<=x[[2]]) %>%
    pairwise_count(word,link,upper=F) %>%
    graph_from_data_frame(directed = F)%>%
    as_tbl_graph(directed=F) %>%
    activate(nodes) %>%
    rename(word=name) %>%
    mutate(degree = centrality_degree()) %>%
    as.tibble() %>%
    rename(degree.nm=degree)-> net.nm
  
  tidy %>%
    filter(year>=x[[1]],year<=x[[3]]) %>%
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
    filter(year>=x[[1]],year<=x[[2]]) %>%
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
  
  cor.test(final.table$P,final.table$delta.degree)->cor.info
  c(start=x[[1]],end=x[[2]],predict=x[[3]],cor=cor.info$estimate,p=cor.info$p.value)
}

crossing(1988:2017,1988:2017,1988:2017) %>%
  select(n=1,m=2,k=3) %>%
  filter(m-n>=2,k>m) ->  greed_table

convertRowsToList(greed_table, name.list = TRUE, name.vector = FALSE,
                  factors.as.char = TRUE, as.vector = TRUE) -> a



# Initiate cluster
cl <- makeCluster(40,type = "FORK")
registerDoParallel(cl)
#registerDoParallel(2)
#getDoParWorkers()


Sys.time() -> time.start
full_table= foreach(i=seq_along(a),.combine=rbind) %dopar% get_cor(a[[i]]) 
Sys.time()-> time.end
time.end - time.start

stopImplicitCluster()

write_csv(full_table,"full_table.csv")






