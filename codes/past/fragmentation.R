
library(igraph)
library(ggraph)
library(tidygraph)
library(tidyverse)
library(widyr)


setwd("G:\\生态学知识图谱构建可行性分析\\数据源")

read_csv("tidy_au.csv") -> tidy


#第一张图：逐年增长的关键词数量
tidy %>% 
  count(year,word) %>% 
  count(year) ->a

fit=lm(nn~year,a)

lm_eqn <- function(df){
  m <- lm(nn ~ year, df);
  eq <- substitute(italic(Y) == a + b %.% italic(X)*","~~italic(R)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

a %>%
  ggplot(aes(year,nn)) + 
  geom_point() +
  geom_smooth(linetype="dashed",method="lm",se=F,show.legend = T) +
  theme_bw() +
  #geom_text(x = 1994, y = 15000, label = lm_eqn(a), parse = TRUE,size=4) +
  ylab("Distinct keyword number")

#第二张图：逐年减少的关键词联系性
make_graph = function(df){
  df %>%
    pairwise_count(word,link,upper=F) %>%
    graph_from_data_frame(directed = F) %>%
    as_tbl_graph(directed=F)
}
  
tidy %>%
  group_by(year) %>%
  nest() %>%
  mutate(graph=map(data,make_graph)) -> graph.table

graph.table %>%
 mutate(edge_den = map_dbl(graph,edge_density)) %>%
  ggplot(aes(year,edge_den))+
  geom_point() +
  geom_smooth(linetype="dashed",se=F,show.legend = T) +
  theme_bw() +
  ylab("Keyword network density")
 
#第三张图：较少关键词即可囊括较多的共现关系（出图速度较慢）

tidy %>%
  pairwise_count(word,link,upper=F)  %>%
  graph_from_data_frame(directed = F)%>%
  as_tbl_graph(directed=F) %>%
  activate(edges) %>%
  rename(weight=n) %>%
  activate(nodes) %>%
  rename(word=name) %>%
  mutate(degree = centrality_degree()) -> tidy.net

tidy.net %>% 
  activate(nodes) %>%
  as_tibble() %>%
  arrange(desc(degree)) %>%
  mutate(rank=row_number(desc(degree))) %>%
  mutate(prop=degree/sum(degree)) %>%
  mutate(cum_prop=cumsum(prop)) -> a

a %>%
  filter(cum_prop>=0.5) %>%
  head(1) %>%
  pull(rank) -> no.50

a %>%
  ggplot(aes(rank,cum_prop))+
  geom_line()+
  geom_point(aes(no.50,0.5,size=1),colour="#FF2400")+
  geom_hline(aes(yintercept = 0.5),linetype="dashed",colour="blue",size=0.8)+
  geom_linerange(aes(x=no.50,ymin=0,ymax=0.5),linetype="dashed",colour="blue",size=0.8) +
  annotate("text",x=no.50+8000,y=0.5-0.05,label="(5251,0.5)") +
  theme_bw() +
  ylab("Cumulative propotion of total degree") +
  theme(legend.position = "none")+
  scale_x_continuous(breaks = c(50000,100000))

a %>%
  mutate(rank_prop=rank/max(rank)) -> b

b%>%
  filter(cum_prop>=0.5) %>%
  dplyr::select(cum_prop,rank_prop) %>%
  head(1)-> no_50

b %>%
  ggplot(aes(rank_prop,cum_prop))+
  geom_line()+
  geom_point(aes(no_50$rank_prop,no_50$cum_prop,size=1),colour="#FF2400")+
  geom_hline(aes(yintercept = 0.5),linetype="dashed",colour="blue",size=0.8)+
  geom_linerange(aes(x=no_50$rank_prop,ymin=0,ymax=0.5),linetype="dashed",colour="blue",size=0.8) +
  annotate("text",x=no_50$rank_prop+0.1,y=0.5-0.05,label="(4.3%,50.0%)") +
  theme_bw() +
  ylab("Cumulative propotion of total degree") +
  xlab("Propotion of keywords that cover most co-ocurrence relations") +
  theme(legend.position = "none")+
  scale_x_continuous(labels = scales::percent)+
  scale_y_continuous(labels = scales::percent)

#第四张图：关键词网络中心性的变化
make_graph = function(df){
  df %>%
    pairwise_count(word,link,upper=F) %>%
    graph_from_data_frame(directed = F) %>%
    as_tbl_graph(directed=F)
}

tidy %>%
  group_by(year) %>%
  nest() %>%
  mutate(graph=map(data,make_graph)) -> graph.table

#连通性度量transitivity
graph.table %>%
  mutate(trans = map_dbl(graph,transitivity)) %>%
  ggplot(aes(year,trans))+
  geom_point() +
  geom_smooth(linetype="dashed",se=F,show.legend = T) +
  theme_bw() +
  ylab("Keyword network transitivity")

#中心性度量
graph.table %>%
  mutate(centr_list = map(graph,centr_degree)) %>%
  mutate(centr = map_dbl(centr_list,"centralization")) %>%
  ggplot(aes(year,centr))+
  geom_point() +
  geom_smooth(linetype="dashed",se=F,show.legend = T) +
  theme_bw() +
  ylab("Keyword network centralization(based on degree)")

graph.table %>%
  mutate(centr_list = map(graph,centr_betw)) %>%
  mutate(centr = map_dbl(centr_list,"centralization")) %>%
  ggplot(aes(year,centr))+
  geom_point() +
  geom_smooth(linetype="dashed",se=F,show.legend = T) +
  theme_bw() +
  ylab("Keyword network centralization(based on betweenness)")



