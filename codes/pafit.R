
#nohup R CMD BATCH "pafit.R"&

library(igraph)
library(ggraph)
library(tidygraph)
library(tidyverse)
library(widyr)
library(PAFit)
library(data.table)

setwd("G:\\生态学知识图谱构建可行性分析\\数据源")
#setwd("/home/tyHuang/pafit")

fread("tidy.csv") %>% as_tibble -> tidy

tidy %>% filter(year<2015) -> tidy27

tidy27 %>%
  count(word) %>%
  rename(freq.27=n) -> tidy27.freq

tidy27 %>%
  pairwise_count(word,doi,upper=F)  %>%
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
  pairwise_count(word,doi,upper=F)  %>%
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


############################################################

tidy27 %>%
  mutate(word.id=as.numeric(as.factor(word))) ->tidy.1

tidy.1 %>%
  distinct(word,word.id) -> word_id

tidy.1 %>% 
  group_by(year) %>%
  pairwise_count(word.id,doi,upper=F) %>%
  select(item1,item2,year) %>%
  as.matrix() %>%
  as.PAFit_net(type="undirected") -> PA.net

PA.net%>%
  get_statistics() -> stat.net

system.time({
  joint_estimate(PA.net,stat.net) -> full_result
})


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

####
fread("full_compare.csv") %>% as_tibble() -> full.compare

with(full.compare,lm(delta.freq~freq.27)%>%summary)
with(full.compare,lm(delta.freq~degree.27)%>%summary)
with(full.compare,lm(delta.freq~P)%>%summary)

with(full.compare,lm(delta.degree~freq.27)%>%summary)
with(full.compare,lm(delta.degree~degree.27)%>%summary)
with(full.compare,lm(delta.degree~P)%>%summary)

library(vegan)

par(mfrow=c(1,2))

with(full.compare,varpart(delta.freq,freq.27,degree.27,P)) -> mod1
op <- par(mar = rep(0, 4))
#showvarparts(3, bg=2:4)
plot(mod1, bg=2:4,Xnames=c("Frequency","Degree","PAFit")) 
par(op)

with(full.compare,varpart(delta.degree,freq.27,degree.27,P)) -> mod2
op <- par(mar = rep(0, 4))
#showvarparts(3, bg=2:4)
plot(mod2, bg=2:4,Xnames=c("Frequency","Degree","PAFit"))
par(op)

par(mfrow=c(1,1))

###################draw
tiff("G:\\生态学知识图谱构建可行性分析\\文集\\PLOS_1\\第一次返修\\Fig_5.tif", 
     width = 2200,height = 800,res = 300)
par(mfrow=c(1,2))

with(full.compare,varpart(delta.freq,freq.27,degree.27,P)) -> mod1
op <- par(mar = rep(0, 4))
#showvarparts(3, bg=2:4)
plot(mod1, bg=2:4,Xnames=c("Frequency","Degree","PAFit")) 
par(op)

with(full.compare,varpart(delta.degree,freq.27,degree.27,P)) -> mod2
op <- par(mar = rep(0, 4))
#showvarparts(3, bg=2:4)
plot(mod2, bg=2:4,Xnames=c("Frequency","Degree","PAFit"))
par(op)

par(mfrow=c(1,1))
dev.off()
###################


###extract the table
mod1$part$fract

mod2$part$fract

#rbind(mod1$part$fract,mod2$part$fract)

cbind(mod1$part$fract,mod2$part$fract) %>% write_csv("vpa_raw.csv")

###optional
library(yhat)
commonalityCoefficients(full.compare,"delta.freq",list("freq.27","degree.27","P"))
commonalityCoefficients(full.compare,"delta.degree",list("freq.27","degree.27","P"))


#####


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
  left_join(tidy.freq) -> tidy.table


tidy %>%
  mutate(word.id=as.numeric(as.factor(word))) ->tidy.2

tidy.2 %>%
  distinct(word,word.id) -> word_id

tidy.2 %>% 
  group_by(year) %>%
  pairwise_count(word.id,doi,upper=F) %>%
  select(item1,item2,year) %>%
  as.matrix() %>%
  as.PAFit_net(type="undirected") -> PA.net

PA.net%>%
  get_statistics() -> stat.net

system.time({
  joint_estimate(PA.net,stat.net) -> full_result
})

#   user  system elapsed 
# 7145.48  342.21 6529.11 

summary(full_result)

full_result$estimate_result$alpha %>% as.numeric() -> alpha

full_result$estimate_result$f %>% 
  enframe() %>%
  mutate(name=as.numeric(name)) %>%
  rename(word.id=name,fitness=value)-> fit

fit %>%
  left_join(word_id)

word_id %>%
  select(word,word.id) %>%
  right_join(fit) %>%
  select(-word.id) %>%
  inner_join(tidy.table) %>%
  mutate(Ak=degree^alpha) %>%
  mutate(P=Ak*fitness) -> full.table

full.table %>%
  arrange(desc(fitness)) %>%
  write.csv("full_table.csv")

full.table %>%
  select(word,freq,degree,Ak,fitness,P) %>%
  arrange(desc(P)) %>%
  head(15) %>%
  write_csv("top_by_Pafit.csv")




