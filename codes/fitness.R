
library(tidyverse)
library(data.table)
#library(textcat)

setwd("G:\\生态学知识图谱构建可行性分析\\数据源")
fread("full_compare.csv") %>% as_tibble() -> full.compare

full.compare

full.compare %>%
  mutate(degree_growth_rate=delta.degree/degree.27,
         freq_growth_rate=delta.freq/freq.27) %>%
  select(-delta.degree,-delta.freq,-word) %>%
  select(-Ak) %>%
  cor %>%
  as.data.frame() %>%
  write_csv("fitness.csv")

full.compare %>%
  arrange(desc(fitness)) %>%
  select(word,fitness) %>% 
  print(n=50)

fread("full_table.csv") %>% as_tibble() -> full.table

full.table %>%
  arrange(desc(fitness)) %>%
  select(word,fitness) %>% 
  slice(-(1:3)) %>%
  head(10) %>%
  mutate(rank=min_rank(desc(fitness))) %>%
  write.csv("fittest.csv")



