
library(tidyverse)
setwd("G:\\生态学知识图谱构建可行性分析\\数据源\\ecology_journals")


dir() -> filename
tibble() -> eco_raw

for(i in seq_along(filename))
{
 read_csv(filename[[i]]) %>% select('Link','Year','Author Keywords','Index Keywords','Source title','Document Type') -> a
 eco_raw %>% bind_rows(a) -> eco_raw
}

eco_raw %>% 
 rename(link='Link',year='Year',au_keyword='Author Keywords',id_keyword='Index Keywords',journal='Source title',type='Document Type') %>%
 filter(type=='Article') %>%
 select(-type) %>%
 na.omit() %>%
 filter(year >= 1988,year<=2017) %>%
 filter(journal != "J. ECOL.") %>%
 distinct() %>%
 mutate(journal = str_to_upper(journal)) %>%
 filter(journal != "JOURNAL OF APPLIED ECOLOGY (CHINA)") %>%
 mutate(journal = str_replace(journal,"FUNCT. ECOL.","FUNCTIONAL ECOLOGY")) %>%
 mutate(journal = str_replace(journal,"THE JOURNAL OF ANIMAL ECOLOGY","JOURNAL OF ANIMAL ECOLOGY")) %>%
 mutate(journal = str_replace(journal,"THE ISME JOURNAL","ISME JOURNAL")) %>%
 mutate(journal = str_replace(journal,"^MICROBIAL.+","MICROBIAL ECOLOGY")) %>%
 mutate(journal = str_replace(journal,"FOREST ECOLOGY & MANAGEMENT","FOREST ECOLOGY AND MANAGEMENT")) %>%
 mutate(journal = str_replace(journal,"TRENDS IN ECOLOGY & EVOLUTION","TRENDS IN ECOLOGY AND EVOLUTION")) %>%
 mutate(journal = str_replace(journal,"ECOLOGICAL APP.+","ECOLOGICAL APPLICATIONS")) %>%
 filter(journal != "TRENDS IN ECOLOGY AND EVOLUTION") %>%
 filter(journal != "FRONTIERS IN ECOLOGY AND THE ENVIRONMENT") %>%
 filter(journal != "ECOSYSTEM SERVICES") -> eco

eco$journal %>% as.factor %>% summary 
eco$journal %>% as.factor %>% table %>% nrow
nrow(eco)

#选取了1988-2017年26个国际生态学期刊，共70668篇文章，只选取了Article
#统计每年文章总数
eco %>%
  count(year,journal) %>%
  mutate(year=as.factor(year)) %>%
  ggplot(aes(x=year,y=n,fill=journal)) +
  geom_bar(stat="identity") +
  ylab("no. of article")

setwd("G:\\生态学知识图谱构建可行性分析\\数据源")

write_csv(eco,"eco.csv")

#作者关键词分词
eco %>% 
  select(-journal,-id_keyword) %>%
  tidytext::unnest_tokens(word, au_keyword, token = stringr::str_split, pattern = ";") %>% 
  mutate(word=str_trim(word)) %>% 
  filter(word != "")-> tidy.au

#索引关键词分词
eco %>% 
  select(-journal,-au_keyword) %>%
  tidytext::unnest_tokens(word, id_keyword, token = stringr::str_split, pattern = ";") %>% 
  mutate(word=str_trim(word)) %>% 
  filter(word != "article") %>%
 # mutate(word = str_replace(word,"\\(.*\\)","")) %>%
  filter(word != "")-> tidy.id

write_csv(tidy.au,"tidy_au.csv")
write_csv(tidy.id,"tidy_id.csv")



