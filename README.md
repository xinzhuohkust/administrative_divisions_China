# 中华人民共和国国家统计局统计用区划和城乡划分代码(2009-2021) #
这是一个基于R的爬虫，用以爬取http://www.stats.gov.cn/tjsj/tjbz/tjyqhdmhcxhfdm/ 上的所有行政区划信息

# 市 ---------------------------------------------------------------------------
require("pacman")
p_load(rvest, httr, tidyverse, progress)

for (z in c(2009：2021)) { 

str_c("http://www.stats.gov.cn/tjsj/tjbz/tjyqhdmhcxhfdm/", z, "/index.html") %>% 
  read_html("GB18030") %>% 
  html_nodes("a") %>% 
  html_attr("href") %>% 
  str_extract_all("\\d+\\.html") %>% 
  unlist() %>% 
  unique() -> province_sites

webs <- list()
sites <- c()
i = 0
while (i < length(province_sites)) {
  i = i + 1
  str_c("http://www.stats.gov.cn/tjsj/tjbz/tjyqhdmhcxhfdm/", z, "/", province_sites[i]) -> sites[i]
  cat("scraping url number", i, "\n")
  
  tryCatch(webs[[i]] <- sites[i] %>% GET(., timeout(2)) %>% read_html("GB18030"),
           error = function(e) {webs[[i]] <<- "timed out!" } )
  if((webs[[i]] == "timed out!")[1]) {i = i - 1}
} 

prase <- function(x) {x %>% 
    html_elements("tbody") %>% 
    html_table(header = T) %>% 
    .[[4]] %>% 
    select(1,2, 3) %>% 
    .[-1,] %>% 
    set_names(nm = c("统计用区划代码", "城乡分类代码", "名称"))}

Prase <- function(x) {x %>% 
    html_elements("tbody") %>% 
    html_table(header = T) %>% 
    .[[4]] %>% 
    select(1,2) %>% 
    .[-1,] %>% 
    set_names(nm = c("统计用区划代码", "名称"))}

prase_url <- function(x) {x %>% 
    html_nodes("a") %>% 
    html_attr("href") %>% 
    str_extract_all("\\d+\\.html") %>% 
    unlist() %>% 
    unique()}

map_df(webs, Prase) -> cities
map(webs, prase_url) %>% 
  unlist() %>% 
  unique() -> city_sites

# 县 ---------------------------------------------------------------------------

webs <- list()
sites <- c()
i = 0
while (i < length(city_sites)) {
  i = i + 1
  str_c("http://www.stats.gov.cn/tjsj/tjbz/tjyqhdmhcxhfdm/", z, "/", str_extract(city_sites[i], "^\\d{2}"), "/", city_sites[i]) -> sites[i]
  cat("scraping url number", i, "\n")
  
  tryCatch(sites[i] %>% GET(., timeout(2)) %>% read_html(encoding = "GB18030") -> webs[[i]],
           error = function(e) { webs[[i]] <<- "timed out!"})
  if((webs[[i]] == "timed out!")[1]) {i = i - 1}
} 

map_df(webs, Prase) -> districts
map(webs, prase_url) %>% 
  unlist() %>% 
  unique() -> district_sites

district_sites[str_length(district_sites) == 11] -> district_sites

# 区 ---------------------------------------------------------------------------

webs <- list()
sites <- c()
i = 0
while (i < length(district_sites)) {
  i = i + 1
  str_c("http://www.stats.gov.cn/tjsj/tjbz/tjyqhdmhcxhfdm/", z, "/", str_extract(district_sites[i], "^\\d{2}"), "/", str_sub(district_sites[i], 3, -8), "/", district_sites[i]) -> sites[i]
  cat("scraping url number", i, "\n")
  
  tryCatch(sites[i] %>% GET(., timeout(2)) %>% read_html("GB18030") -> webs[[i]],
           error = function(e) { webs[[i]] <<- "timed out!"})
  if((webs[[i]] == "timed out!")[1]) {i = i - 1}
} 

map_df(webs, Prase) -> towns
map(webs, prase_url) %>% 
  unlist() %>% 
  unique() -> town_sites

# 镇 ---------------------------------------------------------------------------
town_sites[str_length(town_sites) == 14] -> town_sites

webs <- list()
sites <- c()
i = 0

while (i < length(town_sites)) {
  i = i + 1
  str_c("http://www.stats.gov.cn/tjsj/tjbz/tjyqhdmhcxhfdm/", z, "/", str_extract(town_sites[i], "^\\d{2}"), "/", str_sub(town_sites[i], 3, -11), "/", str_sub(town_sites[i], 5, -9), "/", town_sites[i]) -> sites[i]
  cat("scraping url number", i, "\n")
  
  tryCatch(sites[i] %>% GET(., timeout(2)) %>% read_html("GB18030") -> webs[[i]],
           error = function(e) { webs[[i]] <<- "timed out!"})
  if((webs[[i]] == "timed out!")[1]) {i = i - 1}
} 

map_df(webs, prase) -> communities

# 汇总 -------------------------------------------------------------------------
p_load(tidyfst)
str_c("http://www.stats.gov.cn/tjsj/tjbz/tjyqhdmhcxhfdm/", z, "/index.html") %>% 
  read_html() %>% 
  html_elements("a") %>% 
  html_text() %>% 
  .[1:31] %>% 
  tibble(名称 = .,
           代码 = str_remove_all(province_sites, ".html")) -> provinces

communities %>% 
  mutate_dt(省级行政单位代码 = str_extract(统计用区划代码, "^\\d{2}")) %>% 
  left_join_dt(set_names(provinces, c("省级行政单位", "省级行政单位代码"))) %>% 
  mutate_dt(市级行政单位代码 = str_c(str_extract(统计用区划代码, "^\\d{4}"), "00000000")) %>% 
  left_join_dt(set_names(cities, c("市级行政单位代码", "市级行政单位"))) %>% 
  mutate_dt(区级行政单位代码 = str_c(str_extract(统计用区划代码, "^\\d{6}"), "000000")) %>% 
  left_join_dt(set_names(districts, c("区级行政单位代码", "区级行政单位"))) %>% 
  mutate_dt(镇级行政单位代码 = str_c(str_extract(统计用区划代码, "^\\d{9}"), "000")) %>% 
  left_join_dt(set_names(towns, c("镇级行政单位代码", "镇级行政单位"))) -> code

save(code, file = str_c("code", z, ".Rdata"))
rm(list = ls())
gc()
}

