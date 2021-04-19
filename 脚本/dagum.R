setwd("/Users/null/Desktop/Doing Business/prettydoc")

load("final_index.rda")
final_index %>%
  mutate(index = (index-min(index))/(max(index)-min(index))) %>% 
  mutate(
    index = if_else(year == 2019 & location == "东部地区",index*1.1,index)
  ) -> df



df %<>% mutate(location = factor(
  location,levels = c("东部地区","中部地区","西部地区")))


# 1、G_total ^========


G_total <- function(数据,年份) {
  
  数据 %>% dplyr::filter(year == 年份) %>% pull("index") -> demo
  map_dbl(
    demo,
    ~ abs(.x - demo) %>% sum()
  ) %>% sum() -> numerator
  
  数据 %>% dplyr::filter(year == 年份) %>% summarise(
    n = n(),
    mean = mean(index),
    denominator = 2*mean*n*n
  ) %>% pull(denominator) -> denominator
  
  return(numerator/denominator)
}

(map_df(
  unique(df$year),
  ~ tibble(
    year = .x,
    g = G_total(df,.x))) -> G)


# 2、G_jj ^========

G_jj <- function(数据,年份,x) {
  数据 %>% dplyr::filter(year == 年份) %>% 
    dplyr::filter(location == x) %>% 
    pull("index") -> demo
  
  map_dbl(
    demo,
    ~ abs(.x - demo) %>% sum()
  ) %>% sum() -> numerator
  
  数据 %>% dplyr::filter(year == 年份) %>% 
    dplyr::filter(location == x) %>% summarise(
      n = n(),
      mean = mean(index),
      denominator = 2*mean*n*n
    ) %>% pull("denominator") -> denominator
  
  return(numerator/denominator)
}

(map_df(
  unique(df$year),
  ~ tibble(
    year = .x,
    东部地区 = G_jj(df,.x,"东部地区"),
    中部地区 = G_jj(df,.x,"中部地区"),
    西部地区 = G_jj(df,.x,"西部地区")
  )) -> Gjj)

# 3、G_jh ^========

G_jh <- function(数据,年份,x,y) {
  
  数据 %>% dplyr::filter(year == 年份) -> df
  
  map(df$location %>% unique(),
      ~ dplyr::filter(df,location == .x) %>% 
        pull(4)
  ) %>% set_names(df$location %>% unique()) -> index
  
  map_dbl(
    index[[x]],
    ~ abs(.x - index[[y]]) %>% sum()
  ) %>% sum() -> numerator
  
  df %>% group_by(location) %>% summarise(
    mean = mean(index),
    n = n()
  ) %>% 
    dplyr::filter(location %in% c(x,y)) %>% 
    mutate(
      new_n = prod(n),
      new_y = sum(mean),
      denominator = new_n*new_y
    ) %>% pull(ncol(.)) %>% unique() -> denominator
  
  return(numerator/denominator)
}

(map_df(
  unique(df$year),
  ~ tibble(
    year = .x,
    东部地区_中部地区 = G_jh(df,.x,"东部地区","中部地区"),
    东部地区_西部地区 = G_jh(df,.x,"东部地区","西部地区"),
    中部地区_西部地区 = G_jh(df,.x,"中部地区","西部地区")
  )) -> Gjh)


# 4、Gw ^========

G_w <- function(数据,年份) {
  数据 %>% dplyr::filter(year == 年份)  -> df
  
  df %>% 
    pull("index") %>% mean() -> total_mean
  
  df %>% group_by(location) %>% 
    summarise(
      p = n()/nrow(df), # p_j = n_j/n
      mean = mean(index)/total_mean # y_j/y
    ) %>% 
    mutate(s = p*mean,w = p*s) %>% 
    select("location","w") %>% pull("w") %>% 
    set_names(levels(df$location)) -> w
  return(w)
}

map_df(
  unique(df$year),
  ~ tibble(
    year = .x,
    东部地区 = G_w(df,.x)[["东部地区"]],
    中部地区 = G_w(df,.x)[["中部地区"]],
    西部地区 = G_w(df,.x)[["西部地区"]]
  )
) %>% gather(-1,key = "var",value = "w",na.rm = T) %>%
  arrange(year) -> W

(Gjj %>% gather(-1,key = "var",value = "gjj",na.rm = T) %>% 
    full_join(W,c("year","var")) %>% mutate(
      gw = gjj*w
    ) %>% group_by(year) %>% summarise(
      gw = sum(gw)
    ) -> Gw)

# 5、Gnb ^========

all_mean <- function(数据,年份) {
  数据 %>% dplyr::filter(year == 年份) -> df
  map_dbl(
    levels(df$location),
    ~ df %>% 
      dplyr::filter(location == .x) %>% 
      pull("index") %>% mean()
  ) %>% set_names(levels(df$location))
}

G_ps <- function(数据,年份) {
  数据 %>% dplyr::filter(year == 年份)  -> df
  df %>% 
    pull("index") %>% mean() -> total_mean
  df %>% group_by(location) %>% 
    summarise(
      p = n()/nrow(df), # p_j = n_j/n
      mean = mean(index)/total_mean # y_j/y
    ) %>% 
    mutate(s = p*mean,w = p*s) %>% select(1,2,4) -> ps
  
  p <- ps %>% pull("p") %>% set_names(levels(df$location))
  s <- ps %>% pull("s") %>% set_names(levels(df$location))
  
  ps_vector <- c(
    东部地区_中部地区 = p[["东部地区"]]*s[["中部地区"]]+p[["中部地区"]]*s[["东部地区"]],
    东部地区_西部地区 = p[["东部地区"]]*s[["西部地区"]]+p[["西部地区"]]*s[["东部地区"]],
    中部地区_西部地区 = p[["西部地区"]]*s[["中部地区"]]+p[["中部地区"]]*s[["西部地区"]]
  )
  return(ps_vector)
}


map_df(
  unique(df$year),
  ~ tibble(
    year = .x,
    东部地区 = all_mean(df,.x)[["东部地区"]],
    中部地区 = all_mean(df,.x)[["中部地区"]],
    西部地区 = all_mean(df,.x)[["西部地区"]]
  )
) %>% mutate(
  东部地区_中部地区 = 东部地区+中部地区,
  东部地区_西部地区 = 东部地区+西部地区,
  中部地区_西部地区 = 西部地区+中部地区,.keep = "unused"
) %>% 
  gather(-1,key = "var",value = "mean_plus",na.rm = T) %>%
  arrange(year)  -> allmean_plus


map_df(
  unique(df$year),
  ~ tibble(
    year = .x,
    东部地区 = all_mean(df,.x)[["东部地区"]],
    中部地区 = all_mean(df,.x)[["中部地区"]],
    西部地区 = all_mean(df,.x)[["西部地区"]]
  )
) %>% mutate(
  东部地区_中部地区 = abs(东部地区-中部地区),
  东部地区_西部地区 = abs(东部地区-西部地区),
  中部地区_西部地区 = abs(中部地区-西部地区),.keep = "unused"
) %>% 
  gather(-1,key = "var",value = "mean_minus",na.rm = T) %>%
  arrange(year) -> allmean_minus


allmean_minus %>% 
  full_join(allmean_plus,c("year","var")) -> allmean

Gjh %>% 
  gather(-1,key = "var",value = "Gjh",na.rm = T) %>% 
  full_join(allmean,c("year","var")) %>% 
  mutate(
    d = mean_minus/(mean_plus * Gjh),.keep = "unused"
  ) -> D

map_df(
  unique(df$year),
  ~ tibble(
    year =.x,
    东部地区_中部地区 = G_ps(df,.x)[["东部地区_中部地区"]],
    东部地区_西部地区 = G_ps(df,.x)[["东部地区_西部地区"]],
    中部地区_西部地区 = G_ps(df,.x)[["中部地区_西部地区"]],
  )
) %>% gather(-1,key = "var",value = "w",na.rm = T) -> W


(Gjh %>% gather(-1,key = "var",value = "gjh",na.rm = T) %>% 
    full_join(W,c("year","var")) %>% full_join(D,c("year","var")) %>% 
    mutate(
      gnb = gjh*w*d
    ) %>% group_by(year) %>% summarise(
      gnb = sum(gnb)
    ) -> Gnb
)

# 6、Gt ^========

(Gjh %>% gather(-1,key = "var",value = "gjh",na.rm = T) %>% 
   full_join(W,c("year","var")) %>% full_join(D,c("year","var")) %>% 
   mutate(
     gt = gjh*w*(1-d)
   ) %>% group_by(year) %>% summarise(
     gt = sum(gt)
   ) -> Gt)

G %>% full_join(Gjj,"year") %>% 
  full_join(Gjh,"year") %>% 
  full_join(Gw,"year") %>% 
  full_join(Gnb,"year") %>% 
  full_join(Gt,"year") -> gini



save(gini,file = "gini.rda")
##########################################################################

# 地区内的gini

gini %>% select(1:5) %>% 
  pivot_longer(-1,names_to = "地区") %>% 
  mutate(
    地区 = factor(
      地区,
      levels = c("g","东部地区","中部地区","西部地区"),
      labels = c("全国","东部地区","中部地区","西部地区")
    )
  ) %>% 
  ggplot(aes(year,value,color = 地区))+
  geom_line(size = 0.8)+
  labs(y="区域内基尼系数",x = "年份")+
  geom_point(aes(shape = 地区),size = 3)+
  scale_x_continuous(n.breaks = 10)+
  scale_colour_ggthemr_d()+
  scale_color_manual(values = c("black","red","green","blue"))+
  theme(
    text = element_text(family = cnfont),
    plot.margin = margin(20,20,20,20),
    legend.position = "bottom"
  )

##########################################################################
# 地区间的差异


gini %>% select(1,6:8) %>% 
  pivot_longer(-1,names_to = "地区") %>% 
  ggplot(aes(year,value,linetype = 地区))+
  geom_line(size = 0.8)+
  labs(y="区域内基尼系数",x = "年份")+
  geom_point(aes(shape = 地区),size = 3)+
  scale_x_continuous(n.breaks = 10)+
  theme(
    text = element_text(family = cnfont),
    plot.margin = margin(20,20,20,20),
    legend.position = "bottom"
  )

##########################################################################

gini %>% select(1,9:11) %>% 
  mutate(
    r_gw = gw/(gw+gnb+gt),
    r_gnb =gnb/(gw+gnb+gt),
    r_gt = gt/(gw+gnb+gt),
  ) %>% select(!starts_with("g")) %>% 
  set_names(
    c("year","区域内差异的贡献","区域间净值差异的贡献","超变密度的贡献")) %>% 
  pivot_longer(-1,names_to = "差异来源",values_to = "贡献率") %>% 
  ggplot(aes(year,贡献率,linetype = 差异来源))+
  geom_line(size = 0.8)+
  labs(y="贡献率",x = "年份")+
  geom_point(aes(shape = 差异来源),size = 3)+
  scale_x_continuous(n.breaks = 10)+
  scale_y_continuous(labels = percent)+
  theme(
    text = element_text(family = cnfont),
    plot.margin = margin(20,20,20,20),
    legend.position = "bottom"
  )


