load("final_index.rda")
load("index.rda")

##########################################################################
# 1 准备数据

## 1.1 准备index_tweak
map_dbl(index[4:8],~min(.x)) %>% sort(decreasing = F) %>% .[[1]] -> min 
map_dbl(index[4:8],~max(.x)) %>% sort(decreasing = F) %>% .[[1]] -> max

index %>% 
  mutate(
    across(
      4:8,
      ~ (.x-min)/(max-min)
    )
  ) -> index_tweak

rm(min,max)

## 1.2 准备 final_index_tweak

final_index %>% 
  mutate(index = (index-min(index))/(max(index)-min(index))) -> final_index_tweak

##########################################################################

# 子系统的可视化

## fig1：定子系统，重点对比不同地区随时间变化的情况
index_tweak %>% 
  group_by(year,location) %>% 
  summarise(
    across(
      where(is.numeric),mean
    )
  ) %>% ungroup() %>% 
  pivot_longer(-c(1:2),names_to = "子系统类别",values_to = "子系统得分") %>% 
  ggplot(aes(year,子系统得分,color = location))+
  geom_line(size = 1)+
  geom_point(aes(shape = location),show.legend = F,size = 2.5)+
  facet_wrap(.~子系统类别,scales = "free")+
  scale_color_manual(values = c("black","blue","green"))+
  scale_x_continuous(n.breaks = 6)+
  labs(color = "地区")+
  theme(
    legend.position = "bottom",text = element_text(family = cnfont),
    plot.margin = margin(20,20,20,20))

## fig2：定地区，看子系统随时间的得分情况

index_tweak %>% 
  group_by(year,location) %>% 
  summarise(
    across(
      where(is.numeric),mean
    )
  ) %>% ungroup() %>% 
  pivot_longer(-c(1:2),names_to = "子系统类别",values_to = "子系统得分") %>%
  ggplot(aes(year,子系统得分,color = 子系统类别))+
  geom_line(size = 1)+
  geom_point(aes(shape = 子系统类别),show.legend = F,size = 2.5)+
  facet_wrap(.~location,scales = "free")+
  scale_x_continuous(n.breaks = 6)+
  labs(color = "")+
  scale_color_manual(values = c("black","blue","green","red","yellow"))+
  theme(
    legend.position = "bottom",text = element_text(family = cnfont),
    plot.margin = margin(20,20,20,20))


##########################################################################

# 营商环境指标整体变化情况

## fig1：全国下的整体和子系统得分对比

final_index_tweak %>% group_by(year) %>%
  summarise(
    across(where(is.numeric),
           list(mean = mean)
    )) %>%
  ungroup() %>% set_names(c("year","均值")) -> df

index_tweak %>% group_by(year) %>%
  summarise(
    across(where(is.numeric),
           list(mean = mean)
    )) %>%
  ungroup() %>% full_join(df,"year") -> df

df %>% pivot_longer(-1) %>% 
  mutate(name = str_remove_all(name,"_mean")) %>% 
  mutate(
    name = factor(
      name,
      levels = c("开放化水平","政府对企业的保护","企业经营环境","企业法治环境","政府公共服务水平","均值"),
      labels = c("开放化水平","政府对企业的保护","企业经营环境","企业法治环境","政府公共服务水平","营商环境整体指标")
    )
  ) %>% dplyr::filter(name != "营商环境整体指标") -> df1


df %>% pivot_longer(-1) %>% 
  mutate(name = str_remove_all(name,"_mean")) %>% 
  mutate(
    name = factor(
      name,
      levels = c("开放化水平","政府对企业的保护","企业经营环境","企业法治环境","政府公共服务水平","均值"),
      labels = c("开放化水平","政府对企业的保护","企业经营环境","企业法治环境","政府公共服务水平","营商环境整体指标")
    )
  )  %>% pivot_wider(names_from = name,values_from = value) %>% 
  select(1,ncol(.)) -> df

ggplot()+
  geom_col(data = df,aes(year,营商环境整体指标),fill = alpha("grey"))+
  geom_line(data = df1,aes(year,value,color = name),size = 1)+
  geom_point(data = df1,
             aes(year,value,shape = name,color = name),
             size = 3,show.legend = F)+
  labs(color = "子系统",x = "年份")+
  scale_color_manual(
    values = c("black","blue","green","red","yellow")
  )+
  scale_shape_manual(
    values = c(1,15:18)
  )+
  scale_x_continuous(n.breaks = 10)+
  theme(
    legend.position = "bottom",text = element_text(family = cnfont),
    plot.margin = margin(20,20,20,20)
  )


## fig2：全国营商环境和三大地区的得分情况对比

final_index_tweak %>% 
  group_by(location,year) %>% 
  summarise(index = mean(index)) %>% 
  pivot_wider(names_from = location,values_from = index) %>% 
  mutate(
    全国 = (东部地区+中部地区+西部地区)/3
  ) %>% 
  pivot_longer(-1,names_to = "location",values_to = "index") %>% 
  ggplot(aes(year,index,color = location))+
  geom_line(size = 1)+
  labs(x = "年份",y = "得分",color = "地区")+
  scale_x_continuous(n.breaks = 10)+
  geom_point(aes(shape = location),show.legend = F,size = 3)+
  scale_color_manual(values = c("black","blue","green","red"))+
  scale_shape_manual(values = c(15:18))+
  theme(
    legend.position = "bottom",text = element_text(family = cnfont),
    plot.margin = margin(20,20,20,20)
  )


## fig3：2019年梯度效应展示

group_sort <- function(df,group_var,id_var,target_var) {
  group_var <- enquo(group_var)
  id_var <- enquo(id_var)
  target_var <- enquo(target_var)
  
  map_df(
    df %>% count(!!group_var) %>% pull(1) %>% rev(),
    ~ df %>% dplyr::filter(!!group_var == .x) %>% arrange(!!target_var)
  ) %>%
    mutate(!!id_var := fct_inorder(!!id_var,ordered = T))
} # end of "group_sort"


final_index_tweak %>% 
  dplyr::filter(year == 2019) %>% 
  group_sort(location,province,index) %>% 
  ggplot(aes(province,index,fill = location))+
  geom_col()+coord_flip()+
  labs(x = "",y = "得分",fill = "地区")+
  scale_fill_viridis_d()+
  scale_y_continuous(limits = c(0,1.25))+
  theme(
    text = element_text(family = cnfont),
    plot.margin = margin(20,20,20,20),
    legend.position = "bottom"
  )
