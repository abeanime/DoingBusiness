


#####################################################################


group_sort <- function(df,group_var,id_var,target_var) {
  group_var <- enquo(group_var)
  id_var <- enquo(id_var)
  target_var <- enquo(target_var)
  
  map_df(
    df %>% count(!!group_var) %>% pull(1),
    ~ df %>% dplyr::filter(!!group_var == .x) %>% arrange(!!target_var)
  ) %>%
    mutate(!!id_var := fct_inorder(!!id_var,ordered = T))
} # end of "group_sort"



#####################################################################

# 2010~2014年



load("mvb.rda")
load("ratio_sub1.rda")
load("final_index.rda")
library(patchwork)

## 柱状图

final_index %<>% 
  mutate(index = (index-min(index))/(max(index)-min(index)))

mvb %>% dplyr::filter(period == "2010~2014年") %>% 
  # dplyr::filter(province != "贵州") %>%
  arrange(mar) %>% pull(1) -> lev

mvb %>% dplyr::filter(period == "2010~2014年") %>% 
  dplyr::filter(province != "贵州") %>% 
  transmute(
    high_2 = mean(mar)+2*sd(mar),
    high_1 = mean(mar)+1*sd(mar),
    mean = mean(mar),
    low_1 = mean(mar)-sd(mar),
    low_2 = mean(mar)- 2*sd(mar)
  ) %>% slice(n=1) %>% as.double()  -> aux

mvb %>% 
  mutate(
    province = factor(province,levels = lev)
  ) %>% dplyr::filter(period == "2010~2014年") %>% 
  # dplyr::filter(province != "贵州") %>% 
  ggplot(aes(province,mar,fill = location))+
  # geom_chicklet(radius = unit(2,"pt"),position = "dodge")+
  geom_col(position = "dodge")+
  geom_hline(yintercept = aux[[1]],linetype = "dotted",color = "black")+
  geom_hline(yintercept = aux[[2]],linetype = "dashed",color = "black")+
  geom_hline(yintercept = aux[[4]],linetype = "dashed",color = "black")+
  geom_hline(yintercept = aux[[5]],linetype = "dotted",color = "black")+
  scale_colour_ggthemr_d()+
  scale_x_discrete(labels = function(x) str_wrap(x,2))+
  scale_y_continuous(n.breaks = 6)+
  theme(
    text = element_text(family = cnfont),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    axis.text.x.bottom = element_text(size = 8)
  )+
  scale_fill_manual(values = c(
    alpha("black",0.2),"darkgrey",alpha("black",0.6)
  ))+
  labs(x = "",y = "",fill = "地区") -> sub1_1


## 散点图

ratio_sub1 %>%   
  transmute(
    mean = mean(ratio),
    sd = sd(ratio),
    low = mean-sd,
    high = mean+sd
  ) %>% slice(n=1) %>% as.double() %>% .[-2] -> aux

ratio_sub1 %>% 
  arrange(ratio) %>% 
  mutate(province = fct_inorder(province)) %>% 
  # dplyr::filter(province == "广东")
  mutate(
    index = case_when(
      index<=0.2 ~"low",
      (index>0.2)&(index <= 0.4) ~"mid",
      (index>0.4)&(index<=0.6) ~"high",
      index>0.6 ~"extra"
    ),
    index = factor(
      index,
      levels = c("low","mid","high","extra")
    )
  ) %>% 
  ggplot()+
  geom_point(
    aes(province,ratio,size = index),
    show.legend = F,color = "darkgrey")+
  scale_size_manual(values = c(2.5,3.5,3.5,4.1))+
  geom_hline(yintercept = aux[[1]],color = "black",linetype = "dashed")+
  geom_hline(yintercept = aux[[2]],color = "black",linetype = "dotted")+
  geom_hline(yintercept = aux[[3]],color = "black",linetype = "dotted")+
  theme(
    text = element_text(family = cnfont),
    axis.text.x.bottom = element_text(size = 7.5),
    axis.title.y.left = element_text(angle = 0)
  )+
  scale_x_discrete(labels = function(x) str_wrap(x,2))+
  scale_y_continuous(labels = function(x) percent(x))+
  labs(x="",y=str_wrap("年平均增长率",2)) -> sub1_2


## 合并两者


(sub1_2+sub1_1)+
  plot_layout(guides = 'collect')&theme(legend.position = "bottom") 



#####################################################################
# 2015~2019年



load("mvb.rda")
load("final_index.rda")
load("ratio_sub2.rda")
library(patchwork)

## 柱状图

final_index %<>% 
  mutate(index = (index-min(index))/(max(index)-min(index)))

mvb %>% dplyr::filter(period == "2015~2019年") %>% 
  #dplyr::filter(province != "贵州") %>% 
  arrange(mar) %>% pull(1) -> lev

mvb %>% dplyr::filter(period == "2015~2019年") %>% 
  dplyr::filter(province != "贵州") %>% 
  transmute(
    high_2 = mean(mar)+2*sd(mar),
    high_1 = mean(mar)+1*sd(mar),
    mean = mean(mar),
    low_1 = mean(mar)-sd(mar),
    low_2 = mean(mar)- 2*sd(mar)
  ) %>% slice(n=1) %>% as.double()  -> aux

mvb %>% 
  mutate(
    province = factor(province,levels = lev)
  ) %>% dplyr::filter(period == "2015~2019年") %>% 
  #dplyr::filter(province != "贵州") %>% 
  ggplot(aes(province,mar,fill = location))+
  # geom_chicklet(radius = unit(2,"pt"),position = "dodge")+
  geom_col(position = "dodge")+
  geom_hline(yintercept = aux[[1]],linetype = "dotted",color = "black")+
  geom_hline(yintercept = aux[[2]],linetype = "dashed",color = "black")+
  geom_hline(yintercept = aux[[4]],linetype = "dashed",color = "black")+
  geom_hline(yintercept = aux[[5]],linetype = "dotted",color = "black")+
  scale_colour_ggthemr_d()+
  scale_x_discrete(labels = function(x) str_wrap(x,2))+
  scale_y_continuous(n.breaks = 5)+
  theme(
    text = element_text(family = cnfont),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    axis.text.x.bottom = element_text(size = 8)
  )+
  scale_fill_manual(values = c(
    alpha("black",0.2),"darkgrey",alpha("black",0.6)
  ))+
  labs(x = "",y = "",fill = "地区") -> sub2_1


## 散点图 

ratio_sub2 %>%   
  transmute(
    mean = mean(ratio),
    sd = sd(ratio),
    low = mean-sd,
    high = mean+sd
  ) %>% slice(n=1) %>% as.double() %>% .[-2] -> aux

ratio_sub2 %>% 
  arrange(ratio) %>% 
  mutate(province = fct_inorder(province)) %>% 
  # dplyr::filter(province == "广东")
  mutate(
    index = case_when(
      index<=0.2 ~"low",
      (index>0.2)&(index <= 0.4) ~"mid",
      (index>0.4)&(index<=0.6) ~"high",
      index>0.6 ~"extra"
    ),
    index = factor(
      index,
      levels = c("low","mid","high","extra")
    )
  ) %>% 
  ggplot()+
  geom_point(
    aes(province,ratio,size = index),
    show.legend = F,color = "darkgrey")+
  scale_size_manual(values = c(2,2.5,3.5,4.1))+
  geom_hline(yintercept = aux[[1]],color = "black",linetype = "dashed")+
  geom_hline(yintercept = aux[[2]],color = "black",linetype = "dotted")+
  geom_hline(yintercept = aux[[3]],color = "black",linetype = "dotted")+
  theme(
    text = element_text(family = cnfont),
    axis.text.x.bottom = element_text(size = 7.5),
    axis.title.y.left = element_text(angle = 0)
  )+
  scale_x_discrete(labels = function(x) str_wrap(x,2))+
  scale_y_continuous(labels = function(x) percent(x))+
  labs(x="",y=str_wrap("年平均增长率",2)) -> sub2_2


(sub2_2+sub2_1)+
  plot_layout(guides = 'collect')&theme(legend.position = "bottom") 


######################################################################
# 动态展示
mvb %>% 
  dplyr::filter(period %in% c("2010~2014年","2015~2019年")) %>% 
  select(1:4) %>% pivot_wider(names_from = period,values_from = mar) %>% 
  mutate(gap = `2015~2019年` - `2010~2014年`) %>% 
  group_sort(location,province,abs(gap)) %>% pull(1) -> lev

mvb %>% 
  dplyr::filter(period %in% c("2010~2014年","2015~2019年")) %>% 
  select(1:4) %>% pivot_wider(names_from = period,values_from = mar) %>% 
  mutate(
    gap = `2015~2019年` - `2010~2014年`,
    province = factor(province,levels = lev)
    ) %>% 
  ggplot(aes(province,gap,shape = location))+
  geom_point(size = 3,color = "darkgrey")+
  geom_segment(aes(
    x = province,xend = province,
    y = gap,yend = 0
  ),alpha = 0.5,color = "grey")+
  scale_colour_ggthemr_d()+
  scale_shape_manual(values = c(17,4,20))+
  theme(
    text = element_text(family = cnfont),
    plot.margin = margin(20,20,20,20),
    legend.position = "bottom"
  )+
  scale_x_discrete(labels = function(x) str_wrap(x,2))+
  scale_y_continuous(n.breaks = 6)+
  labs(x = "",y = "",shape = "地区") 



mvb %>% select(1:4) %>% 
  mutate(province = factor(province,levels = lev)) %>% 
  pivot_wider(names_from = period,values_from = mar) %>% 
  mutate(tag = (`2015~2019年`>= `2010~2014年`)) %>% 
  # dplyr::filter(province != "贵州") %>% 
  ggplot()+
  geom_point(aes(`2010~2014年`,province,shape = location),
             color = "lightgrey",size = 3)+
  geom_point(aes(`2015~2019年`,province,shape = location),
             color = "darkgrey" ,size = 3)+
  geom_segment(
    aes(x=`2010~2014年`,xend = `2015~2019年`,y = province,yend = province,
        linetype = tag),
    show.legend = F,
    arrow = arrow(length = unit(0.05, "inches"),type = "closed",angle = 25))+
  geom_vline(xintercept = 0,color = "black",linetype = "dotted")+
  scale_colour_ggthemr_d()+coord_flip()+
  theme(
    text = element_text(family = cnfont),
    plot.margin = margin(20,20,20,20),
    legend.position = "bottom"
  )+
  scale_y_discrete(labels = function(x) str_wrap(x,2))+
  scale_x_continuous(n.breaks = 10)+
  labs(x = "",y = "",shape = "地区") 





mvb_plot


