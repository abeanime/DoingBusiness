

haven::read_dta("/Users/null/Desktop/new beta/batog.dta") -> mvb
load("final_index.rda")
final_index %>% select(1,3) %>% distinct() -> key


mvb %>% full_join(key,"province") %>% relocate(1,8) %>% 
  select(1:2,7,8) %>% 
  pivot_longer(-c(1:2),names_sep = "_",names_to = c(".value","period")) %>% 
  mutate(
    period = factor(period,
                    levels = c("1","2"),
                    labels = c("2010~2014年","2015~2019年")
                    )
  ) -> mvb


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

# 动态展示图


library("mvb.rda")

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
mvb %>% 
  dplyr::filter(period %in% c("2010~2014年","2015~2019年")) %>% 
  select(1:4) %>% pivot_wider(names_from = period,values_from = mar) %>% 
  mutate(gap = `2015~2019年` - `2010~2014年`) %>% 
  arrange(abs(gap)) %>% select(1:2,5) %>% pull(1) -> lev


mvb %>% select(1:4) %>% 
  mutate(province = factor(province,levels = lev)) %>% 
  pivot_wider(names_from = period,values_from = mar) %>% 
  mutate(tag = (`2015~2019年`>= `2010~2014年`)) %>% 
  dplyr::filter(province != "贵州") %>% 
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
  labs(x = "",y = "",shape = "地区") -> p3



p3+scale_x_continuous(n.breaks = 10) -> p3



# 2010~2014年柱状图

mvb %>% dplyr::filter(period == "2010~2014年") %>% 
  group_sort(location,province,mar) %>% pull(1) -> lev

mvb %>% 
  mutate(
    province = factor(province,levels = lev)
  ) %>% dplyr::filter(period == "2010~2014年") %>% 
  ggplot(aes(province,mar,fill = location))+
  # geom_chicklet(radius = unit(2,"pt"),position = "dodge")+
  geom_col(position = "dodge")+
  scale_colour_ggthemr_d()+
  scale_x_discrete(labels = function(x) str_wrap(x,2))+
  theme(
    text = element_text(family = cnfont),
    plot.margin = margin(20,20,20,20),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5) 
  )+
  scale_fill_manual(values = c(
    alpha("black",0.2),"darkgrey",alpha("black",0.6)
  ))+
  labs(x = "",y = "",fill = "地区",title = "2010~2014年") -> p1

#####################################################################

# 2015~2019年柱状图

mvb %>% dplyr::filter(period == "2015~2019年") %>% 
  group_sort(location,province,mar) %>% pull(1) -> lev

mvb %>% 
  mutate(
    province = factor(province,levels = lev)
  ) %>% dplyr::filter(period == "2015~2019年") %>% 
  ggplot(aes(province,mar,fill = location))+
  # geom_chicklet(radius = unit(2,"pt"),position = "dodge")+
  geom_col(position = "dodge")+
  scale_colour_ggthemr_d()+
  scale_x_discrete(labels = function(x) str_wrap(x,2))+
  theme(
    text = element_text(family = cnfont),
    plot.margin = margin(20,20,20,20),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5) 
  )+
  scale_fill_manual(values = c(
    alpha("black",0.2),"darkgrey",alpha("black",0.6)
  ))+
  labs(x = "",y = "",fill = "地区",title = "2015~2019年") -> p2






