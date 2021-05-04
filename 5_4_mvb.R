#####################################################################

# 准备数据
haven::read_dta("/Users/null/Desktop/new beta/mvb.dta") -> mvb

mvb %>% select(1,2,7,8) %>% 
  pivot_longer(-c(1,2)) %>% 
  mutate(
    name = factor(name,
                  levels = c("mar_1","mar_2"),
                  labels = c("sub1","sub2")
                  )
  ) -> mvb


mvb %<>% rename(mar = value)


haven::read_dta("/Users/null/Desktop/new beta/mvb.dta") %>% 
  select(1:6) %>% 
  mutate(
    across(
      where(is.numeric),~log(2)/.x
    )
  ) %>% 
  mutate(
    t_1 = total_alpha_0-ex_alpha_0,
    t_2 = total_alpha_1-ex_alpha_1
  ) %>% select(1,2,7,8) -> half_life

mvb %>% 
  pivot_wider(names_from = name,values_from = mar) %>% 
  full_join(half_life,c("province","location")) %>% 
  set_names(
    c("省市","地区",
      "2010~2014年mvb",
      "2015~2019年mvb",
      "2010~2014年半期收敛系数",
      "2015~2019年半期收敛系数"
      )
  ) -> tab_mvb


save(tab_mvb,file = "tab_mvb.rda")







save(mvb,file = "mvb.rda")
#####################################################################

# 2010~2014年
load("mvb.rda")
mvb %>% dplyr::filter(name == "sub1") %>% 
  dplyr::filter(!province  %in% c("青海","贵州")) %>% 
  mutate(
    tag = (mar >= 0 )
  ) -> mvb_1

mvb_1 %>% dplyr::filter(tag == T) %>% pull(mar) %>% sum() -> pos
mvb_1 %>% dplyr::filter(tag == F) %>% pull(mar) %>% sum() -> neg
mvb_1 %>% 
  mutate(
    ratio = if_else(tag == T,mar/pos,-mar/neg)
  ) %>% 
  arrange(ratio) %>% mutate(province = fct_inorder(province)) %>% 
  ggplot(aes(province,ratio,fill = location))+
  geom_col(position = "dodge")+
  scale_colour_ggthemr_d()+
  scale_x_discrete(labels = function(x) str_wrap(x,2))+
  scale_y_continuous(n.breaks = 6,labels = percent)+
  theme(
    text = element_text(family = cnfont),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    axis.text.x.bottom = element_text(size = 7.5)
  )+
  scale_fill_manual(values = c(
    alpha("black",0.2),"darkgrey",alpha("black",0.6)
  ))+
  labs(x = "",y = "",fill = "地区",title = "2010~2014") -> p1

#####################################################################

# 2015~2019年

mvb %>% dplyr::filter(name == "sub2") %>% 
  dplyr::filter(!province  %in% c("青海","海南")) %>%
  mutate(
    tag = (mar >= 0 )
  ) -> mvb_2
mvb_2 %>% dplyr::filter(tag == T) %>% pull(mar) %>% sum() -> pos
mvb_2 %>% dplyr::filter(tag == F) %>% pull(mar) %>% sum() -> neg
mvb_2 %>% 
  mutate(
    ratio = if_else(tag == T,mar/pos,-mar/neg)
  ) %>% 
  arrange(ratio) %>% mutate(province = fct_inorder(province)) %>% 
  ggplot(aes(province,ratio,fill = location))+
  geom_col(position = "dodge")+
  scale_colour_ggthemr_d()+
  scale_x_discrete(labels = function(x) str_wrap(x,2))+
  scale_y_continuous(n.breaks = 6)+
  scale_y_continuous(n.breaks = 6,labels = percent)+
  theme(
    text = element_text(family = cnfont),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    axis.text.x.bottom = element_text(size = 7.5)
  )+
  scale_fill_manual(values = c(
    alpha("black",0.2),"darkgrey",alpha("black",0.6)
  ))+
  labs(x = "",y = "",fill = "地区",title = "2015~2019") -> p2

#####################################################################

# 合并两张图并存储

library(patchwork)

(p1+p2)+
  plot_layout(guides = 'collect')&theme(legend.position = "bottom") -> mvb_plot

save(mvb_plot,file = "mvb_plot.rda")

load("mvb_plot.rda")
mvb_plot
ggsave("~/Desktop/Rplot.png",dpi=300, family = cnfont,height = 10,width = 16)
#####################################################################

# 2010~2014年散点图

load("final_index_tweak.rda")

final_index_tweak %>% 
  dplyr::filter(!province  %in% c("青海","贵州")) %>% 
  dplyr::filter(year <= 2014) %>% group_by(province) %>% 
  mutate(
    ratio = (index[year == 2014] - index[year == 2010])/5
  ) %>% distinct(province,ratio,index) %>% ungroup() -> sc1


sc1 %>% pull(3) %>% mean() -> mean


sc1 %>% arrange(ratio) %>% mutate(province = fct_inorder(province)) %>% 
  ggplot(aes(province,ratio))+
  geom_point(aes(size = index),show.legend = F)+
  geom_hline(yintercept = mean,color = "black",linetype = "dotted")+
  scale_colour_ggthemr_d()+
  theme(
    text = element_text(family = cnfont),
    axis.text.x.bottom = element_text(size = 7.5),
    axis.title.y.left = element_text(angle = 0),
    plot.margin = margin(20,20,20,20)
  )+
  scale_x_discrete(labels = function(x) str_wrap(x,2))+
  scale_y_continuous(labels = function(x) percent(x),n.breaks = 6)+
  labs(x="",y=str_wrap("年平均增长率",2))






# 2015~2019年散点图

load("final_index_tweak.rda")

final_index_tweak %>% 
  dplyr::filter(!province  %in% c("青海","海南")) %>%
  dplyr::filter(year >= 2015) %>% group_by(province) %>% 
  mutate(
    ratio = (index[year == 2019] - index[year == 2015])/5
  ) %>% distinct(province,ratio,index) %>% ungroup() -> sc2


sc2 %>% pull(3) %>% mean() -> mean


sc2 %>% arrange(ratio) %>% mutate(province = fct_inorder(province)) %>% 
  ggplot(aes(province,ratio))+
  geom_point(aes(size = index),show.legend = F)+
  geom_hline(yintercept = mean,color = "black",linetype = "dotted")+
  scale_colour_ggthemr_d()+
  theme(
    text = element_text(family = cnfont),
    axis.text.x.bottom = element_text(size = 7.5),
    axis.title.y.left = element_text(angle = 0),
    plot.margin = margin(20,20,20,20)
  )+
  scale_x_discrete(labels = function(x) str_wrap(x,2))+
  scale_y_continuous(labels = function(x) percent(x),n.breaks = 6)+
  labs(x="",y=str_wrap("年平均增长率",2))



