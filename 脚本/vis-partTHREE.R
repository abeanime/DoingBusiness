

setwd("/Users/null/Desktop/Doing Business/prettydoc")
load("gini.rda")



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



##########################################################################

