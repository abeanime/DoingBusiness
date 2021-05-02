



























load("ratio_sub1.rda")

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
  scale_size_manual(values = c(1,2,3,3.5))+
  geom_hline(yintercept = aux[[1]],color = "black",linetype = "dashed")+
  geom_hline(yintercept = aux[[2]],color = "black",linetype = "dotted")+
  geom_hline(yintercept = aux[[3]],color = "black",linetype = "dotted")+
  theme(
    text = element_text(family = cnfont),
    axis.text.x.bottom = element_text(size = 7.5),
    axis.title.y.left = element_text(angle = 0)
  )+
  scale_x_discrete(labels = function(x) str_wrap(x,2))+
  labs(x="",y=str_wrap("营商环境指标增长率",2))
