
# 变异系数法
##########################################################################
load("final_index.rda")


final_index %>%
  mutate(index = (index-min(index))/(max(index)-min(index))) %>% 
  group_by(year,location) %>%
  summarise(
    sigma = sqrt(mean((index - mean(index))^2))/mean(index)
  ) %>% ungroup() %>% pivot_wider(names_from = location,values_from = sigma) %>% 
  mutate(
    全国 = (东部地区+中部地区+西部地区)/3
  ) %>% 
  pivot_longer(-1) %>% 
  mutate(
    name = factor(name,levels = c("全国","东部地区","中部地区","西部地区"))
  ) %>% 
  ggplot(aes(year,value,color = name))+
  geom_point(aes(shape = name),size =3 )+
  geom_line(size = 1)+
  scale_colour_ggthemr_d()+
  scale_x_continuous(n.breaks = 10)+
  theme(
    text = element_text(family = cnfont),
    plot.margin = margin(20,20,20,20),
    legend.position = "bottom"
  )



##########################################################################
# 绝对beta  收敛

library(docxtractr)

read_docx("beta.docx") %>% 
  docx_extract_tbl(tbl_number = 1,header = T) %>% 
  slice(n=1) %>% as.character() -> name

name[[1]] <- "模型"

read_docx("beta.docx") %>% 
  docx_extract_tbl(tbl_number = 1,header = T) %>%
  slice(n=-1) %>% set_names(name) %>% 
  slice(n = -c(3:138)) %>% 
  mutate(
    模型 = case_when(
      模型 == "_cons" ~ "常数项",
      模型 == "N" ~ "观测值",
      模型 == "r2_o" ~ "整体拟合优度",
      模型 == "" ~ " ",
      T ~ "贝塔系数"
    )
  ) -> beta

beta %>% complete(模型 = c(beta %>% pull(1),"时间固定效应","地区固定效应")) %>% 
  slice(n = c(5,7)) -> add

beta %>% slice(-c(1:4)) -> other

beta %>% slice(1:4) %>% bind_rows(add) %>% bind_rows(other) %>% 
  mutate(
    across(
      everything(),
      ~ if_else(is.na(.x),"控制",.x)
    )
  )





##########################################################################
# moran

load("moranSC.rda")

moranSC %>% mutate(year = 2010:2019) %>% relocate(3) %>% 
  set_names(c("年份","Moran指数","p值")) %>%
  pivot_longer(-1,names_to = "类型") %>% 
  ggplot(aes(年份,value,color = 类型))+
  scale_colour_ggthemr_d()+
  scale_color_viridis_d()+
  geom_line(size = 1)+
  geom_point(aes(shape = 类型),size = 3)+
  scale_x_continuous(n.breaks = 10)+
  labs(y="",x="年份")+
  geom_hline(yintercept = 0.1,linetype = "dashed")+
  theme(
    text = element_text(family = cnfont),
    plot.margin = margin(20,20,20,20),
    legend.position = "bottom"
  )












auto %>% 
  ggplot(aes(weight,price))+
  theme(
    text = element_text(family = cnfont),
    plot.margin = margin(20,20,20,20),
    legend.position = "bottom",
    axis.title.y.left = element_text(angle = 0)
  )+
  geom_point()+
  labs(y = "价\n格",title = "this is a \ntitle")
