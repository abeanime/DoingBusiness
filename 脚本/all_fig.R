
# fig-1

load("final_index.rda")
load("index.rda")
load("new_index")

##########################################################################
# 1 准备数据

## 1.1 准备index_tweak
map_dbl(index[4:8],~min(.x)) %>% sort(decreasing = F) %>% .[[1]] -> min
map_dbl(index[4:8],~max(.x)) %>% sort(decreasing = F) %>% .[[1]] -> max

index %>%
  mutate(
    across(
      4:8,
      ~ (.x - min) / (max - min)
    )
  ) -> index_tweak

rm(min,max)

## 1.2 准备 final_index_tweak

final_index %>%
  mutate(index = (index-min(index))/(max(index)-min(index))) -> final_index_tweak


#####################################################################
# fig1、定子系统，重点对比不同地区随时间变化的情况 ^========

index_tweak %>%
  group_by(year, location) %>%
  summarise(across(where(is.numeric), mean)) %>%
  ungroup() %>%
  pivot_longer(-c(1:2), names_to = "子系统类别", values_to = "子系统得分") %>%
  ggplot(aes(year, 子系统得分, shape = location)) +
  geom_line(size = 1, color = alpha("darkgrey", 0.5)) +
  geom_point(color = alpha("black", 0.5), size = 2.5) +
  facet_wrap(. ~ 子系统类别, scales = "free") +
  scale_colour_ggthemr_d() +
  scale_x_continuous(n.breaks = 6) +
  labs(shape = "地区", x = "年份", y = "") +
  theme(
    legend.position = "bottom",
    text = element_text(family = cnfont),
    plot.margin = margin(20, 20, 20, 20)
  ) -> fig1


save(fig1,file = "pic/fig1.rda")


#####################################################################
# fig2、 ^========


index_tweak %>%
  group_by(year, location) %>%
  summarise(across(
    where(is.numeric),
    ~ mean(.x)
  )) %>%
  ungroup() %>%
  pivot_longer(-c(1:2),
    names_to = "子系统类别",
    values_to = "子系统得分"
  ) -> df


df %>% dplyr::filter(location == "东部地区") %>% 
  ggplot(aes(year,子系统得分,shape = 子系统类别))+
  geom_line(size = 1,color = "darkgrey",alpha = 0.5)+
  geom_point(show.legend = T,
             size = 2.5,
             color = "black",alpha = 0.5)+
  scale_shape_manual(values = c(15:18,4))+
  scale_x_continuous(n.breaks = 6)+
  labs(shape = "子系统",x = "年份",y = str_wrap("子系统得分",2),
       title = "东部地区")+
  scale_colour_ggthemr_d()+
  theme(
    legend.position = "bottom",text = element_text(family = cnfont),
    plot.margin = margin(20,20,20,20),
    plot.title = element_text(hjust = 0.5),
    axis.title.y.left = element_text(angle = 0)
    ) -> east


df %>% dplyr::filter(location == "中部地区") %>% 
  ggplot(aes(year,子系统得分,shape = 子系统类别))+
  geom_line(size = 1,color = "darkgrey",alpha = 0.5)+
  geom_point(show.legend = T,
             size = 2.5,
             color = "black",alpha = 0.5)+
  scale_shape_manual(values = c(15:18,4))+
  scale_x_continuous(n.breaks = 6)+
  labs(shape = "子系统",x = "年份",y = str_wrap("子系统得分",2),
       title = "中部地区"
       )+
  scale_colour_ggthemr_d()+
  theme(
    legend.position = "bottom",text = element_text(family = cnfont),
    plot.margin = margin(20,20,20,20),
    plot.title = element_text(hjust = 0.5),
    axis.title.y.left = element_blank()
  )  -> mid


df %>% dplyr::filter(location == "西部地区") %>%
  ggplot(aes(year, 子系统得分, shape =  子系统类别)) +
  geom_line(size = 1,
            color = "darkgrey",
            alpha = 0.5) +
  geom_point(
    show.legend = T,
    size = 2.5,
    color = "black",
    alpha = 0.5
  ) +
  scale_shape_manual(values = c(15:18, 4)) +
  scale_x_continuous(n.breaks = 6) +
  labs(
    shape = "子系统",
    x = "年份",
    y = str_wrap("子系统得分", 2),
    title = "西部地区"
  ) +
  scale_colour_ggthemr_d() +
  theme(
    legend.position = "bottom",
    text = element_text(family = cnfont),
    plot.margin = margin(20, 20, 20, 20),
    plot.title = element_text(hjust = 0.5),
    axis.title.y.left = element_blank()
  )  -> west



east+mid+west+
  plot_layout(guides = 'collect')&theme(legend.position = "bottom") -> fig2

save(fig2,file = "pic/fig2.rda")

#####################################################################
# fig 3 


load("new_index.rda")
new_index %>% 
  group_by(location,year) %>% 
  summarise(index = mean(index)) %>% 
  pivot_wider(names_from = location,values_from = index) %>% 
  mutate(
    全国 = (东部地区+中部地区+西部地区)/3
  ) %>% 
  pivot_longer(-1,names_to = "location",values_to = "index") %>% 
  ggplot(aes(year,index,shape = location))+
  geom_line(size = 1,color = "darkgrey",alpha = 0.5)+
  geom_point(size = 3,color = "black",alpha = 0.5) +
  labs(x = "年份",y = str_wrap("营商环境指标得分",2),
       shape = "地区")+
  scale_x_continuous(n.breaks = 10)+
  scale_y_continuous(n.breaks = 6)+
  scale_shape_manual(values = c(15:18))+
  theme(
    legend.position = "bottom",
    text = element_text(family = cnfont),
    plot.margin = margin(20,20,20,20),
    axis.title.y.left = element_text(angle = 0)
  ) -> fig3


save(fig3,file = "pic/fig3.rda")

#####################################################################

load("new_index.rda")
load("raw.rda")
pacman::p_load(lubridate,sf,geojsonsf,geojsonio,jsonlite,haven)


new_index %>%  
  mutate(new = round(index,3)) -> new_index

prov_unify <- function(df,province=province) {
  df %>% pull({{province}}) -> target
  if_else(str_detect(target,"[内蒙古黑龙]"),
          str_replace_all(target,"(.{3}).*","\\1"),
          str_replace_all(target,"(.{2}).*","\\1")) -> target
  df %>% bind_cols(target) %>% rename("new" = ncol(.)) %>%
    relocate(ncol(.),.after = {{province}}) %>% select(-{{province}}) %>%
    rename("province" = "new")
} # end of "cn_province"

raw %>% prov_unify(省) %>% 
  sf_geojson() %>% 
  fromJSON(simplifyVector = FALSE) -> provmap
new_index %>% pull(4) %>% summary() -> qqq
new_index %>% pull(4) -> all_index


highchart() %>% # 使用JavaScript方式
  hc_chart(
    type = "map", # 指定何种geoms
    spacingBottom = 10,
    spacingTop = 10,
    spacingLeft = 10,
    spacingRight = 10,
    style = list(
      fontFamily = cnfont
    )
  ) %>% 
  hc_add_series_map(map = provmap,
                    df = new_index %>% dplyr::filter(year == 2019), 
                    joinBy = c("province", "province"), 
                    value = "index", 
                    name = "营商环境指标：",
                    borderWidth = 1,
                    borderColor = "white",
                    states = list(hover = list(color = '#bada55')),
                    dataLabels = list(enabled = FALSE)) %>% 
  hc_add_series(
    data = provmap,
    type = "mapline",geojson = T,
    color = "black",borderWidth = 0.5,show.legend = T
  ) %>% 
  hc_colorAxis(dataClasses = list(
    list(to = qqq[[1]], color = alpha("grey",0.4), name = "数据缺失"),
    list(from = qqq[[1]], to = qqq[[3]], color = alpha("black",0.1)),
    list(from = qqq[[3]], to = 4.7, color = alpha("darkgrey")),
    list(from = 4.7, to = max(all_index), color = alpha("black",0.5)))) %>% 
  hc_tooltip(headerFormat = "",
             pointFormat = "<b>{point.province}</b><br>营商环境指标：{point.new}",
             borderRadius = 5) %>% 
  hc_legend(align = 'center',
            # layout = 'horizontial',
            verticalAlign = "bottom",
            itemMarginTop= 3,
            itemMarginBottom= 3,
            valueDecimals = 3,
            # floating = TRUE,
            symbolRadius = 0,
            # x = 200, y = -20,
            symbolHeight = 14, 
            title = list(text = " ")) %>% 
  hc_title(
    text = str_c(2019,"年全国各地营商环境指标分布情况")
  )  -> fig4

save(fig4,file = "pic/fig4.rda")


#####################################################################

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

# load("final_index.rda")
# final_index %<>% mutate(index = (index-min(index))/(max(index)-min(index)))

load("new_index.rda")

new_index %>% 
  dplyr::filter(year == 2019) %>% 
  group_sort(location,province,index) %>% 
  ggplot(aes(province,index,fill = location))+
  geom_col()+
  coord_flip()+
  # geom_hline(yintercept = qqq[[2]],linetype = "dashed",color = "black")+
  # geom_hline(yintercept = qqq[[3]],linetype = "dashed",color = "black")+
  # geom_hline(yintercept = qqq[[4]],linetype = "dashed",color = "black")+
  labs(x = "",y = "得分",fill = "地区")+
  scale_fill_manual(values = c(alpha("black",0.2),"darkgrey",alpha("black",0.6)))+
  theme(
    text = element_text(family = cnfont),
    plot.margin = margin(20,20,20,20),
    legend.position = "bottom",
    axis.text.y.left = element_text(size = 8)
  ) -> fig5

save(fig5,file = "pic/fig5.rda")

#####################################################################

load("gini.rda")

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
  ggplot(aes(year,value,shape = 地区))+
  geom_point(aes(shape = 地区),size = 3,color = "black",alpha = 0.5)+
  geom_line(size = 0.8,color = "darkgrey",alpha = 0.5)+
  scale_colour_ggthemr_d()+
  labs(y=str_wrap("区域内基尼系数",2),x = "年份")+
  scale_shape_manual(values = c(15:18))+
  scale_x_continuous(n.breaks = 10)+
  theme(
    text = element_text(family = cnfont),
    plot.margin = margin(20,20,20,20),
    legend.position = "bottom",
    axis.title.y.left = element_text(angle = 0)
  ) -> fig6

save(fig6,file = "pic/fig6.rda")
#####################################################################



gini %>% select(1,6:8) %>% 
  pivot_longer(-1,names_to = "地区") %>% 
  ggplot(aes(year,value,shape = 地区))+
  geom_point(aes(shape = 地区),size = 3,color = "black",alpha = 0.5)+
  geom_line(size = 0.8,color = "darkgrey",alpha = 0.5)+
  labs(y=str_wrap("区域内基尼系数",2),x = "年份")+
  scale_x_continuous(n.breaks = 10)+
  scale_colour_ggthemr_d()+
  theme(
    text = element_text(family = cnfont),
    plot.margin = margin(20,20,20,20),
    legend.position = "bottom",
    axis.title.y.left = element_text(angle = 0)
  ) -> fig7

save(fig7,file = "pic/fig7.rda")

#####################################################################
gini %>% select(1,9:11) %>% 
  mutate(
    r_gw = gw/(gw+gnb+gt),
    r_gnb =gnb/(gw+gnb+gt),
    r_gt = gt/(gw+gnb+gt),
  ) %>% select(!starts_with("g")) %>% 
  set_names(
    c("year","区域内差异的贡献","区域间净值差异的贡献","超变密度的贡献")) %>% 
  pivot_longer(-1,names_to = "差异来源",values_to = "贡献率") %>% 
  ggplot(aes(year,贡献率,shape = 差异来源))+
  geom_line(size = 0.8,color = "darkgrey",alpha = 0.5)+
  geom_point(size = 3,color = "black",alpha = 0.5)+
  scale_colour_ggthemr_d()+
  labs(y=str_wrap("贡献率",2),x = "年份")+
  scale_x_continuous(n.breaks = 10)+
  scale_y_continuous(labels = percent)+
  theme(
    text = element_text(family = cnfont),
    plot.margin = margin(20,20,20,20),
    legend.position = "bottom",
    axis.title.y.left = element_text(angle = 0)
  ) -> fig8


save(fig8,file = "pic/fig8.rda")

#####################################################################

setwd("/Users/null/Desktop/Doing Business/prettydoc")
load("final_index.rda")
library(ggridges)

final_index %>% 
  mutate(index = (index-min(index))/(max(index)-min(index))) -> final_index_tweak

final_index_tweak %>% 
  ggplot(aes(index,factor(year)))+
  geom_density_ridges(rel_min_height = 0.05,alpha = 0.5,
                      color = "darkblue",size = 0.4,
                      scale = 1.5,show.legend = F,
                      quantile_lines = T,quantiles = c(0.025,0.5,0.975),
                      linetype = "dashed")+
  labs(x = "营商环境指标",y = "",title = "全国整体")+
  scale_color_viridis_d()+
  theme(
    text = element_text(family = cnfont),
    plot.margin = margin(20,20,20,20),
    plot.title = element_text(hjust = 0.5)
  ) -> fig9

save(fig9,file = "pic/fig9.rda")
#####################################################################


final_index_tweak %>% 
  ggplot(aes(index,factor(year)))+
  geom_density_ridges(rel_min_height = 0.03,alpha = 0.4,
                      color = "darkblue",size = 0.4,
                      scale = 1.9,show.legend = F,
                      quantile_lines = T,quantiles = c(0.025,0.5,0.975),
                      linetype = "dashed")+
  labs(x = "营商环境指标",y = "")+
  scale_color_viridis_d()+
  theme(
    text = element_text(family = cnfont),
    plot.margin = margin(40,40,40,40),
    plot.title = element_text(hjust = 0.5)
  ) +
  facet_wrap(.~location,scales = "free_x") -> fig10

save(fig10,file = "pic/fig10.rda")
#####################################################################

load("final_index.rda")

final_index %>%
  mutate(index = (index-min(index))/(max(index)-min(index))) %>% 
  group_by(year,location) %>% 
  summarise(
    sigma = sqrt(mean((index - mean(index))^2))/mean(index)
  ) %>% ungroup() %>% 
  pivot_wider(names_from = location,values_from = sigma)  -> df



final_index %>%
  mutate(index = (index-min(index))/(max(index)-min(index))) %>% 
  group_by(year) %>% 
  summarise(
    全国 = sqrt(mean((index - mean(index))^2))/mean(index)
  ) %>% ungroup() %>% full_join(df,"year") %>% 
  pivot_longer(-1) %>% 
  mutate(
    name = factor(name,levels = c("全国","东部地区","中部地区","西部地区"))
  ) %>% set_names(c("年份","地区","变异系数")) %>% 
  ggplot(aes(年份,变异系数,shape = 地区))+
  geom_point(size =3,color = "black",alpha = 0.5)+
  geom_line(size = 1,color = "darkgrey",alpha = 0.5)+
  scale_colour_ggthemr_d()+
  scale_shape_manual(values = c(15:19))+
  scale_x_continuous(n.breaks = 10)+
  labs(y = str_wrap("变异系数",2))+
  theme(
    text = element_text(family = cnfont),
    plot.margin = margin(20,20,20,20),
    legend.position = "bottom",
    axis.title.y.left = element_text(angle = 0)
  ) -> fig11

save(fig11,file = "pic/fig11.rda")
#####################################################################


load("mvb_plot.rda")
mvb_plot -> fig12


save(fig12,file = "pic/fig12.rda")

fig12



#####################################################################
load("moranSC.rda")

moranSC %>% mutate(year = 2010:2019) %>% relocate(3) %>% 
  set_names(c("年份","Moran’s I指数","p值")) %>% 
  ggplot()+
  geom_point(aes(年份,`Moran’s I指数`),size = 3,color = "black",alpha = 0.5)+
  geom_line(size = 1,aes(年份,`Moran’s I指数`),color = alpha("darkgrey",0.5))+
  geom_text(aes(label = round(`Moran’s I指数`,3),x =年份,y = `Moran’s I指数`),
            nudge_x = 0.05,family=cnfont,
            nudge_y = 0.009,color = alpha("black",0.5)
  )+
  geom_col(aes(年份,p值),fill = alpha("lightgrey",0.5))+
  scale_x_continuous(n.breaks = 10)+
  labs(y="",x="年份")+
  geom_hline(yintercept = 0.1,linetype = "dashed",color = alpha("black",0.5))+
  geom_hline(yintercept = 0.05,linetype = "dashed",color = alpha("black",0.5))+
  theme(
    text = element_text(family = cnfont),
    plot.margin = margin(20,20,20,20),
    legend.position = "bottom"
  ) -> fig13

save(fig13,file = "pic/fig13.rda")
#####################################################################

load("moran.rda")
moran %>%
  dplyr::filter(year %in% c(2010,2016,2019)) %>%
  mutate(face = paste(year,"年Moran’s I指数")) %>%
  mutate(
    LABEL = if_else(
      (year == 2019&location == "西部地区")&(std_index>0|w_std_index>0),
      province,""
    )
  ) %>%
  ggplot(aes(std_index,w_std_index,label = LABEL))+
  geom_point(size = 1.5,aes(shape = location),color = alpha("black",0.5))+
  geom_smooth(method = "lm",se = F,linetype = "solid",
              color = alpha("darkgrey",0.5))+
  scale_colour_ggthemr_d()+
  labs(x="",y="",shape = "地区")+
  geom_abline(intercept = 0, slope = 0,
              linetype = "dotted",color = alpha("darkgrey",0.5))+
  geom_vline(xintercept = 0,linetype = "dotted",
             color = alpha("darkgrey",0.5))+
  facet_grid(.~face)+
  theme(
    text = element_text(family = cnfont),
    plot.margin = margin(20,20,20,20),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  ) -> fig14

save(fig14,file = "pic/fig14.rda")