


setwd("/Users/null/Desktop/Doing Business/prettydoc")
load("final_index.rda")

##########################################################################

# 准备 final_index_tweak

final_index %>% 
  mutate(index = (index-min(index))/(max(index)-min(index))) %>% 
  mutate(
    location = factor(
      location,
      levels = c("east","mid","west"),
      labels = c("东部地区","中部地区","西部地区")
    )
  ) -> final_index_tweak

##########################################################################
# 山脊图

## fig1：三个地区

library(ggridges)



single_location <- function(df,loca,sca,hei = 0.03) {
  df %>% 
    dplyr::filter(location == loca) %>% 
    ggplot(aes(index, factor(year)))+
    geom_density_ridges(rel_min_height = hei,alpha = 0.5,
                        color = "darkblue",size = 0.4,
                        scale = sca,show.legend = F,
                        quantile_lines = T,quantiles = c(0.025,0.5,0.975),
                        linetype = "dashed")+
    labs(x = "营商环境指标",y = "",title = loca)+
    theme(
      text = element_text(family = cnfont),
      plot.margin = margin(20,20,20,20),
      plot.title = element_text(hjust = 0.5)
    )
} # end of "single_location"


single_location(final_index_tweak,"西部地区",1.5,0.03) -> p1
single_location(final_index_tweak,"东部地区",1.5,0.04) -> p2
single_location(final_index_tweak,"中部地区",1.99,0.04) -> p3


final_index_tweak %>% 
  ggplot(aes(index,factor(year)))+
  geom_density_ridges(rel_min_height = 0.03,alpha = 0.4,
                      color = "darkblue",size = 0.4,
                      scale = 1.9,show.legend = F,
                      quantile_lines = T,quantiles = c(0.025,0.5,0.975),
                      linetype = "dashed")+
  labs(x = "营商环境指标",y = "")+
  theme(
    text = element_text(family = cnfont),
    plot.margin = margin(40,40,40,40),
    plot.title = element_text(hjust = 0.5)
  ) +
  facet_wrap(.~location,scales = "free_x")


p1+p2+p3




library(patchwork)

p1+p2+p3





## fig2：全国整体层面

final_index_tweak %>% 
  ggplot(aes(index,factor(year)))+
  geom_density_ridges(rel_min_height = 0.05,alpha = 0.5,
                      color = "darkblue",size = 0.4,
                      scale = 1.5,show.legend = F,
                      quantile_lines = T,quantiles = c(0.025,0.5,0.975),
                      linetype = "dashed")+
  labs(x = "营商环境指标",y = "",title = "全国整体")+
  theme(
    text = element_text(family = cnfont),
    plot.margin = margin(20,20,20,20),
    plot.title = element_text(hjust = 0.5)
  )



