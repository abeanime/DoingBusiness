
setwd("/Users/null/Desktop/Doing Business/prettydoc")

haven::read_dta("demo.dta") -> df
load("key.rda")

df %>% full_join(key,"province") %>% relocate(1,2,ncol(.)) -> df

df %<>% relocate(1,2,3:10,政府分配经济资源比重,政府规模,非国有_工业发展,非国有_经济发展,非国有_劳动力投入,tax_主营业务税金及附加,tax_本年应交增值税,专利授申比例,专利纠纷年度案件总数,融资规模,每万人拥有的网点数,信贷资金分配,rd投入经费,创新人员投入,创新产出比例,职工平均工资,商业营业用房平均销售价格,购进价格指数,交通便利程度,人均邮电业务量,财政政策透明度,政府透明度,职务犯罪立案数,公职人员总数,律师人数,律所数量,受理行政复议案件,行政应诉案件,失业保险,养老保险,医疗保险,环境污染治理投资占比重,生活垃圾无害化处理率,工业固体废物综合利用率,每万人拥有公共汽车数量,人均城市道路面积,人均公园绿地面积)


df %>% mutate(
  财政政策透明度 = if_else(year >= 2017,100*财政政策透明度/670,财政政策透明度)
) -> df


df %>% 
  mutate(
    across(
      c(政府分配经济资源比重,tax_主营业务税金及附加,tax_本年应交增值税,信贷资金分配,职工平均工资,商业营业用房平均销售价格,购进价格指数),
      ~ 1/.x)
  ) -> df


##########################################################################
# xt_pca

xt_pca <- function(df,levels = 0.9,name) {
  df %>% arrange(province,year) %>% select(1:3) -> key
  
  df %>% arrange(province,year) %>% 
    select(-c(1,2,3)) %>% prcomp(scale = T) %>% summary() -> pca_summary
  
  pca_summary$importance %>% as_tibble() %>% slice(n=3) %>% 
    select(
      where(~ .x >= levels)
    ) %>% names() %>% .[[1]] %>% 
    str_extract("\\d{1,}") %>% as.integer() -> number
  
  pca_summary$importance %>% as_tibble() %>% 
    slice(n=2) %>% select(1:number) %>% 
    as.double() -> wei
  
  pca_summary$x %>% as_tibble() %>% select(1:number) -> val
  
  map2_dfc(val,wei,~ .x*.y) %>% rowSums()/sum(wei) -> pca_result
  
  key %>% bind_cols(pca_result) %>% 
    set_names(c("province","year","location",name %>% as.character()))
  
} # end of "xt_pca"


##########################################################################
# 子系统处理

df %>% select(1:10) -> p1
df %>% select(1,2,3,11:19) -> p2
df %>% select(1,2,3,20:30) -> p3
df %>% select(1,2,3,31:38) %>% 
  mutate(
    反贪力度 = 职务犯罪立案数/公职人员总数
  ) %>% 
  select(-c(职务犯罪立案数,公职人员总数)) -> p4
df %>% select(1,2,3,39:47) -> p5


map2_dfc(
  list(p1,p2,p3,p4,p5),
  c("开放化水平","政府对企业的保护","企业经营环境","企业法治环境","政府公共服务水平"),
  ~ xt_pca(.x,name = .y)
) %>% select(1,2,3,seq(4,20,by =4)) %>% names() %>% 
  str_remove_all("[0-9\\.{1,}]") -> name

map2_dfc(
  list(p1,p2,p3,p4,p5),
  c("开放化水平","政府对企业的保护","企业经营环境","企业法治环境","政府公共服务水平"),
  ~ xt_pca(.x,name = .y)
) %>% select(1,2,3,seq(4,20,by =4)) %>% 
  set_names(name) -> index
# save(index,file = "index.rda")


load("index.rda")

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


##########################################################################
# 分面图可视化

# fig 1

# fig2


##########################################################################

# 综合指标

df %>% 
  mutate(
    反贪力度 = 职务犯罪立案数/公职人员总数
  ) %>% 
  select(-c(职务犯罪立案数,公职人员总数)) %>% 
  xt_pca(name = "index") -> final_index

# save(final_index,file = "final_index.rda")





# highchart() %>% # 使用JavaScript方式
#   hc_chart(
#     spacingBottom = 40,
#     spacingTop = 60,
#     spacingLeft = 40,
#     spacingRight = 60,
#     style = list(fontFamily = cnfont)
#   ) %>%
#   hc_xAxis(
#     categories = unique(df$year)
#   ) %>% 
#   hc_yAxis_multiples(
#     list(
#       title = list(text = "整体营商环境指标"),opposite = F
#     ),
#     list(
#       title = list(text = "营商环境子系统得分"),opposite = T
#     )
#   ) %>% 
#   hc_add_series(
#     type = "column",
#     data = df$均值,
#     name = "全国营商环境指标均值",
#     yAxis = 0
#   ) %>% 
#   hc_add_series(
#     type = "line",
#     data = df$开放化水平_mean,
#     name = "开放化水平均值",
#     yAxis = 0
#   ) %>% 
#   hc_add_series(
#     type = "line",
#     data = df$政府对企业的保护_mean,
#     name = "政府对企业的保护均值",
#     yAxis = 0
#   ) %>% 
#   hc_add_series(
#     type = "line",
#     data = df$企业经营环境_mean,
#     name = "企业经营环境均值",
#     yAxis = 0
#   ) %>% 
#   hc_add_series(
#     type = "line",
#     data = df$企业法治环境_mean,
#     name = "企业法治环境均值",
#     yAxis = 0
#   ) %>% 
#   hc_add_series(
#     type = "line",
#     data = df$政府公共服务水平_mean,
#     name = "政府公共服务水平均值",
#     yAxis = 0
#   ) %>% 
#   hc_title(
#     text = "全国营商环境指数及其子系统变化趋势"
#   )

















