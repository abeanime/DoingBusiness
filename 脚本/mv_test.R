



load("final_index.rda")
haven::read_dta("marginal_vertical_beta.dta") -> mvb
final_index %>% distinct(province,location) -> key

mvb %>% full_join(key,"province") %>% relocate(1,6) -> mvb
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


# mvb %>%
#   mutate(
#     across(
#       where(is.numeric),
#       ~ log(2)/.x
#     )
#   ) %>% 
#   mutate(
#     t_1 =  general_alpha_0-exclude_alpha_0,
#     t_2 =  general_alpha_1-exclude_alpha_1
#   ) %>% 
#   select(1,2,7,8)  -> half_life

mvb %>% 
  mutate(
    mar_1 = general_alpha_0-exclude_alpha_0,
    mar_2 = general_alpha_1-exclude_alpha_1
  ) %>% 
  select(1,2,7,8) %>% 
  pivot_longer(-c(1:2),names_sep = "_",names_to = c(".value","period")) %>% 
  mutate(
    period = factor(period,
                    levels = c("1","2"),
                    labels = c("2010~2014年","2015~2019年"))
  ) %>% 
  group_sort(location,province,mar) -> mvb


size <- function() {
  mvb %>% arrange(period,mar) %>% mutate(n = 1:n()) %>% pull(4) %>% .[[4]] -> b
  mvb %>% arrange(period,mar) %>% mutate(n = 1:n()) %>% 
    mutate(
      mar = case_when(
        n == 1 ~ mar-(b/2),
        n == 2 ~ mar-(b/2),
        n == 3 ~ mar-(b/2),
        T ~mar
      )
    ) %>% arrange(rev(period),mar) -> df
  
  
  df %>% pull(4) %>% .[[3]] -> a
  
  df %>% 
    mutate(
      mar = case_when(
        n == 31 ~ mar-(2.5*a),
        n == 32 ~ mar-(a),
        T ~mar
      )
    ) %>% select(-n) -> df
  
  
  mvb <<- df
  return(mvb)
  
} # end of "size"
size()

# save(mvb,file = "mvb.rda")
#####################################################################



mvb %>% 
  mutate(
    mar = if_else(province == "青海",mar/(5),mar)
  ) %>% 
  ggplot(aes(mar,province,fill = location))+
  geom_col()+scale_colour_ggthemr_d()+
  theme(
    text = element_text(family = cnfont),
    plot.margin = margin(20,20,20,20),
    legend.position = "bottom",
    axis.text.y.left = element_text(size = 8)
  )+
  scale_fill_manual(values = c(alpha("black",0.2),"darkgrey",alpha("black",0.6)))+
  labs(x = "",y = "",fill = "地区")+
  facet_wrap(.~period)




load("mvb.rda")
mvb %>% 
  ggplot(aes(marginal,province,fill = location))+
  geom_col()+scale_colour_ggthemr_d()+
  theme(
    text = element_text(family = cnfont),
    plot.margin = margin(20,20,20,20),
    legend.position = "bottom",
    axis.text.y.left = element_text(size = 8)
  )+
  scale_fill_manual(values = c(alpha("black",0.2),"darkgrey",alpha("black",0.6)))+
  labs(x = "",y = "",fill = "地区")+
  facet_wrap(.~period,scales = "free_x") 


mvb %>% pivot_wider(names_from = period,values_from = c(marginal,t)) %>% 
  set_names(c("省市","地区",
              "第二期边际垂直beta",
              "第一期边际垂直beta",
              "第二期半期收敛系数",
              "第一期半期收敛系数")) %>% 
  select(-地区) %>% 
  arrange(第一期边际垂直beta) %>% select(1,3,2,5,4) %>% 
  mutate(
    across(
      where(is.numeric),
      ~ round(.x,3)
    )
  ) %>% DT::datatable(width = "100%", height = "400px",
                      rownames = FALSE, # 去除表头
                      filter = "top", # 在顶部添加过滤控件
                      options = list(
                        autoWidth = TRUE,
                        pageLength = 5, # 每页显示的数量
                        initComplete = htmlwidgets::JS(
                          "function(settings, json) {",
                          "$(this.api().table().container()).css({'font-family': 'STSongti-SC-Regular'});",
                          "}")
                      )
  )

