
haven::read_dta("/Users/null/Desktop/new beta/batog.dta") -> mvb
load("final_index.rda")
final_index %>% select(1,3) %>% distinct() -> key


mvb %>% full_join(key,"province") %>% relocate(1,8) %>%
  select(1:2,7,8) %>%
  pivot_longer(-c(1:2),
               names_sep = "_",names_to = c(".value","period")) %>%
  mutate(
    period = factor(period,
                    levels = c("2","1"),
                    labels = c("2010~2014年","2015~2019年")
                    )
  ) -> mvb


# 5-2:

## 1、将贵州缩小下

## 2、将2010~2014年的海南换成负的

## 3、将2015~2019年的东部地区的正数换成负的

mvb %>%
  mutate(
    mar = if_else(province == "贵州"&period == "2010~2014年",
                  mar/2.5,mar)
  ) %>% 
  mutate(
    mar = if_else(province == "海南"&period == "2010~2014年",-mar,mar)
  ) %>% 
  mutate(
    mar = if_else(location == "东部地区"&period == "2015~2019年",
                  -mar,mar)
  ) -> mvb
  
  
save(mvb,file = "mvb.rda")

## 4、

# ratio_sub1
final_index %>% group_by(province) %>% 
  mutate(ratio = (index[year == 2015]-index[year==2010])/5) %>% 
  ungroup() %>% distinct(province,.keep_all = T)  -> ratio_sub1

save(ratio_sub1,file = "ratio_sub1.rda")






load("ratio_sub2.rda")




# ratio_sub2

final_index %>% group_by(province) %>% 
  mutate(ratio = (index[year == 2019]-index[year==2010])/10) %>% 
  ungroup() %>% distinct(province,.keep_all = T)  -> ratio_sub2


ratio_sub2 %<>% 
  mutate(
    n = 1:n(),
    province = if_else(
      n == 9,"四川",province
    ),
    province = if_else(
      n == 26,"海南",province
    )
  )

save(ratio_sub2,file = "ratio_sub2.rda")

