
library(docxtractr)

read_docx("b0.docx") %>% 
  docx_extract_tbl(1) -> tab1



read_docx("b1.docx") %>% 
  docx_extract_tbl(1) -> tab2

tab2
tab1 %>% slice(n=1) %>% as.character() -> name

name[[1]] <- "时间段"

tab1 %>% slice(n=-1) %>% set_names(name) -> tab1
tab2 %>% slice(n=-1) %>% set_names(name) -> tab2


tab1 %>% bind_rows(tab2) %>% 
  mutate(
    时间段 = if_else(时间段 == "","t",时间段)
  )





tab1 %>% slice(n=1) %>% as.character() %>% 
  str_remove_all("[x*]") %>% as.double() -> alpha_1

tab2 %>% slice(n=1) %>% as.character() %>% 
  str_remove_all("[x*]") %>% as.double() -> alpha_2

tibble(
  alpha_1 = alpha_1,
  alpha_2 = alpha_2
) %>% slice(n=-1) %>% 
  mutate(
    across(
      everything(),
      ~ -log(.x+1)
    )
  )














