

library(docxtractr)

read_docx("/Users/null/Desktop/new beta/beta_result.docx") %>% 
  docx_extract_tbl(1) -> df

df %>% 
  set_names(
    c(
      "时间段",
      "模型参数",
      "(1-1)全国",
      "(1-2)全国",
      "(2-1)东部地区",
      "(2-2)东部地区",
      "(3-1)中部地区",
      "(3-2)中部地区",
      "(4-1)西部地区",
      "(4-2)西部地区"
    )
  ) -> beta_reg


save(beta_reg,file = "beta_reg.rda")

