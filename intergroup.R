# 製作 上下限 & 間距

group_inter <- function(DF){
  DF %>%
    group_by(批發市場, 品項) %>%
    summarise(
      minQ = min(`交易量(公斤)`, na.rm = T),
      maxQ = max(`交易量(公斤)`, na.rm = T),
      interQ = (maxQ - minQ)/10,
      
      minP = min(`交易價`, na.rm = T),
      maxP = max(`交易價`, na.rm = T),
      interP = (maxP - minP)/10,
      
      maxQ_change = max(`交易量漲跌`, na.rm = T),
      minQ_change = min(`交易量漲跌`, na.rm = T),
      interQ_change = (maxQ_change - minQ_change)/10,
      
      maxP_change = max(`交易價漲跌`, na.rm = T),
      minP_change = min(`交易價漲跌`, na.rm = T),
      interP_change = (maxP_change - minP_change)/10,
      
      .groups = "drop"
    ) %>%
    mutate(
      位數 = ceiling(interQ) %>% as.character(.) %>% {nchar(.)},
      interQ2 = ifelse(maxQ >= minQ+9*round(interQ/10^(位數-1))*10^(位數-1),
                       round(interQ/10^(位數-1),0)*10^(位數-1),
                       round(interQ/10^(位數-2),0)*10^(位數-2)),
      upQ = round((minQ+9*interQ2)/10^(位數-1),0)*10^(位數-1),
      lowQ = upQ-interQ2*8,
      
      interP2 = round(interP,0),
      lowP = round(minP+interP2,0),
      upP = round(lowP+interP2*8,0),
      
      interQ_change2 = round(interQ_change, 2),
      lowQ_change = round(minQ_change+interQ_change2, 2),
      upQ_change = round(lowQ_change+8*interQ_change2, 2),
      
      interP_change2 = round(interP_change, 2),
      lowP_change = round(minP_change+interP_change2, 2),
      upP_change = round(lowP_change+8*interP_change2, 2)
    ) %>%
    select(批發市場, 品項,
               minQ, maxQ, interQ, interQ2, upQ, lowQ,
               minP, maxP, interP, interP2, upP, lowP,
               minQ_change, maxQ_change, interQ_change, interQ_change2, 
               upQ_change, lowQ_change,
               minP_change, maxP_change, interP_change, interP_change2, 
               upP_change, lowP_change) %>%
    tidyr::gather(., key = "variable", value = "num",  -批發市場,-品項) %>%
    mutate(
      group = ifelse(str_detect(variable, "(Q$|Q2$)"),1, NA) %>%
        ifelse(str_detect(variable, "(P$|P2$)"),2, .) %>%
        ifelse(str_detect(variable, "Q_change"),3, .) %>%
        ifelse(str_detect(variable, "P_change"),4, .)
    ) %>%
    return()
}

