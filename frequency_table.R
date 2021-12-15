# 製作次數分配表

freq_group <- function(c, mkt, DFgroup, TradeDF, var_num){
  var_name <- c("交易量(公斤)", "交易價", "交易量漲跌", "交易價漲跌")
  choice <- var_name[var_num]  # 被選擇的類別
  
  # 次數分配總表
  DailyTrade_Freq <- matrix(ncol = 6) %>% as.data.frame()
  colnames(DailyTrade_Freq) <- c("Month", "區間", "次數", "批發市場", "品項", "變數")
  

  inter_df2 <- DFgroup %>% 
    filter(group == var_num) %>%
    filter(str_detect(variable, "(2$|^up|^low)"))
    
  # get the correspond intervals
  #inter <- seq(inter_df2$num[3], inter_df2$num[2], by = inter_df2$num[1])
  lowLimit <- inter_df2$num[3] %>% as.numeric
  upLimit <- inter_df2$num[2] %>% as.numeric
  diff <- inter_df2$num[1] %>% as.numeric
  i <- 1
  inter <- c()
  
  while (lowLimit < upLimit) {
    inter[i] <- lowLimit
    lowLimit = lowLimit + diff
    i = i + 1
  }
    
  # get the each month 次數分配表 of the correspond market and category
  for (mon in c(1:12)) {
    df_freq <- TradeDF %>%
      filter(月份 == mon) %>%
      filter((品項 == c) & (批發市場 == mkt)) %>%
      {cut(.[[choice]], breaks = c(-Inf, inter, Inf))} %>%
      {table(., dnn = list("interval"))} %>%
      as.data.frame(.) %>%
      tibble::add_column(., 月份 = mon, .before = 1) %>%
      mutate(
        批發市場 = mkt,
        品項 = c,
        變數 = choice,
        low = str_extract(interval, "(?<=\\()[-[:digit:]+\\.e]+(?=\\,)"),
        low1 = str_extract(interval,"(?<=\\()[-\\.[:digit:]+]+(?=e)") %>%
          as.numeric, 
        low2 = str_extract(interval,"(?<=\\+)[:digit:]+(?=\\,)") %>% as.numeric,
        low_final = ifelse(str_detect(low, "[e+]"), low1*10^low2, as.numeric(low)),
        up = str_extract(interval, "(?<=\\,)[-[:digit:]+\\.e]+(?=\\])"),
        up1 = str_extract(interval, "(?<=\\,)[-\\.[:digit:]+]+(?=e)") %>%
          as.numeric, 
        up2 = str_extract(interval, "(?<=\\+)[:digit:]+(?=\\])") %>% as.numeric,
        up_final = ifelse(str_detect(up, "[e+]"), up1*10^up2, as.numeric(up)),
        interval = paste0(">=", low_final, ", ", "<", up_final) %>%
          ifelse(is.na(low_final), paste0("<", up_final),.) %>%
          ifelse(is.na(up_final), paste0(">=", low_final),.)
      ) %>%
      select(Month = 月份, 區間 = interval, 次數 = Freq, 批發市場, 品項, 變數)
    
    DailyTrade_Freq <- rbind(DailyTrade_Freq, df_freq)
  }
  
  return(DailyTrade_Freq[-1,])

}

