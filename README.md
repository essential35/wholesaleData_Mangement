# 農產品交易批發市場資料整理
- 這是一個協助整理農產品交易市場資料的App。
- 主要能做到：
  - 輸入資料檢視：主要是協助查看是否有品項在某些交易市場沒有進行買賣。
  - 合併原始零碎的資料。（不同市場、不同品項的期間資料） 
  - 列出 1996/01/01 ～ 2020/12/31 每天的日期，並與原始資料合併檔合併。
  - 生成不同市場的各個品項，在不同月份的次數分配表。每月次數分配表依照 **交易量、交易價、與前一天交易量的漲跌幅、與前一天交易價的漲跌幅** 分10組。
  - 可下載上述所有結果。

## 如何運作
請在R studio或終端機開啟r，並運行以下的code。

```
# install.packages(c("shiny", "shinythemes","dplyr","stringr","openxlsx","DT","tidyr"))
library(shiny)
runGitHub("wholesaleData_Mangement", "essential35", ref = "main")
```
or
```
# install.packages(c("shiny", "shinythemes","dplyr","stringr","openxlsx","DT","tidyr"))
shiny::runGitHub("wholesaleData_Mangement", "essential35", ref = "main")
```

> ⚠️ 如果一次丟入太多資料，請稍等片刻。
