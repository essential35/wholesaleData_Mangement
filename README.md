# 資料合併
- 主要能做到：
  - 合併原始零碎的資料。（不同市場、不同品項的期間資料） 
  - 下載資料合併檔。

## 如何運作
請在R studio或終端機開啟r，並運行以下的code。

```
# install.packages(c("shiny", "shinythemes","dplyr","stringr","openxlsx","DT","tidyr","readxl","lubridate","tibble"))
library(shiny)
runGitHub("wholesaleData_Mangement", "essential35", ref = "main")
```
or
```
# install.packages(c("shiny", "shinythemes","dplyr","stringr","openxlsx","DT","tidyr","readxl","lubridate","tibble"))
shiny::runGitHub("wholesaleData_Mangement", "essential35", ref = "main")
```

> ⚠️ 如果一次丟入太多資料，請稍等片刻。
