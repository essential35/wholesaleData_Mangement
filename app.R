#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(dplyr)
library(stringr)
library(openxlsx)

options(shiny.maxRequestSize=100*1024^2)

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("cosmo"),
    
    # Application title
    titlePanel("合併批發市場資料 & 次數分配表"), 
    br(),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("startyr", "資料起始年", 1996, min=1996, max=Inf),
            numericInput("endyr","資料結束年",1996, min=1996, max=Inf),
            fileInput("files", "上傳批發市場資料", multiple = T,
                      accept = c("xls/xlsx", ".xlsx", ".xls")),
            
            div(
                h4("檔案上傳限制："),
                p("1. 檔案需為「.xls」或「.xlsx」檔。"),
                p("2. 檔案名稱格式為「(蔬菜名字)_(市場簡稱).xls」，例如「小白菜_台中市.xlsx」。")
            ),

            tags$hr(),
            
            h3("Download"),
            # 下載合併資料
            h5("- 合併資料下載："),
            # download data button
            downloadButton("dl", "Combine Data")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("原始資料", 
                         DT::dataTableOutput(outputId = "originData")),
                tabPanel("所有日資料",
                         DT::dataTableOutput(outputId = "allDayData")),
                tabPanel("無原始資料的日資料",
                         DT::dataTableOutput(outputId = "nonData"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output){
    
    everyday <- reactive({
      # create a dataframe include all dates 
      DayTrade <- data.frame(
        day = rep(1:31, times = length(c(input$startyr:input$endyr))*12),
        month = rep(1:12, each = 31),
        year = rep(c(input$startyr:input$endyr), each = 31*12)
      ) %>%
        .[-c(which(.$month %in% c(4,6,9,11) & (.$day > 30)),
             which((.$month == 2) & (.$year %% 4 == 0) & (.$day > 29)),
             which((.$month == 2) & (.$year %% 4 != 0) & (.$day > 28))),] %>%
        mutate(Date = str_c(year, "/", month, "/", day) %>% as.Date(.)) %>%
        .[order(.$Date),] %>%
        select(Date, month) %>%
        rename(., 月份 = month) %>%
        mutate(Date = as.character(Date) %>% str_replace_all(., "-","/"),
               月份 = str_extract(Date,"(?<=/)[:digit:]+(?=/)"))
      
      DayTrade
    })
    
    # data combine
    df <- reactive({
        req(input$files)
      
        # input all data
        all_pq <- matrix(ncol = 8) %>% as.data.frame() %>%
          `colnames<-`(c("Date","Day","月份","交易量(公斤)","交易量(公噸)",
                         "交易價","批發市場","品項"))
        
        for(i in 1:length(input$files[,1])){
            all_pq <- readxl::read_excel(input$files[[i, 'datapath']],
                                         col_names = c("交易日期","平均價",
                                                       "交易量(公斤)")) %>%
                .[-c(1:3),] %>%
                mutate(
                    平均價 = as.numeric(平均價),
                    `交易量(公斤)` = as.numeric(`交易量(公斤)`),
                    `交易量(公噸)` = `交易量(公斤)`/1000
                ) %>%
                tibble::add_column(
                    年 = str_extract(.$交易日期,"^[:digit:]+(?=/)") %>% 
                        as.numeric %>% {.+1911},
                    月份 = str_extract(.$交易日期,"(?<=/)[:digit:]+(?=/)"),
                    日 = str_extract(.$交易日期,"(?<=/)[:digit:]+$"),
                    .before = 2
                ) %>%
                tibble::add_column(
                    Date = str_c(.$年,"/",.$月份,"/",.$日)
                ) %>%
                select(Date, 月份, `交易量(公斤)`, `交易量(公噸)`, 交易價 = 平均價) %>%
                left_join(everyday(),., by=c("Date","月份")) %>%
                mutate(
                    批發市場 = str_extract(input$files[[i, "name"]],
                                       "(?<=_)[\u4E00-\u9FFF]+(?=.xls)"),
                    品項 = str_extract(input$files[[i, "name"]],
                                     "^[\u4E00-\u9FFF]+(?=_)"),
                    DayLabel = as.Date(Date) %>% lubridate::wday(., label=TRUE)
                ) %>%
                left_join(., 
                          data.frame(DayLabel = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"),
                                     Day = c("週日","週一","週二","週三","週四","週五","週六")),
                          by="DayLabel") %>%
                select(Date, Day, 月份, `交易量(公斤)`, `交易量(公噸)`, 交易價, 批發市場, 品項) %>%
                rbind(all_pq, .)
        }
        
        all_pq <- all_pq %>%
            group_by(批發市場, 品項) %>%
            mutate(
                `前一天交易量(公斤)` = lag(`交易量(公斤)`),
                `前一天交易價` = lag(`交易價`),
                `交易量差(公斤)` = `交易量(公斤)` - `前一天交易量(公斤)`,
                `交易價差` = `交易價` - `前一天交易價`,
                `交易量漲跌` = `交易量差(公斤)`/`前一天交易量(公斤)`,
               `交易價漲跌` = `交易價差`/`前一天交易價`,
                月份 = as.numeric(月份),
                `有無原始資料` = ifelse(is.na(`交易量(公斤)`),FALSE,TRUE)
            ) 
        
        all_pq
    })
    
    # 無原始資料的日資料
    #nondf <- reactive({
      
      # 無原始資料
    #  markets <- df()$批發市場 %>% as.factor %>% levels
    #  category <- df()$品項 %>% as.factor %>% levels
    #  
    #  nonData <- matrix(ncol = 15) %>% as.data.frame() %>%
    #    `colnames<-`(c("Date","Day","月份","交易量(公斤)","交易量(公噸)",
    #                   "交易價","批發市場","品項", "前一天交易量(公斤)",
    #                   "前一天交易價","交易量差(公斤)","交易價差",
    #                   "交易量漲跌","交易價漲跌","有無原始資料"))
    #  for (m in markets) {
    #      for (c in category) {
    #          check <- df() %>% filter((品項 == c) & (批發市場 == m))
    #        
    #          if(sum(!is.na(check$`交易量(公噸)`)) == 0){
    #            nonData <- rbind(check, nonData)
    #          }else{
    #            next()
    #          }
    #     }
    #  }
    #  
        #nonData <- nonData[-1,]
    #    nonData
    #})
    
    # final all date data
    finalAllDayTrade <- reactive({
        df() %>% .[order(.$批發市場),]
    })
    
    # 無原始資料的日資料
    #output$nonData <- DT::renderDataTable({
    #    nondf() 
    #})
    
    # 原始資料
    output$originData <- DT::renderDataTable({
        finalAllDayTrade() %>% .[which(!is.na(.$`交易量(公斤)`)),]
    })
    
    # 所有日資料
    output$allDayData <- DT::renderDataTable({
        finalAllDayTrade()
    })
    
    
    # download data
    output$dl <- downloadHandler(
        filename = function() {
            "Combine_Data.xlsx"
        },
        content = function(file) {
            all_pq2 <- list(
                `原始檔_台北一～西螺鎮` = finalAllDayTrade() %>% 
                    .[which(!is.na(.$`交易量(公斤)`)),] %>% .[order(.$批發市場),] %>%
                    filter(批發市場 %in% c("台北一","台北二","板橋區","三重區","宜蘭市",
                                       "桃農","台中市","豐原區","永靖鄉","溪湖鎮","南投市",
                                       "西螺鎮")),
                `原始檔_高雄市～花蓮市` = finalAllDayTrade() %>%
                    .[which(!is.na(.$`交易量(公斤)`)),] %>% .[order(.$批發市場),] %>%
                    filter(!(批發市場 %in% c("台北一","台北二","板橋區","三重區","宜蘭市",
                                       "桃農","台中市","豐原區","永靖鄉","溪湖鎮","南投市",
                                       "西螺鎮"))),
                `日交易_含漲跌幅_台北一～西螺鎮` = finalAllDayTrade() %>%
                    filter(批發市場 %in% c("台北一","台北二","板橋區","三重區","宜蘭市",
                                       "桃農","台中市","豐原區","永靖鄉","溪湖鎮","南投市",
                                       "西螺鎮")),
                `日交易_含漲跌幅_高雄市～花蓮市` = finalAllDayTrade() %>%
                  filter(!(批發市場 %in% c("台北一","台北二","板橋區","三重區","宜蘭市",
                                     "桃農","台中市","豐原區","永靖鄉","溪湖鎮","南投市",
                                     "西螺鎮")))
            )
            
            write.xlsx(x=all_pq2, file = file)
        }
    )

}
    
    
# Run the application 
shinyApp(ui = ui, server = server)
