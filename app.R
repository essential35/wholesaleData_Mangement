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
            fileInput("files", "上傳批發市場資料", multiple = T,
                      accept = c("xls/xlsx", ".xlsx", ".xls")),
            
            div(
                h4("檔案上傳限制："),
                p("1. 檔案需為「.xls」或「.xlsx」檔。"),
                p("2. 檔案名稱格式為「(蔬菜名字)_(市場簡稱).xls」，例如「小白菜_台中市.xlsx」。")
            ),

            tags$hr(),
            
            div(
                checkboxGroupInput("VarCheck",
                                   h4("次數分配表變數選擇："),
                                   choices = c("交易量(公斤)" = 1,
                                               "交易價" = 2,
                                               "交易量漲跌" = 3,
                                               "交易價漲跌" = 4
                                   ),
                                   selected = 1)
            ),
            tags$hr(),
            
            h3("Download"),
            # 下載合併資料
            h5("- 合併資料下載："),
            # download data button
            downloadButton("dl", "Combine Data"),
            
            # 下載所有結果
            h5("- 所有結果下載："),
            downloadButton("dl2","All Result")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("原始資料", 
                         DT::dataTableOutput(outputId = "originData")),
                tabPanel("所有日資料",
                         DT::dataTableOutput(outputId = "allDayData")),
                tabPanel("無原始資料的日資料",
                         DT::dataTableOutput(outputId = "nonData")),
                tabPanel("次數分配表",
                         h3("上下限 & 組距"),
                         DT::dataTableOutput(outputId = "intervals"),
                         br(),
                         h3("次數分配表"),
                         DT::dataTableOutput(outputId = "freqDistriTable"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output){
    
    everyday <- reactive({
      # create a dataframe include all dates from 1996/01/01 to 2020/12/31
      DayTrade <- data.frame(
        year = c(1996:2020),
        month = rep(1:12, length(c(1996:2020))),
        day = rep(1:31, 25*12)
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
        all_pq <- matrix(ncol = 7) %>% as.data.frame()
        colnames(all_pq) <- c("Date","月份","交易量(公斤)","交易量(公噸)",
                              "交易價","批發市場","品項")
        
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
                                     "^[\u4E00-\u9FFF]+(?=_)")
                ) %>%
                rbind(all_pq, .)
        }
        
        all_pq <- all_pq[-1,] %>%
            group_by(批發市場, 品項) %>%
            mutate(
                `前一天交易量(公斤)` = lag(`交易量(公斤)`),
                `前一天交易價` = lag(`交易價`),
                `交易量差(公斤)` = `交易量(公斤)` - `前一天交易量(公斤)`,
                `交易價差` = `交易價` - `前一天交易價`,
                `交易量漲跌` = `交易量差(公斤)`/`前一天交易量(公斤)`,
                `交易價漲跌` = `交易價差`/`前一天交易價`,
                月份 = as.numeric(月份),
                `有無原始資料` = TRUE
            ) 
        
        all_pq
    })
    
    # 無原始資料的日資料
    nondf <- reactive({
      
      # 無原始資料
      markets <- df()$批發市場 %>% as.factor %>% levels
      category <- df()$品項 %>% as.factor %>% levels
      
      nonData <- matrix(ncol = 14) %>% as.data.frame()
      colnames(nonData) <- c("Date","月份","交易量(公斤)","交易量(公噸)",
                             "交易價","批發市場","品項", "前一天交易量(公斤)",
                             "前一天交易價","交易量差(公斤)","交易價差",
                             "交易量漲跌","交易價漲跌","有無原始資料")
      for (m in markets) {
          for (c in category) {
              check <- df() %>% filter((品項 == c) & (批發市場 == m))
            
              if(NROW(check) == 0){
                nonData <- everyday() %>%
                  mutate(
                    Date = as.character(Date),
                    月份 = as.numeric(月份),
                    批發市場 = as.character(m),
                    品項 = as.character(c),
                    `交易量(公斤)` = NA, 
                    `交易量(公噸)` = NA, 
                    交易價 = NA,
                    `前一天交易量(公斤)` = NA,
                    `前一天交易價` = NA,
                    `交易量差(公斤)` = NA,
                    `交易價差` = NA,
                    `交易量漲跌` = NA,
                    `交易價漲跌` = NA,
                    `有無原始資料` = FALSE
                  ) %>%
                  rbind(nonData,.)
              }else{
                next()
              }
          }
      }
      
        nonData <- nonData[-1,]
        nonData
    })
    
    # final all date data
    finalAllDayTrade <- reactive({
        if (NROW(nondf()) == 0){
          df() %>% .[order(.$批發市場),]
        }else{
          bind_rows(df(), nondf()) %>% .[order(.$批發市場),]
        }
    })
    
    # 無原始資料的日資料
    output$nonData <- DT::renderDataTable({
        nondf() 
    })
    
    # 原始資料
    output$originData <- DT::renderDataTable({
        df() %>% .[,c(1:7)] %>% .[which(!is.na(.$`交易量(公斤)`)),]
    })
    
    # 所有日資料
    output$allDayData <- DT::renderDataTable({
        finalAllDayTrade()
    })
    
    # 上下限 & 間距
    interval_df <- reactive({
        source("intergroup.R")  # 引入function: group_inter() 
        inter_df <- group_inter(df()) 
        inter_df
    })
    
    output$intervals <- DT::renderDataTable({
        interval_df() %>%
            .[which(.$group %in% as.numeric(input$VarCheck)),] %>%
            select(-group) %>%
            tidyr::spread(., key = variable, value = num)
    })
    
    # 次數分配表
    freq_df <- reactive({
        
        FreqOutputTable <- matrix(ncol = 6) %>% as.data.frame() 
        colnames(FreqOutputTable) <- c("Month", "區間", "次數", 
                                       "批發市場", "品項", "變數")
        
        
        source("frequency_table.R")  
        # 引入function：freq_group(品項,批發市場,間距資料,所有日交易資料,變數選擇數字)
        
        markets <- df()$批發市場 %>% as.factor %>% levels
        category <- df()$品項 %>% as.factor %>% levels
       
        for (mkt in markets) {
            for (c in category) {
                
                inter_df <- interval_df() %>% 
                    filter((品項 == c) & (批發市場 == mkt))
                
                if(NROW(inter_df) == 0){
                    next()  # if there has no data, then continuously run the next loop.
                }else{
                  for (v in as.numeric(input$VarCheck)) {
                      FreqOutputTable <- freq_group(c, mkt, inter_df, df(), v) %>%
                          rbind(FreqOutputTable, .)
                  }
                }
            }
        }
        
        FreqOutputTable <- FreqOutputTable[-1,]
        FreqOutputTable
    })
    
    output$freqDistriTable <- DT::renderDataTable({
        freq_df()
    })
    
    # download data
    output$dl <- downloadHandler(
        filename = function() {
            "Combine_Data.xlsx"
        },
        content = function(file) {
            all_pq2 <- list(
                `原始檔` = df() %>% .[,c(1:7)] %>%
                    .[which(!is.na(.$`交易量(公斤)`)),] %>% .[order(.$批發市場),],
                `日交易_含漲跌幅` = finalAllDayTrade()
            )
            
            write.xlsx(x=all_pq2, file = file)
        }
    )

    output$dl2 <- downloadHandler(
        filename = function() {
            "All_Results.xlsx"
        },
        content = function(file) {
            all_pq3 <- list(
                `原始檔` = df() %>% .[,c(1:7)] %>%
                    .[which(!is.na(.$`交易量(公斤)`)),],
                `日交易_含漲跌幅` = finalAllDayTrade(),
                `上下限_間距` = interval_df() %>%
                    .[which(.$group %in% as.numeric(input$VarCheck)),] %>%
                    select(-group) %>%
                    tidyr::spread(., key = variable, value = num),
                `次數分配表` = freq_df()
            )
            
            write.xlsx(x=all_pq3, file = file)
        }
    )

}
    
    
# Run the application 
shinyApp(ui = ui, server = server)
