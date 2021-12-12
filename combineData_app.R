#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(stringr)
library(openxlsx)

options(shiny.maxRequestSize=100*1024^2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("合併批發市場資料"), 

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("files", "上傳批發市場資料", multiple = T,
                      accept = c("xls/xlsx", ".xlsx", ".xls")),
            tags$hr(),
            h3("上傳檔案限制："),
            p("1. 檔案需為「.xls」或「.xlsx」檔。"),
            p("2. 檔案名稱格式為「(蔬菜名字)_(市場簡稱).xls」，
                例如「小白菜_台中市.xlsx」。"),
            br(),
            br(),
            h3("Download All Result:"),
            # download data button
            downloadButton("dl", "Download")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("原始資料", 
                         DT::dataTableOutput(outputId = "originData")),
                tabPanel("所有日資料",
                         DT::dataTableOutput(outputId = "allDayData"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # data combine
    df <- reactive({
        req(input$files)
        
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
                left_join(DayTrade,., by=c("Date","月份")) %>%
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
                `交易價漲跌` = `交易價差`/`前一天交易價`
            ) 
        
        all_pq
    })

    # 原始資料
    output$originData <- DT::renderDataTable({
        df() %>% .[,c(1:7)] %>% .[which(!is.na(.$`交易量(公斤)`)),]
    })
    
    # 所有日資料
    output$allDayData <- DT::renderDataTable({
        df()
    })
    
    
    # download data
    output$dl <- downloadHandler(
        
        
        filename = function() {
            "Combine_Data.xlsx"
            #paste0(input$dl_choice,".xlsx")
        },
        content = function(file) {
            all_pq2 <- list(
                `原始檔` = df() %>% .[,c(1:7)] %>%
                    .[which(!is.na(.$`交易量(公斤)`)),],
                `日交易_含漲跌幅` = df()
            )
            
            write.xlsx(x=all_pq2, file = file)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
