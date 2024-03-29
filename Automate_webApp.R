# Load packages ----
library(shiny)
library(DT)
library(gdata)
library(rstatix)
library(multcomp)
library(tidyverse)
library(ggpubr)
library(shinyBS,verbose=FALSE)

# User interface ----
popoverTempate <- 
  '<div class="popover popover-lg" role="tooltip"><div class="arrow"></div><h3 class="popover-title"></h3><div class="popover-content"></div></div>'
pdfSize_for_GOI <- paste(strong("Boxplot:"),"<br>",
                         "Gene number = 1,","height = 3, width = 3 <br>",
                         "Gene number = 2,","height = 3, width = 6 <br>",
                         "Gene number = 3,","height = 3, width = 9 <br>",
                         "Gene number = 4,","height = 6, width = 6 <br>",
                         "Gene number = 5 ~ 6,","height = 6, width = 9 <br>",
                         "Gene number = 7 ~ 9,","height = 7.5, width = 6.75 <br>",
                         "Gene number = 10 ~ 12,","height = 7.5, width = 9 <br>",
                         "Gene number = 13 ~ 16,","height = 9, width = 9 <br>",
                         "Gene number = 17 ~ 25,","height = 11.5, width = 11.5 <br>",
                         "Gene number = 26 ~ 36,","height = 13.5, width = 13.5 <br>",
                         "Gene number = 37 ~ 49,","height = 15.75, width = 15.75 <br>",
                         "Gene number = 50 ~ 64,","height = 18, width = 18 <br>",
                         "Gene number = 65 ~ 81,","height = 20.5, width = 20.5 <br>",
                         "Gene number = 82 ~ 200,","height = 22.5, width = 22.5 <br>",
                         "Gene number > 200,", "height = 30, width = 30 <br>")

ui <- fluidPage(
  tags$style(".popover{
            max-width: 1000px;
          }"),
  tags$head(includeHTML(("google-analytics.html"))),
  titlePanel("Automate"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file",
        strong(
          span("Select a count matrix file"),
          span(icon("info-circle"), id = "icon1", 
               options = list(template = popoverTempate))
        ),
        accept = c("xlsx", "txt", "csv"),
        multiple = FALSE,
        width = "80%"),
      bsPopover("icon1", "Count matrix format (txt, csv, or xlsx):", 
                content=paste(strong("The replication number"), "is represented by", strong("the underline"),".<br>", strong("Do not use it for anything else"),".<br><br>",
                              img(src="format example.png", width = 1000,height = 400)), 
                placement = "right",options = list(container = "body")),
      strong(span("Output plot size setting for pdf (0: default)"),
             span(icon("info-circle"), id = "pair_pdf_icon", 
                  options = list(template = popoverTempate))),
      fluidRow(
        column(5, numericInput("pair_pdf_height", "pdf_height", value = 0, min = 0)),
        column(5, numericInput("pair_pdf_width", "pdf_width", value = 0, min = 0))
      ),
      bsPopover("pair_pdf_icon", "Output plot size setting for pdf (default: 0): ", 
                content=paste("You can adjust the plot size by using", strong('pdf_height'), "and", strong('pdf_width'), "parameters.<br>", 
                              "Default size: <br>", pdfSize_for_GOI),trigger = "click",placement = "right",options = list(container = "body")), 
      actionButton("goButton", "example data"),
      tags$head(tags$style("#goButton{color: black;
                                 font-size: 12px;
                                 font-style: italic;
                                 }"),
                tags$style("
          body {
            padding: 0 !important;
          }"
                ))
    ),

    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Data", dataTableOutput("outFile")),
        tabPanel(
          "Plot", 
          fluidRow(
            column(4, htmlOutput("PlotType")),
            column(4, htmlOutput("Test")),
            column(4, downloadButton("download_data", "Download"))
          ),
          div(
          plotOutput("Plot", height = "100%"), #Plotの表示
          style = "height: calc(100vh  - 100px)"),
          bsCollapse(id="input_collapse_panel",open="Tukey_panel",multiple = FALSE,
                     bsCollapsePanel(title="Tukey-HSD or Welch_t-test:",
                                     value="Tukey_panel",
                                     fluidRow(
                                       column(4, downloadButton("download_Stat1", "Download"))
                                     ),
                                     dataTableOutput('outStat1')
                     ),
                     bsCollapsePanel(title="Dunnett:",
                                     value="Dunnett_panel",
                                     fluidRow(
                                       column(4, downloadButton("download_Stat2", "Download"))
                                     ),
                                     dataTableOutput('outStat2')
                     )
          )
        ),
        tabPanel("About",
                 strong("Automate_shiny"), br(),
                 "`Automate_shiny` is an RShiny web apps", a("(https://kan-e.shinyapps.io/Automate_shiny/)", href = "https://kan-e.shinyapps.io/Automate_shiny/"), "for automated data visualization from count matrix files.", br(),
                 "It has simplified functions for the creation of a basic graph.", br(),
                 "The condition number is automatically recognized from the count matrix file and then the statical analysis is performed.", br(),
                 "In the case of just 2 conditions (pairwise comparison), Welch's t-test is performed. In the case of more than 3 conditions (multiple comparisons), the Tukey HSD test and Dunnett's test are performed.", br(),
                 br(),
                 strong("Input file format"), br(),
                 "Input file format must be excel file format (.xlsx), tab-separated text file format (.txt), or CSV file format (.csv).", br(),
                 "A1 cell in the excel sheet must be __Row.names__.", br(),
                 "The replication number is represented by the underbar. Do not use it for anything else.", br(),
                 img(src="format example.png", height = 384, width = 890), br(),
                 br(),
                 strong("Output example"), br(),
                 "Errorplot (TukeyHSD)", br(),
                 img(src="example autoerror tukeyHSD.png", height = 700, width = 700), br(),
                 "Statical analysis", br(),
                 img(src="example result of tukeyHS.png", height = 500, width = 800), br(),
                 br(),
                 strong("Reference"), br(),
                 "H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016.", br(),
                 "Alboukadel Kassambara (2020). ggpubr: 'ggplot2' Based Publication Ready Plots. R package version 0.4.0. https://CRAN.R-project.org/package=ggpubr", br(),
                 "Hadley Wickham, Romain François, Lionel Henry and Kirill Müller (2021). dplyr: A Grammar of Data Manipulation. R package version 1.0.7. https://CRAN.R-project.org/package=dplyr", br(),
                 "Hadley Wickham (2021). tidyr: Tidy Messy Data. R package version 1.1.3. https://CRAN.R-project.org/package=tidyr", br(),
                 "Alboukadel Kassambara (2021). rstatix: Pipe-Friendly Framework for Basic Statistical Tests. R package version 0.7.0. https://CRAN.R-project.org/package=rstatix", br(),
                 "Torsten Hothorn, Frank Bretz and Peter Westfall (2008). Simultaneous Inference in General Parametric Models. Biometrical Journal 50(3), 346--363.", br(),
                 "Winston Chang, Joe Cheng, JJ Allaire, Carson Sievert, Barret Schloerke, Yihui Xie, Jeff Allen, Jonathan McPherson, Alan Dipert and Barbara Borges (2021). shiny: Web Application Framework for R. R package version 1.7.1. https://CRAN.R-project.org/package=shiny", br(),
                 br(),
                 strong("Author: Kan Etoh kaneto@kumamoto-u.ac.jp"), br(),
                 "Source code:", a("https://github.com/Kan-E/Automate_shiny", href = "https://github.com/Kan-E/Automate_shiny"), br(),
                 "2022 April"),
        footer=p(hr(),p("ShinyApp created by Kan Etoh",align="center",width=4),
                 p(("Copyright (C) 2022, code licensed under GPLv3"),align="center",width=4),
                 p(("Code available on Github:"),a("https://github.com/Kan-E/Automate_shiny",href="https://github.com/Kan-E/Automate_shiny"),align="center",width=4)
        )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {

  inFile <- reactive({
    tmp <- input$file$datapath
    if(is.null(input$file) && input$goButton == 0) return(NULL)
    if(is.null(input$file) && input$goButton > 0 )  tmp = "data/example.xlsx"
      if(tools::file_ext(tmp) == "xlsx") df <- read.xls(tmp, header=TRUE)
      if(tools::file_ext(tmp) == "csv") df <- read.csv(tmp, header=TRUE, sep = ",")
      if(tools::file_ext(tmp) == "txt") df <- read.table(tmp, header=TRUE, sep = "\t")
      return(df)
  })

  inFile2 <- reactive({
    tmp <- inFile()
    if (is.null(tmp)){
      return(NULL)
    } else {
      collist <- collist()
      rowlist <- rowlist()
      tmp <- tmp %>% tidyr::gather(key=sample, value=value,-Row.names)
      tmp$sample<-gsub("\\_.+$", "", tmp$sample)
      tmp$Row.names <- as.factor(tmp$Row.names)
      tmp$sample <- as.factor(tmp$sample)
      tmp$value <- as.numeric(tmp$value)
      tmp$sample <- factor(tmp$sample,levels=collist,ordered=TRUE)
      return(tmp)
    }
  })

  collist <- reactive({
    tmp <- inFile()
    if (is.null(tmp)) {
      return(NULL)
    } else {
      collist <- gsub("\\_.+$", "", colnames(tmp))
      collist <- unique(collist[-1])
      return(collist)
    }
    })
  
  rowlist <- reactive({
    tmp <- inFile()
    if (is.null(tmp)) {
      return(NULL)
    } else {
      rowlist <- gsub("\\_.+$", "", tmp[,1])
      rowlist <- unique(rowlist)
      return(rowlist)
    }
  })


  stat1 <- reactive({
    tmp <- inFile2()
    if (is.null(tmp)) {
      return(NULL)
    } else {
      collist <- collist()
      rowlist <- rowlist()
      res <- data.frame(matrix(rep(NA, 11), nrow=1))[numeric(0), ]
      colnames(res) <- c("Row.names", "group1", "group2", "term", "null.value","Std.Error","coefficients","t.value","p.adj","xmin", "xmax")
      if (length(collist) >= 3){
        stat.test <- tmp %>% group_by(Row.names)
        stat.test <- stat.test %>% tukey_hsd(value ~ sample)
        stat.test <- stat.test %>% add_significance("p.adj")
        stat.test <- stat.test %>% add_xy_position(scales = "free", step.increase = 0.2)
      }else{
        stat.test <- tmp %>% group_by(Row.names)
        stat.test <- stat.test %>% t_test(value ~ sample)
        stat.test <- stat.test %>% add_significance()
        stat.test <- stat.test %>% add_xy_position(scales = "free", step.increase = 0.2)
      }
      return(stat.test)
    }
  })
  stat2 <- reactive({
    tmp <- inFile2()
    if (is.null(tmp)) {
      return(NULL)
    } else {
      collist <- collist()
      rowlist <- rowlist()
      res <- data.frame(matrix(rep(NA, 11), nrow=1))[numeric(0), ]
      colnames(res) <- c("Row.names", "group1", "group2", "term", "null.value","Std.Error","coefficients","t.value","p.adj","xmin", "xmax")
      if (length(collist) >= 3){
        for (name2 in rowlist){
          data2 <- dplyr::filter(tmp, Row.names == name2)
          dun <- stats::aov(value~sample, data2)
          dunnette <- multcomp::glht(model = dun, linfct=multcomp::mcp(sample="Dunnett"))
          dunnette2 <- summary(dunnette)
          p.adj <- c()
          coefficients <- c()
          Std.Error <- c()
          t.value <- c()
          group1 <- c()
          group2 <- c()
          term <- c()
          null.value <- c()
          xmin <- c()
          xmax <- c()
          for (i in 1:(length(collist)-1)){
            p.adj <- c(p.adj, dunnette2[["test"]][["pvalues"]][i])
            coefficients <- c(coefficients, dunnette2[["test"]][["coefficients"]][i])
            Std.Error <- c(Std.Error, dunnette2[["test"]][["sigma"]][i])
            t.value <- c(t.value, dunnette2[["test"]][["tstat"]][i])
            group1 <- c(group1, c(collist[1]))
            group2 <- c(group2, c(collist[i+1]))
            term <- c(term, c("sample"))
            null.value <- c(null.value, 0)
            xmin <- c(xmin, c(1))
            xmax <- c(xmax, c(i+1))
          }
          res2 <- data.frame(Row.names = name2, group1 = group1, group2 = group2, term = term,
                            null.value = null.value, Std.Error = Std.Error, coefficients = coefficients,
                            t.value = t.value, p.adj = p.adj, xmin = xmin, xmax = xmax)
          res <- rbind(res, res2)
        }
        res <- res %>% arrange(Row.names)
        res <- res %>% group_by(Row.names)
        stat.test2 <- tmp %>% group_by(Row.names)
        stat.test2 <- stat.test2 %>% get_y_position(value ~ sample, scales = "free", step.increase = 0.15, fun = "mean_se")
        stat.test2 <- stat.test2 %>% dplyr::filter(group1 == collist[1])
        stat.test3 <- cbind(stat.test2,res[,-1:-3])
        stat.test3$Row.names <- as.factor(stat.test3$Row.names)
        stat.test3 <- stat.test3 %>% add_significance("p.adj")
      }
      return(stat.test3)
    }
  })

  output$outFile <- DT::renderDataTable({
    inFile()
  })

  output$outStat1 <- DT::renderDataTable({
    stat1()
  })
  output$outStat2 <- DT::renderDataTable({
    stat2()
  })

  #以下Plotplotの描画部分
  output$Test <- renderUI({
    selectInput('Test', 'Statics', c(None='None', "TukeyHSD or Welch_t-test", "Dunnett"))
  })
  output$PlotType <- renderUI({
    selectInput('PlotType', 'PlotType', c("Boxplot", "Barplot", "Errorplot", "Violinplot"))
  })
  
  plot_result <- reactive({
    if(is.null(inFile2()) || is.null(input$PlotType) || is.null(input$Test)){
      return(NULL)
    }else{
      withProgress(message = "Drawing a plot, please wait", {
        if (input$PlotType == "Boxplot"){
          p <- ggboxplot(inFile2(), x = "sample", y = "value",fill = "sample",
                         scales = "free", add = "jitter",
                         add.params = list(size=0.5),
                         xlab = FALSE, ylim = c(0, NA))
        }
        if (input$PlotType == "Barplot"){
          p <- ggbarplot(inFile2(),x = "sample", y = "value", scales = "free",
                         facet.by = "Row.names", fill = "sample",add = c("mean_se", "jitter"),
                         add.params = list(size=0.5), xlab = FALSE)
        }
        if (input$PlotType == "Errorplot"){
          p <- ggerrorplot(inFile2(),x = "sample", y = "value",
                           scales = "free", add = "jitter", facet.by = "Row.names",
                           add.params = list(size=0.5), xlab = FALSE, error.plot = "errorbar")
        }
        if (input$PlotType == "Violinplot"){
          p <- ggviolin(inFile2(),x = "sample", y = "value",
                        facet.by = "Row.names", fill = "sample",add = c("jitter"),
                        add.params = list(size=0.5), xlab = FALSE)
        }
        if (input$Test == "TukeyHSD or Welch_t-test") p <- p + stat_pvalue_manual(stat1(),hide.ns = T, size = 5)
        if (input$Test == "Dunnett") p <- p + stat_pvalue_manual(stat2(),hide.ns = T, size = 5)
        p <- (facet(p, facet.by = "Row.names",
                    panel.labs.background = list(fill = "transparent", color = "transparent"),
                    scales = "free", short.panel.labs = T, panel.labs.font = list(size=15))+
                theme(axis.text.x = element_blank(),
                      panel.background = element_rect(fill = "transparent", size = 0.5),
                      title = element_text(size = 10),text = element_text(size = 12),
                      axis.title.y = element_text(size=15),legend.text = element_text(size=15),
                      legend.title = element_blank()))
        return(p)
      })
    }
  })

  output$Plot <- renderPlot({
    plot_result()
     })
  
  output$download_Stat1 <- downloadHandler(
    filename = function() {paste0(gsub("\\..+$", "", input$file), ".txt")},
    content = function(file){
      df <- apply(as.data.frame(stat1()),2,as.character)
      write.table(df, file, row.names = F, sep = "\t", quote = F)})
  output$download_Stat2 <- downloadHandler(
    filename = function() {paste0(gsub("\\..+$", "", input$file), ".txt")},
    content = function(file){
      df <- apply(as.data.frame(stat2()),2,as.character)
      write.table(df, file, row.names = F, sep = "\t", quote = F)})

  output$download_data = downloadHandler(
    filename = function(){
      paste0(gsub("\\..+$", "", input$file), ".pdf")
    },
    content = function(file) {
      withProgress(message = "Preparing download",{
        tmp <- inFile()
        rowlist <- rownames(tmp)
        if(input$pair_pdf_height == 0){
          pdf_height <- pdf_h(rowlist)
        }else pdf_height <- input$pair_pdf_height
        if(input$pair_pdf_width == 0){
          pdf_width <- pdf_w(rowlist)
        }else pdf_width <- input$pair_pdf_width
        pdf(file, height = pdf_height, width = pdf_width)
        print(plot_result())
        dev.off()
        incProgress(1)
      })
    }
  )
  
  
  observeEvent(input$Test, ({
    if(input$Test == "TukeyHSD or Welch_t-test"){
      updateCollapse(session,id =  "input_collapse_panel", open="Tukey_panel")
    }
    if(input$Test == "Dunnett"){
      updateCollapse(session,id =  "input_collapse_panel", open="Dunnett_panel")
    }
  }))
}

pdf_h <- function(rowlist){
  if ((length(rowlist) > 81) && (length(rowlist) <= 200)) pdf_hsize <- 22.5
  if ((length(rowlist) > 64) && (length(rowlist) <= 81)) pdf_hsize <- 20.25
  if ((length(rowlist) > 49) && (length(rowlist) <= 64)) pdf_hsize <- 18
  if ((length(rowlist) > 36) && (length(rowlist) <= 49)) pdf_hsize <- 15.75
  if ((length(rowlist) > 25) && (length(rowlist) <= 36)) pdf_hsize <- 13.5
  if ((length(rowlist) > 16) && (length(rowlist) <= 25)) pdf_hsize <- 11.5
  if ((length(rowlist) > 12) && (length(rowlist) <= 16)) pdf_hsize <- 9
  if ((length(rowlist) > 9) && (length(rowlist) <= 12)) pdf_hsize <- 7.5
  if ((length(rowlist) > 6) && (length(rowlist) <= 9)) pdf_hsize <- 7.5
  if ((length(rowlist) > 4) && (length(rowlist) <= 6)) pdf_hsize <- 6
  if (length(rowlist) == 4) pdf_hsize <- 6
  if (length(rowlist) == 3) pdf_hsize <- 3
  if (length(rowlist) == 2) pdf_hsize <- 3
  if (length(rowlist) == 1) pdf_hsize <- 3
  if (length(rowlist) > 200) pdf_hsize <- 30
  return(pdf_hsize)
}
pdf_w <- function(rowlist){
  if ((length(rowlist) > 81) && (length(rowlist) <= 200)) pdf_wsize <- 22.5
  if ((length(rowlist) > 64) && (length(rowlist) <= 81)) pdf_wsize <- 20.25
  if ((length(rowlist) > 49) && (length(rowlist) <= 64)) pdf_wsize <- 18
  if ((length(rowlist) > 36) && (length(rowlist) <= 49)) pdf_wsize <- 15.75
  if ((length(rowlist) > 25) && (length(rowlist) <= 36)) pdf_wsize <- 13.5
  if ((length(rowlist) > 16) && (length(rowlist) <= 25)) pdf_wsize <- 11.5
  if ((length(rowlist) > 12) && (length(rowlist) <= 16)) pdf_wsize <- 9
  if ((length(rowlist) > 9) && (length(rowlist) <= 12)) pdf_wsize <- 9
  if ((length(rowlist) > 6) && (length(rowlist) <= 9)) pdf_wsize <- 6.75
  if ((length(rowlist) > 4) && (length(rowlist) <= 6)) pdf_wsize <- 9
  if (length(rowlist) == 4) pdf_wsize <- 6
  if (length(rowlist) == 3) pdf_wsize <- 9
  if (length(rowlist) == 2) pdf_wsize <- 6
  if (length(rowlist) == 1) pdf_wsize <- 3
  if (length(rowlist) > 200) pdf_wsize <- 30
  return(pdf_wsize)
}

# Run the app
shinyApp(ui, server)
