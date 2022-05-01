# Load packages ----
library(shiny)
library(DT)
library(gdata)
library(rstatix)
library(multcomp)
library(tidyverse)
library(tools)
library(ggpubr)

# User interface ----
ui <- fluidPage(
  titlePanel("Simple standard statics"),

  sidebarLayout(
    sidebarPanel(
      fileInput(
        "file",
        label = "File input",
        accept = c("xlsx", "txt", "csv"),
        multiple = FALSE,
        width = "80%")
    ),

    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Data", dataTableOutput("outFile")),
        tabPanel(
          "Plot", plotOutput("Plot"), #Plotの表示
          fluidRow(
            column(4, htmlOutput("PlotType")),
            column(4, htmlOutput("Test")),
            column(4, downloadButton("download_data", "Download"))
          )
        ),
        tabPanel("Tukey-HSD or Welch_t-test", dataTableOutput("outStat1")),
        tabPanel("Dunnett", dataTableOutput("outStat2"))
      )
    )
  )
)

# Server logic
server <- function(input, output) {

  inFile <- reactive({
    tmp <- input$file
    if (is.null(tmp)){
      return(NULL)
    } else {
      if(tools::file_ext(tmp$datapath) == "xlsx") df <- read.xls(tmp$datapath, header=TRUE)
      if(tools::file_ext(tmp$datapath) == "csv") df <- fread(tmp$datapath, header=TRUE, sep = ",")
      if(tools::file_ext(tmp$datapath) == "txt") df <- fread(tmp$datapath, header=TRUE, sep = "\t")
      return(df)
    }
  })

  inFile2 <- reactive({
    tmp <- inFile()
    if (is.null(tmp)){
      return(NULL)
    } else {
      collist <- gsub("\\_.+$", "", colnames(tmp))
      collist <- unique(collist[-1])
      rowlist <- gsub("\\_.+$", "", tmp[,1])
      rowlist <- unique(rowlist)
      tmp <- tmp %>% tidyr::gather(key=sample, value=value,-Row.names)
      tmp$sample<-gsub("\\_.+$", "", tmp$sample)
      tmp$Row.names <- as.factor(tmp$Row.names)
      tmp$sample <- as.factor(tmp$sample)
      tmp$value <- as.numeric(tmp$value)
      tmp$sample <- factor(tmp$sample,levels=collist,ordered=TRUE)
      return(tmp)
    }
  })



  stat1 <- reactive({
    tmp <- inFile()
    if (is.null(tmp)) {
      return(NULL)
    } else {
      collist <- gsub("\\_.+$", "", colnames(tmp))
      collist <- unique(collist[-1])
      rowlist <- gsub("\\_.+$", "", tmp[,1])
      rowlist <- unique(rowlist)
      tmp <- tmp %>% tidyr::gather(key=sample, value=value,-Row.names)
      tmp$sample<-gsub("\\_.+$", "", tmp$sample)
      tmp$Row.names <- as.factor(tmp$Row.names)
      tmp$sample <- as.factor(tmp$sample)
      tmp$value <- as.numeric(tmp$value)
      tmp$sample <- factor(tmp$sample,levels=collist,ordered=TRUE)
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
    tmp <- inFile()
    if (is.null(tmp)) {
      return(NULL)
    } else {
      collist <- gsub("\\_.+$", "", colnames(tmp))
      collist <- unique(collist[-1])
      rowlist <- gsub("\\_.+$", "", tmp[,1])
      rowlist <- unique(rowlist)
      tmp <- tmp %>% tidyr::gather(key=sample, value=value,-Row.names)
      tmp$sample<-gsub("\\_.+$", "", tmp$sample)
      tmp$Row.names <- as.factor(tmp$Row.names)
      tmp$sample <- as.factor(tmp$sample)
      tmp$value <- as.numeric(tmp$value)
      tmp$sample <- factor(tmp$sample,levels=collist,ordered=TRUE)
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

  r <- reactive({
    tmp <- inFile()
    if (is.null(tmp)) {
      return(NULL)
    } else {
      res <- ana_cor(tmp)
      return(res)
    }
  })

  output$outFile <- DT::renderDataTable({
    inFile()
  }, extensions = c('Buttons'), options = list(dom = 'Blfrtip', buttons = c('csv', 'excel', 'pdf'))
  )

  output$outStat1 <- DT::renderDataTable({
    stat1()
  }, extensions = c('Buttons'), options = list(dom = 'Blfrtip', buttons = c('csv', 'excel', 'pdf'))
  )
  output$outStat2 <- DT::renderDataTable({
    stat2()
  }, extensions = c('Buttons'), options = list(dom = 'Blfrtip', buttons = c('csv', 'excel', 'pdf'))
  )

  #以下Plotplotの描画部分
  output$Test <- renderUI({
    selectInput('Test', 'Statics', c(None='None', "TukeyHSD or Welch_t-test", "Dunnett"))
  })
  output$PlotType <- renderUI({
    selectInput('PlotType', 'PlotType', c("Boxplot", "Barplot", "Errorplot", "Violinplot"))
  })

  output$Plot <- renderPlot({
    if (input$PlotType == "Boxplot"){
    p <- ggboxplot(inFile2(), x = "sample", y = "value",fill = "sample",
                   scales = "free", add = "jitter",
                   add.params = list(size=0.5),
                   xlab = FALSE, legend = "none", ylim = c(0, NA))
    }
    if (input$PlotType == "Barplot"){
    p <- ggbarplot(inFile2(),x = "sample", y = "value", scales = "free",
                   facet.by = "Row.names", fill = "sample",add = c("mean_se", "jitter"),
                   add.params = list(size=0.5), xlab = FALSE, legend = "none")
    }
    if (input$PlotType == "Errorplot"){
    p <- ggerrorplot(inFile2(),x = "sample", y = "value",
                     scales = "free", add = "jitter", facet.by = "Row.names",
                     add.params = list(size=0.5), xlab = FALSE, error.plot = "errorbar")
    }
    if (input$PlotType == "Violinplot"){
    p <- ggviolin(inFile2(),x = "sample", y = "value",
                     facet.by = "Row.names", fill = "sample",add = c("mean_se", "jitter"),
                     add.params = list(size=0.5), xlab = FALSE, legend = "none")
    }
    if (input$Test == "TukeyHSD or Welch_t-test") p <- p + stat_pvalue_manual(stat1(),hide.ns = T, size = 2)
    if (input$Test == "Dunnett") p <- p + stat_pvalue_manual(stat2(),hide.ns = T, size = 2)
    p <- (facet(p, facet.by = "Row.names",
                panel.labs.background = list(fill = "transparent", color = "transparent"),
                scales = "free", short.panel.labs = T)+
            theme(axis.text.x= element_text(size = 5),
                  axis.text.y= element_text(size = 7),
                  panel.background = element_rect(fill = "transparent", size = 0.5),
                  title = element_text(size = 7),text = element_text(size = 10)))
    print(p)
     })

  output$download_data = downloadHandler(
    filename = function() {paste0(input$file, ".pdf")},
    content = function(file){
      tmp <- inFile()
      collist <- gsub("\\_.+$", "", colnames(tmp))
      collist <- unique(collist[-1])
      rowlist <- gsub("\\_.+$", "", tmp[,1])
      rowlist <- unique(rowlist)
      if ((length(rowlist) > 81) && (length(rowlist) <= 200))
      {pdf_hsize <- 15
      pdf_wsize <- 15}
      if ((length(rowlist) > 64) && (length(rowlist) <= 81))
      {pdf_hsize <- 13.5
      pdf_wsize <- 13.5}
      if ((length(rowlist) > 49) && (length(rowlist) <= 64))
      {pdf_hsize <- 12
      pdf_wsize <- 12}
      if ((length(rowlist) > 36) && (length(rowlist) <= 49))
      {pdf_hsize <- 10.5
      pdf_wsize <- 10.5}
      if ((length(rowlist) > 25) && (length(rowlist) <= 36))
      {pdf_hsize <- 9
      pdf_wsize <- 9}
      if ((length(rowlist) > 16) && (length(rowlist) <= 25))
      {pdf_hsize <- 7.5
      pdf_wsize <- 7.5}
      if ((length(rowlist) > 12) && (length(rowlist) <= 16))
      {pdf_hsize <- 6
      pdf_wsize <- 6}
      if ((length(rowlist) > 9) && (length(rowlist) <= 12))
      {pdf_hsize <- 5
      pdf_wsize <- 6}
      if ((length(rowlist) > 6) && (length(rowlist) <= 9))
      {pdf_hsize <- 5
      pdf_wsize <- 4.5}
      if ((length(rowlist) > 4) && (length(rowlist) <= 6))
      {pdf_hsize <- 4
      pdf_wsize <- 6}
      if (length(rowlist) == 4)
      {pdf_hsize <- 4
      pdf_wsize <- 4}
      if (length(rowlist) == 3)
      {pdf_hsize <- 2
      pdf_wsize <- 6}
      if (length(rowlist) == 2)
      {pdf_hsize <- 2
      pdf_wsize <- 4}
      if (length(rowlist) == 1)
      {pdf_hsize <- 2
      pdf_wsize <- 2}
      if (length(rowlist) > 200)
      {pdf_hsize <- 30
      pdf_wsize <- 30}
      pdf(file, width = pdf_wsize, height = pdf_hsize)
      if (input$PlotType == "Boxplot"){
        p <- ggboxplot(inFile2(), x = "sample", y = "value",fill = "sample",
                       scales = "free", add = "jitter",
                       add.params = list(size=0.5),
                       xlab = FALSE, legend = "none", ylim = c(0, NA))
      }
      if (input$PlotType == "Barplot"){
        p <- ggbarplot(inFile2(),x = "sample", y = "value", scales = "free",
                       facet.by = "Row.names", fill = "sample",add = c("mean_se", "jitter"),
                       add.params = list(size=0.5), xlab = FALSE, legend = "none")
      }
      if (input$PlotType == "Errorplot"){
        p <- ggerrorplot(inFile2(),x = "sample", y = "value",
                         scales = "free", add = "jitter", facet.by = "Row.names",
                         add.params = list(size=0.5), xlab = FALSE, error.plot = "errorbar")
      }
      if (input$PlotType == "Violinplot"){
        p <- ggviolin(inFile2(),x = "sample", y = "value",
                      facet.by = "Row.names", fill = "sample",add = c("mean_se", "jitter"),
                      add.params = list(size=0.5), xlab = FALSE, legend = "none")
      }
      if (input$Test == "TukeyHSD or Welch_t-test") p <- p + stat_pvalue_manual(stat1(),hide.ns = T, size = 2)
      if (input$Test == "Dunnett") p <- p + stat_pvalue_manual(stat2(),hide.ns = T, size = 2)
      p <- (facet(p, facet.by = "Row.names",
                  panel.labs.background = list(fill = "transparent", color = "transparent"),
                  scales = "free", short.panel.labs = T)+
              theme(axis.text.x= element_text(size = 5),
                    axis.text.y= element_text(size = 7),
                    panel.background = element_rect(fill = "transparent", size = 0.5),
                    title = element_text(size = 7),text = element_text(size = 10)))
      print(p)
      dev.off()
    }
  )


}

# Run the app
shinyApp(ui, server)
