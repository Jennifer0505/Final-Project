library(shiny)
library(rsconnect)
library(shinydashboard)
library(tm)
library(wordcloud)
library(memoise)
library(plotrix)
library(tuber)
library(dplyr)
library(tidytext)
library(stringr)
library(lubridate)
library(tm)
library(SnowballC)
library(RColorBrewer)
frequency_real <- read.csv("frequency_real.csv", header=TRUE)
frequency_2017 <- read.csv("frequency_2017.csv", header=TRUE)
frequency_2016 <- read.csv("frequency_2016.csv", header=TRUE)
frequency_2015 <- read.csv("frequency_2015.csv", header=TRUE)
frequency_2014 <- read.csv("frequency_2014.csv", header=TRUE)
frequency_2013 <- read.csv("frequency_2013.csv", header=TRUE)
frequency_2012 <- read.csv("frequency_2012.csv", header=TRUE)
tdm <- read.csv("tdm.csv", header=TRUE)
rownames(tdm) <- tdm[,1]
tdm <- tdm[2:8]
tdm <- as.matrix(tdm)
#global
clouds <- list("2018"="2018.csv","2017"="2017.csv","2016"="2016.csv",
                "2015"="2015.csv","2014"="2014.csv",
                "2013"="2013.csv", "2012"="2012.csv")
getTermMatrix <- memoise(function(year){
  if (!(year %in% clouds))
    stop("Unknown year")
  text <- read.csv(year,header=TRUE, stringsAsFactors = FALSE, fileEncoding = "latin1")
  rownames(text) <- text[,1]
  text <- text[2:3]
  text <- as.matrix(text)
  sort(rowSums(text), decreasing = TRUE)
})

ui <- dashboardPage(
  dashboardHeader(title = "School Shootings"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "Intro", icon = icon("file-text-o")),
      menuItem("Word Cloud", icon = icon("line-chart"),
               menuSubItem("Individual Word Cloud", tabName = "IWC", icon = icon("angle-right")),
               menuSubItem("Comparison Word Cloud", tabName = "CWC", icon = icon("angle-right"))),
      menuItem("Sentiment Analysis",tabName = "SA", icon = icon("pie-chart")),
      menuItem("Frequent Words", tabName = "FW", icon = icon("bar-chart-o")),
      menuItem("Time Series", tabName = "TS", icon = icon("line-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "IWC",
              fluidRow(
                titlePanel("Individual Word Cloud"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("selection", "Choose a year:",
                                choices = clouds),
                    actionButton("update", "Change"),
                    hr(),
                    sliderInput("freq",
                                "Minimum Frequency:",
                                min = 1, max = 50, value = 15),
                    sliderInput("max",
                                "Maximum Number of Words:",
                                min = 1, max = 500, value = 100)
                  ),
                  mainPanel(
                    plotOutput("plot1"),background = "light-blue"
                  )
                )
              )
      ),
      tabItem(tabName = "CWC",
              box(plotOutput("plot11")),
              box(collapsible = TRUE, tableOutput("comparisonCloudcomments"),background = "light-blue")
      ),
      tabItem(tabName = "FW",
              fluidRow(
                tabBox(
                  title = "View By Year",
                  tabPanel("2012", "Word Frequency: 2012", plotOutput("plot3")),
                  tabPanel("2013", "Word Frequency: 2013", plotOutput("plot4")),
                  tabPanel("2014", "Word Frequency: 2014", plotOutput("plot5")),
                  tabPanel("2015", "Word Frequency: 2015", plotOutput("plot6")),
                  tabPanel("2016", "Word Frequency: 2016", plotOutput("plot7")),
                  tabPanel("2017", "Word Frequency: 2017", plotOutput("plot8")),
                  tabPanel("2018", "Word Frequency: 2018", plotOutput("plot2"))
                ),box(collapsible = TRUE, tableOutput("frequent"),background = "light-blue")
              )
      ),
      tabItem(tabName = "SA",
              box(plotOutput("plot9")),
              box(collapsible = TRUE, tableOutput("sentimentanalysis"),background = "light-blue")
      ),
      tabItem(tabName = "TS",
              box(plotOutput("plot10")),
              box(collapsible = TRUE, tableOutput("timeseries"),background = "light-blue"),
              selectInput("select", label = h3("Number of videos related with school shootings in each year"), 
                          choices = list("2018" = 11, "2017" = 226, "2016" = 382, 
                                         "2015" = 214, "2014"=207, "2013"=576,
                                         "2012" = 129), 
                          selected = 1),
              
              hr(),
              fluidRow(column(3, verbatimTextOutput("test")))
              
      ),
      tabItem(tabName = "Intro",
              box(title="INTENTION",tableOutput("introduction"), background = "light-blue"),
              box(title = "What I did in this project", tableOutput("introduction2"), background = "light-blue")
      )
    )
  )
)
server <- function(input, output, session){
  terms <- reactive({
    input$update
    isolate({
      withProgress({
        setProgress(message = "Processing Corpus...")
        getTermMatrix(input$selection)
      })
    })
  })
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot1 <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq=input$freq, max.words=input$max, 
                  colors = brewer.pal(8, "Dark2"))
  })
  output$plot2 <- renderPlot({
    barplot(frequency_real$freq, width=1,space= 0.5,
          names.arg = frequency_real$word,ylim=c(0,2500),
          col =rainbow(20), main ="Most frequent words_real",
          ylab = "Word frequencies",cex.names = 0.6)})
  output$plot3 <- renderPlot({
    barplot(frequency_2012$freq, width=1,space= 0.5,
          names.arg = frequency_2012$word,ylim=c(0,2500),
          col =rainbow(20), main ="Most frequent words_2012",
          ylab = "Word frequencies",cex.names = 0.6)})
  output$plot4 <- renderPlot({
    barplot(frequency_2013$freq, width=1,space= 0.5,
          names.arg = frequency_2013$word,ylim=c(0,2500),
          col =rainbow(20), main ="Most frequent words_2013",
          ylab = "Word frequencies",cex.names = 0.6)})
  output$plot5 <- renderPlot({
    barplot(frequency_2014$freq, width=1,space= 0.5,
          names.arg = frequency_2014$word,ylim=c(0,2500),
          col =rainbow(20), main ="Most frequent words_2014",
          ylab = "Word frequencies",cex.names = 0.6)})
  output$plot6 <- renderPlot({
    barplot(frequency_2015$freq, width=1,space= 0.5,
          names.arg = frequency_2015$word,ylim=c(0,2500),
          col =rainbow(20), main ="Most frequent words_2015",
          ylab = "Word frequencies",cex.names = 0.6)})
  output$plot7 <- renderPlot({
    barplot(frequency_2016$freq, width=1,space= 0.5,
          names.arg = frequency_2016$word,ylim=c(0,2500),
          col =rainbow(20), main ="Most frequent words_2016",
          ylab = "Word frequencies",cex.names = 0.6)})
  output$plot8 <- renderPlot({
    barplot(frequency_2017$freq, width=1,space= 0.5,
          names.arg = frequency_2017$word,ylim=c(0,2500),
          col =rainbow(20), main ="Most frequent words_2017",
          ylab = "Word frequencies",cex.names = 0.6)}) 
  output$plot9 <- renderPlot({
    slices <- c(anticipation_total <- 1000, anger_total <- 1881, joy_total <- 706)
    lbls <- c("anticipation", "anger", "joy")
    slices <- c(1000, 1881, 706)
    pct <- round(slices/sum(slices)*100)
    lbls <- paste(lbls, pct)
    lbls <- paste(lbls,"%",sep="")
    pie3D(slices,labels=lbls,explode=0.1,
          main="Real Time: Pie Chart of sentiment analysis")
  })
  output$plot10 <- renderPlot({
    ts_shooting <- ts(c(11, 226, 382, 214, 207, 576, 129), start=c(2012), end=c(2018))
    plot(ts_shooting, main= "Time Series for Related School Shootings' Videos")
    namebank <- as.character(c(2012:2018))
    text(c(2012:2018), ts_shooting, namebank,cex=0.9)
  })
  output$plot11 <- renderPlot({
    v <- terms()
    comparison.cloud(tdm, random.order=FALSE, scale = c(0.9, 0.6),
                     colors = c("pink", " yellow", "red", " orange", " black","blue","green"),
                     title.size=0.4)
  })
  output$test <- renderPrint({input$select})
  output$introduction <- renderPrint({
    HTML("More and more school shootings begin to exist recently. They happened without any preparations. 
         Many students and faculties have been killed by the shooter. From my perspective,
         it is an important social issue that everyone needs to pay
         attention to in order to have a stable society in the future and to create a safe 
         an environment for both students and professors.
         ")
  })
  output$comparisonCloudcomments <- renderPrint({
    HTML(" According to the word cloud, we could notice that comments in 2017 are the most, 
         despite that we only include part of the comments in 2018. We could conjecture 
         that school shootings are more serious and debated among people around 2017, so
         there are more comments begin from 2017. However, we still should admit that 2017
         has the most comments might also due to other circumstances. Most words in the word 
         cloud are negative words, although they contain the same emotions, we could detect
         that the main topic of school shootings among different years are slightly different. 
         real-time data, which is 2018's data is more focus on the judgment toward the shooter, 
         with words like judge, penalty. 2017's comments are mainly about the school's issue. 
         Moreover, 2014's comments have mentioned about words like government and crime.")
  })
  output$sentimentanalysis <- renderPrint({
    HTML(" I could conclude that, as we expected, most comments are negative and intense. 
         We could also conclude from the pie chart that anger has occupied a 52% of the comments 
         However, one thing that needs to notice is that people's anticipations or suggestions
         on the current situation have not been mentioned a lot in the comment, which only
         contains 28% but this might attribute to that the word 'gun',
         which implies the gun control is included in the anger sentiment.")
  })
  output$timeseries <- renderPrint({
    HTML(" According to the plots, we could see that there is a sharp increase in 
         2013 about the videos that related to school shootings. According to the data in the past, I guess that
         this is because the number of school shootings is doubled from 2012 and 2013, so there were 
         large amount of attention on this issue so the number of videos increases in the youtube. 
         2014 and 2015 have the similar number of videos about school shootings and this might also because.
         the number of school shootings in 2014 and 2014 are similar. The number of videos about shool 
         shootings is increasing from 2016 and 2017 because the number of school shootings also 
         decreased. However, the number of videos about school shootings in 2018 are decreasing 
         because this is only part of the data in 2018. Since the number of shool shootings 
         has already reached 17 cases in 2018, I predicted that the number of videos about 
         school shootings would also increase compare to previews years.")
  })
  output$frequent <- renderPrint({
    HTML("According to these graphs, although different years are having different most 
         frequent words, we could clearly see that the word 'gun' has 
         been highly mentioned in 2012, 2013, 2014, 2015, and 2018, which 
         is the real-time data. Although in 2016 and 2017 the word 'gun'
         has not been mentioned as the most frequent word, it is still in the rank of the top 
         ten mentioned words. This indicates that as the school shooting happening, the most 
         comments, besides the expression of sympathy to the children, would be the eagerness 
         of the gun control or things related to the gun. It is a continuous topic from 2012 to 2018
         that people keep discussing and debating about. The real-time data is only from 04/01 
         till 04/22, so it has a smaller amount of words compare to the rest.2012 also has a 
         small number of words because it is a long time ago and people might not be that active 
         online to comment during 2012 ")
  })
  output$introduction2 <- renderPrint({
    HTML("1. Word Cloud for real-time data",
         "2. Bar plot for the Top 10 words in the word cloud",
         "3. Sentiment Analysis and Pie Chart for real-time data",
         "4. Comparison Word cloud for data from 2012 to now",
         "5. Comparison of top 10 words in the word cloud from 2012 to now", 
         "6. Time Series on video related to school shootings")
  })
  }


shinyApp(ui, server)