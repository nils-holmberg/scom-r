#~ ##################################################################### 200608
library(shiny)

# Define UI for data upload app ----
ui <- fluidPage(

  # App title ----
  titlePanel("get wordcloud"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),

      # Horizontal line ----
      tags$hr(),

      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),

      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),

      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),

      # Horizontal line ----
      tags$hr(),

      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Data file ----
#      tableOutput("contents")

      # Output: Histogram ----
      plotOutput(outputId = "cloud_plot")

    )

  )
)

#~ ##################################################################### 200608
# Define server logic to read selected file ----
server <- function(input, output) {

  output$contents <- renderTable({

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    req(input$file1)

    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                 header = input$header,
                 sep = input$sep,
                 quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )

    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }

  })

#df = get_cloud(df)
#if(nrow(df)<2){
#df = data.frame(feature=c("word","cloud"), frequency=c(50,100))
#}

  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$cloud_plot <- renderPlot({

#    x    <- faithful$waiting
#    bins <- seq(min(x), max(x), length.out = input$bins + 1)

#    hist(x, breaks = bins, col = "#75AADB", border = "white",
#         xlab = "Waiting time to next eruption (in mins)",
#         main = "Histogram of waiting times")

if(nrow(df)<2){
df = data.frame(feature=c("word","cloud"), frequency=c(50,100))
}else{
df = get_cloud(df)
}

df = as.data.frame(df[,1:2])
#textplot_wordcloud(socm_dfm, max_words=200)
wordcloud(words=df$feature, freq=df$frequency, min.freq=1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"), scale=c(4.0, 0.5))
#wordcloud2(data=df, size=1.0, minSize=0.1, color='random-dark')
#renderWordcloud2(df)

    })

}

# Create Shiny app ----
shinyApp(ui, server)
