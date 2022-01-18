library(shiny)
library(wordcloud2)
#source("/home/sol-nhl/dev/r-cran/scom-r/src/shiny-wordcloud/wordcloud-functions.R")

# Define UI for data upload app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Uploading Files"),

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

      # Output: Histogram ----
      plotOutput(outputId="cloud_plot")
#wordcloud2Output(outputId="cloud_plot")

      # Output: Data file ----
#      tableOutput("contents")

    )

  )
)

# Define server logic to read selected file ----
server <- function(input, output) {

get_input <- reactive({
if (exists("input$file1")) {
df = data.frame(feature=c("word","cloud"), frequency=c(50,100))
} else if (!is.null(input$file1)){
df = data.frame(feature=c("some","cloud"), frequency=c(50,100))
}
})

  output$cloud_plot <- renderPlot({

df = get_input()

#if(exists("input$file1")){
#    df = data.frame(feature=c("word","cloud"), frequency=c(50,100))
#    return(df)
#}else{
#    df = data.frame(feature=c("some","cloud"), frequency=c(50,100))
#}

#if(nrow(df)<2){
#df = data.frame(feature=c("word","cloud"), frequency=c(50,100))
#}else{
#df = read.csv('/home/sol-nhl/dev/r-cran/scom-r/csv/org-emo.csv', header=T, sep="\t", strip.white=TRUE, stringsAsFactors=FALSE)
#df = get_cloud(df)
#}

df = as.data.frame(df[,1:2])
#quanteda::textplot_wordcloud(socm_dfm, max_words=200)
wordcloud::wordcloud(words=df$feature, freq=df$frequency, min.freq=1, max.words=200, random.order=FALSE, rot.per=0.35, colors=RColorBrewer::brewer.pal(8, "Dark2"), scale=c(4.0, 0.5))
#wordcloud2::wordcloud2(data=df, size=1.0, minSize=0.1, color='random-dark')

    })

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

#

    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }

  })

}

# Create Shiny app ----
shinyApp(ui, server)
