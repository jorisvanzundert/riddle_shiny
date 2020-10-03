library( shiny )
library( litRiddle )
library(RColorBrewer)

options( shiny.port = 4800 )

data( respondents )

myPalette <- rev( brewer.pal( 5, "Pastel2" ) )
rev( myPalette )

ui <- fluidPage(
  titlePanel( "title panel" ),
  sidebarLayout( position = "left",
    sidebarPanel( "sidebar panel",
      sliderInput(
        inputId='age_range',
        label='Gebruik de schuifregelaar om een leeftijdsgroep te kiezen',
        value=c( 25, 35 ), min=min( respondents[ 'age.resp' ] ), max=max( respondents[ 'age.resp' ] )
      )
    ),
    mainPanel( "main panel",
      fluidRow(
        splitLayout( cellWidths = c( '50%', '50%' ), plotOutput( 'plotgraph1' ), plotOutput( 'plotgraph2' ) )
      )
    )
  )
)

server <- function( input, output ){
  hist_plot <- reactive({
    title <- paste( 'Verdeling van het aantal boeken per jaar binnen\nde groep van', input$age_range[1], 'tot', input$age_range[2], 'jaar' )
    return( hist( respondents[ respondents[ 'age.resp' ] >= input$age_range[1] & respondents[ 'age.resp' ] <= input$age_range[2], 'books.per.year' ], breaks=seq( 0, 700, by=25 ),
      main=title, xlim=c( 0, 400 ), ylim=c( 0, 9000 ), xlab='Boeken per jaar', ylab='Aantal respondenten', cex=1.2, cex.lab=1.2, cex.main=1.5, col=myPalette ) )
  })
  pie_plot <- reactive({
    total_books_per_year <- sum( na.omit( respondents[ 'books.per.year' ] ) )
    total_books_per_year
    books_per_year_at_age <- sum( na.omit( respondents[ respondents[ 'age.resp' ] >= input$age_range[1] & respondents[ 'age.resp' ] <= input$age_range[2], 'books.per.year' ] ) )
    slices <- c( total_books_per_year - books_per_year_at_age, books_per_year_at_age )
    lbls <- c( 'Anderen', paste( input$age_range[1], 'tot', input$age_range[2] ,'jaar' ) )
    return( pie( slices, labels=lbls, main=paste( 'Totaal aantal boeken per jaar gelezen door\nmensen in de leeftijdsgroep van', input$age_range[1], 'tot', input$age_range[2], 'jaar' ),
      cex=1.2, cex.main=1.5, col=myPalette ) )
  })
  output$plotgraph1 = renderPlot( pie_plot() )
  output$plotgraph2 = renderPlot( hist_plot() )
}

shinyApp( ui=ui, server=server )
