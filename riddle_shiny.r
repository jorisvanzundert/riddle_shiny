library( shiny )
library( litRiddle )
library( RColorBrewer )
library( here )
library( ggplot2 )
library( waffle )

options( shiny.port = 4800 )
addResourcePath( 'resources', file.path( here(), 'resources' ) )
data( respondents )

waffle_palette <- rev( brewer.pal( 5, "Pastel2" ) )
bar_palette <- brewer.pal( 5, "Greens" )

ui <- bootstrapPage(
  titlePanel( "title panel" ),
  theme='resources/bootstrap_extensions.css',
  sidebarLayout( position="left",
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
      main=title, xlim=c( 0, 400 ), ylim=c( 0, 9000 ), xlab='Boeken per jaar', ylab='Aantal respondenten', cex=1.2, cex.lab=1.2, cex.main=1.5, col=bar_palette ) )
  })
  waffle_plot <- reactive({
    total_books_per_year <- sum( na.omit( respondents[ 'books.per.year' ] ) )
    books_per_year_at_age <- sum( na.omit( respondents[ respondents[ 'age.resp' ] >= input$age_range[1] & respondents[ 'age.resp' ] <= input$age_range[2], 'books.per.year' ] ) )
    vals <- c( books_per_year_at_age, total_books_per_year - books_per_year_at_age )
    vals <- round( vals/2000 )
    names( vals ) <- sprintf( '%s', scales::percent( round( vals/sum( vals ), 2 ) ) )
   # return( pie( slices, labels=lbls, main=paste( 'Totaal aantal boeken per jaar gelezen door\nmensen in de leeftijdsgroep van', input$age_range[1], 'tot', input$age_range[2], 'jaar' ),
   #    cex=1.2, cex.main=1.5, col=myPalette ) )
    waffle_title <- paste( 'Boeken gelezen per jaar door mensen\nin de leeftijdsgroep van', input$age_range[1], 'tot', input$age_range[2], 'jaar\nten opzichte van het totaal van alle\ngelezen boeken per jaar' )
    return( waffle( vals, size=1, colors=c( waffle_palette[1:2], 'white' ),
      title=waffle_title ) + theme( legend.position='bottom' ) )
  })
  output$plotgraph1 = renderPlot( waffle_plot() )
  output$plotgraph2 = renderPlot( hist_plot() )
}

shinyApp( ui=ui, server=server )
