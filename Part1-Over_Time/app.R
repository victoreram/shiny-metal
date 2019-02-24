library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(plotly)
library(shinyWidgets)
library(gganimate)

source("plotting.R", local = TRUE)

df <- read.csv('../Data/albums.csv', stringsAsFactors = FALSE ) %>% 
  filter(!(genre_early_main %in% c("Rock", "Other"))) %>% 
  arrange(Release.date) %>%
  mutate(adj_rating = Average.rating - 100 + Number.of.reviews)
bands <- unique(df$Band)
doom_bands <- unique(df %>% filter(genre_early_main == 'Doom Metal') %>% arrange(Band) %>% pull(Band))
early_genres <- unique(df$genre_early_main)

df_genre_text <- read.csv('genre_text.csv', stringsAsFactors = F)

df_year <- df %>% 
  filter(!is.na(genre_early_main), 
         genre_early_main != '', 
         release_year >= 1970, 
         release_year < 2019) %>%
  group_by(release_year) %>% 
  count(genre_early_main) %>%
  mutate(percent = n/sum(n)) %>% 
  ungroup() %>% 
  mutate(genre_early_main = fct_reorder(genre_early_main, release_year)) %>% 
  select(-n) %>%
  tidyr::spread(key = genre_early_main, value = percent, fill = 0) %>%
  tidyr::gather(key = genre_early_main, value = percent, - release_year)
#summarize(a = count(genre_early_main)/total)# %>%
#count(genre_early_main)

df_year2 <- df %>% 
  filter(!is.na(genre_early_main), 
         genre_early_main != '', 
         release_year >= 1970, 
         release_year < 2019) %>%
  group_by(release_year) %>% 
  count(genre_early_main) %>%
  mutate(percent = n/sum(n)) %>% 
  ungroup() %>% 
  mutate(genre_early_main = fct_reorder(genre_early_main, release_year)) %>% 
  select(-n) %>%
  tidyr::spread(key = genre_early_main, value = percent, fill = 0)



# p <- plot_ly(data, x = ~year, y = ~Food.and.Tobacco, name = 'Food and Tobacco', type = 'scatter', mode = 'none', stackgroup = 'one', groupnorm = 'percent', fillcolor = '#F5FF8D') %>%
#   add_trace(y = ~Household.Operation, name = 'Household Operation', fillcolor = '#50CB86') %>%
#   add_trace(y = ~Medical.and.Health, name = 'Medical and Health', fillcolor = '#4C74C9') %>%
#   add_trace(y = ~Personal.Care, name = 'Personal Care', fillcolor = '#700961') %>%
#   add_trace(y = ~Private.Education, name = 'Private Education', fillcolor = '#312F44') %>%
#   layout(title = 'United States Personal Expenditures by Categories',
#          xaxis = list(title = "",
#                       showgrid = FALSE),
#          yaxis = list(title = "Proportion from the Total Expenditures",
#                       showgrid = FALSE,
#                       ticksuffix = '%'))
# 
# plotly_genres <- function(data){
#   data %>%
#    plot_ly(x=~release_year, y=percent)
# }

Previous_Button<-tags$div(actionButton("Prev_Tab",HTML('
&lt;div class="col-sm-4"&gt;&lt;i class="fa fa-angle-double-left fa-2x"&gt;&lt;/i&gt;&lt;/div&gt;
                                                       ')))
Next_Button<-div(actionButton("Next_Tab",HTML('
                                              &lt;div class="col-sm-4"&gt;&lt;i class="fa fa-angle-double-right fa-2x"&gt;&lt;/i&gt;&lt;/div&gt;
                                              ')))

ui <- fluidPage(
  # setBackgroundColor(
  #   color = c("#000000", "#B6B6B6"),
  #   gradient = "linear",
  #   direction = "bottom"
  # ),
  headerPanel(textOutput('title')),
  
  # tags$head(tags$style("#title{color: white;
  #                      font-size: 28px;
  #                      }"
  #                        )
  # ),
    tabsetPanel(
      tabPanel("The Genres",
               fluidRow(
                 h1("What's in a genre?", align="center"),
                 h5(em("Lay down your soul to the gods rock n' roll"), align="center"),
                 hr(),
                 p("Metal is a broad and diverse form of music with countless styles. Genres are a convenient way to set boundaries between distinct styles, but they are far from definitive. Countless productive hours have been spent on arguing if X band belongs to Y genre, but I had to draw the line somewhere. The categories laid out here are a combination of what's programmatically convenient and my own personal interpreations.")
               ),
               fluidRow(
                 hr(),
                 column(width = 3,
                        selectInput("genre_select1", "Select a genre:", choices = early_genres)
                 ),
                 column(width = 9, 
                        textOutput("genre_text"),
                        tags$style(type="text/css", "#genre_text {white-space: pre-wrap;}")
                 )
                 
               )
               ,
               fluidRow(
                 hr(),
                 h3(textOutput("genre_albums_text"), align = "center"),
                 br(),
                 tableOutput("albums_by_genre")
               )
               #fluidRow(plotlyOutput("genre_year_ly"))
               ),
      tabPanel("Genres Over The Years",
               fluidRow(
                 
                   sliderInput("year",
                               "Year",
                               min = 1970,
                               max = 2018,
                               value = 1970,
                               step = 1,
                               sep = "",
                               dragRange=FALSE,
                               animate = animationOptions(interval = 3000, loop = F)
                   )
                 
                 
                 #uiOutput("Next_Previous")
               ),
               fluidRow(
                 #plotlyOutput("genre_year_ly")
                 plotOutput("genre_year")
               )
               ),
      
      tabPanel(
        "Top Albums By Year",
        fluidRow(
          column(width = 6,
                 plotlyOutput("plotly_ratings"),
                 #plotOutput("plot_ratings"),
                 sliderInput("release_year",
                             "Release Year",
                             min = 1970,
                             max = 2018,
                             value = 1970,
                             step = 1,
                             sep = "",
                             dragRange=FALSE,
                             animate = animationOptions(interval = 3000, loop = F)),
                 checkboxGroupInput("genre_checkbox", "Genres", choices=early_genres, selected=early_genres)
                 
          ),
          column(width = 6,
                 h2(textOutput("top_albums")),
                 tableOutput("best_albums")
          )
        )
        
      ),
      tabPanel(
        "Metal Releases Over The Years",
        fluidRow(
          #plotOutput("album_years"),
          plotlyOutput("album_years_ly"),
          selectInput("band_select", "Select Band", choices = doom_bands,selected = "Black Sabbath"),
          #searchInput("band_search", "Search Band", value = "Black Sabbath", resetValue = "Black Sabbath"),
          numericInput("min_reviews", "Minimum Reviews", value = 1, min = 1, max = 40),
          selectInput("genre_select", "Genres", choices=early_genres, selected = "Doom Metal")
        )
      )

    )

    
    
  )
  


server <- function(input, output, session){
  output$title <- renderText({ paste("Only Data Is Real") })
  albums <- reactive({
    df %>% 
      filter(release_year == input$release_year,
             genre_early_main %in% input$genre_checkbox)
  })
  
  albums_by_genre <- reactive({
    df %>%
      filter(genre_early_main %in% input$genre_select1,
             Number.of.reviews >= 8) %>%
      arrange(desc(adj_rating)) %>% 
      rename(AlbumName = Release,
             ReleaseYear = release_year,
             LyricalThemes = Lyrical.themes) %>%
      distinct(Band, .keep_all=TRUE) %>%
      select(Band, AlbumName, ReleaseYear, LyricalThemes, Location) %>%
      head(10)
    
  })
  output$albums_by_genre <- renderTable({
    albums_by_genre()
  })
  output$genre_text <- renderText({
    df_genre_text %>% filter(genre_early_main == input$genre_select1) %>% pull(Text)
  })
  output$genre_albums_text <- renderText({
    sprintf("Notable %s Albums ", input$genre_select1)
    
  })
  output$plotly_ratings <- renderPlotly(
    albums() %>%
      plot_albums() %>%
      plotly_simple()
  )
  output$plot_ratings <- renderPlot(
    albums() %>%
      plot_albums()
  )
  output$best_albums <- renderTable(
    albums() %>% 
      select(Band, Release, genre_early_main, Number.of.reviews, Average.rating) %>%
      filter(Number.of.reviews >= 3) %>%
      mutate(adj_rating = Average.rating - 100 + Number.of.reviews) %>%
      arrange(desc(adj_rating)) %>%
      head(10) %>%
      rename(MainGenre = genre_early_main, 
             AlbumName = Release,
             NReviews = Number.of.reviews, 
             Rating = Average.rating,
             AdjRating = adj_rating)
  )
  genre_year_plot <- reactive({
    background <- df_year %>%
      plot_genres()
    foreground <- df_year %>% 
      plot_genres(a1 = 0.8, year = input$year)
    background + foreground
  })
  
  output$genre_year <- renderPlot(
    #genre_year_plot
    df_year %>% plot_genres(year = input$year)
  )
  output$genre_year_ly <- renderPlotly(
    df_year %>%
      plot_genres() %>%
      plotly_simple()
    
  )
  output$top_albums <- renderText(
    paste0("Top Albums of ", unique(albums()$release_year))
    
  )
  output$album_years <- renderPlot(
    df %>%
      filter(Band == input$band_search) %>%
      plot_years()
  )
  df_one_genre <- reactive({
    df %>% 
      filter(#Band == input$band_search, 
        genre_early_main == input$genre_select,
        Number.of.reviews >= input$min_reviews
      )
    }
  )
  df_multi_genre <- reactive({
    df %>%
      filter(genre_early_main %in% input$genre_checkbox)
  })
  selected_bands <- reactive({
    #print(df_one_genre %>% filter(Band == input$band_search %>% head()))
    background <- df_one_genre() %>% plot_years()
    selected <- background +
      geom_point(aes(size = Number.of.reviews,
                     fill = genre_early_main,
                     text=sprintf("Band: %s
                                Album: %s
                                  Release Date: %s
                                  Genre: %s
                                  Rating: %s
                                  Number of Reviews: %s
                                  Tags: %s", 
                                  Band, Release, Release.date, genre_early_main, Average.rating, Number.of.reviews, genre_early_stripped)),
                 alpha = 0.8,
                 shape = 21,
                 stroke = 1,
                 color = "green",
                 data = df_one_genre() %>% filter(Band == input$band_select)
                 ) +
      geom_line(alpha = 0.8,
                data = df_one_genre() %>% filter(Band == input$band_select)
                )
                              
    selected
  }) 
  #selected_bands <- df %>% 
  # output$album_years_ly <- renderPlotly(
  #   df %>% filter(Band == input$band_search,
  #                 Number.of.reviews >= input$min_reviews) %>%
  #     plot_years() %>%
  #     plotly_simple()
  #     #plotly_simple()
  # )
  output$album_years_ly <- renderPlotly(
    selected_bands() %>%
      plotly_simple()
  )

  observeEvent(df_one_genre(), {
    d <- df_one_genre()
    updateSelectInput(session,
                      "band_select", 
                      choices = d %>% arrange(Band) %>% pull(Band) %>% unique(),
                      selected = input$band_select
                      )
  }, ignoreInit = T)
  # observeEvent(input$genre_select, {
  #   updateSelectInput(input$band_select, choices = df_one_genre()$Band)
  # }, ignoreInit = T)

  # output$Next_Previous=renderUI({
  #   #tab_list=input$List_of_tab[-length(input$List_of_tab)]
  #   years <- df_year %>% arrange(year) %>% distinct(year) %>% pull(year)
  #   nb_tab=length(tab_list)
  #   nb_tab <- length(years)
  #   if (which(years==input$tabBox_next_previous)==nb_tab)
  #     column(1,offset=1,Previous_Button)
  #   else if (which(years==input$tabBox_next_previous)==1)
  #     column(1,offset = 10,Next_Button)
  #   else
  #     div(column(1,offset=1,Previous_Button),column(1,offset=8,Next_Button))
  # })


  
}
shinyApp(ui, server)