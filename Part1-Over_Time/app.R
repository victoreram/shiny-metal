library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(plotly)
library(shinyWidgets)
library(gganimate)

df <- read.csv('../Data/albums.csv', stringsAsFactors = FALSE ) %>% 
  filter(!(genre_early_main %in% c("Rock", "Other"))) %>% 
  arrange(Release.date)
bands <- unique(df$Band)
doom_bands <- unique(df %>% filter(genre_early_main == 'Doom Metal') %>% arrange(Band) %>% pull(Band))
early_genres <- unique(df$genre_early_main)

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

plot_genres <- function(df_year){
  df_year %>%
    ggplot(aes(x=release_year, 
                      y=percent, 
                      # text = sprintf("Genre: %s
                      #                Number of Releases: %s,
                      #                Percent: %s",
                      #                genre_early_main, n, scales::percent(percent)),
                      fill=fct_rev(fct_inorder(genre_early_main))
  )) + 
    geom_area(position = 'stack') + 
    labs(x="Release Year", 
         y = "Percent of Releases", 
         fill = "Main Genre",
         title = "Percent of Metal Genre Releases By Year") + 
    #xlim(c(1970, 2018)) + ylim(c(0, 1)) + 
    scale_y_continuous(limits=c(0,1), labels = scales::percent, expand = c(0, 0)) + 
    scale_x_continuous(limits=c(1970, 2018), expand = c(0, 0)) + 
    scale_fill_manual(values = c("Black Metal" = "#000000", #black
                                 "Death Metal" = "#8f0000", #dark red
                                 "Thrash Metal" = "#7cf000", #light green
                                 "Doom Metal" = "#7e3f0c", #brown
                                 "Ambient" = "#7d7d7d", #gray
                                 "Power Metal" = "#f72bad", #pink
                                 "Heavy Metal" = "#1d00fa", #blue
                                 "Metalcore" = "#ee6917",
                                 "Nu Metal" = "#ffd60a",
                                 "Progressive Metal" = "#0adeff", #light blue
                                 "Folk Metal" = "#b120d9" #purple
    )
    ) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  
}

plot_years <- function(data) {
  data %>%
    ggplot(aes(x = release_year, 
               y = Average.rating,
               color = genre_early_main,
               group = Band)
    ) + 
    geom_line(alpha=0.1) + 
    geom_point(aes(size = Number.of.reviews,
                   text=sprintf("Band: %s
                                Album: %s
                                Release Date: %s
                                Genre: %s
                                Rating: %s
                                Number of Reviews: %s
                                Tags: %s", 
                                Band, Release, Release.date, genre_early_main, Average.rating, Number.of.reviews, genre_early_stripped)),
               alpha = 0.1
    ) + 
    labs(x= "Release Year", 
         y = "Rating", 
         color = "Main Genre",
         size = "Number of Reviews",
         title = "Metal Releases By Rating and Release Year") + 
    scale_color_manual(values = c("Black Metal" = "#000000", #black
                                  "Death Metal" = "#8f0000", #dark red
                                  "Thrash Metal" = "#3ddd03", #light green
                                  "Doom Metal" = "#7e3f0c", #brown
                                  "Ambient" = "#7d7d7d", #gray
                                  "Power Metal" = "#f72bad", #pink
                                  "Heavy Metal" = "#1d00fa", #blue
                                  "Metalcore" = "#ee6917",
                                  "Nu Metal" = "#ffd60a",
                                  "Progressive Metal" = "#0adeff", #light blue
                                  "Folk Metal" = "#b120d9" #purple
    )
    ) + 
    scale_y_continuous(limits=c(0,100),  expand = c(0, 0)) + 
    scale_x_continuous() + 
    #scale_x_date() + 
    scale_size_continuous(range=c(1,8)) + 
    theme_light() + 
    theme(#panel.grid.major = element_blank(), 
      #panel.grid.minor = element_blank(),
      #panel.background = element_blank(), 
      axis.line = element_line(colour = "black"),
      legend.position = 'none')
  #geom_jitter()
  
}

# plot_band <- function(data) {
#   data %>%
#     ggplot(aes(x = release_year, 
#                y = Average.rating,
#                color = genre_early_main,
#                group = Band)
#     ) + 
#     geom_line(alpha=0.5) + 
#     geom_point(aes(size = Number.of.reviews,
#                    text=sprintf("Band: %s
#                                 Album: %s
#                                 Release Date: %s
#                                 Genre: %s
#                                 Rating: %s
#                                 Number of Reviews: %s
#                                 Tags: %s", 
#                                 Band, Release, Release.date, genre_early_main, Average.rating, Number.of.reviews, genre_early_stripped)),
#                alpha = 0.5
#     )
  #   ) + 
  #   labs(x= "Release Year", 
  #        y = "Rating", 
  #        color = "Main Genre",
  #        size = "Number of Reviews",
  #        title = "Metal Releases By Rating and Release Year") + 
  #   scale_color_manual(values = c("Black Metal" = "#000000", #black
  #                                 "Death Metal" = "#8f0000", #dark red
  #                                 "Thrash Metal" = "#3ddd03", #light green
  #                                 "Doom Metal" = "#7e3f0c", #brown
  #                                 "Ambient" = "#7d7d7d", #gray
  #                                 "Power Metal" = "#f72bad", #pink
  #                                 "Heavy Metal" = "#1d00fa", #blue
  #                                 "Metalcore" = "#ee6917",
  #                                 "Nu Metal" = "#ffd60a",
  #                                 "Progressive Metal" = "#0adeff", #light blue
  #                                 "Folk Metal" = "#b120d9" #purple
  #   )
  #   ) + 
  #   scale_y_continuous(limits=c(0,100),  expand = c(0, 0)) + 
  #   scale_x_continuous() + 
  #   #scale_x_date() + 
  #   scale_size_continuous(range=c(1,8)) + 
  #   theme_light() + 
  #   theme(#panel.grid.major = element_blank(), 
  #     #panel.grid.minor = element_blank(),
  #     #panel.background = element_blank(), 
  #     axis.line = element_line(colour = "black"))
  # #geom_jitter()
  


# ggplotly(g1,
#          tooltip = "text",
#          width = 900, height = 600)
plot_albums <- function(data) {
  data %>%
    ggplot() + 
    geom_point(aes(x = Number.of.reviews, 
                   y = Average.rating,
                   color = genre_early_main,
                   text=sprintf("Band: %s
                                Album: %s
                                Release Date: %s
                                Genre: %s
                                Rating: %s
                                Number of Reviews: %s
                                Tags: %s", 
                                Band, Release, Release.date, genre_early_main, Average.rating, Number.of.reviews, genre_early_stripped)), 
               position = position_jitter(width = 0.5, height = 0.5),
               size = 1,
               alpha = 0.6
    ) + 
    labs(x="Number of Reviews", 
         y = "Rating", 
         color = "Main Genre",
         title = "Metal Releases By Rating and Number of Reviews") + 
    scale_color_manual(values = c("Black Metal" = "#000000", #black
                                  "Death Metal" = "#8f0000", #dark red
                                  "Thrash Metal" = "#7cf000", #light green
                                  "Doom Metal" = "#7e3f0c", #brown
                                  "Ambient" = "#7d7d7d", #gray
                                  "Power Metal" = "#f72bad", #pink
                                  "Heavy Metal" = "#1d00fa", #blue
                                  "Metalcore" = "#ee6917",
                                  "Nu Metal" = "#ffd60a",
                                  "Progressive Metal" = "#0adeff", #light blue
                                  "Folk Metal" = "#b120d9" #purple
    )
    ) + 
    scale_y_continuous(limits=c(0,100),  expand = c(0, 0)) + 
    scale_x_continuous(limits=c(1, max(data$Number.of.reviews)), expand = c(0, 0)) + 
    theme_light() + 
    theme(#panel.grid.major = element_blank(), 
      #panel.grid.minor = element_blank(),
      #panel.background = element_blank(), 
      axis.line = element_line(colour = "black"))
}
plotly_albums <- function(plot_obj){
  plot_obj %>% 
    ggplotly(tooltip = "text") %>%
    config(displayModeBar = F) %>%
    layout(xaxis=list(fixedrange=TRUE)) %>% 
    layout(yaxis=list(fixedrange=TRUE))
}

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
      tabPanel("Metal Genres Over The Years",
               fluidRow(
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
                             animate = animationOptions(interval = 1000, loop = TRUE)),
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
  
  output$plotly_ratings <- renderPlotly(
    albums() %>%
      plot_albums() %>%
      plotly_albums()
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
  output$genre_year <- renderPlot(
    df_year %>%
      plot_genres()
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
  #     plotly_albums()
  #     #plotly_albums()
  # )
  output$album_years_ly <- renderPlotly(
    selected_bands() %>%
      plotly_albums()
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
  
  
}
shinyApp(ui, server)