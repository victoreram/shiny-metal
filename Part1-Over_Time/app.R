library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(plotly)
library(shinyWidgets)
library(gganimate)
library(glue)
library(scales)

source("plotting.R", local = TRUE)

df <- read.csv('../Data/albums.csv', stringsAsFactors = FALSE ) %>% 
  filter(!(genre_early_main %in% c("Rock", "Other"))) %>% 
  arrange(Release.date) %>%
  filter(Release.type %in% c("Full-length", "EP", "Live album", "Demo")) %>%
  mutate(adj_rating = Average.rating - 100 + Number.of.reviews,
         adj_rating = rescale(adj_rating))

df_min_5_reviews <- df %>% 
  filter(Number.of.reviews >= 5) %>%
  mutate(
    n_reviews_norm = rescale(log10(Number.of.reviews)),
    rating_norm = rescale(Average.rating^(3)),
    weighted_rating = rescale(sqrt(n_reviews_norm^2 + rating_norm^2))
    
  )
  
 

#df_albums_laid_out <- 
bands <- unique(df$Band)
doom_bands <- unique(df %>% filter(genre_early_main == 'Doom Metal') %>% arrange(Band) %>% pull(Band))
early_genres <- unique(df$genre_early_main)
release_types <- unique(df$Release.type)
df_genre_text <- read.csv('genre_text.csv', stringsAsFactors = F)
#df_no_ratings <- read.csv('../Data/albums_with_no_reviews.csv')

# df_year <- df %>% 
#   filter(!is.na(genre_early_main), 
#          genre_early_main != '', 
#          release_year >= 1970, 
#          release_year < 2019) %>%
#   group_by(release_year) %>% 
#   count(genre_early_main) %>%
#   mutate(percent = n/sum(n)) %>% 
#   ungroup() %>% 
#   mutate(genre_early_main = fct_reorder(genre_early_main, release_year))
# df_year <- df_year %>% 
#   select(-n) %>%
#   tidyr::spread(key = genre_early_main, value = percent, fill = 0) %>%
#   tidyr::gather(key = genre_early_main, value = percent, - release_year) %>%
#   left_join(
#     df_year %>%
#       select(-percent) %>%
#       tidyr::spread(key = genre_early_main, value = n, fill = 0) %>%
#       tidyr::gather(key = genre_early_main, value = n, - release_year)
#   ) %>% 
#   mutate(description = glue("<strong>{genre_early_main}</strong> made up of <strong>{scales::percent(percent)}</strong> ({n} total)"))
# 
df_year <- read.csv("../Data/albums_summary.csv")

tweaks <- 
  list(tags$head(tags$style(HTML("
                                 .multicol { 
                                   height: 180px;
                                   -webkit-column-count: 2; /* Chrome, Safari, Opera */ 
                                   -moz-column-count: 2;    /* Firefox */ 
                                   column-count: 2; 
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 } 
                                 ")) 
  ))


# values to show, or not show, these will be the 'choices' and 'selected' values
# for the checkboxGroupInput()
all_rows <- early_genres

controls <-
  list(
       tags$div(align = 'left', 
                class = 'multicol', 
                prettyCheckboxGroup(inputId  = 'all_albums_subgenres_checkbox', 
                                   label    = NULL, 
                                   choices  = all_rows,
                                   selected = all_rows,
                                   inline   = TRUE))) 

ui <- fluidPage(
  tweaks,
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
      tabPanel("The Subgenres",
               fluidRow(
                 h1("The Subgenres of Metal (According to Me)", align="center"),
                 h5(em("'Lay down your soul to the gods rock n' roll!'"), align="center"),
                 h5(em("- Venom, Black Metal, 1981"), align="center"),
                 hr(),
                 column(
                   11,
                   p("Metal is a broad and diverse form of music with countless styles.",
                   "Subgenres are a convenient way to set boundaries between distinct styles, but they are far from definitive.", 
                   "When metalheads aren't rattling their head to the sounds of filthy power chords, they're arguing on the internet or at a dingy dive bar over semantics about metal itself:",
                   "'Does band X belong in subgenre Y or Z?', 'Is band X even metal at all?'",
                   "Nothing will ever settle these debates (in fact I'll probably add fuel to this fire), so I will caveat the categorizations of certain bands here.",
                   "The categories laid out here are a combination of what's programmatically convenient and my own personal interpretations. ",
                   ""
                   ),
                   p("Let's start with defining what these genres even are. ",
                     "The uninitiated may wonder: 'what the hell is the difference between death metal and power metal anyway?'. ",
                     "Or, they may think that metal is just a jumbled mess of blast beats and incoherent screaming.",
                     "I don't blame you for thinking either of those things if your only exposure to metal is through baseball stadiums and rushedly walking past a Hot-Topic.",
                     "I'm here to show that there's much, much more to metal.",
                     "Below, you can learn about the main subgenres of metal and the most notable albums in that genre."
                     ),
                   offset=0.5
                   
                 )
                
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
      tabPanel("Over The Years",
               fluidRow(

                   h1("Over The Years", align="center"),
                   h5(em("'Wimps and posers, leave the hall'"), align="center"),
                   h5(em("- Manowar, Metal Warriors, 1992"), align="center"),
                   hr(),
                   column(
                     11,
                   p("'When and where did all of these subgenres come from?', you may be wondering.",
                     "Since Black Sabbath's Tony Iommi first struck the devil's tritone, countless bands mimicked that dark sound, injected new musical elements, and projected that into new forms of expression.",
                     "Just as Black Sabbath was a reaction against the hippy culture of the 1960's, thrash metal was a rebellion against the exploding glam scene of the early 80's.",
                     "And as thrash started to run its course, more extreme forms of metal such as death metal and black metal rose to prominence."
                   ),
                   p("The graph below shows you how the metal scene has evolved over time. To the right is a summary of the subgenre composition of metal for a given year."),
                   offset=0.5
                   
                 )
               ),
              fluidRow( 
              hr(),
                   column(8,
                          p("Use the slider below to see the landscape of metal for a given year."),
                          # tags$head(
                          #   tags$style(type="text/css", ".irs { max-width: 800px; }")
                          # ),
                          column(width = 6,
                            sliderInput("year",
                                        "Year",
                                        min = 1970,
                                        max = 2018,
                                        value = 1970,
                                        step = 1,
                                        sep = "",
                                        dragRange=FALSE,
                                        width = "100%",
                                        animate = animationOptions(interval = 3000, loop = F)
                                        ),
                            offset = 1
                          ),
                          plotOutput("genre_year_line"),
                          
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br()
                          #plotOutput("genre_year")
                          ),
                   column(4,
                          h2(textOutput("genre_current_year")),
                          br(),
                          htmlOutput("genre_year_html")
                          #verbatimTextOutput("genre_year_text")
                          )
                 #uiOutput("Next_Previous")
               ),
               fluidRow(
                 #plotlyOutput("genre_year_ly")
                 
               
              hr(),
              column(
                      11,
                      p("As you've seen, there have been many, many metal albums released over the years. How do we determine the best ones?",
                        "Thankfully, the many opinionated metalheads at metal-archives have contributed over 100,000 reviews across about 40,000 albums."
                        ),
                      p(
                        "Not all albums have reviews, so I filtered those out.",
                        "I then took all the albums with at least 1 review and calculated a 'Weighted Rating', a metric I devised* that weighs an album's average rating with number of reviews.",
                        "An album that has a high rating with many reviews should rank higher than an album with a high rating with few reviews.",
                        "The Weighted Rating is not immune from wonky rankings coming from albums would many reviews - I personally would not rank Wintersun's Time I over, say, Mgla's With Heart's Towards None, or many albums for that matter. ",
                        "But, it does handle popularity and rating for most cases."
                      ), 
                      p("*more like 'kinda just made up so it looks about right'"),
                      offset=0.5
                      ),
              br(),
              hr()
              ),
              fluidRow(
                hr(),
                #plotOutput("plot_ratings"),
                # column(6,
                #        selectInput("release_year",
                #                    "Release Year",
                #                    choices = unique(df_year$release_year)
                #        )
                # ),
                column(6,
                       selectInput("subgenres_top_albums",
                                   "Subgenre",
                                   choices = c("Metal", early_genres),
                                   selected = "Metal"
                       ),
                       offset = 0.5
                       
                ),
                # checkboxGroupInput("genre_checkbox", "Genres", 
                #                    choices=early_genres, 
                #                    selected=early_genres,
                #                    inline=TRUE)
                #plotlyOutput("plotly_ratings")
                br()
              
                
              ),
              fluidRow(
                h2(textOutput("top_albums"), align='center'),
                tableOutput("best_albums")
              )
               ),
      
      # tabPanel(
      #   "Top Albums By Year",
      #   fluidRow(
      #     
      #     h1("Top Albums By Year", align="center"),
      #     h5(em("'A lyric'"), align="center"),
      #     h5(em("- Band, Album, Year"), align="center"),
      #     hr(),
      #     column(
      #       11,
      #       p("As you've seen, there have been many, many metal albums released over the years. How do we determine the best ones?",
      #         "Thankfully, the many opinionated metalheads at metal-archives have contributed over 100,000 reviews across about 40,000 albums.",
      #         "Not all albums have reviews, so I filtered those out.",
      #         "I then took all the albums with at least 1 review and calculated an 'Adjusted Rating' that weighs an albums average rating by the number of reviews",
      #         "The formula for Adjusted Rating is simply Adjusted Rating = Average.rating - 100 + Number.of.reviews"),
      #       offset=0.5
      #       
      #       )
      #   )
      #   # hr(),
      #   # fluidRow(
      #   #          #plotOutput("plot_ratings"),
      #   #         column(6,
      #   #                selectInput("release_year",
      #   #                            "Release Year",
      #   #                            choices = unique(df_year$release_year)
      #   #                            )
      #   #                ),
      #   #         column(6,
      #   #                selectInput("subgenres_top_albums",
      #   #                            "Subgenre",
      #   #                            choices = c("Metal", early_genres),
      #   #                            selected = "Metal"
      #   #                            )
      #   # 
      #   #          ),
      #   #          # checkboxGroupInput("genre_checkbox", "Genres", 
      #   #          #                    choices=early_genres, 
      #   #          #                    selected=early_genres,
      #   #          #                    inline=TRUE)
      #   #          #plotlyOutput("plotly_ratings")
      #   #          
      #   #          
      #   #          h2(textOutput("top_albums"), align='center'),
      #   #          tableOutput("best_albums")
      #   #   
      #   # )
      #   
      # ),
      tabPanel(
        "The Albums",
        fluidRow(
          
          h1("The Albums", align="center"),
          h5(em("'A lyric'"), align="center"),
          h5(em("- Band, Album, Year"), align="center"),
          hr(),
          column(
            11,
            p("Alas, not every album is universally praised and noticed by metalheads.",
              ""
              ),
            p("Want to check out the universal classics? The upper right corner has the best albums that metal has ever produced.",
              "Go a bit lower and you'll see popular albums that received less critical praise.",
              "Typically these are the middle of the road, less-inspired offerings from popular bands, to put it nicely.",
              "Stray too far to the bottom, and you'll run into some of the most creatively bankrupt, what-the-hell-were-they-thinking-when-they-made-this albums of all time.",
              "You have been warned."
              ),
            p("Looking for an underground-ish gem? Check out some albums on the upper left corner.",
              "These albums received praise albeit from a few people.",
              "Most albums live in the mid to lower left region. ",
              "There's still plenty of good stuff here, mostly for people who can't get enough from a certain band or subgenre.",
              "At the bottom, you'll see some probably but not necessarily bad albums.",
              "Albums with fewer reviews are prone to fluctuations with their ratings.",
              "One angry 0% review can overpower a few other reviewers who positively reviewed an otherwise good album.",
              "Don't ignore some of the releases here as you may discover something you'd like."
              ),
            offset=0.5
            
          )
        ),
        hr(),
        fluidRow(
          column(4,
                 h4("Number of Reviews"),
                 numericInput("all_albums_min_reviews",
                              "Minimum Reviews",
                              value = 5,
                              min = 5
                 ),
                 numericInput("all_albums_max_reviews",
                              "Maximum Reviews",
                              value = 40,
                              min = 6,
                              max = 40
                 )
                 ),
          column(4,
                 h4("Subgenres"),
                 controls
                 ),
          column(4,
                 h4("Release Year"),
                 numericInput("all_albums_min_year",
                              "Minimum Year",
                              value = 1970,
                              min = 1970
                 ),
                 numericInput("all_albums_max_year",
                              "Maximum Year",
                              value = 2018,
                              min = 1971,
                              max = 2018
                 )
                 )
        ),
        # fluidRow(
        #   column(4,
        #          splitLayout(
        #            p("Show all albums between"),
        #            numericInput("all_albums_min_reviews",
        #                         NULL,
        #                         value = 5,
        #                         min = 5
        #                         ),
        #            p("   and   "),
        #            numericInput("all_albums_max_reviews",
        #                         NULL,
        #                         value = 40,
        #                         min = 6,
        #                         max = 40
        #            ),
        #            p("   reviews"),
        #            cellWidths = c("170", "90", "35", "90", "110")
        #          )
        #   ),
        #   offset = 0.5,
        #   br()
        # ),
        # fluidRow(
        #   br(),
        #   column(4,
        #          tweaks,
        #          controls,
        #          # splitLayout(p("from these subgenres: "),
        #          #             prettyCheckboxGroup("subgenres_checkbox",
        #          #                                NULL,
        #          #                                choices = early_genres,
        #          #                                selected = early_genres,
        #          #                                inline = FALSE),
        #          #             cellWidths = c("160", "600")
        #          # ),
        #          offset = 0.5
        #   )
        # ),
        # fluidRow(
        #   br(),
        #   column(4,
        #          splitLayout(p("released between"),
        #                      numericInput("all_albums_min_year",
        #                                   NULL,
        #                                   value = 1970,
        #                                   min = 1970
        #                      ),
        #                      p("   and   "),
        #                      numericInput("all_albums_max_year",
        #                                   NULL,
        #                                   value = 2018,
        #                                   min = 1971,
        #                                   max = 2018
        #                      ),
        #                      cellWidths = c("140", "100", "35", "100", "110")
        #                      )
        #          )
        # ),
        fluidRow(
          plotlyOutput("plotly_ratings")
          
        )
        
      ),
      tabPanel(
        "Releases By Band",
        fluidRow(h1("Releases By Band", align="center"),
                 h5(em("'So understand don't waste your time "), align="center"),
                 h5(em("Always searching for those wasted years'"), align="center"),
                 h5(em(" - Iron Maiden, Wasted Years, 1986"), align="center"),
                 column(
                   11,
                   p(
                   ),
                   offset=0.5
                   
                   ),
                 hr()
                 ),
        fluidRow(
          column(11,
            searchInput("band_select", 
                        "Search for a band", 
                        value = "Black Sabbath",
                        placeholder = "'Bathory', 'Death', etc.",
                        btnSearch = icon(name = "search"),
                        btnReset = icon("remove"),
                        resetValue = "Black Sabbath"
            ),
            checkboxGroupInput("release_types",
                               "Release Type",
                               choices = release_types,
                               selected = release_types,
                               inline=TRUE
            ),
            offset = 0.5
          )
          
          
          
          #plotOutput("album_years"),

          
          #searchInput("band_search", "Search Band", value = "Black Sabbath", resetValue = "Black Sabbath"),
          # numericInput("min_reviews", 
          #              "Minimum Reviews",
          #              value = 1, 
          #              min = 1, 
          #              max = 40
          #              ),
          # selectInput("genre_select", 
          #             "Genres", 
          #             choices = early_genres, 
          #             selected = "Heavy Metal"
          #             )
        ),
        fluidRow(
          plotlyOutput("album_years_ly")
          
        )
      )

    )

    
    
  )
  


server <- function(input, output, session){
  output$title <- renderText({ paste("Only Data Is Real") })
  albums <- reactive({
    df_min_5_reviews %>% 
      filter(release_year == input$year
             #genre_early_main %in% input$genre_checkbox)
      )
  })
  

  albums_by_genre <- reactive({
    df_min_5_reviews %>%
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
  
  all_albums_laid_out <- reactive({
    df_min_5_reviews %>% 
      filter(Number.of.reviews >= input$all_albums_min_reviews,
             Number.of.reviews <= input$all_albums_max_reviews,
             genre_early_main %in% input$all_albums_subgenres_checkbox,
             release_year >= input$all_albums_min_year,
             release_year <= input$all_albums_max_year)
    
  })
  output$plotly_ratings <- renderPlotly(
    all_albums_laid_out() %>%
      plot_albums() %>%
      plotly_simple()
  )
  output$plot_ratings <- renderPlot(
    albums() %>%
      plot_albums()
  )
  best_albums_reactive <- reactive({
    df_best_albums <- albums() %>% 
      filter(Number.of.reviews >= 5) %>%
      select(Band, Release, genre_early_main, Lyrical.themes, Location, Number.of.reviews, Average.rating, adj_rating) %>%
      #mutate(adj_rating = Average.rating - 100 + Number.of.reviews) %>%
      arrange(desc(adj_rating)) %>%
      rename(MainGenre = genre_early_main, 
             AlbumName = Release,
             NReviews = Number.of.reviews, 
             LyricalThemes = Lyrical.themes,
             Rating = Average.rating,
             WeightedRating = adj_rating)
    if(input$subgenres_top_albums == 'Metal'){
      return(df_best_albums)
    } else {
      return(df_best_albums %>% 
               mutate(MainGenre = as.character(MainGenre)) %>%
               filter(MainGenre == input$subgenres_top_albums))
    }
  })
  output$best_albums <- renderTable({
    best_albums_reactive() %>% head(10)
  })
  genre_year_plot <- reactive({
    background <- df_year %>%
      plot_genres()
    foreground <- df_year %>% 
      plot_genres(a1 = 0.8, year = input$year)
    background + foreground
  })
  output$genre_year_line <- renderPlot(
    df_year %>% plot_genres_line(year = input$year)
  )
  genre_year_text <- reactive({
    df_genre_year_text <- df_year %>% 
      filter(release_year==input$year) %>%
      pull(description)
    #print(glue_collapse(df_genre_year_text, sep = "\n"))
    glue_collapse(df_genre_year_text, sep = "\n")
  })
  output$genre_current_year <- renderText(
    sprintf("In %s...", input$year)
  )
  output$genre_year_text <- renderText(
    genre_year_text()
  )
  genre_year_html <- reactive({
    df_genre_year_text <- df_year %>% 
      filter(release_year==input$year)
    descriptions <- df_genre_year_text %>%
      pull(description)
    #print(glue_collapse(df_genre_year_text, sep = "\n"))
    total_albums <- df_genre_year_text %>% 
      summarize(total = sum(n)) %>% 
      mutate(total = glue("<h3>There were <strong>{as.character(total)}</strong> metal releases</h3>")) %>%
      pull(total)
    
    glue(total_albums, glue_collapse(descriptions, sep = "<br/>"))
  })
  output$genre_year_html <- renderUI(
    HTML(genre_year_html())
  )
  
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
    sprintf("Top %s Albums of %s", 
            input$subgenres_top_albums, 
            input$year)
    
  )
  output$album_years <- renderPlot(
    df %>%
      filter(Band == input$band_search) %>%
      mutate(release_year = as.integer(release_year)) %>%
      plot_years()
  )
  df_one_genre <- reactive({
    df %>% 
      filter(tolower(Band) == tolower(input$band_select),
             Release.type %in% input$release_types
        #genre_early_main == input$genre_select,
        #Number.of.reviews >= input$min_reviews
      )
    }
  )
  df_multi_genre <- reactive({
    df %>%
      filter(genre_early_main %in% input$genre_checkbox)
  })
  selected_bands <- reactive({
    validate(
      need(df_one_genre() %>% nrow > 0, "No albums found!")
    )
    #print(df_one_genre %>% filter(Band == input$band_search %>% head()))
    #background <- df_one_genre() %>% plot_years()
    #selected <- background +
    # geom_point(aes(size = Number.of.reviews,
    #                fill = genre_early_main,
    #                text=sprintf("Band: %s
    #                             Album: %s
    #                             Release Date: %s
    #                             Genre: %s
    #                             Rating: %s
    #                             Number of Reviews: %s
    #                             Tags: %s", 
    #                             Band, Release, Release.date, genre_early_main, Average.rating, Number.of.reviews, genre_early_stripped)),
    #            alpha = 0.8,
    #            shape = 21,
    #            stroke = 1,
    #            color = "green",
    #            data = df_one_genre() %>% filter(Band == input$band_select)
    # ) +
    #   geom_line(alpha = 0.8,
    #             data = df_one_genre() %>% filter(Band == input$band_select)
    #   )
    data <- df_one_genre() %>% 
      rename(Year = release_year,
             Rating = Average.rating) 
    selected <- data %>%
      ggplot(aes(x=Year, y=Rating)) +
      geom_point(aes(size = Number.of.reviews,
                     fill = genre_early_main,
                     text=sprintf("Band: %s
                                Album: %s
                                  Release Date: %s
                                  Genre: %s
                                  Rating: %s
                                  Number of Reviews: %s
                                  Tags: %s", 
                                  Band, Release, Release.date, genre_early_main, Rating, Number.of.reviews, genre_early_stripped)),
                 alpha = 0.8,
                 shape = 21,
                 stroke = 1,
                 color = "green"
                 #data = df_one_genre() %>% filter(Band == input$band_select)
                 ) +
      geom_line(alpha = 0.8
                #data = df_one_genre() %>% filter(Band == input$band_select)
                ) + 
      scale_y_continuous(limits=c(0,100),  expand = c(0.01, 0.01)) + 
      scale_x_continuous(breaks = pretty_breaks()) +
      theme(axis.title.x=element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            #axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            #axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            legend.position="none"
            
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
  available_bands <- reactive({
    df_one_genre() %>% 
      arrange(Band) %>% 
      pull(Band) %>% 
      unique()
    
  })
  
  #observeEvent()
  # observeEvent(df_one_genre(), {
  #   #d <- df_one_genre()
  #   updateSelectInput(session,
  #                     "band_select", 
  #                     choices = available_bands(),
  #                     selected = available_bands()[1]
  #                     )
  # }, ignoreInit = T)
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