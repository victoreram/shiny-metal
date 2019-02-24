plot_genres <- function(df_year){
  df_year %>% 
    mutate(MainGenre = fct_inorder(genre_early_main)) %>%
    rename(Percent = percent) %>%
    ggplot(aes(x=release_year, 
               y=Percent, 
               # text = sprintf("Genre: %s
               #                Number of Releases: %s,
               #                Percent: %s",
               #                genre_early_main, n, scales::percent(percent)), # makes undesired art
               fill=MainGenre
    )
    ) + 
    geom_area(position = 'stack', alpha = 0.1) + 
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
plotly_simple <- function(plot_obj){
  plot_obj %>% 
    ggplotly(tooltip = "text") %>%
    config(displayModeBar = F) %>%
    layout(xaxis=list(fixedrange=TRUE)) %>% 
    layout(yaxis=list(fixedrange=TRUE))
}