# ===============================================
# Fill in the following fields
# ===============================================
# Title: Text Analysis of U2 Lyrics
# Author: Priya Reddy 
# Date: 12/03/21


# ===============================================
# Packages
# ===============================================
library(tidyverse)
library(tidytext)
library(textdata)
library(shiny)


# ===============================================
# Import data
# ===============================================
u2_lyrics <- read_csv("u2-lyrics2.csv")
sentiments = readRDS("bing.rds")

# ===============================================
# Define UserInterface "ui" for application
# ===============================================

ui <- fluidPage(
  titlePanel("Textual Analysis of U2 Lyrics"),
  h4('By Priya Reddy'),
  fluidRow( 
    column(4,
           
           #choose whether to remove stopwords
           br(),
           br(),
           br(),
           p(em("Should the analysis be run without stopwords like 'the', 'of', and 'a'?")),
           radioButtons(inputId = "stopwords", 
                        label = "Remove Stopwords?", 
                        choices = c("Remove" = "removestop",
                                    "Keep" = "keepstop"), 
                        selected = "removestop")
    ),
    
    column(4,
           
           #choose how many of the top words
           p(h4(em("Word Frequency Analysis"))),
           hr(),
           sliderInput(inputId = "topselection", 
                       label = "Show Top __ Words:",
                       min = 1,
                       max = 20,
                       value = 5,
                       step = 1),
          
           
           #choose an album to filter by
           selectInput(inputId = "album_name", 
                       label = "In Which Album?",
                       choices = c('No Specific Album' = 'none',
                                   'Boy' = 'Boy', 
                                   'October' = 'October', 
                                   'War' = 'War',
                                   'The Unforgettable Fire'= 'The Unforgettable Fire',				
                                   'The Joshua Tree' = 'The Joshua Tree',				
                                   'Rattle And Hum' = 'Rattle And Hum',				
                                   'Achtung Baby' = 'Achtung Baby',				
                                   'Zooropa' = 'Zooropa',				
                                   'Passengers' = 'Passengers',				
                                   'Pop' = 'Pop',
                                   'The Best Of 1980-1990'= 'The Best Of 1980-1990',				
                                   'All That You Cant Leave Behind' = 'All That You Cant Leave Behind',				
                                   'How To Dismantle An Atomic Bomb' = 'How To Dismantle An Atomic Bomb',				
                                   'Medium Rare Remastered' = 'Medium Rare Remastered',				
                                   'No Line On The Horizon' = 'No Line On The Horizon',				
                                   'Songs Of Innocence' = 'Songs Of Innocence',				
                                   'Songs Of Experience' = 'Songs Of Experience'), 
                       selected = 'none'),
           
           #choose between comparing by album or among all albums
           p(em('-OR-')),
           radioButtons(inputId = "byalbum",
                        label = 'Top words among all albums or compared between albums?',
                        choices = c("All" = "all_albums",
                                    "Compare Between Albums" = "by_album"),
                        selected = "all_albums"),
           p(h6(em("Note: This button choice will only work if 'No Specific Album' is selected in the 'Which Album' Dropdown menu")))
    ),
    

    column(4,
           p(h4(em("Sentiment Analysis"))),
           hr(),
           #sentiment analysis by album or by song
           radioButtons(inputId = "album_song", 
                       label = "Breakdown each Album's  Sentiment Score by Song?",
                       choices = c('No' = 'sentalbum',
                                   'Yes' = 'sentsong'), 
                       selected = 'sentalbum')
    ),
  ),
  hr(),
  
  tabsetPanel(type = "tabs",
              tabPanel("Word Frequency Analysis",
                       h3("Word Frequency Analysis"),
                       plotOutput("plot1", height = '600px'),
                       hr(),
                       h4('Table of Most Common Words in the entire U2 Discography' ),
                       h5(em('Adjust the subset shown with the slider at the top (ex. Top 10 most common)')),
                       dataTableOutput('table1')),
              
              tabPanel("Sentiment Analysis", 
                       h3("Sentiment Analysis"),
                       plotOutput("plot2", height = '700px'),
                       hr(),
                       tableOutput('table2'),
                         column(6,
                                h4('Top 5 Most Positive U2 Songs'),
                                h5(em('From the entire U2 discography')),
                                tableOutput('table3')),
                         column(6,
                                h4('Top 5 Most Negative U2 Songs'),
                                h5(em('From the entire U2 discography')),
                                tableOutput('table4'))
              )
                       
              
  )
)


# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {
  
  #dataframe for the word frequency analysis
  dat1 <- reactive({
    if (input$stopwords == 'removestop'){
      unnest_tokens(tbl = u2_lyrics, output = word, input = lyrics) %>%
        anti_join(stop_words, by = "word") %>%
        group_by(album) %>% 
        add_count(word, name = "count") %>% 
        select(-song)%>%
        distinct()
    }
    else {
      unnest_tokens(tbl = u2_lyrics, output = word, input = lyrics) %>% 
        group_by(album) %>% 
        add_count(word, name = "count")%>% 
        select(-song)%>%
        distinct()
    }
  })
  
  #dataframe for sentiment analysis
  dat2 <- reactive({
    
    words_per_song <- u2_lyrics %>% 
      group_by(song) %>%
      summarise(num_words = str_count(lyrics, "\\w+"))
    
    if (input$stopwords == 'removestop'){
      unnest_tokens(tbl = u2_lyrics, output = word, input = lyrics) %>%
        anti_join(stop_words, by = 'word') %>%
        add_count(word, name = "count") %>%
        inner_join(sentiments, by = "word") %>%
        inner_join(words_per_song, by = "song")%>%
        count(album, song, year, sentiment, num_words) %>%
        spread(sentiment, n, fill = 0)%>%
        mutate(sentiment = ((positive - negative)/num_words)*100)
    }
    else{
      unnest_tokens(tbl = u2_lyrics, output = word, input = lyrics) %>%
        add_count(word, name = "count") %>%
        inner_join(sentiments, by = "word") %>%
        inner_join(words_per_song, by = "song")%>%
        count(album, song, year, sentiment, num_words) %>%
        spread(sentiment, n, fill = 0)%>%
        mutate(sentiment = ((positive - negative)/num_words)*100)
    }
  
    
  })
  
  
  # ===============================================
  # Outputs for the first TAB (i.e. word frequency)
  # ===============================================
  
  # code for barplot
  output$plot1 <- renderPlot({
    
    if(input$album_name != 'none'){
      dat1() %>%
        filter(album == input$album_name)%>%
        ungroup()%>%
        group_by(word)%>%
        summarise(n = sum(count))%>%
        arrange(desc(n)) %>%
        slice_head(n = input$topselection) %>%
        ggplot(aes(x = reorder(word, n), y = n, fill = word)) +
        geom_col(show.legend = FALSE) + 
        labs(title = paste('Top', input$topselection, 'most frequent words in the album', input$album_name)) +
        xlab("Word") +
        ylab("Number of Occurrences") + 
        coord_flip() +
        theme_minimal()+ 
        geom_text(aes(label = n), vjust = 0, hjust = 0, nudge_y = 0.75, size = 5)+
        theme(plot.title = element_text(size = 20), axis.title = element_text(size = 16))
    }
    
    else if (input$byalbum == "by_album"){
      dat1() %>%
        arrange(desc(count)) %>%
        slice_head(n = input$topselection) %>%
        ggplot(aes(x = reorder(word, count), y = count, fill = word)) +
        geom_col(show.legend = FALSE) + 
        facet_wrap(~ album, scales = "free")+
        labs(title = paste('Top', input$topselection, 'most frequent words per album')) +
        xlab("Word") +
        ylab("Number of Occurrences") + 
        coord_flip() +
        theme_minimal()+ 
        geom_text(aes(label = count), vjust = 0, hjust = 0, nudge_y = 0.75, size = 2)+
        theme(plot.title = element_text(size = 20), 
              axis.title = element_text(size = 16))
    }
    else if (input$byalbum == 'all_albums'){
      dat1() %>%
        ungroup()%>%
        group_by(word)%>%
        summarise(n = sum(count))%>%
        arrange(desc(n)) %>%
        slice_head(n = input$topselection) %>%
        ggplot(aes(x = reorder(word, n), y = n, fill = word)) +
        geom_col(show.legend = FALSE) + 
        labs(title = paste('Top', input$topselection, 'most frequent words among all U2 albums')) +
        xlab("Word") +
        ylab("Number of Occurrences") + 
        coord_flip() +
        theme_minimal()+ 
        geom_text(aes(label = n), vjust = 0, hjust = 0, nudge_y = 0.75, size = 5)+
        theme(plot.title = element_text(size = 20), axis.title = element_text(size = 16))
    }
  })
  
  #code for summaries of frequencies
  output$table1 <- renderDataTable({
    dat1()%>%
        ungroup()%>%
        group_by(word)%>%
        summarise(n = sum(count))%>%
        arrange(desc(n)) %>%
        rename('Word'= 'word', 'Occurences' = 'n')%>%
        slice_head(n = input$topselection)
    
  })
  
  
  # =======================================================
  # Outputs for the second TAB (i.e. sentiment analysis)
  # =======================================================
  
  # code for histogram
  output$plot2 <- renderPlot({
    if (input$album_song == 'sentalbum'){
      dat2()%>%
      group_by(album)%>%
      summarize(tot_sentiment = sum(sentiment))%>%
      ggplot(mapping = aes(x = reorder(album, tot_sentiment), y = tot_sentiment, fill = tot_sentiment)) +
      geom_col() +
      theme_bw() +
      coord_flip()+
      labs(title = "Overall Sentiment of U2 albums", 
           subtitle = str_wrap('Using the Bing lexicon where a more negative score indicates a more negative sentiment and a more positive score indicates a more positive sentiment. Scores are normalized by the number of words in each song/album, and then multiplied by 100 for visual clarity.', 120),
           fill = 'Sentiment Score')+
      xlab('U2 Albums')+
      ylab('Overall Sentiment Score')+
      theme(plot.title = element_text(size = 20), axis.title = element_text(size = 16))
    }
    else{
      dat2()%>%
        select(-year) %>% 
        #distinct() %>% #remove duplicate songs (remastered albums contain duplicate songs)
        ggplot(mapping = aes(x = reorder(song, sentiment), y = sentiment, fill = sentiment)) +
        geom_col() +
        facet_wrap(~album, scales = 'free', ncol = 3)+
        theme_minimal() +
        coord_flip()+
        labs(title = "Sentiment of U2 Songs in Each Album", 
             subtitle = str_wrap('Using the Bing lexicon where a more negative score indicates a more negative sentiment and a more positive score indicates a more positive sentiment. Scores are normalized by the number of words in each song/album, and then multiplied by 100 for visual clarity.', 120),
             fill = 'Sentiment Score')+
        xlab('U2 Songs')+
        ylab('Sentiment Score')+
        theme(plot.title = element_text(size = 20), 
              axis.title = element_text(size = 16),
              axis.text = element_text(size = 5)
              )
    }
  })
  

  # code for statistics
  output$table2 <- renderTable({
    mean_sent <- dat2()%>%
      summarise('Mean Sentiment Score of Entire U2 Discography' = mean(sentiment)) 

  })
  
  output$table3 <- renderTable({
    dat2()%>%
      arrange(desc(sentiment)) %>%
      slice_head(n=5) %>%
      rename('Album'= 'album', 'Song Title' = 'song', 'Sentiment Score' = 'sentiment')%>%
      select('Song Title', 'Album', 'Sentiment Score', -positive, -negative, -year)
  })
  
  output$table4 <- renderTable({
    dat2()%>%
      arrange(sentiment) %>%
      slice_head(n=5) %>%
      rename('Album'= 'album', 'Song Title' = 'song', 'Sentiment Score' = 'sentiment')%>%
      select('Song Title', 'Album', 'Sentiment Score', -positive, -negative, -year)
  })
  
}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

