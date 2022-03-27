library(shiny)
library(stringr)
library(tidyverse)

ui <- fluidPage(
  titlePanel("Wordle Word Finder"),
  HTML('Place the letters that you know in the "Use these:" section. Place all other letters in the "Don\'t use these:" section. Then click "Find Words" to obtain all possible words from 12dicts 2of12inf.txt dictionay.</p>'),
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "letters_req",
                label = "Use these:", 
                value = ""),
      textInput(inputId = "letters_opt",
                label = "Don't use these:", 
                value = ""),
      textInput(inputId = "letters_loc",
                label = "Known locations (example: *ha*n):", 
                value = ""),
      actionButton(inputId = "find_words", 
                   label = "Find Words"),
    ),
    
    mainPanel(
      tableOutput(outputId = "myTable")
    )
  )
)

server <- function(input, output) {
  #This dictionay came from 12dict's 2of12inf.txt file, filtering to 5 letter words. 
  words <- read_lines("five_letter_words.txt")
  my_dat <- tibble(word = words) %>% 
    mutate(matches = c(0)) %>% 
    mutate(removes = c(0))
  
  my_table <- eventReactive(input$find_words, {
    letters_required <- input$letters_req %>% str_to_lower() %>% str_remove_all("[^[:alpha:]]")
    letters_optional <- input$letters_opt %>% str_to_lower() %>% str_remove_all("[^[:alpha:]]")
    letters_location <- input$letters_loc %>% str_to_lower()

    # letters_required <- "hny"
    # letters_optional <- "ae"
    # letters_location <- "h****"
    
    wanted_letters_min <- str_length(letters_required)
    wanted_letters_max <- str_length(letters_required)+str_length(letters_optional)
    
    my_let_table <- 
      tibble(letter = letters_required %>% str_split("") %>% unlist()) %>% 
      distinct()%>%
      mutate(freq = str_count(letters_required,letter))
    unique_letters <- my_let_table %>% pull(letter)
    letters_freq <- my_let_table %>% pull(freq)
    
    req_dict <- my_dat
    
    i <- 0
    while(i < length(unique_letters)){
      i <- i+1
      req_dict <- req_dict %>% 
        mutate(matches = matches + pmin(str_count(word,unique_letters[i]),letters_freq[i]))
    }
    
    req_dict <- req_dict %>% 
      filter(matches >= wanted_letters_min)

    req_dict_updated <- req_dict
    
    my_let_table_opt <- 
      tibble(letter = letters_optional %>% str_split("") %>% unlist()) %>% 
      distinct()%>%
      mutate(freq_opt = str_count(letters_optional,letter)) %>% 
      left_join(my_let_table, by = "letter") %>% 
      mutate(freq=replace_na(freq,0))
    unique_letters_opt <- my_let_table_opt %>% pull(letter)
    letters_freq_opt <- my_let_table_opt %>% pull(freq_opt)
    letters_freq_used <- my_let_table_opt %>% pull(freq)
    
    i <- 0
    while(i < length(unique_letters_opt)){
      i <- i+1
      req_dict_updated <- req_dict_updated %>% 
        mutate(removes = removes + pmin(str_count(word,unique_letters_opt[i])-letters_freq_used[i],letters_freq_opt[i]))
    }
    
    req_dict_updated <- req_dict_updated %>% 
      filter(removes == 0)
    
    # letters_location <- paste0("*h*","*****")
    let_loc <- letters_location %>% str_split("") %>% unlist()
    i <- 0
    while(i < 5){
      i <- i+1
      if(let_loc[i] %in% letters){
        req_dict_updated <- 
          req_dict_updated %>% 
          mutate(keeps = (str_locate(word,let_loc[i])[,"start"]==i)) %>% 
          filter(keeps == TRUE)
      }
    }
    
    req_dict_updated %>% select(word)
    
    
  } )
  
  output$myTable <- renderTable(my_table())
}

shinyApp(ui = ui, server = server)
