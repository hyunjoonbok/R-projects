
setwd("C:/Users/bokhy/Documents/R-projects/Title_Recommender_with_market_basket_analysis/")
library(shiny)

# Load algorithm implementations and similarity calculations
source("cf_algorithm.R")
source("similarity_measures.R")
past_orders_matrix <- readRDS("past_orders_matrix.rds")
item_list <- readRDS("item_list.rds")

ui <- fluidPage(
  
  # App title ----
  headerPanel("Title Recommender for Aracde"),
  
  fluidRow(
    
    # Input selection
    column(6, 
           # INPUT
           h3("Select Titles and Complete Suggestions"),    
           wellPanel(
             selectInput("input_item1", "Title #1", choices = c("", item_list)),
             selectInput("input_item2", "Title #2", choices = c("", item_list)),
             selectInput("input_item3", "Title #3", choices = c("", item_list)),
             actionButton("submit", "Complete the selection")
           )
    ),
    
    # Output table
    column(6,
           h3("Other Titles You Might Be Interested in"),     
           tableOutput("item_recom")
    )
  ),
  
  # COMMENTS    
  fluidRow(                                    
    column(12,
           p("For the full code, please visit my", 
             a("GitHub page", href = "https://github.com/hyunjoonbok/R-projects/Title_Recommender_with_market_basket_analysis", target="_blank"))
    )
  )
)



server <- function(input,output) {
  
  output$item_recom <- renderTable({
    # react to submit button
    input$submit
    # gather input in string
    customer_order <- 
      isolate(
        
        unique(c(input$input_item1, input$input_item2, input$input_item3))
      )
    
    
    # put in a matrix format
    new_order <- item_list %>%
      # Add a 'value' column with 1's for customer order items
      mutate(value = as.numeric(Game %in% customer_order)) %>%
      # Spread into sparse matrix format
      spread(key = Game, value = value) %>%
      # Change to a matrix
      as.matrix() %>% 
      # Convert to class "dgCMatrix"
      as("dgCMatrix")
    
    # Add new order to retail matrix - binding 2 matrices
    all_orders_dgc <- t(rbind(new_order,past_orders_matrix))
    
    # Set items to predict range
    items_to_predict <- which(all_orders_dgc[ ,1] == 0)
    # items_to_predict <- 1:nrow(all_orders_dgc)
    # Set user to 1
    users <- c(1)
    # Set prediction indices
    prediction_indices <- as.matrix(expand.grid(items_to_predict, users = users))
    
    # Run UBCF model
    recomm <- predict_cf(all_orders_dgc, prediction_indices, 
                         "ubcf", FALSE, cal_cos, 3, FALSE, 4000, 2000)
    
    # Put recommended products into a dataframe
    recomm[,users] %>% 
      as.data.frame() %>% 
      rownames_to_column('NOTE that not all combinations of titles return suggestions') %>% 
      filter(.>0) %>% 
      select('NOTE that not all combinations of titles return suggestions')
    
  })
}

# Run the App
shinyApp(ui = ui, server = server)

