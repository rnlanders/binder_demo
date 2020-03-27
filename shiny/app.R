# Notice I do not load tidyverse here, because it's bloated for what we need
library(shiny)
library(dplyr)
library(ggplot2)

# This is the user interface. It receives output$ and sends input$.
ui <- fluidPage(
    titlePanel("Garcia et al. Interactive Analysis"),
    sidebarLayout( 
        sidebarPanel(
            selectInput("dvnum",
                        "Selected DV:",
                        c("Appropriateness of Response"=1,
                          "Anger toward Target"=2,
                          "Likeability of Target"=3)),
            "Garcia, D. M., Schmitt, M. T., Branscombe, N. R., & Ellemers, N. (2010). Women's reactions to ingroup members who protest discriminatory treatment: The importance of beliefs about inequality and response appropriateness. European Journal of Social Psychology, 40(5), 733–745."
        ),

        mainPanel(
           plotOutput("garcia_scatterplot")
        )
    )
)

# This is the server-side code. It receives input$ and sends output$.
server <- function(input, output) {

    output$garcia_scatterplot <- renderPlot({
        
        for_shiny_tbl <- readRDS("for_shiny.rds")
        
        if(input$dvnum == 1) {
            for_shiny_tbl <- mutate(for_shiny_tbl, selected_dv = resp_pred_values)
            label_txt <- "Appropriateness"
        } else if(input$dvnum == 2) {
            for_shiny_tbl <- mutate(for_shiny_tbl, selected_dv = anger_pred_vals)
            label_txt <- "Anger"
        } else {
            for_shiny_tbl <- mutate(for_shiny_tbl, selected_dv = liking_pred_vals)
            label_txt <- "Likeability"
        }
        
        ggplot(for_shiny_tbl, aes(x=sexism, y=selected_dv, group=protest)) +
            geom_smooth(method="lm", se=F) +
            labs(x="Sexism", y=label_txt)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
