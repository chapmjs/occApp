#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(readxl)
library(DT)
library(readr)
library(tidyr)
library(janitor) # clean_names()


## import data

# import master1, loading this produces different names than read.csv function
# master1 <- read_csv("~/Dropbox/R-Studio/R_Training/Master1.txt")  
# master1 <- read.csv("~/Dropbox/R-Studio/R_Training/Master1.txt")
# View(master_txt)
# clean_names(master1,"snake")

# import soc_code
soc_code <- read.delim("~/Dropbox/R-Studio/EPIC/soc_code.txt")
soc_code <- soc_code[-1,]
#clean_names(soc_code,"snake")

## merge soc category name into master
# rename field in soc_code table to same name in master1
names(soc_code)[1] <- "soc.cat"
# left join tables
master2 <- master1 %>% left_join(soc_code, by="soc.cat")

## select following columns
# occ.name, entry.degree, Experience, cip.name, school.id, degree.name, cip.cat, soc.cat,X25p,X50p,X90p, SOC_Cat_Name

master3 <- master2 %>% select(occ.name, entry.degree, Experience, cip.name, school.id, degree.name, cip.cat, soc.cat,X25p,X50p,X90p, SOC_Cat_Name)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Occupation to Degree Mapping Tool"),

    # Select degrees or occupations
    sidebarLayout(
        sidebarPanel(
            selectInput("soc_cat_input", 
                        "Select Occupation Category",
                        choices = levels(as.factor(master3$SOC_Cat_Name))
            ),
            selectInput(uiOutput("cip_select")
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            DT::dataTableOutput(outputId = "table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # model.data0 <- reactive ({
    #     data.frame( "COURSE" = sample(LETTERS[1:3], n, replace=TRUE),
    #                 "VALUE"  = sample(1:10, n, replace=TRUE)) 
    # })
    # 
    
    # Render selectInput 
    output$cip_input <- renderUI({
        cip.names <- as.vector( master3 %>% select (SOC_Cat_Name, cip.name) %>% filter(cip.name == "input$cip_input"))  #unique(model.data0()$COURSE) )
        selectInput("cip_input","Select Interesting Degrees", choices=cip.names, multiple=TRUE)    
    })

    output$table <- renderDT({
        DT::datatable(
            req("cip_input"),
            master3 %>% 
                filter(SOC_Cat_Name %in% input$soc_cat_input) %>% 
                group_by (occ.name, cip.cat),
            options = list(pageLength = 50)
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
