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
    titlePanel("Degree-to-Profession Mapping Tool"),

    # Select degrees or occupations
    sidebarLayout(
        sidebarPanel(
            selectInput("occ_input", 
                        "Select Degree",
                        choices = levels(as.factor(master3$occ.name))
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

    output$table <- renderDT({
        DT::datatable(
            master3 %>% 
                filter(occ.name %in% input$occ_input) %>% 
                group_by (SOC_Cat_Name),
            options = list(pageLength = 50)
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
