# 2019 07 03

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




shinyApp(
  ui = pageWithSidebar(
    headerPanel("Occupation and Degree App"),
    sidebarPanel(
      uiOutput("select_var1"), 
      uiOutput("select_var2"),
      uiOutput("select_var3"),
      uiOutput("select_var4"),
      uiOutput("select_var5")
    ),
    
    mainPanel(
      tableOutput("table")
    )
  ),
  
  server = function(input, output, session) {
    
    tab <- reactive({ 
      
      tib %>% 
        filter(var_one == input$var1) %>% 
        filter(var_two == input$var2) %>% 
        filter(var_three == input$var3) %>% 
        filter(var_four == input$var4) %>% 
        filter(var_five == input$var5)
      
    })
    
    output$select_var1 <- renderUI({
      
      selectizeInput('var1', 'Select variable 1', choices = c("select" = "", levels(tib$var_one)))
      
    })
    
    output$select_var2 <- renderUI({
      
      
      choice_var2 <- reactive({
        tib %>% 
          filter(var_one == input$var1) %>% 
          pull(var_two) %>% 
          as.character()
        
      })
      
      selectizeInput('var2', 'Select variable 2', choices = c("select" = "", choice_var2())) # <- put the reactive element here
      
    })
    
    output$select_var3 <- renderUI({
      
      choice_var3 <- reactive({
        tib %>% 
          filter(var_one == input$var1) %>% 
          filter(var_two == input$var2) %>% 
          pull(var_three) %>% 
          as.character()
        
      })
      
      selectizeInput('var3', 'Select variable 3', choices = c("select" = "", choice_var3()))
      
    })
    
    output$select_var4 <- renderUI({
      
      choice_var4 <- reactive({
        tib %>% 
          filter(var_one == input$var1) %>% 
          filter(var_two == input$var2) %>% 
          filter(var_three == input$var3) %>% 
          pull(var_four) %>% 
          as.character()
        
      })
      
      selectizeInput('var4', 'Select variable 4', choices = c("select" = "", choice_var4()))
      
    })
    
    output$select_var5 <- renderUI({
      
      choice_var5 <- reactive({
        tib %>% 
          filter(var_one == input$var1) %>% 
          filter(var_two == input$var2) %>% 
          filter(var_three == input$var3) %>% 
          filter(var_four == input$var4) %>% 
          pull(var_five) %>% 
          as.character()
        
      })  
      
      selectizeInput('var5', 'Select variable 5', choices = c("select" = "", choice_var5()))
      
    })
    
    output$table <- renderTable({ 
      
      tab()
      
    })
    
  },
  
  options = list(height = 500)
  
)