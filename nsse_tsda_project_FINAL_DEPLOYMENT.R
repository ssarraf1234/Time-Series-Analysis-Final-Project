#Web App for Time Series Data Analysis Course at IU Luddy School
#Topic: Predicting participation in NSSE
#Spring 2022
#Author: Shimon Sarraf

library(gtsummary)
library(gt)
library(shiny)
library(readxl)
library(tsibble)
library(tidyverse)
library(tsibbledata)

#set working directory where file exists (pre-deployment to web)
#remove before deployment step
#setwd("//bl-educ-ed5070fs.educ.indiana.edu/center/CPR/USERS/ssarraf/Misc/Courses/D590 - Time Series Analysis/Semester Project/Part 4")

#import file and create tsibble dataset
nsse <- read_excel("nsse.xlsx", col_names = TRUE) %>%
  as_tsibble(
    index = year,
    key = c(key, group),
  )

#creating lists for dropdown menu 
key_values <- c("Select Option", "All Colleges and Universities" = "Total", "Carnegie Group" = "Carnegie", "Public-Private Status" = "Control", "City-Suburb-Town Status" = "Locale", "Nation", "Region", "Enrollment Size" = "Ugenrollment")
group_values <- c("Select Option", "All Colleges and Universities" =	'Total',  '**CARNEGIE**', 'Doc/Very High', 'Doc/High',	'Doc/Prof',	"Master's L",	"Master's M",	"Master's S",	'Bac/A&S',	'Bac/Diverse',	'Bac/Assoc-Assoc Dom',	'Bac/Assoc-Mixed',  'Spec/Arts',	'Spec/Bus',	'Spec/Health',	'Spec/Medical',	'Spec/Special',	'Spec/Tech',	'Special/Faith',	'Tribal',	'**PUBLIC-PRIVATE STATUS**', 'Public',	'Private for-profit',	'Private not-for-profit',	'**LOCALE**', 'City: Large',	'City: Midsize',	'City: Small', 	'Suburb: Large',	'Suburb: Midsize',	'Suburb: Small', 'Town: Distant',	'Town: Fringe',	'Town: Remote', 	'Rural: Distant',	'Rural: Fringe',	'Rural: Remote', '**NATION**',  'US', 'Canada', "Other Countries" =	'Other', '**REGION**', 'Far West',	'Great Lakes',	'Mid East',	'New England',	'Plains',	'Rocky Mountains',	'Southeast',	'Southwest', 'Outlying areas',	'US Service schools', '**ENROLLMENT SIZE**',   "Under 1,000" = 'Under 1000', "1,000 - 2,500" =	'1000 - 2500', "2,501 - 5,000" =	'2501 - 5000', 	"5,001 - 10,000" =	 '5001 - 10000', "10,001 - 20,000" =	'10001 - 20000', 	"Over 20,000" = 'Over 20000')
#above list reorganizes list into logical order; some values are not associated with any real values in data


ui = fluidPage(
  titlePanel("NSSE Historical & Projected Participation (2000 to 2022)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput('key', 'Select School Characteristic for Statistics Tab', choices = c(key_values)),
      selectInput('group', 'Select Sub-Group for Projection Tab', choices = c(group_values)),
      actionButton("update", "Update")
      ),
  
      mainPanel(
        tabsetPanel(
          tabPanel('Statistics', gt_output('table')) ,
          tabPanel('Projection',  plotOutput('projection'))
        )
      )
  )
)


library(fpp3)
library(fable)

server = function(input, output) {
  
  # create table of descriptive statistics
  observeEvent(input$update,{
    output$table <- render_gt({
    nsse %>%
    filter(key == input$key & group != "Missing")  %>%
    as_tibble %>% 
    tbl_summary( # https://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html
      by = group,
      type = all_continuous() ~ "continuous2",
      statistic = all_continuous() ~ c("{mean}", "{sd}", "{min}" , "{max}"),
      missing = "no",
      digits = all_continuous() ~ 0,
      include = count,
      label = count ~ "School Count") %>%
    modify_header(label = "**.**") %>%
    modify_header(stat_by =  "**{level}**")  %>%
    modify_spanning_header(all_stat_cols() ~ "**NSSE Participation Count by Group (2000 to 2022)**") %>%
    as_gt()
    })
  })
    
    #create historical trend line plot with projection
  observeEvent(input$update,{
    output$projection <- renderPlot({
    fit <- nsse %>%
      filter(group == input$group) %>%
      #filter(group == "US") %>%
      model(ARIMA(count))
    fit %>% forecast(h=3) %>%
      autoplot(nsse) +
      labs(y = "Institution Count", x = "Year", title = "NSSE Participation by Year and 3-Year Projection")
    }) 
  }) 
}

# Run the application 
shinyApp(ui = ui, server = server)


