library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(scales)
library(shinycssloaders)

# time-seires plots
overall_cate <- read.csv("./data/overall_cate.csv")
myselect <- pivot_wider(overall_cate,names_from = cate,values_from = cate)
pop_sum <- read.csv("./data/population_sum.csv")
household <- read.csv("./data/household.csv")

#population data
data_MA <- read.csv("./data/data_MA.csv")
data_MA_tidy <- read.csv("./data/data_MA_tidy.csv")
result_MA <- read.csv("./data/result_MA.csv")
data_NJ <- read.csv("./data/data_NJ.csv")
data_NJ_tidy <- read.csv("./data/data_NJ_tidy.csv")
result_NJ <- read.csv("./data/result_NJ.csv")
pop_label <- c("Population" = "population",
               "young (<20)" = "young",
               "middle-aged (30 - 69)" = "middle_aged",
               "old (>70)" = "old",
               "young,middle_aged,old" = "young,middle_aged,old")



ui <- fluidPage(
  #overall theme
  theme = shinytheme("united"),
  setSliderColor(color = c("#ff6600","#ff6600"),
                 sliderId = c(1,2)),
  
#  tags$head(
#    tags$link(rel = "stylesheet",
#              type = "text/css",
#              href = "boostrap.min.css")
#  ),
  
  #shiny title
  titlePanel(title = div(img(src = "MSSP_logo.png", height = '40px', width = '99px'),
                         img(src = "BPDA_logo.png", height = '40px', width = '99px'),
                         "The Changing Composition of the Brazilian Population in the U.S.")),
  navbarPage("",
             position = "static-top",
             #first page
             # navbarMenu("ACS"
             # ),
             navbarMenu("Overview",
                        tabPanel("population",
                                 sidebarLayout(
                                   sidebarPanel(
                                     varSelectInput("cate",
                                                    h3("variable"),
                                                    data = myselect %>% select(-X)),
                                     sliderInput("range",
                                                 h3("Year"),
                                                 min = 2005,
                                                 max = 2019,
                                                 value = c(2005,2019),
                                                 sep = ""),
                                     conditionalPanel("input.cate == 'Population'",
                                                      selectInput("pop_group",
                                                                  h3("Plot Type"),
                                                                  choices = c("Population" = "population"))),
                                     conditionalPanel("input.cate == 'Age'",
                                                      selectInput("age_group",
                                                                  h3("Age"),
                                                                  choices = list("all" = "young,middle_aged,old",
                                                                              "young (<20)" = "young",
                                                                              "middle-aged (30 - 69)" = "middle_aged",
                                                                              "old (>70)" = "old"))),
                                     conditionalPanel("input.cate == 'Gender Distribution'",
                                                      selectInput("gender_group",
                                                                  h3("Gender"),
                                                                  choices = c("all" = "male,female",
                                                                              "Male"  = "male",
                                                                              "Female" = "female"))),
                                     conditionalPanel("input.cate == 'Marital Status'",
                                                      selectInput("marital_group",
                                                                  h3("Marital Status"),
                                                                  choices = c("all" = "widowed,divorced,separated,never_married_or_under_15_years_old",
                                                                              "widowed" = "widowed",
                                                                              "divorced" = "divorced",
                                                                              "separated" = "separated",
                                                                              "never married or under 15 years old" = "never_married_or_under_15_years_old"))),
                                     conditionalPanel("input.cate == 'Citizenship'",
                                                      selectInput("citizen_group",
                                                                  h3("Citizenship status"),
                                                                  choices = c("all" = "us_citizen_by_naturalization,not_a_citizen_of_the_us",
                                                                              "U.S. citizen by naturalization" = "us_citizen_by_naturalization",
                                                                              "not a citizen of the U.S." = "not_a_citizen_of_the_us"))),
                                     conditionalPanel("input.cate == 'Year of Entry'",
                                                      selectInput("entry_group",
                                                                  h3("Before Or After 2000"),
                                                                  choices = c("all" = "entered_us_before_2000,entered_us_in_2000_or_later",
                                                                              "entered U.S. before 2000" = "entered_us_before_2000",
                                                                              "entered U.S. in 2000 or later" = "entered_us_in_2000_or_later"))),
                                     conditionalPanel("input.cate == 'Ability to speak English'",
                                                      selectInput("english_group",
                                                                  h3("English Speaking level"),
                                                                  choices = c("all" = "speaks_english_less_than_very_well,speaks_english_very_well",
                                                                              "speaks English less than very well" = "speaks_english_less_than_very_well",
                                                                              "speaks English very well" = "speaks_english_very_well"))),
                                     conditionalPanel("input.cate == 'Labor Force by Gender'",
                                                      selectInput("labor_group",
                                                                  h3("Gender"),
                                                                  choices = c("all" = "males_in_civilian_labor_force,females_in_civilian_labor_force",
                                                                              "males in civilian labor force" = "males_in_civilian_labor_force",
                                                                              "females in civilian labor force" = "females_in_civilian_labor_force"))),
                                     conditionalPanel("input.cate == 'Unemployment by Gender'",
                                                      selectInput("unemploy_group",
                                                                  h3("Gender"),
                                                                  choices = c("all" = "unemployed_males,unemployed_females",
                                                                              "males unemployed" = "unemployed_males",
                                                                              "females unemployed" = "unemployed_females"))),
                                     conditionalPanel("input.cate == 'Employment Type'",
                                                      selectInput("employ_type_group",
                                                                  h3("Employment Type"),
                                                                  choices = c("all" = "private_wage_and_salary_workers,government_workers,self_employed_not_incorporated,self_employed_incorporated",
                                                                              "private wage and salary workers" = "private_wage_and_salary_workers",
                                                                              "government workers" = "government_workers",
                                                                              "self employed not incorporated" = "self_employed_not_incorporated",
                                                                              "self employed incorporated" = "self_employed_incorporated"))),
                                     conditionalPanel("input.cate == 'Employment by Industry'",
                                                      selectInput("industry_group",
                                                                  h3("Industry"),
                                                                  choices = c("accomodation, food services, arts entertainment, and recreation" = "accommodation_and_food_services_and_arts_entertainment_and_recreation",
                                                                              "construction" = "construction",
                                                                              "education" = "education",
                                                                              "finance insurance and real estate" = "finance_insurance_and_real_estate",
                                                                              "health care social assistance" = "health_care_social_assistance",
                                                                              "information" = "information",
                                                                              "manufacturing" = "manufacturing",
                                                                              "other services" = "other_services",
                                                                              "professional scientific and administrative services" = "professional_scientific_management_and_administrative_services",
                                                                              "public administration" = "public_administration",
                                                                              "retail" = "retail",
                                                                              "transportation warehousing utilities natural resources" = "transportation_warehousing_utilities_natural_resources",
                                                                              "wholesale trade" = "wholesale_trade",
                                                                              "other" = "other"))),
                                     conditionalPanel("input.cate == 'Employment by Occupation'",
                                                      selectInput("occupation_group",
                                                                  h3("Occupation"),
                                                                  choices = c("management professional" = "management_professional",
                                                                              "services" = "services",
                                                                              "sales and office" = "sales_and_office",
                                                                              "construction and extracion" = "construction_and_extracion",
                                                                              "maintenance and repair" = "maintenance_and_repair",
                                                                              "production transportation and material moving" = "production_transportation_and_material_moving",
                                                                              "educational instruction and library occupations" = "educational_instruction_and_library_occupations",
                                                                              "arts design entertainment sports and media occupations" = "arts_design_entertainment_sports_and_media_occupations",
                                                                              "healthcare practitioner and technical occupations" = "healthcare_practitioner_and_technical_occupations"))),
                                     conditionalPanel("input.cate == 'Poverty Status'",
                                                      selectInput("poverty_group",
                                                                  h3("Poverty Status"),
                                                                  choices = c("all" = "individuals_below_poverty,individuals_above_poverty",
                                                                              "individuals below poverty" = "individuals_below_poverty",
                                                                              "individuals above poverty" = "individuals_above_poverty"))),
                                     conditionalPanel("input.cate == 'Median Personal Earnings'",
                                                      selectInput("earning_group",
                                                                  h3("Earning"),
                                                                  choices = c("all" = "median_personal_earnings,median_personal_earnings_excl_0_and_neg",
                                                                              "median personal earnings" = "median_personal_earnings",
                                                                              "median personal earnings exclude 0 and negative" = "median_personal_earnings_excl_0_and_neg"))),
                                     conditionalPanel("input.cate == 'Educational Attainment of population aged 25 or over'",
                                                      selectInput("education_group",
                                                                  h3("Education Level"),
                                                                  choices = c("all" = "less_than_high_school,high_school_graduate_ged_or_alternative_credential,some_college_associate_degree,bachelors_degree_or_higher",
                                                                              "less than high school" = "less_than_high_school",
                                                                              "high school graduate or alternative credential" = "high_school_graduate_ged_or_alternative_credential",
                                                                              "some college associate degree" = "some_college_associate_degree",
                                                                              "bachelors degree or higher" = "bachelors_degree_or_higher"
                                                                  )))
                                   ),
                                   mainPanel(
                                     fluidRow(
                                       column(12, withSpinner(plotOutput("population_plot"),
                                                              color = "#ff6600"))
                                     )
                                   )
                                 )),
                        
                        tabPanel("household",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("house_cate",
                                                 h3("Variable"),
                                                 choices = c("Householder",
                                                             "Ownership",
                                                             "Family Household",
                                                             "Family Poverty",
                                                             "Monthly Owner Costs",
                                                             "Monthly Gross Rent",
                                                             "Overcrowding",
                                                             "Median Income")),
                                     sliderInput("range2",
                                                 h3("Year"),
                                                 min = 2005,
                                                 max = 2019,
                                                 value = c(2005,2019),
                                                 sep = ""),
                                     conditionalPanel("input.house_cate == 'Ownership'",
                                                      selectInput("owner_group",
                                                                  h3("Ownership Type"),
                                                                  choices = c("all" = "ownership_owner_occupied_unit,ownershio_renter_occupied_unit",
                                                                              "owner occupied unit" = "ownership_owner_occupied_unit",
                                                                              "renter occupied unit" = "ownershio_renter_occupied_unit"))),
                                     conditionalPanel("input.house_cate == 'Family Poverty'",
                                                      selectInput("poverty_group",
                                                                  h3("Poverty Group"),
                                                                  choices = c("all" = "poverty_families_below_poverty,poverty_family_above_poverty",
                                                                              "families below poverty" = "poverty_families_below_poverty",
                                                                              "families above poverty" = "poverty_family_above_poverty"))),
                                     conditionalPanel("input.house_cate == 'Householder'",
                                                      selectInput("house_group",
                                                                  h3("Householder"),
                                                                  choices = c("householder" = "total_householders"))),
                                     conditionalPanel("input.house_cate == 'Family Household'",
                                                      selectInput("family_group",
                                                                  h3("Family Householders"),
                                                                  choices = c("Family Householders" = "total_family_households"))),
                                     conditionalPanel("input.house_cate == 'Monthly Owner Costs'",
                                                      selectInput("cost_group",
                                                                  h3("Percent of Househould Income"),
                                                                  choices = c("all" = "month_cost_under_20_percent_of_hh_income,month_cost_x20_to_25_percent_of_hh_income,month_cost_x25_to_30_percent_of_hh_income,month_cost_x30_to_35_percent_of_hh_income,month_cost_x35_percent_of_hh_income",
                                                                              "under 20 percent" = "month_cost_under_20_percent_of_hh_income",
                                                                              "20 to 25 percent" = "month_cost_x20_to_25_percent_of_hh_income",
                                                                              "25 to 30 percent" = "month_cost_x25_to_30_percent_of_hh_income",
                                                                              "30 to 35 percent" = "month_cost_x30_to_35_percent_of_hh_income",
                                                                              "> 35%" = "month_cost_x35_percent_of_hh_income"))),
                                     conditionalPanel("input.house_cate == 'Monthly Gross Rent'",
                                                      selectInput("rent_group",
                                                                  h3("Percent of Household Income"),
                                                                  choices = c("all" = "month_rent_under_15_percent_of_hh_income,month_rent_x15_percent_to_20_percent_of_hh_income,month_rent_x20_percent_to_25_percent_of_hh_income,month_rent_x25_percent_to_30_percent_of_hh_income,month_rent_x30_percent_to_35_percent_of_hh_income,month_rent_x35_percent_of_hh_income_2",
                                                                              "< 15%" = "month_rent_under_15_percent_of_hh_income",
                                                                              "15 - 20 %" = "month_rent_x15_percent_to_20_percent_of_hh_income",
                                                                              "20 - 25 %" = "month_rent_x20_percent_to_25_percent_of_hh_income",
                                                                              "25 - 30 %" = "month_rent_x25_percent_to_30_percent_of_hh_income",
                                                                              "30 - 35 %" = "month_rent_x30_percent_to_35_percent_of_hh_income",
                                                                              "> 35 %" = "month_rent_x35_percent_of_hh_income_2"))),
                                     conditionalPanel("input.house_cate == 'Overcrowding'",
                                                      selectInput("crowd_group",
                                                                  h3("crowded or not corwded unit"),
                                                                  choices = c("all" = "overcrowding_crowded_units,overcrowding_not_crowded_units",
                                                                              "Crowded Unit" = "overcrowding_crowded_units",
                                                                              "Not Crowded Unit" = "overcrowding_not_crowded_units"))),
                                     conditionalPanel("input.house_cate == 'Median Income'",
                                                      selectInput("income_group",
                                                                  h3("Family or Household?"),
                                                                  choices = c("all" = "median_household_income,median_family_income",
                                                                              "Household" = "median_household_income",
                                                                              "Family" = "median_family_income")))
                                     
                                   ),
                                   mainPanel(
                                     fluidRow(
                                       column(12, withSpinner(plotOutput("myplot"),
                                                              color = "#ff6600"))
                                     )
                                   )
                                 ))
                        ),
             navbarMenu("Analysis",
                        tabPanel("MA vs. NJ (Population)",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("field",
                                                 "Field",
                                                 choices = c("Education",
                                                             "Labor Force Rate",
                                                             "Correlation"))
                                   ),
                                   mainPanel(
                                     conditionalPanel("input.field == 'Education'",
                                                      plotOutput("plot1"),
                                                      plotOutput("plot4")),
                                     conditionalPanel("input.field == 'Labor Force Rate'",
                                                      plotOutput("plot2"),
                                                      plotOutput("plot5")),
                                     conditionalPanel("input.field == 'Correlation'",
                                                      plotOutput("plot3"),
                                                      plotOutput("plot6"))
                                   )
                                 )),
                        tabPanel("Five States (Household)",
                                 sidebarLayout(
                                   #sidebarPanel("CA","FL","NY","MA","NJ"),
                                   sidebarPanel(
                                     selectInput("state",
                                                 label = "Five State",
                                                 choices = c("California",
                                                             "Florida",
                                                             "New York",
                                                             "Massachusets",
                                                             "New Jersey"))
                                   ),
                                   mainPanel(
                                     conditionalPanel("input.state == 'California'",
                                                      img(src="ca.png",height = 300,width=450),
                                                      img(src="ca2.png",height = 300,width=450)),
                                     conditionalPanel("input.state == 'Florida'",
                                                      img(src="fl.png",height = 300,width=450),
                                                      img(src="fl2.png",height = 300,width=450)),
                                     conditionalPanel("input.state == 'New York'",
                                                      img(src="ny.png",height = 300,width=450),
                                                      img(src="ny2.png",height = 300,width=450)),
                                     conditionalPanel("input.state == 'Massachusets'",
                                                      img(src="ma.png",height = 300,width=450),
                                                      img(src="ma2.png",height = 300,width=450)),
                                     conditionalPanel("input.state == 'New Jersey'",
                                                      img(src="nj.png",height = 300,width=450),
                                                      img(src="nj2.png",height = 300,width=450))
                                   )
                                 ))
             ))
  
)


server <- function(input, output) {

#year input for output - Su
  my_range <- reactive({
    cbind(input$range[1],input$range[2])
  })

  #population data graph - Su
  my_select1 <- reactive({
    if (input$cate == 'Population') {
      input$pop_group
    } else if (input$cate == 'Age') {
      input$age_group
    } else if (input$cate == 'Gender Distribution'){
      input$gender_group
    } else if (input$cate == 'Marital Status') {
      input$marital_group
    } else if (input$cate == 'Citizenship') {
      input$citizen_group
    } else if (input$cate == 'Year of Entry') {
      input$entry_group
    } else if (input$cate == 'Ability to speak English') {
      input$english_group
    } else if (input$cate == 'Labor Force by Gender') {
      input$labor_group
    } else if (input$cate == 'Unemployment by Gender') {
      input$unemploy_group
    } else if (input$cate == 'Employment Type') {
      input$employ_type_group
    } else if (input$cate == 'Employment by Industry') {
      input$industry_group
    } else if (input$cate == 'Employment by Occupation') {
      input$occupation_group
    } else if (input$cate == 'Poverty Status') {
      input$poverty_group
    } else if (input$cate == 'Median Personal Earnings') {
      input$earning_group
    } else if (input$cate == 'Educational Attainment of population aged 25 or over') {
      input$education_group
    }
  })
  
  output$population_plot <- renderPlot({
    pop_sum %>% 
      filter(myvar %in% strsplit(my_select1(), ",")[[1]]) %>% 
      filter(year >= my_range()[1],
             year <= my_range()[2]) %>% 
      ggplot() + 
      geom_bar(aes(x = year,
                   y = account,
                   color = reorder(myvar,-account),
                   fill = reorder(myvar,-account)),
               stat = "identity",
               position = "dodge") +
      scale_color_grey()+
      # geom_line(aes(x = year,
      #               y = 1.1*account,
      #               group = myvar),
      #           stat = "identity") +
      scale_x_continuous("Year", 
                         labels = as.character(pop_sum$year), 
                         breaks = pop_sum$year) +
      geom_text(aes(label = account,
                    x = year,
                    y = account,
                    group = reorder(myvar,-account)),
                position = position_dodge(width = 1),
                color = "black",
                vjust = -0.5,
                size = 2.5) +
      scale_y_continuous(labels = comma,
                         name = "Account") + 
      ggtitle("Changing Composition in each year") +
      labs(fill = "Variable",
           color = "Variable")+
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.45,
                                      size = 16),
            legend.position = "bottom",
            legend.direction="vertical")
  })
  
  # household data graph -Su
  #year input for output 
  my_range2 <- reactive({
    cbind(input$range2[1],input$range2[2])
  })
  
  my_select2 <- reactive({
    if (input$house_cate == 'Ownership'){
      input$owner_group
    }else if (input$house_cate == 'Family Poverty'){
      input$poverty_group
    }else if (input$house_cate == 'Householder'){
      input$house_group
    }else if (input$house_cate == 'Family Household') {
      input$family_group
    }else if (input$house_cate == 'Monthly Owner Costs'){
      input$cost_group
    }else if (input$house_cate == 'Monthly Gross Rent'){
      input$rent_group
    }else if (input$house_cate == 'Overcrowding'){
      input$crowd_group
    }else{
      input$income_group
    }
  })
  
  output$myplot <- renderPlot({
    household %>% 
      filter(myvar %in% strsplit(my_select2(), ",")[[1]]) %>% 
      filter(year >= my_range2()[1],
             year <= my_range2()[2]) %>% 
      ggplot() +
      geom_bar(aes(x = year,
                   y = account,
                   color = reorder(myvar,-account),
                   fill = reorder(myvar,-account)),
               stat = "identity",
               position = "dodge") +
      scale_color_grey()+
      # geom_line(aes(x = year,
      #               y = 1.1*account,
      #               group = myvar),
      #           stat = "identity") +
      scale_x_continuous("Year", 
                         labels = as.character(household$year), 
                         breaks = household$year) +
      geom_text(aes(label = account,
                    x = year,
                    y = account,
                    group = reorder(myvar,-account)),
                position = position_dodge(width = 1),
                color = "black",
                vjust = -0.5,
                size = 2.5) +
      scale_y_continuous(labels = comma,
                         name = "Account") + 
      ggtitle("Changing Composition in each year") +
      labs(fill = "Variable",
           color = "Variable")+
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.45,
                                      size = 16),
            legend.position = "bottom",
            legend.direction="vertical")
      # geom_bar(stat = "identity", 
      #          color = "black", 
      #          fill = "black") +
      # geom_line(aes(x = year,
      #               y = 1.1*account),
      #           stat = "identity",
      #           color = "red") +
      # scale_x_continuous("Year", 
      #                    labels = as.character(household$year), 
      #                    breaks = household$year) +
      # geom_text(aes(label = account,
      #               x = year,
      #               y = account),
      #           color = "black",
      #           vjust = -0.5,
      #           size = 2.5) +
      # scale_y_continuous(labels = comma,
      #                    name = "Account") + 
      # ggtitle("Changing Composition in each year") +
      # theme_bw()+
      # theme(plot.title = element_text(hjust = 0.45,
      #                                 size = 16))
  })
  
  #population part plot
  output$plot1 <- renderPlot({
    ggplot(data=data_MA_tidy,
           aes(x=time,
               y=population,
               color=education_level))+
      geom_line()+
      ggtitle("Education in MA")
  })
  output$plot2 <- renderPlot({
    ggplot(data=data_MA_tidy,
           aes(y=rate,
               x=time))+
      geom_line(color="skyblue")+
      ggtitle("Labor Force Rate in MA")
  })
  output$plot3 <- renderPlot({
    ggplot(result_MA,
           aes(x=ed_level,
               y=cor_number))+
      geom_col()+
      ggtitle("Correlation in MA")
  })
  output$plot4 <- renderPlot({
    ggplot(data=data_NJ_tidy,
           aes(x=time,
               y=population,
               color=education_level))+
      geom_line()+
      ggtitle("Education in NJ")
  })
  output$plot5 <- renderPlot({
    ggplot(data=data_NJ,
           aes(y=rate,
               x=time))+
      geom_line(color="skyblue")+
      ggtitle("Labor Force Rate in NJ")
  })
  output$plot6 <- renderPlot({
    ggplot(result_NJ,
           aes(x=ed_level,
               y=cor_number))+
      geom_col()+
      ggtitle("Correlation in NJ")
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
