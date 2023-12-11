library(shiny)
library("shinythemes")
source("INFO201-Final Project.R")
library(plotly)
library(fmsb)


about_view <- fluidPage(
  h1("About Us"),
  hr(),
  sidebarLayout(
   
     sidebarPanel(
      h1("Contributors"),
      h3("INFO201 Fall 2023"),
      hr(),
      
      
      tags$img(src='iSchool_og.png', height = "50%", width = "70%"),
      br(),
      br(),
      
      
      h5("Ubaidillah Mohammad Razali"),
      p("he/him"),
      p("ubrzl@uw.edu"),
      br(),
      
      h5("Madelyn Lee"),
      p("she/her"),
      p("madely@uw.edu"),
      hr(),
      
      h3("Hotlines"),
      h5("Click the organization name to view their website."),
      a("Substance Abuse and Mental Health Services Administration", href = "https://www.samhsa.gov/"),
      
      p("1-800-662-4357"),
      a("HIV.gov", href = "https://www.hiv.gov/"),
      p("1-800-HIV-0440"),
      br(),
    ),
    
    mainPanel(
      tags$img(src='drugs.gif', height = "50%", width = "80%"),
      h1("Our Purpose"),
      p("Having witnessed a relative affected by medical drug overdose, we were 
         curious to explore the scale to which this issue affects. During our 
         research, we came across an article that found drug overdose to be a 
         common cause of death amongst those diagnosed with HIV. This led us to 
         explore the question: “Who exactly is affected by HIV diagnosis and 
         drug overdose?” We wanted to explore the demographics significantly 
         affected by this issue to discover any patterns and look for ways to 
         potentially mitigate drug overdose, especially amongst those diagnosed 
         with HIV/AIDS."),
      br(),
      br(),
      h3("Abstract"),
      p("According to an article titled: Why Overdose Matters, drug overdose is 
        the leading cause of mortality among people living with HIV. If an 
        individual diagnosed with HIV survives, overdosing has been found to 
        exacerbate the any HIV infections or symptoms. With this in mind, we set
        out to collect data that could provide an explanation as to why this is. 
        Is the cause due to lack of medical attention? Or could the cause be
        hereditary or based on environmental factors?"),
      br(),
      br(),
      h3("Research Questions"),
      h4("1. Is the cause of the issue hereditary?"),
      p("As HIV/AIDs is known to be a hereditary disease, a disease that can only
        be passed on from parents to their offspring, we called into question 
        the possibility that vulnerability to death by drug overdosage could
        also be based on genetics. To test this, we will look at cases of drug
        overdose deaths known to have been diagnosed with HIV/AIDs. We will filter 
        the cases by genetic categories, such as sex and race, to find any patterns
        that may provide an answer to this question."),
      h4("2. Is the cause of the issue based on environmental factors?"),
      p("If not hereditary, we wonder what if the cases of drug overdose deaths 
        of people diagnosed with HIV/AIDs are effected by their environment. To
        test whether this is true or not, we will look at known cases based on
        various neighborhoods in New York City. Additionally, to see if there is 
        the issue is both hereditary and based on environmental factors, we will
        filter these neighborhood cases by race."),
      h4("3. How have cases of drug overdose deaths of those diagnosed with HIV/AIDs
        changed over time?"),
      p("We believe it is important to track the amount of cases over time to 
        observe if any other factors(such as the 2020 COVID pandemic) has had 
        any effect on this issue. Considering the case count with the year will
        help us discover if the issue is affected by any other external factors."),
      br(),
      br(),
      h3("Expected Impact"),
      p("Based on our research questions, this data could provide possible 
        predictions for stakeholders as it helps raise awareness for cases of 
        drug overdose in HIV/AIDs patients. Making this data more accessible
        will help provide understanding as to why drug overdose is one of the 
        leading causes of mortality in HIV/AIDs patients. Additionally, its
        publication can potentially help identify which demographics this issue
        takes prevalence in to help those be more mindful of their health and
        help doctors better support their patients."),
      br(),
      br(),
      h3("Limitations"),
      p("Our dataset has missing data from 2014 and 2015, limiting us from 
        seeing the change in case counts during those years. Additionally, 
        some neighborhoods stopped tracking the case count for people of Asian 
        descent after 2016 as well. Because our dataset is incomplete, we cannot
        be certain that something pivotal did or did not happen in those years 
        that could also provide an explanation to our research questions. We
        carried out this project being aware that our data may be slightly 
        inaccurate or may not be able to represent the whole picture of what
        we are calling into question."),
      br(),
      br(),
      h3("Findings"),
      p("Through this dataset, we have found that the demographic with the most
        HIV/AIDs patients affected by drug overdosage are males of Black descent
        across various neighborhoods. This suggests that the issue may hereditary
        and most common in males of Black descent. While people of other races
        also suffer from this issue, it seems evident that males of Black descent
        may call for additional resources and support when faced with this
        dilemma. We learned that in comparison to females of other races, males
        of Black descent have the highest case count of HIV/AIDs patients 
        affected by drug overdose."),
      br(),
      br(),
      h3("References"),
      p("Green TC, McGowan SK, Yokell MA, Pouget ER, Rich JD. HIV infection and risk of overdose: a systematic review and meta-analysis. AIDS. 2012 Feb 20;26(4):403-17. doi: 10.1097/QAD.0b013e32834f19b6. PMID: 22112599; PMCID: PMC3329893."),
      br(),
      p("Why overdose matters for HIV - open society foundations. (n.d.). https://www.opensocietyfoundations.org/uploads/3e5fb9b4-4203-47ce-a024-8ec79ce244a9/why-overdose-matters-20100715.pdf "),
      
      br(),
      br(),
      h3("Acknowlegements"),
      p("Thank you to our INFO201 Professor Julia Deeb-Swihart and our TAs for
      your guidance throughout this class!"),
      br(),
      br()
    )
  )
)

# FOR SEX PAGE
sex_view <- fluidPage(
  titlePanel("Drug Overdose Deaths diagnosed with HIV/AIDs in relation to Sex"),
  p("The bar graph below compares the amount of people who were diagnosed 
    with HIV/AIDs and were affected by drug overdose by sex. This is shown over 
    the years from 2010 to 2021. We felt that it was important to quantify 
    the amount of people affected by both issues to discover any trends where 
    drug overdose deaths and HIV diagnoses may affect one gender more than the 
    other. We were curious to see if the issues are hereditary."),
  p("Based on the bar graph, at the beginning of the 2010s, more males were 
      affected by HIV/AIDS and death by drug overdose than females. However, in 
      2016, the gap between the two sexes close. Since then, the amount of females
      affected by HIV/AIDS and death by drug overdose has decreased (with an
      exception being in 2020 during the COVID pandemic). During this entire 
      time, the amount of males affected by these issues have staggered but stayed
      roughly high. This leads us to believe that HIV/AIDS and death by drug
      overdose is more common in males than females. By looking at these insights, 
      we can learned that while case counts for females have fluctuated in the past
      decade, the case count for males have stayed substantially high and thus, 
      it seemes that males are more affected by this issue than females."),
  hr(),
  sidebarPanel(
    h1("Compare between sexes over time"),
    h5("*Note there is no data for 2014 and 2015."),
    hr(),
    h5("Slide the circle back and forth to view more or less information about
       the case counts in each year by sex."),
    sliderInput(
      inputId = "year_slider",
      label = "Filter by year",
      min = 2010,
      max = 2021, 
      value = 2013
    ),
  ),
  mainPanel(
  plotOutput(outputId = "bar_sex"),
  ),
)

# FOR RACE PAGE
race_view <- fluidPage(
  titlePanel("Drug Overdose Deaths diagnosed with HIV/AIDs in relation to Races"),
  p("The scattered plot below displays the amount of people diagnosed 
    with HIV/AIDS and later on died by drug overdose by race over time. We 
    wanted to categorize information by race and year to discover any trends
    in how these health issues may affect people of different races 
    differently. We primarily were curious to see if these issues may be
    hereditary and thought this scatter plot could help narrow down the
    demographic that these issues target, if hereditary."),
  p("When looking at the plot of races in comparison to each other, there is 
    a substantially greater amount of cases from those of Hispanic and Black 
    descent. The amount of people from Black descent affected by these issues 
    has decreased since 2016, but it is still substantially greater than the
    amount of cases from those of White and Asian descent. This data suggests
    that HIV/AIDS and death by drug overdose affects more people of Black and
    Hispanic descent than other races. In addition to our bar graph comparing
    case counts by sex, our data further suggests that males of Black and 
    Hispanic descent are most affected by this issue."),
  hr(),
  sidebarLayout(
    sidebarPanel (
      h3("Choose which races to compare!"),
      h5("*Note there is no data for 2014 and 2015."),
      hr(),
      h5("Check on/off each box to view and compare the case counts of different
         races over the past decade."),
      checkboxInput(
       inputId = "incl_bl",
        label = "Include Black race?",
        value = TRUE
      ),
     checkboxInput(
       inputId = "incl_wh",
       label = "Include White race?",
       value = TRUE
     ),
     checkboxInput(
       inputId = "incl_as",
       label = "Include Asian race?",
       value = TRUE
     ),
     checkboxInput(
       inputId = "incl_his",
       label = "Include Hispanic race?",
       value = TRUE
     )
    ),
    mainPanel(
      plotOutput(outputId = "scatter_race"),
    )
  ),
)



# FOR NEIGHBORHOOD PAGE
neighborhood_view <- fluidPage(
  titlePanel("Drug Overdose Deaths diagnosed with HIV/AIDs in relation to Neighborhoods"),
  p("The graph below displays the amount of people diagnosed 
    with HIV/AIDS and were affected by drug overdose by race and 
    neighborhood in New York within the past decade. We wanted to further categorize information by 
    neighborhood to see if these issues are caused by an environmental factor.
    We included race in this graph because we were curious
    to see if different races in the same neighborhood were affected 
    differently by both issues"),
  p("With this data, we learn that the amount of cases
    in each neighborhood stay about the same each year. However, some neighborhoods 
    have higher case counts for people of Black descent than other races. For 
    example, if you select Jamaica, the case count for people of Black descent
    is substantially higher. Additionally, if you choose a year, the case count
    for individuals with Black descent are almost always above the average.
    Based off this information, we learned that people of Black descent
    across all neighborhoods are most affected by this issue. This also suggests
    that the issue called into question may be based on environmental factors."),
  hr(),
  sidebarLayout(
    sidebarPanel(
      h1("Compare between neighborhoods and find the averages each year!"),
      h5("*Note there is no data for 2014 and 2015 and some neighborhoods
         do not have data past 2013."),
      hr(),
      h5("Select a neighborhood to view its case counts by race over the years."),
      selectInput(
        inputId = "neigh_name",
        label = "Select a neighborhood",
        choices = clean_df$Neighborhood,
      ),
      h5("Select a year to the average case count of all the neighborhoods in that year."),
      selectInput(
        inputId = "neigh_year",
        label = "Select a year",
        choices = clean_df$Year,
      )
    ),
    mainPanel(
      #tabsetPanel(
        tabPanel("Plot", plotOutput(outputId = "graph_neigh")),
      #)
    )
  )
)


ui <- fluidPage(
  
  theme = shinytheme("yeti"),
  
  navbarPage(
  "HIV/AIDS Diagnoses and Drug Overdose Deaths",
  tabPanel("About", about_view),
  tabPanel("Sex", sex_view),
  tabPanel("Race", race_view),
  tabPanel("Neighborhood", neighborhood_view)
  ),
)


server <- function(input, output){
  # shinyOptions(shinyapps.io = list(appPrimaryDoc = "INFO201-Final Project.R"))
  
  # FOR SEX PAGE
  output$bar_sex <-  renderPlot({
    filt_df <- filter(clean_sex_df, Year <= input$year_slider)
    ggplot(filt_df, aes(fill=Sex, y=SumHivAids, x=Year)) +
      geom_bar(position='dodge', stat='identity') +
      scale_x_continuous(breaks=seq(2010,2021,by=1), limits=c(2010,2021)) +
      scale_y_continuous(limits=c(0,350)) +
      scale_color_manual(
        values = c("Male" = "brown3", "Female" = "cyan4")  
      ) +
      ggtitle("HIV/AIDS Diganoses and Drug Overdose Deaths by Sex Over Time") +
      labs(y = "HIV/AIDS Diganoses and Drug Overdose Deaths", x = "Year")
  })
  
# FOR RACE PAGE
 output$scatter_race <- renderPlot({
   if(input$incl_bl == FALSE){
     clean_df <- filter(clean_df, Race != "Black")
   } 
   if(input$incl_wh == FALSE){
     clean_df <- filter(clean_df, Race != "White")
   }
   if(input$incl_as == FALSE){
     clean_df <- filter(clean_df, Race != "Asian")
   }
   if(input$incl_his == FALSE){
     clean_df <- filter(clean_df, Race != "Hispanic")
   }
  pl_scatter_ra <- ggplot(clean_df, aes(x=Year, y=SumHivAids)) +
     geom_point(aes(color = Race)) +
    scale_x_continuous(breaks=seq(2010,2021,by=1)) +
    scale_y_continuous(limits=c(0,300)) +
     scale_color_manual(
        values = c("White" = "brown3", "Asian" = "gray", "Black" = "purple", "Hispanic" = "cyan4")  
      )+
     ggtitle("HIV/AIDS Diganoses and Drug Overdose Deaths by Race Over Time") + 
    labs(y = "HIV/AIDS Diganoses and Drug Overdose Deaths", x = "Year")
  plot(pl_scatter_ra)
  })
 
 # FOR NEIGHBORHOOD PAGE
 output$graph_neigh <- renderPlot({
  cases_total <- summarise(clean_df)
  clean_neigh_df <- filter(clean_df, Neighborhood == input$neigh_name)
   mean_year_hiv <- mean(filter(clean_df, Year == input$neigh_year)$SumHivAids)
   neigh_plot <- ggplot(data = clean_neigh_df, aes(x = Year, y = SumHivAids))+
     geom_point(aes(color = Race)) +
     scale_x_continuous(breaks=seq(2010,2021,by=1), limits=c(2010,2021)) +
     scale_y_continuous(limits=c(0,100)) +
     geom_hline(yintercept=mean_year_hiv, color = "blue")+
     ggtitle("HIV/AIDS Diganoses and Drug Overdose Deaths by Neighborhood and Race") + 
     labs(y = "HIV/AIDS Diganoses and Drug Overdose Deaths", x = "Year")
   plot(neigh_plot)
 })
}

shinyApp(ui = ui, server = server)

