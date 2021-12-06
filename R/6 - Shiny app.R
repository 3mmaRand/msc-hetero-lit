library(tidyverse) # for general data manipulation and graphing
library(shiny) # for
library(LDAvis) # for interactive visualisation of LDA topic model
library(tidytext) # for text mining tools, particularly tidy() function
library(cowplot) # for compound figure creation

setwd("C:/Users/Evelyn/Documents/My Files/University/Work/3rd year/BSc Research project/dissertation")

# import table of number of papers from each category published each year from 1988-2020
category_years_counts <- read.table("category_years_counts.csv", header = T)

# import table of terms, labels and frequencies for pre-2012 papers
top_terms_tidy_2012_names <- read.table("top_terms_tidy_2012_names.csv", header = T)
# set up a function to name individual facets with the topic names
topic.names_2012 <- as_labeller(
  c(`1` = "1: PCR", `2` = "2: software",`3` = "3: cells", 
    `4` = "4: genomes + evolution",`5` = "5: RNA", `6` = "6: statistical methods",
    `7` = "7: gene editing techniques", `8` = "8: results", `9` = "9: outcomes",
    `10` = "10: databases", `11` = "11: sequence specificity", `12` = "12: proteins"))
# import table of gamma value for pre-2012 topics/papers
model_tidy_gamma_2012 <- read.table("model_tidy_gamma_2012.csv")

#as previous block but for 2020 papers
top_terms_tidy_2020_names <- read.table("top_terms_tidy_2020_names.csv", header = T)
topic.names_2020 <- as_labeller(
  c(`1` = "1: CRISPR-Cas9", `2` = "2: sequencing",`3` = "3: signal transduction \npathways", 
    `4` = "4: antibodies",`5` = "5: gene editing", `6` = "6: therapies",
    `7` = "7: discussion", `8` = "8: sequences", `9` = "9: protein assays",
    `10` = "10: animal models", `11` = "11: cell culture", `12` = "12: plants"))
model_tidy_gamma_2020 <- read.table("model_tidy_gamma_2020.csv")

# load table of rates of growth of the literature
rate_increase <- read.table("rate_increase.csv", header = T)

# load the lists needed for LDAvis generation
load("json_list_2012.RData")
load("json_list_2020.RData")

# add a column denoting whether each row represent the pre- or post-CRISPR era
category_years_counts$era <- ifelse(category_years_counts$year<2012, "pre", "post")

ui <- navbarPage("Title",
  tabPanel("Exploring metadata",
    navlistPanel(
      "Exploring metadata",
      tabPanel("Introduction",
               p("This app explores a dataset of 8340 open-source journal articles on the topic of gene editing."),
               plotOutput("count_over_time",
                          width = 1100, height = 600),
               p("Specifically, it uses topic modelling to examine the ways the field of gene editing might have changed since the advent of CRISPR-Cas9 in 2012.")
               ),
      tabPanel("By category",
               sliderInput("year", "Choose a year:", min = 1988, max = 2020, value = "2000"),
               plotOutput("yearchart")
               ),
      tabPanel("By year",
               selectInput("category", "Choose a category", choices = top15, selected = "Multidisciplinary Sciences"),
               plotOutput("categorychart")
               ),
      widths = c(2, 10)
    )
  ),
  tabPanel("Exploring pre-CRISPR era texts",
    navlistPanel(
      "Exploring pre-CRISPR era texts",
      tabPanel("Introduction"
               ),
      tabPanel("Topic model",
               headerPanel(
                 shinybusy::add_busy_spinner(spin = "fading-circle")
               ),
               sliderInput("nTerms_2012", "Number of terms to display", min = 20, max = 40, value = 30),
               visOutput("ldavis_2012")
               ),
      tabPanel("Topics",
               plotOutput("labelled_categories_2012",
                          width = 1100, height = 600)
              ),
      tabPanel("See how accurate the topic model was",
                 actionButton("goButton2012", "See a random paper"),
                 h3(textOutput("title_2012")),
                 textOutput("abstract_2012"),
                 tableOutput("table_2012")
               ),
      widths = c(2, 10)
    )
  ),
  tabPanel("Exploring post-CRISPR era texts",
    navlistPanel(
      "Exploring post-CRISPR era texts",
      tabPanel("Introduction"
               ),
      tabPanel("Topic model",
               headerPanel(
                 shinybusy::add_busy_spinner(spin = "fading-circle")
               ),
               sliderInput("nTerms_2020", "Number of terms to display", min = 20, max = 40, value = 30),
               visOutput("ldavis_2020")
               ),
      tabPanel("Topics",
               plotOutput("labelled_categories_2020",
                          width = 1100, height = 600)
               ),
      tabPanel("See how accurate the topic model was",
               actionButton("goButton2020", "See a random paper"),
               h3(textOutput("title_2020")),
               textOutput("abstract_2020"),
               tableOutput("table_2020")
      ),
      widths = c(2, 10)
    )
  )
)

server <-   function(input, output, session){
  # metadata - papers published per year
  output$count_over_time <- renderPlot({
    # draw plot of count over time with arrow pointing out the year 2012
    count_plot <- ggplot(rate_increase, aes(year, count)) +
      geom_area(alpha = 0.6, fill = "#91bdda", col = "#91bdda", size = 2) +
      theme_minimal() +
      xlab("") +
      ylab("Total papers published") +
      theme(text = element_text(size = 20)) +
      geom_segment(aes(x = 2012, y = 200,
                       xend = 2012, yend = 185),
                   arrow = arrow(length = unit(0.5, "cm"),ends = "last"),
                   size = 1.5,
                   lineend = "butt",
                   linejoin = "mitre") +
      geom_segment(aes(x = 2012, y = 185,
                       xend = 2012, yend = 350),
                   size = 1.5) +
      theme(text = element_text(size = 20))
    # draw plot of difference in growth over time with arrow for 2012
    growth_plot <- ggplot(rate_increase, aes(year, diff_growth)) +
      geom_bar(stat = "identity", fill = "#cc474d") +
      theme_minimal() +
      xlab("") +
      ylab("Increase in papers on\n previous year") +
      theme(text = element_text(size = 20)) +
      geom_segment(aes(x = 2012, y = -1,
                       xend = 2012, yend = 0),
                   arrow = arrow(length = unit(0.5, "cm"),ends = "last"),
                   size = 1.5,
                   lineend = "butt",
                   linejoin = "mitre") +
      geom_segment(aes(x = 2012, y = -40,
                       xend = 2012, yend = 0),
                   size = 1.5) +
      theme(text = element_text(size = 20))
    # output plots one on top of the other using cowplot
    plot_grid(count_plot, growth_plot, ncol = 1, labels = c("A", "B"))
  })
  # metadata plot - total papers published in each category, by year
  output$yearchart <- renderPlot({
    ggplot(subset(subset(category_years_counts, year == input$year), WC %in% top15)) +
      geom_col(aes(WC, percent, fill = WC)) +
      scale_y_continuous(limits = c(0, 100)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  # metadata plot - total papers published each year, by category
  output$categorychart <- renderPlot({
    ggplot(subset(category_years_counts, WC == input$category)) +
      geom_col(aes(year, count, fill = WC)) +
      scale_y_continuous(limits = c(0, 250)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  # visualisation of LDA data for papers published before 2012
  output$ldavis_2012 <- renderVis({
    with(json_list_2012,
         createJSON(phi_2012, theta_2012, doc.length_2012$n, vocab_2012, term.frequency_2012$n,
                    R = input$nTerms_2012))
  })
  # visualisation of LDA data for papers published in 2020
  output$ldavis_2020 <- renderVis({
    with(json_list_2020,
          createJSON(phi_2020, theta_2020, doc.length_2020$n, vocab_2020, term.frequency_2020$n,
                    R = input$nTerms_2020))
  })
  # bar charts showing top ten words per topic for pre-2012 data
  output$labelled_categories_2012 <- renderPlot({
    ggplot(top_terms_tidy_2012_names, aes(beta, reorder_within(term, beta, newtopic))) +
      geom_col(show.legend = FALSE, fill = "#28527f") +
      facet_wrap(~ newtopic, scales = "free", labeller = topic.names_2012) +
      scale_y_reordered() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.text.x = element_text(size = 15, face = "bold")) +
      xlab("Beta value") +
      ylab("Term") +
      theme(text = element_text(size = 18))
  })
  # bar charts showing top ten words per topic for 2020 data
  output$labelled_categories_2020 <- renderPlot({
    ggplot(top_terms_tidy_2020_names, aes(beta, reorder_within(term, beta, newtopic))) +
      geom_col(show.legend = FALSE, fill = "#28527f") +
      facet_wrap(~ newtopic, scales = "free", labeller = topic.names_2020) +
      scale_y_reordered() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.text.x = element_text(size = 15, face = "bold")) +
      xlab("Beta value") +
      ylab("Term") +
      theme(text = element_text(size = 18))
  })
  # event triggered when 2012 button pushed - selects a random DOI from pre-2012 data
  paper_2012 <- eventReactive(input$goButton2012, {
    model_tidy_gamma_2012[sample(nrow(model_tidy_gamma_2012), 1),]$document
  })
  # event triggered when 2012 button pushed - creates a table containing topics and gamma scores
  paper_table_2012 <- eventReactive(input$goButton2012, {
    arrange(left_join(filter(model_tidy_gamma_2012, document == paper_2012()), topic_labels_2012)[c("label", "gamma")], desc(gamma))
  })
  # event triggered when 2012 button pushed - grabs the title corresponding to the DOI
  paper_title_2012 <- eventReactive(input$goButton2012, {
    filter(metadata, DI == paper_2012())$TI
  })
  # event triggered when 2012 button pushed - grabs the abstract corresponding to the DOI
  paper_abstract_2012 <- eventReactive(input$goButton2012, {
    filter(metadata, DI == paper_2012())$AB
  })
  # event triggered when 2012 button pushed - outputs random 2012 DOI
  output$doi_2012 <- renderText({
    paper_2012()
  })
  # event triggered when 2012 button pushed - outputs random 2012 title
  output$title_2012 <- renderText({
    paper_title_2012()
  })
  # event triggered when 2012 button pushed - outputs random 2012 abstract
  output$abstract_2012 <- renderText({
    paper_abstract_2012()
  })
  # event triggered when 2012 button pushed - outputs random 2012 topic-gamma table
  output$table_2012 <- renderTable({
    paper_table_2012()
  })
  # event triggered when 2020 button pushed - selects a random DOI from 2020 data
  paper_2020<- eventReactive(input$goButton2020, {
    model_tidy_gamma_2020[sample(nrow(model_tidy_gamma_2020), 1),]$document
  })
  # event triggered when 2020 button pushed - creates a table containing topics and gamma scores
  paper_table_2020 <- eventReactive(input$goButton2020, {
    arrange(left_join(filter(model_tidy_gamma_2020, document == paper_2020()), topic_labels_2020)[c("label", "gamma")], desc(gamma))
  })
  # event triggered when 2020 button pushed - grabs the title corresponding to the DOI
  paper_title_2020 <- eventReactive(input$goButton2020, {
    filter(metadata, DI == paper_2020())$TI
  })
  # event triggered when 2020 button pushed - grabs the abstract corresponding to the DOI
  paper_abstract_2020 <- eventReactive(input$goButton2020, {
    filter(metadata, DI == paper_2020())$AB
  })
  # event triggered when 2020 button pushed - outputs random 2020 DOI
  output$doi_2020 <- renderText({
    paper_2020()
  })
  # event triggered when 2020 button pushed - outputs random 2020 title
  output$title_2020 <- renderText({
    paper_title_2020()
  })
  # event triggered when 2020 button pushed - outputs random 2020 abstract
  output$abstract_2020 <- renderText({
    paper_abstract_2020()
  })
  # event triggered when 2020 button pushed - outputs random 2020 topic-gamma table
  output$table_2020 <- renderTable({
    paper_table_2020()
  })
}

shinyApp(ui, server)

