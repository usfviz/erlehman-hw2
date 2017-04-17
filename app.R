
library('reshape')
library('plotly')
library('shiny')
library('ggplot2')

rename <-reshape::rename

#Read in Data
life_exp <- read.csv('API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv', skip = 4)
fert_rate <- read.csv('API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv', skip = 4)
region_df <- read.csv('Metadata_Country_API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv')
pop_df <- read.csv('API_SP.POP.TOTL_DS2_en_csv_v2.csv')

#Remove un-needed columsn
life_exp <- subset(life_exp, select = -c(Indicator.Name, Indicator.Code, X))
fert_rate <- subset(fert_rate, select = -c(Indicator.Name, Indicator.Code, X))
pop_df <- subset(pop_df, select = -c(Indicator.Name, Indicator.Code, X))

#Melt life expectancy file to useful form
life_exp_melt <- reshape:::melt(life_exp, id=c("Country.Name","Country.Code"),
                   variable = 'year', na.rm = TRUE)
life_exp_melt <- rename(life_exp_melt, c('value'='life_expectancy'))
life_exp_melt$year = as.character(life_exp_melt$year)

#Melt fertility file to useful form
fert_rate_melt <- reshape:::melt(fert_rate, id=c("Country.Name","Country.Code"),
                   variable = 'year', na.rm = TRUE)
fert_rate_melt <- rename(fert_rate_melt, c('value'='fertility_rate'))
fert_rate_melt$year = as.character(fert_rate_melt$year)

#Melt population file to useful form
pop_melt <- reshape:::melt(pop_df, id=c("Country.Name","Country.Code"),
                   variable = 'year', na.rm = TRUE)
pop_melt <- rename(pop_melt, c('value'='population'))
pop_melt$year = as.character(pop_melt$year)

#Combine into single file
combine1 = merge(life_exp_melt, fert_rate_melt)
combine2 = merge(combine1, pop_melt)
combine2$year = as.integer(gsub('X', "", combine2$year))
region_df <- subset(region_df, select = -c(IncomeGroup, SpecialNotes, TableName, X))
final_df <- merge(combine2, region_df)
final_df <- final_df[final_df$Region != "",]
final_df$Region <- factor(final_df$Region)

ui <- fluidPage(fluidRow(
                 column(12,plotlyOutput("out"))),
                fluidRow(
                 column(5, offset = 1,sliderInput(inputId = "num", label = "Year",animate = TRUE,
                                      ticks = FALSE,
                                       value = 2000, min = 1960, max = 2014, sep = '', width = '80%')),
                 column(6, sliderInput(inputId = 'pop', label = 'Population',
                             value = 10, min = 1, max = 30, width = '30%', ticks = FALSE))
                )
               
      )






server <- function(input, output) {
  output$out <- renderPlotly({
    new_df <- final_df[final_df$year == input$num,]
    plot1 <- ggplot(new_df, aes(life_expectancy, fertility_rate, colour=Region, size = population, key = Country.Name)) +
      geom_point(alpha = 0.60) + labs(x = 'Life expectancy', y = 'Fertility rate')
    plot1 <- plot1 + theme_bw() + theme(panel.grid.major = element_line(colour = "grey"))
    plot1 <- plot1 + theme(panel.grid.minor = element_blank())
    plot1 <- plot1 + scale_colour_manual(values = c("blue","red3", "orange", "green3", "purple", "steelblue1", "violetred2"))
    plot1 <- plot1 + scale_x_continuous(breaks = seq(10,90,10), limits = c(10,90))
    plot1 <- plot1 + scale_y_continuous(breaks = seq(0,9,1), limits = c(0,9))
    plot1 <- plot1 + scale_size(range = c(1,input$pop), guide = FALSE)
    plot1 <- plot1 + theme(legend.title = element_blank()) + ggtitle('World Health Organization Regional Data')
    plot1
  })
}

shinyApp(ui = ui, server = server)