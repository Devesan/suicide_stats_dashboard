# Removing previous lists
rm(list = ls(all.names = TRUE))
getwd()
# install packages
needed_packages <- c('maps','tidyverse','ggplot2','sqldf','gganimate','hrbrthemes','plotrix','RColorBrewer',
                     'rsconnect','shinydashboard')                                    
not_installed <- needed_packages[!(needed_packages %in% installed.packages()[ , "Package"])]    
if(length(not_installed)) install.packages(not_installed)     
library(tidyverse)
library(maps)
library(sqldf)
library(ggplot2)
library(gganimate)
library(hrbrthemes)
library(plotrix)
library(RColorBrewer)
library(shinydashboard)
library(rsconnect)
suicide <- read_csv('https://raw.githubusercontent.com/Devesan/Dataset/main/master.csv')

glimpse(suicide)

unique(suicide$country)

#=============================DATA CLEANING===============================================

#Renaming and removing unwanted columns
drops <- c("country-year","yearlyHDI","Mobile cellular subscriptions (per 100 people)","Contributing family workers, total (% of total employment) (modeled ILO estimate)",
           "suicide%","Expense (% of GDP)","Compensation of employees (% of expense)")

suicide1 <- suicide[,!(names(suicide) %in% drops )]
glimpse(suicide1)

suicide2 <- suicide1 %>% rename("internet"="Individuals using the Internet (% of population)",
                                "unemployment"="Unemployment, total (% of total labor force) (modeled ILO estimate)",
                                "physicians"="Physicians (per 1,000 people)",
                                "legal_rights"="Strength of legal rights index (0=weak to 12=strong)",
                                "total_labor_force"="Labor force, total",
                                "life_expectancy"="Life expectancy at birth, total (years)",
                                "refugees" = "Refugee population by country or territory of origin",
                                "electicity" = "Access to electricity (% of population)",
                                "education" = "Lower secondary completion rate, total (% of relevant age group)")

glimpse(suicide2)

#Finding Null Values (% of null values)
na_count <-lapply(suicide2, function(y) round((sum(length(which(is.na(y)))))/length(y)*100))
na_count <- data.frame(na_count)
na_count

#Removing NA values
# Removing the column legal_rights as it has more than 60% of data as missing
drops1 <- c("legal_rights","education")
suicide3 <- suicide2[,!(names(suicide2) %in% drops1)]
glimpse(suicide3)

na_count <-lapply(suicide3, function(y) round((sum(length(which(is.na(y)))))/length(y)*100))
na_count <- data.frame(na_count)
na_count

#Filling numerical data with mean 
mean_cols <-names(na_count[,na_count>0])
mean_cols

for(i in mean_cols){
  suicide3[is.na(suicide3[,i]), i] <- lapply(suicide3, mean, na.rm = TRUE)$i
}
# Rechecking for NA values
na_count <-lapply(suicide3, function(y) round((sum(length(which(is.na(y)))))/length(y)*100))
na_count <- data.frame(na_count)
na_count

#removing 2016 data and removes 'years' in age column

suicide4 <- filter(suicide3,year != 2016)
suicide4$age <- gsub(" years","",suicide4$age)
#updating country names
names <- c("Russian Federation","United Kingdom","United States")
to_names <- c("Russia","UK","USA")
suicide4$country<-
  replace(suicide4$country,suicide4$country %in% names,to_names)

# For the shiny app
countries1 <-unique(suicide4$country)
countries1<-as.data.frame(countries1)
countries1<-append(countries1$countries1,'ALL')
unique(countries1)


#================================DATA EXPLORATION========================================

#============================Exploration 1(MAP)==========================

#Country wise suicides
most_suicide <- sqldf("select country, sum(suicidesper100k) as total from suicide4 group by country ")
most_suicide
head(most_suicide %>% arrange(desc(total)))
world_map <- map_data("world")


map<-ggplot(most_suicide, aes(map_id = country)) +
  geom_map(dat=world_map, map = world_map, aes(map_id=region), fill="white", color="black")+
  geom_map(aes(fill = total ), map = world_map) + expand_limits(x = world_map$long, y = world_map$lat)+
  scale_fill_viridis_c(option = "C")
#map
#animated map
#map <- map+transition_reveal(total)
#map
#anim_save("map.gif")

#top 5 countries with highest suicides df
countries2 <- head(most_suicide %>% arrange(desc(total)),5)
countries2 <- countries2$country
countries2

#===========================Exploration 2(Year wise Exploration)==============================

#Year wise suicides

year_suicide <- sqldf('select year,sum(suicidesper100k) as total from suicide4 group by year')
head(year_suicide)
yearly<-ggplot(year_suicide, aes(x=year, y=total)) +
  geom_line( color="steelblue") + 
  geom_point() +
  xlab("Years") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  scale_x_continuous(limit=c(1984,2016))
#yearly
#yearly <- yearly +transition_reveal(year)
#yearly
#anim_save("yearly.gif")


#Year wise suicide & gender wise

year_suicide_gender <- sqldf('select year,sex,sum(suicidesper100k) as total from suicide4 group by year,sex')
head(year_suicide_gender)
yearly_gen<- ggplot(year_suicide_gender,aes(x=year,y=total,colour=sex,group=sex)) + geom_line()
#yearly_gen
#yearly_gen <- yearly_gen +transition_reveal(year)
#yearly_gen
#anim_save("yearly_gen.gif")



#===============================Exploration 3(Age wise Exploration)=======================================

#Analysing age wise suicides

age_suicide <- sqldf('select age, sum(suicidesper100k) as total from suicide4 group by age')
head(age_suicide)
age_suicide$perc <- round(age_suicide$total / sum(age_suicide$total)*100)
pie3D(age_suicide$total, labels =paste(age_suicide$age,",",age_suicide$perc,"%"), main = "An exploded 3D pie chart",
      explode=0.1, radius=0.9, labelcex = 0.9,  start=0.7)+transition_reveal(age)


#Year wise suicide & age wise

year_suicide_age <- sqldf('select year,age,sum(suicidesper100k) as total from suicide4 group by year,age')
head(year_suicide_age)
yearly_age <- ggplot(year_suicide_age,aes(x=year,y=total,colour=age,group=age)) + geom_line()+theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))
#yearly_age
#yearly_age <- yearly_age+transition_reveal(year)
#yearly_age
#anim_save("yearly_age.gif")

#=================================Exploration 4(Gender wise Exploration)============================

#gender vs suicides


coul <- brewer.pal(5, "Set2") 
gender_suicide <- sqldf('select sex, sum(suicidesper100k) as total from suicide4 group by sex')
gender_suicide$perc <- round(gender_suicide$total/sum(gender_suicide$total)*100)
gender_suicide %>% filter(sex =="male") %>% select(perc)
barplot(height=gender_suicide$total, names=gender_suicide$sex,col = coul)
#country wise gender suicides

country_gen_suicide <- sqldf('select country,sex,sum(suicidesper100k) as total from suicide4 group by country,sex')

country_gen_suicide <- country_gen_suicide  %>% arrange(desc(total))

head(country_gen_suicide)
gen_2 <- filter(country_gen_suicide,country %in% countries2)
ggplot(country_gen_suicide, aes(x=reorder(country, total), y=total,fill=sex)) +
  geom_bar(position="stack", stat="identity")+
  labs(title = "Proportions of suicides that are Male & Female, by Country", 
       x = "Country", 
       y = "Suicides per 100k",
       fill = "Sex")+
  theme_light() +
  coord_flip()+
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )

#=================================Exploration 5(GDP wise Exploration)============================
#year gdp suicide
coeff <- 1
year_gdp_suicide <- sqldf('select year,sum("GDPpyear") as GDP, sum(suicidesper100k) as total from suicide4 group by year')
year_gdp_suicide$GDP <- year_gdp_suicide$GDP/ (10^11)
head(year_gdp_suicide)
gdp<- ggplot(year_gdp_suicide,aes(x=year))+
  geom_line( aes(y=GDP), size=1,color = "#69b3a2") + 
  geom_line( aes(y=total), size=1,color = rgb(0.2, 0.6, 0.9, 1))+
  scale_y_continuous(
    
    # Features of the first axis
    name = "GDP ($) in 100 Billions",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Suicides")
  ) + 
  
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color ="#69b3a2" , size=13),
    axis.title.y.right = element_text(color =rgb(0.2, 0.6, 0.9, 1) , size=13)
  ) +
  
  ggtitle("GDP down, suicides up")
#gdp
#gdp <- gdp +transition_reveal(year)
#gdp
#anim_save("gdp.gif")

#top 5 countries with high gdp and suicides
country_gdp <- sqldf('select country, sum("GDPpyear") as GDP from suicide4 group by country order by GDP desc limit 5')
head(country_gdp)

#countries with highest GDP
countries <- as.list(country_gdp$country)

country_gdp_suicide <- sqldf('select country, year,sum("suicidesper100k") as total from suicide4 group by country,year ')
country_gdp_suicide <- country_gdp_suicide%>%filter(country %in% countries)
country_gdp_suicide

#mergin data
country_final <- merge.data.frame(country_gdp,country_gdp_suicide, by.x = "country",by.y = "country")
country_final$GDP <- country_final$GDP/ (10^11)
country_final
suicide_country <- ggplot(country_final,aes(x=year)) + 
  geom_line(aes(y=total,col=country,group=country), size=1) + 
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) 
#suicide_country
#suicide_country <- suicide_country + transition_reveal(year)
#suicide_country
#anim_save("suicide_country.gif")

#=================================Exploration 6(Gender wise Exploration)============================
#gen vs suicide top 5 country wise
# Set a number of 'empty bar' to add at the end of each group
countries2

gen_suicide <- sqldf('select country, sum(suicidesper100k) as total, generation from suicide4 group by country,generation')
gen_suicide
gen_suicide <- gen_suicide%>%filter(country %in% countries2)
gen_suicide
gen_suicide$country<-as.factor(gen_suicide$country)
gen_suicide$generation <- as.factor(gen_suicide$generation)
gen_suicide$total <- round(gen_suicide$total)

data <-gen_suicide
names(data)<- c("group","value","individual")
data

empty_bar <- 3
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(group)
data$id <- seq(1, nrow(data))
# Get the name and the y position of each label
label_data <- data

number_of_bar <- nrow(label_data)
number_of_bar
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)
label_data
# prepare a data frame for base lines
base_data <- data %>% 
  group_by(group) %>% 
  summarise(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

data
# Make the plot
p <- ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  
  geom_segment(data=grid_data, aes(x = end, y = 500, xend = start, yend = 500), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 1000, xend = start, yend = 1000), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 1500, xend = start, yend = 1500), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 2000, xend = start, yend = 2000), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 2500, xend = start, yend = 2500), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(data$id),5), y = c(500, 1000, 1500, 2000,2500), label = c("500", "1000", "1500", "2000","2500") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  ylim(-2500,4000) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=value+500, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -100, label=group), hjust=c(1,1,1,0,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)

#p



#==============================DASHBOARD APP======================================


server <- shinyServer(function(input, output){
  
  output$map <- renderPlot({
    #Country wise suicides
    most_1 <- filter(most_suicide,country==input$Country)
    most_1
    if(input$Country=='ALL'){
      ggplot(most_suicide, aes(map_id = country)) +
        geom_map(dat=world_map, map = world_map, aes(map_id=region), fill="white", color="black")+
        geom_map(aes(fill = total ), map = world_map) + expand_limits(x = world_map$long, y = world_map$lat)+
        scale_fill_viridis_c(option = "C")
    }
    else{
      ggplot(most_1, aes(map_id = country)) +
        geom_map(dat=world_map, map = world_map, aes(map_id=region), fill="white", color="black")+
        geom_map(aes(fill = total ), map = world_map) + expand_limits(x = world_map$long, y = world_map$lat)+
        scale_fill_viridis_c(option = "C")
    }
    
  })
  
 output$age_pie <- renderPlot({
    
    #Analysiing age wise suicides
    library(plotrix)
       pie3D(age_suicide$total, labels =paste(age_suicide$age,",",age_suicide$perc,"%"), main = "An exploded 3D pie chart",
          explode=0.1, radius=0.9, labelcex = 0.9,  start=0.7)
  })
  
  output$country_gen <- renderPlot({
    
    if(input$Country=='ALL'){
      gen_2 <- filter(country_gen_suicide,country %in% countries2)
      ggplot(gen_2, aes(x=reorder(country, total), y=total,fill=sex)) +
        geom_bar(position="stack", stat="identity")+
        labs(title = "Proportions of suicides that are Male & Female, by Country", 
             x = "Country", 
             y = "Suicides per 100k",
             fill = "Sex")+
        theme_light() +
        coord_flip()+
        theme(
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank()
        )
    }
    else{
      gen_1 <- filter(country_gen_suicide,country==input$Country)
      ggplot(gen_1, aes(x=reorder(country, total), y=total,fill=sex)) +
        geom_bar(position="stack", stat="identity")+
        labs(title = "Proportions of suicides that are Male & Female, by Country", 
             x = "Country", 
             y = "Suicides per 100k",
             fill = "Sex")+
        theme_light() +
        coord_flip()+
        theme(
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank()
        )
    }
    
    
  })
  
  output$progressBox <- renderValueBox({
    valueBox(
      paste0(gender_suicide %>% filter(sex =="male") %>% select(perc), "%"), "Male Suicides", icon = icon("mars"),
      color = "purple"
    )
  })
  output$progressBox1 <- renderValueBox({
    valueBox(
      paste0(head(country_gen_suicide$country,1)), "Most Suicides Country", icon = icon("globe"),
      color = "blue"
    )
  })
  
  
  output$gdp_suicide <- renderPlot({
    #year gdp suicide
    ggplot(year_gdp_suicide,aes(x=year))+
      geom_line( aes(y=GDP), size=1,color = "#69b3a2") + 
      geom_line( aes(y=total), size=1,color = rgb(0.2, 0.6, 0.9, 1))+
      scale_y_continuous(
        
        # Features of the first axis
        name = "GDP ($) in 100 Billions",
        
        # Add a second axis and specify its features
        sec.axis = sec_axis(~.*coeff, name="Suicides")
      ) + 
      
      theme_ipsum() +
      
      theme(
        axis.title.y = element_text(color ="#69b3a2" , size=13),
        axis.title.y.right = element_text(color =rgb(0.2, 0.6, 0.9, 1) , size=13)
      ) +
      
      ggtitle("GDP down, suicides up")
  })
  output$flower <- renderPlot({
    p
  })
  
  
})

ui<- shinyUI(
  dashboardPage(
    skin = "purple",
    dashboardHeader(title = "Global Suiciide Viisualisation"),
    dashboardSidebar(
      selectInput("Country","Select a country",
                  choices = c(unique(countries1)),selected = 'ALL'),
      
      sidebarMenu(
        id ="tabs",
        menuItem("Dashboard",tabName = "dashboard",icon = icon("dashboard"))
      ),
      textOutput("res"),
      valueBox(1995, "Year with most suicides", icon = icon("calendar"),width = 12),
      valueBoxOutput("progressBox",width = 12),
      valueBoxOutput("progressBox1",width = 12)
    ),
    dashboardBody(
      
      fillPage(
        tags$style(type = "text/css",
                   "
                   #age_pie{height:300px !important}
                   #age_pie img{height:300px}
                   #gdp_suicide{height:290px !important}
                   #gdp_suicide img{height:290px}
                   #country_gen{height:250px !important}
                   #age_pie country_gen{height:250px}
                   
                   "
        ),
        fluidRow(
          box(title = "Global Suicide rate per 100K", plotOutput("map"),width = 7),
          box(title = "Top 4 Countries Suicide rate Generation wise",plotOutput("flower"),width = 5)

        ),
        fluidRow(
          box(title = "Age wise Suicide Percentage",plotOutput("age_pie"),width=4),
          box(title = "GDP wise Suicide Percentage",plotOutput("gdp_suicide"),width=4),
          box(title = "Top 5 countries suicide with gender categorisation",plotOutput("country_gen"),width=4),
          
          
        )
      )
      
    )
  )
)




shinyApp(ui = ui, server = server)
