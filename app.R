library(shiny)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(viridis)
library(shinydashboard)
library(stringr)
library(scales)
library(reshape2)
library(tidyverse)
library(rsconnect)

#read dataset
#uni employment survey data
grad <- read.csv("https://raw.githubusercontent.com/wzs02/UniStatsBank/main/grads.csv")
#us uni data
us <- read.csv("https://raw.githubusercontent.com/wzs02/UniStatsBank/main/College_Data.csv")

#HANDLE SG UNI DATA
#create factor column for year
grad <- grad %>% mutate(Year=as.factor(year))

#convert all nummber columns to numeric
grad[,c(5:12)] <- as.numeric(unlist(grad[,c(5:12)]))

#HANDLE US UNI DATA
#create column for total cost 
us <- us %>% mutate(Total=Outstate+Room.Board+Books+Personal+Expend)
#create column for % acceptance to 1 dp
us <- us %>% mutate(Accepted=round(100*(Accept/Apps),1))
#create column for % confirmed students to 1dp
us <- us %>% mutate(Confirmed=round(100*(Enroll/Accept),1))



#create user interface 

#names of sg unis
sg_u_names= unique(grad$university)

ui <- dashboardPage(skin="black",dashboardHeader(title = "UniStatsBank"),   
                    dashboardSidebar(sidebarMenu(menuItem("SG Unis",tabName="singu"),
                                                 selectizeInput("sguni1",label="Choose Uni 1",choices=grad$university,options=list(maxItems=1)),
                                                 uiOutput("sgcourse1"),
                                                 selectizeInput("sguni2",label="Choose Uni 2",choices=grad$university,options=list(maxItems=1)),
                                                 uiOutput("sgcourse2"),
                                                 selectizeInput("sguni3",label="Choose Uni 3",choices=grad$university,options=list(maxItems=1)),
                                                 uiOutput("sgcourse3"),
                                                 actionButton("sgfinal","Compare Results"),
                                                 menuItem("US Unis",tabName="usu"),
                                                 selectizeInput("usunames",label="Choose up to 5 US unis",
                                                                choices=us$X,multiple=T,options=list(maxItems=5)),
                                                 actionButton("usfinal","Compare Results"))),
                    dashboardBody(tabItems(tabItem(tabName="singu",strong(h1("Comparative Stats for SG Unis")),br(),
                                                   h2("Comparison of Basic Mean Salaries"),br(),
                                                   fluidRow(box(width=4,height=380,
                                                                title="Basic Mean Salary for Uni 1\n
                    Course 1", status="info",solidHeader=TRUE,
                                                                collapsible=TRUE,plotOutput("mean1")),
                                                            box(width=4,height=380,title= "Basic Mean Salary for Uni 2\n
                    Course 2", status="info",solidHeader=TRUE,
                                                                collapsible=TRUE, plotOutput("mean2")),
                                                            box(width=4,height=380,
                                                                title="Basic Mean Salary for Uni 3\n
                    Course 3", status="info",solidHeader=TRUE,
                                                                collapsible=TRUE,plotOutput("mean3"))),
                                                   fluidRow(box(title="Significance of title",strong("Basic Salary"),
                                                                p("Regular amount earned before any additions (due to 
                          bonus, commission etc.) or deductions (due to CPF etc.)"),
                                                                br(),width=12,height=120,background="aqua")),br(),
                                                   h2("Comparison of Full-Time and Overall Employment Rates"), br(),
                                                   
                                                   fluidRow(box(width=4,height=380,title="Full-Time and Overall Employment\n
                                        for Uni 1 Course 1", status="success",solidHeader=TRUE,
                                                                collapsible=TRUE,plotOutput("emp1")),
                                                            box(width=4,height=380,title="Full-Time and Overall Employment\n
                                        for Uni 2 Course 2", status="success",solidHeader=TRUE,
                                                                collapsible=TRUE,plotOutput("emp2")),
                                                            box(width=4,height=380,title="Full-Time and Overall Employment\n
                                        for Uni 3 Course 3", status="success",solidHeader=TRUE,
                                                                collapsible=TRUE,plotOutput("emp3"))),
                                                   
                                                   fluidRow(box(title="Significance of plot elements",
                                                                strong("Lines"),p("General trend for % of graduates
                                                                  employed over the years"),br(),
                                                                strong("Legend"),p("FT --> Full-time Employment"),
                                                                p("OV --> Overall Employment"),
                                                                width=12,height=220,background="olive")), 
                                                   br(),br(),h2("Comparison of Gross Monthly Salaries"),br(),
                                                   
                                                   fluidRow(box(width=4,height=380,title="Gross Monthly Salary for Uni 1 \nCourse 1", status="warning",solidHeader=TRUE,
                                                                collapsible=TRUE,plotOutput("gr1")),
                                                            box(width=4,height=380,title="Gross Monthly Salary for Uni 2 \nCourse 2", status="warning",solidHeader=TRUE,
                                                                collapsible=TRUE,plotOutput("gr2")),
                                                            box(width=4,height=380,title="Gross Monthly Salary for Uni 3 \nCourse 3", status="warning",solidHeader=TRUE,
                                                                collapsible=TRUE,plotOutput("gr3"))),
                                                   fluidRow(box(title="Significance of title and plot elements",
                                                                strong("Gross Salary"),p("Total amount earned before
                                                                          taxes or other deductions, is
                                                                          inclusive of property and services
                                                                          received from one's employer"),
                                                                br(), strong("Legend"), p("25 --> Gross monthly salary of 
                                                                           graduates at the bottom 25th % in terms 
                                                                           of gross monthly earnings (out of 
                                                                           graduates surveyed)"),
                                                                p("50 --> Gross monthly salary of 
                                                                           graduates at the 50th % in terms 
                                                                           of gross monthly earnings (out of 
                                                                           graduates surveyed)"),
                                                                p("75 --> Gross monthly salary of 
                                                                           graduates at the top 25% in terms 
                                                                           of gross monthly earnings (out of 
                                                                           graduates surveyed)"),
                                                                width=12,height=250,background="orange"))),
                                           tabItem(tabName="usu", strong(h1("Comparative Stats for US Unis")),
                                                   br(),h2("Factors affecting Gradation Rate"),
                                                   fluidRow(box(width=12,height=490,title="Look at the 3rd row from bottom and 3rd column from the left",
                                                                plotOutput("p1"))),
                                                   fluidRow(box(background="blue",width=12,height=380,title="Significance of plot elements",strong("Numbers"),
                                                                p("The magnitude of each number indicates the strength of 
                                           linear correlation between the 2 corresponding variables on 
                                           the x and y axes, with +1 indicative of strong positive 
                                           linear correlation, -1 of strong negative linear correlation 
                                           and 0 of no linear correlation"),strong("Axis elements"),
                                                                p("Top25perc --> New students from top 25% of high school class"),
                                                                p("Top10perc --> New students from top 10% of high school class"),
                                                                p("Total --> Total amount spent on out of state tuition, board
                                             room and board costs, books, personal spending and 
                                             instructional expenditure"),
                                                                p("Expend --> Instructional expenditure per student, which is the 
                                           amount spent on salaries and benefits for teachers and teacher aides,
                                            including textbooks and supplies"),
                                                                p("Grad.Rate --> Graduation Rate"),
                                                                p("Books --> Estimated book costs"),
                                                                p("S.F. Ratio --> Student-Faculty Ratio"))),br(),
                                                   h2("Relevant Percentages"),
                                                   fluidRow(title="% Acceptance, % Enrollment and % of Students in top 10% and 25%",
                                                            box(width=12,plotOutput("p2"))),
                                                   fluidRow(box(background="light-blue",width=12,title="Signficance of plot elements",
                                                                p("Accepted --> % of applicants accepted by the university"),
                                                                p("Confirmed --> % of successful applicants who accepted the 
                                           offer made by the university"),
                                                                p("Grad Rate --> % of students who graduated from the university"),
                                                                p("Top10perc --> New students from top 10% of high school class"),
                                                                p("Top25perc --> New students from top 25% of high school class"))),br(),
                                                   h2("Total Cost by University"),
                                                   fluidRow(box(width=12,plotOutput("p3"))),
                                                   fluidRow(box(background="aqua",width=12,title="Significance of title",
                                                                p("Total --> Total amount spent on out of state tuition, board
                                             room and board costs, books, personal spending and 
                                             instructional expenditure"))),br(),
                                                   h2("Comparison of cost by component"),
                                                   fluidRow(box(width=12,plotOutput("p4"))),
                                                   fluidRow(box(background="teal",width=12,title="Significance of plot elements",
                                                                p("Books --> Estimated book costs"),
                                                                p("Expend --> Instructional expenditure per student, 
                                     which is the amount spent on salaries and benefits 
                                     for teachers and teacher aides,
                                     including textbooks and supplies"),
                                                                p("Outstate --> Out-of-state tuition"),
                                                                p("Personal --> Estimated personal spending"),
                                                                p("Room.Board --> Room and board costs"))))
                    )))

#function to filter by degree and course for sg uni
double <- function(uni,course){
    d1 <- grad %>% filter(university==uni & degree==course)
    return(d1)
}

#work on rendering code for server
server <- function(input,output){
    output$sgcourse1 <- renderUI({
        #filter out viable course options 
        op <- grad %>% filter(university==input$sguni1) %>% select(degree)
        #output ui dropdown bar 
        selectizeInput("sgucourse1",label="Choose Uni 1 Course",choices=op,options=list(maxItems=1))
    })
    
    output$sgcourse2 <- renderUI({
        #filter out viable course options 
        op <- grad %>% filter(university==input$sguni2) %>% select(degree)
        #output ui dropdown bar 
        selectizeInput("sgucourse2",label="Choose Uni 2 Course",choices=op,options=list(maxItems=1))
    })
    
    output$sgcourse3 <- renderUI({
        #filter out viable course options 
        op <- grad %>% filter(university==input$sguni3) %>% select(degree)
        #output ui dropdown bar 
        selectizeInput("sgucourse3",label="Choose Uni 3 Course",choices=op,options=list(maxItems=1))
    })
    
    #trigger action button to create plots for us uni
    observeEvent(input$usfinal,{
        #compile dataframe with selected unis and use for subsequent plots
        usfin <- us %>% filter(X %in% input$usunames)
        #Output heatmap to show RS btw graduation
        lmdata <- us[,c(6,7,12,16,18,19,20)]
        #compute correlation matrix
        cormat <- round(cor(lmdata),2)
        #remove upper parts of matrix
        get_upper_tri <- function(cormat){
            cormat[lower.tri(cormat)]<- NA
            return(cormat)
        }
        #reorder matrix 
        reorder_cormat <- function(cormat){
            # Use correlation between variables as distance
            dd <- as.dist((1-cormat)/2)
            hc <- hclust(dd)
            cormat <-cormat[hc$order, hc$order]
        }
        #output matrix 
        cormat <- reorder_cormat(cormat)
        upper_tri <- get_upper_tri(cormat)
        # Melt the correlation matrix
        melted_cormat <- melt(upper_tri, na.rm = TRUE)
        # Create ggheatmap
        ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
            geom_tile(color = "white")+
            scale_fill_gradient2(low = "green", high = "purple", mid = "pink", 
                                 midpoint = 0, limit = c(-1,1), space = "Lab", 
                                 name="Pearson\nCorrelation") +
            theme_minimal()+ # minimal theme
            theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                             size = 12, hjust = 1))+
            coord_fixed()
        #output heatmap
        output$p1 <- renderPlot({
            ggheatmap + geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
                theme(
                    axis.title.x = element_blank(),
                    axis.title.y = element_blank(),
                    panel.grid.major = element_blank(),
                    panel.border = element_blank(),
                    panel.background = element_blank(),
                    axis.ticks = element_blank(),
                    legend.justification = c(1, 0),
                    legend.position = c(0.6, 0.7),
                    legend.direction = "horizontal")+
                guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                             title.position = "top", title.hjust = 0.5))
        })
        
        
        #remodel dataframe to plot stacked barplot comparing acceptance rate vs
        #people who take offer, including top 10% and 25% to gauge academic
        #competition
        sub1 <- gather(usfin,state,percent,c("Accepted","Confirmed","Top10perc","Top25perc","Grad.Rate"))
        #output grouped barplot
        output$p2 <- renderPlot({
            ggplot(sub1, aes(x =X, y= percent, fill = state)) +
                geom_bar(width=0.6,stat = "identity", position = "dodge")+   
                theme_classic()+
                labs(x="University",y="Percentage (%)")+
                guides(fill=guide_legend(title="University")) +
                theme(legend.position="bottom")+
                geom_text(aes(X, label = percent),
                          position = position_dodge(width = 0.6)) + scale_fill_brewer(palette="Set3")
            
        })
        #output barplot for total cost
        output$p3 <- renderPlot({
            ggplot(usfin,aes(X,Total,fill=X))+geom_bar(width=0.4,stat="identity")+
                scale_fill_brewer(palette="Paired") +
                labs(x="University",y="Total ($)")+
                guides(fill=guide_legend(title="University")) +
                geom_text(aes(X, label = Total),
                          position = position_dodge(width = 0.4))+ theme_classic()+
                theme(legend.position="bottom")+
                scale_x_discrete(labels = function(X) lapply(strwrap(X, width = 10, simplify = FALSE), paste, collapse="\n"))
        })
        
        #output barplot for individual cost components
        #remodel data to do faceted barplot so that individual components are 
        #organised
        sub3<- gather(usfin,type,amt,c(10:13,18))
        #output faceted barplot
        output$p4 <- renderPlot({
            ggplot(sub3,aes(X,amt,fill=X,label=amt))+
                labs(x="University",y="Amount ($)")+
                geom_bar(width=0.6,stat="identity")+theme(legend.position = "bottom")+ 
                labs(y="Amount ($)",x="University")+ theme_classic() +
                guides(fill=guide_legend(title="University")) +
                theme(legend.position="bottom")+
                scale_fill_brewer(palette="Set2") +
                facet_wrap(~type)+
                geom_text(position = position_stack(vjust = .5))  +
                scale_x_discrete(labels = function(X) lapply(strwrap(X, width = 10, simplify = FALSE), paste, collapse="\n"))
        })
        
    })
    
    
    #trigger action button to create plots for sg uni
    observeEvent(input$sgfinal,{
        #output 1st plot comparing basic monthly salary by year
        d1 <- double(input$sguni1,input$sgucourse1)
        d2 <- double(input$sguni2,input$sgucourse2)
        d3 <- double(input$sguni3,input$sgucourse3)
        output$mean1 <- renderPlot({
            ggplot(d1,aes(x=as.factor(year),y=basic_monthly_mean,
                          fill=as.factor(year))) + geom_bar(stat="identity")+
                scale_fill_viridis_d(option="viridis") + theme_classic() +
                labs(x="Year",y="Salary($)") + guides(fill=guide_legend(title="Year")) +
                theme(legend.position="bottom") 
        },height=300,width=300)
        
        #output 2nd plot for 2nd course
        output$mean2 <- renderPlot({
            ggplot(d2,aes(x=as.factor(year),y=basic_monthly_mean,
                          fill=as.factor(year))) + geom_bar(stat="identity")+
                scale_fill_viridis_d(option="viridis") + theme_classic() +
                labs(x="Year",y="Salary($)") + guides(fill=guide_legend(title="Year")) +
                theme(legend.position="bottom")
        },height=300,width=300) 
        
        #output 3rd plot for 3rd course
        output$mean3 <- renderPlot({
            ggplot(d3,aes(x=as.factor(year),y=basic_monthly_mean,
                          fill=as.factor(year))) + geom_bar(stat="identity")+
                scale_fill_viridis_d(option="viridis") + theme_classic() +
                labs(x="Year",y="Salary($)") + guides(fill=guide_legend(title="Year")) +
                theme(legend.position="bottom")
        },height=300,width=300) 
        
        
        #create new dataframe to process scatterplot and trend line for 1st course
        ft1 <- d1 %>% select(year,employment_rate_ft_perm)
        ov1 <- d1 %>% select(year,employment_rate_overall)
        #add new column to ft1 and ov1
        ft1$status <- as.vector(rep("FT",nrow(ft1)))
        ov1$status <- as.vector(rep("OV",nrow(ov1)))
        #use these 2 dataframes to create a new data frame
        percent1 <- as.vector(rbind(ft1$employment_rate_ft_perm,ov1$employment_rate_overall))
        year1 <- as.vector(rbind(ft1$year,ov1$year))
        year1 <- as.factor(year1)
        Type <- as.vector(rbind(ft1$status,ov1$status))
        #output new dataframe 
        new1 <- data.frame(percent1,year1,Type)
        #output scatterplot and trend line for overall and FT employment, 1st course
        output$emp1 <- renderPlot({
            ggplot(new1,aes(x=year1,y=percent1,color=Type,group=Type)) + 
                geom_point(size=3)+ scale_color_brewer(palette="Dark2") + 
                theme_classic() + geom_smooth(method=lm,se=FALSE) +
                labs(x="Year",y="Salary($)") + labs(x="Year",y="Percentage (%)")+
                theme(legend.position="bottom")
        },height=300,width=300)
        
        #output scatterplot and trend line for employment rate, 2nd course
        #create new dataframe to process dotchart for 2nd course
        ft2 <- d2 %>% select(year,employment_rate_ft_perm)
        ov2 <- d2 %>% select(year,employment_rate_overall)
        #add new column to ft1 and ov1
        ft2$status <- as.vector(rep("FT",nrow(ft2)))
        ov2$status <- as.vector(rep("OV",nrow(ov2)))
        #use these 2 dataframes to create a new data frame
        percent2 <- as.vector(rbind(ft2$employment_rate_ft_perm,ov2$employment_rate_overall))
        year2 <- as.vector(rbind(ft2$year,ov2$year))
        year2 <- as.factor(year2)
        Type <- as.vector(rbind(ft2$status,ov2$status))
        #output new dataframe 
        new2 <- data.frame(percent2,year2,Type)
        #output scatterplot and trend line for overall and FT employment, 2nd course
        output$emp2 <- renderPlot({
            ggplot(new2,aes(x=year2,y=percent2,color=Type,group=Type)) + 
                geom_point(size=3)+ scale_color_brewer(palette="Dark2") + 
                theme_classic() + geom_smooth(method=lm,se=FALSE) +
                labs(x="Year",y="Salary ($)") + labs(x="Year",y="Percentage (%)")+
                theme(legend.position="bottom")
        },height=300,width=300) 
        
        #output scatterplot and trendline for employment rate, 3rd course
        #create new dataframe to process dotchart for 3rd course
        ft3 <- d3 %>% select(year,employment_rate_ft_perm)
        ov3 <- d3 %>% select(year,employment_rate_overall)
        #add new column to ft1 and ov1
        ft3$status <- as.vector(rep("FT",nrow(ft3)))
        ov3$status <- as.vector(rep("OV",nrow(ov3)))
        #use these 2 dataframes to create a new data frame
        percent3 <- as.vector(rbind(ft3$employment_rate_ft_perm,ov3$employment_rate_overall))
        year3 <- as.vector(rbind(ft3$year,ov3$year))
        year3 <- as.factor(year3)
        Type <- as.vector(rbind(ft3$status,ov3$status))
        #output new dataframe 
        new3 <- data.frame(percent3,year3,Type)
        #output scatterplot and trend line for overall and FT employment, 3rd course
        output$emp3 <- renderPlot({
            ggplot(new3,aes(x=year3,y=percent3,color=Type,group=Type)) + 
                geom_point(size=3)+ scale_color_brewer(palette="Dark2") + 
                theme_classic() + geom_smooth(method=lm,se=FALSE) +
                labs(x="Year",y="Salary ($)") + labs(x="Year",y="Percentage (%)")+
                theme(legend.position="bottom")
        },height=300,width=300) 
        
        
        #output line graph for 1st course (gross 25,50,75)
        #create new dataframe to process barplot for course 1
        gr25_1 <- d1 %>% select(year,gross_mthly_25_percentile)
        gr50_1 <- d1 %>% select(year,gross_monthly_median)
        gr75_1 <- d1 %>% select(year,gross_mthly_75_percentile)
        #create status column
        gr25_1$status <- as.vector(rep("25",nrow(gr25_1)))
        gr50_1$status <- as.vector(rep("50",nrow(gr50_1)))
        gr75_1$status <- as.vector(rep("75",nrow(gr75_1)))
        #combine columns
        gry1 <- as.vector(rbind(gr75_1$year,rbind(gr25_1$year,gr50_1$year)))
        gry1 <- as.factor(gry1)
        grs1 <- as.vector(rbind(gr75_1$gross_mthly_75_percentile,
                                rbind(gr25_1$gross_mthly_25_percentile,
                                      gr50_1$gross_monthly_median)))
        Percentile <- as.vector(rbind(gr75_1$status,rbind(gr25_1$status,gr50_1$status)))
        #create data frame 
        b1 <- data.frame(gry1,grs1,Percentile)
        #output line graph
        output$gr1 <- renderPlot({
            ggplot(b1,aes(gry1,grs1,color=Percentile,group=Percentile))+geom_line(aes(linetype=Percentile),size=1)+
                geom_point(aes(shape=Percentile),size=3) + theme(legend.position="bottom") +
                theme_classic() + labs(x="Year",y="Salary ($)") + 
                scale_color_brewer(palette="Set2") + theme(legend.position="bottom")
        },height=300,width=300)
        
        #output line graph for 2nd course(25,50,75 gross)
        #create new dataframe to process barplot for course 1
        gr25_2 <- d2 %>% select(year,gross_mthly_25_percentile)
        gr50_2 <- d2 %>% select(year,gross_monthly_median)
        gr75_2 <- d2 %>% select(year,gross_mthly_75_percentile)
        #create status column
        gr25_2$status <- as.vector(rep("25",nrow(gr25_2)))
        gr50_2$status <- as.vector(rep("50",nrow(gr50_2)))
        gr75_2$status <- as.vector(rep("75",nrow(gr75_2)))
        #combine columns
        gry2 <- as.vector(rbind(gr75_2$year,rbind(gr25_2$year,gr50_2$year)))
        gry2 <- as.factor(gry2)
        grs2 <- as.vector(rbind(gr75_2$gross_mthly_75_percentile,
                                rbind(gr25_2$gross_mthly_25_percentile,
                                      gr50_2$gross_monthly_median)))
        Percentile <- as.vector(rbind(gr75_2$status,rbind(gr25_2$status,gr50_2$status)))
        #create data frame 
        b2 <- data.frame(gry2,grs2,Percentile)
        #output barplot 
        output$gr2 <- renderPlot({
            ggplot(b2,aes(gry2,grs2,color=Percentile,group=Percentile))+geom_line(aes(linetype=Percentile),size=1)+
                geom_point(aes(shape=Percentile),size=3) + theme(legend.position="bottom") +
                theme_classic() + labs(x="Year",y="Salary ($)") + 
                scale_color_brewer(palette="Set2") + theme(legend.position="bottom")
        },height=300,width=300)
        
        #output line graph for 3rd course(25,50,75 gross)
        #create new dataframe to process barplot for course 3
        gr25_3 <- d3 %>% select(year,gross_mthly_25_percentile)
        gr50_3 <- d3 %>% select(year,gross_monthly_median)
        gr75_3 <- d3 %>% select(year,gross_mthly_75_percentile)
        #create status column
        gr25_3$status <- as.vector(rep("25",nrow(gr25_3)))
        gr50_3$status <- as.vector(rep("50",nrow(gr50_3)))
        gr75_3$status <- as.vector(rep("75",nrow(gr75_3)))
        #combine columns
        gry3 <- as.vector(rbind(gr75_3$year,rbind(gr25_3$year,gr50_3$year)))
        gry3 <- as.factor(gry3)
        grs3 <- as.vector(rbind(gr75_3$gross_mthly_75_percentile,
                                rbind(gr25_3$gross_mthly_25_percentile,
                                      gr50_3$gross_monthly_median)))
        Percentile <- as.vector(rbind(gr75_3$status,rbind(gr25_3$status,gr50_3$status)))
        #create data frame 
        b3 <- data.frame(gry3,grs3,Percentile)
        #output line graph
        output$gr3 <- renderPlot({
            ggplot(b3,aes(gry3,grs3,color=Percentile,group=Percentile))+geom_line(aes(linetype=Percentile),size=1)+
                geom_point(aes(shape=Percentile),size=3) + theme(legend.position="bottom") +
                theme_classic() + labs(x="Year",y="Salary ($)") + 
                scale_color_brewer(palette="Set2") + theme(legend.position="bottom")
        },height=300,width=300)
    })
}


shinyApp(ui = ui, server = server)
