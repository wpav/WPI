
#  Final project for DS501 - Fall 2021  
#

library(shiny)
library(ggplot2)
library(NbClust)
library(factoextra)
library(corrplot)

library(datasets)
library(viridis)


data <-read.csv("heart.csv")
data = dplyr::select(data, c('Age', 'RestingBP', 'Cholesterol', 'MaxHR', 'Oldpeak', 'HeartDisease'))

list_for_lm = colnames(data)
list_for_lm = list_for_lm[list_for_lm != "HeartDisease"]

ui <- fluidPage(
    headerPanel('Heart Disease Data Analysis'),
    
    navbarPage(
        #navlistPanel(
        "Heart Disease Analysis - DS 501",
        tabPanel("Overview",
                 mainPanel(
                     h4("Data Selection", align = "left"),
                     p("I selected the Kaggle Heart Failure Prediction dataset.  I chose this dataset because of the 
                        health implications for understanding possible predictors of heart disease so early prevention 
                        and mitigation tactics can be used to possibly prevent or delay the onset of the disease."),
                     p("For analysis purposes I also liked that this dataset had a variety of categorical and 
                       continuous data."),
                     hr(),
                     h4("Model Selection", align = "left"),
                     p("I selected Kmeans for my model here because I wanted to cluster the continuous data to see if 
                       there were any themes or patterns in the data."),
                     p("the features I chose were the continuous predictors in the dataset: age, resting blood pressure,
                       cholesterol, max heart rate and old peak.  The target was a binary categorical variable for whether or 
                       not someone had heart disease."),
                     p("When I ran a few different analyses on the dataset to determine the optimal clusters, I found 
                       most consistently that four clusters were optimal. I used the NbClust library to accomplish a majority
                       of the anlaysis. I found that 4 clusters were optimal in almost all the tests run, which is the 
                       default value on the tabs for the the cluestering analysis. Visually I can see how there are at least
                       two distinct cluseters.  There is one stand out cluster on the upper side of the data, and then another
                       set of of clusters below that.  The latter cluseters seem to have a lot of overlap so more analysis needs
                       to be done."),
                     h4("How to use this app", align = "left"),
                     p("The first two tabs in the Kmeans section provide some results from different optimal cluseter analyses.
                       The third tab displays a plot of the clusters with the option to change the number of cluseters and see
                       the plot update with their location.  The last tab provides input values for all the features and the target,
                       and the optimal number of clusters and will output the cluster the data is in and a plot for reference.  
                       The default values provided initially are the averages for all the features and 0 (or No) for the target.
                       The default number of clusters is 4."),
                     h4("What's next", align = "left"),
                     p("More analysis needs to be done with the categorical variables in the dataset.  It would be good to also 
                       run a logistic regression analsis to see if there are any key features and others that contribute less to
                       an optimal model. In using the cluster simulator on the last tab, there is no change in cluster
                       if someone has heart disease or not so this may be an issue and not the best choice for this dataset.
                       The other issue could be the averages that are using and just simply changing the target does not make a 
                       difference for the average feature values. Again, more analysis needs to be done.")
                     
                 )),
        #"Kmeans",
        tabPanel("Kmeans Analysis",
                 
                 mainPanel(
                     
                     tabsetPanel(type = "tabs",
                                 tabPanel("Kmeans Results - WSS", plotOutput("wss")),
                                 tabPanel("Kmeans Results - Silhoustte", plotOutput('sil')),
                                 tabPanel("Plot", 
                                          sidebarPanel(
                                              h4('Please select the number of clusters:'),  
                                              #selectInput('xcol', 'First variable', names(data)),
                                              #selectInput('ycol', 'Second variable', names(data),
                                              #            selected=names(data)[[2]]),
                                              numericInput('clusters', 'Number of clusters', 4,
                                                           min = 2, max = 10),
                                              width = 4,
                                          ),
                                          mainPanel(
                                              h2('K-means Clustering - All Continuous Variables and the Target'), 
                                              plotOutput('fviz'))
                                 ),
                                 #tabPanel("Optimal Cluster Count", plotOutput("choose.cluster")),
                                 tabPanel("Predict Kmeans Cluster", 
                                          sidebarPanel(
                                              h4("Enter the values for Kmeans analysis:", align = "center"),
                                              p("(The default values are the average for that feature in the dataset.)"),
                                              numericInput("ageInput", "Enter the age", value = round(mean(data$Age),1)),
                                              numericInput("bpInput", "Enter the Resting BP", value = round(mean(data$RestingBP),1)),
                                              numericInput("cholInput", "Enter the Cholesterol", value = round(mean(data$Cholesterol),1)),
                                              numericInput("hrInput", "Enter the Max HR", value = round(mean(data$MaxHR),1)),
                                              numericInput("oldpeakInput", "Enter the Old Peak", value = round(mean(data$Oldpeak),1)),
                                              numericInput("targetInput", "Enter if the person had heart disease (0 = No, 1 = Yes)", value = 0),
                                              hr(),
                                              numericInput("clustersPredict", "Number of clusters to predict with", 4, min = 2, max = 10),
                                              actionButton("submitButton", "Submit")
                                          ),
                                          mainPanel(
                                              hr(),
                                              htmlOutput("predictedCluster"),
                                              hr(),
                                              h2('Predicted Cluster Plot'), 
                                              plotOutput('fvizPredict')
                                          )
                                 )
                                 
                     )
                     
                 )
                 
        )
        
    ),
    
    
    
    ### Footer
    hr(),
    hr(),
    hr(),
    print("Data Source: https://www.kaggle.com/fedesoriano/heart-failure-prediction")
)


server <- function(input, output, session) {
    # Combine the selected variables into a new data frame
    selectedData <- reactive({
        data[, c(input$xcol, input$ycol)]
    })
    
    clusters <- reactive({
        kmeans(selectedData(), input$clusters)
    })
    
    output$wss = renderPlot({
        fviz_nbclust((data[,c('Age', 'RestingBP', 'Cholesterol', 'MaxHR', 'Oldpeak', 'HeartDisease')]), kmeans, nstart=1, method = "wss") + 
            geom_vline(xintercept = 4, linetype = 1)
    })
    
    output$sil = renderPlot({
        fviz_nbclust((data[,c('Age', 'RestingBP', 'Cholesterol', 'MaxHR', 'Oldpeak', 'HeartDisease')]), kmeans,method = "silhouette")
        
    })
    
    output$choose.cluster <- renderPlot({
        withProgress(message = 'Making plot', value = 0, {
            set.seed(26)
            nbclust = NbClust(data = data[,c('Age', 'RestingBP', 'Cholesterol', 'MaxHR', 'Oldpeak', 'HeartDisease')], distance = "euclidean",
                              min.nc = 2, max.nc = 15, method="complete",index="all")
            fviz_nbclust(nbclust)
        })
        
    })
    
    output$fviz = renderPlot({
        withProgress(message = 'Making plot', value = 0, {
            dfk = kmeans(data[,c('Age', 'RestingBP', 'Cholesterol', 'MaxHR', 'Oldpeak', 'HeartDisease')], input$clusters, nstart = 10)
            fviz_cluster(dfk, data[,c('Age', 'RestingBP', 'Cholesterol', 'MaxHR', 'Oldpeak', 'HeartDisease')], geom = c("point"), ellipse.type = "euclid")
        })   
    })
    
    output$fviz2 = renderPlot({
        
        dfk = kmeans(data[,c(input$xcol, input$ycol)], input$clusters, nstart = 10)
        fviz_cluster(dfk, data[,c(input$xcol, input$ycol)], geom = c("point"), ellipse.type = "euclid")
        
    })
    
    #run model here and produce output string
    text_reactive <- eventReactive(input$submitButton, {
        
        d = data[,c('Age', 'RestingBP', 'Cholesterol', 'MaxHR', 'Oldpeak', 'HeartDisease')]
        l = list(        
            input$ageInput,
            input$bpInput,
            input$cholInput,
            input$hrInput,
            input$oldpeakInput,
            input$targetInput)
        
        d = rbind(d,l)
        
        set.seed(999)
        kmeansPredict <- kmeans((d), input$clustersPredict)
        kkmeansPredictTbl <- data.frame(kmeansPredict$size, kmeansPredict$centers)
        kmeansPredictDB <- data.frame(Cluster = kmeansPredict$cluster, (d))
        last_row = tail(kmeansPredictDB, n =1)
        HTML(paste("<b>","The cluster number for the predicted values is ", last_row$Cluster,".","<b>"))
        
        #paste("Thanks for entering data")
    })
    
    output$predictedCluster <- renderText({
        text_reactive()
    })
    
    plot_reactive <- eventReactive(input$submitButton, {
        
        d = data[,c('Age', 'RestingBP', 'Cholesterol', 'MaxHR', 'Oldpeak', 'HeartDisease')]
        l = list(        
            input$ageInput,
            input$bpInput,
            input$cholInput,
            input$hrInput,
            input$oldpeakInput,
            input$targetInput)
        
        d = rbind(d,l)
        
        set.seed(999)
        
        kmeansPredict1 = kmeans(d, input$clustersPredict, nstart = 10)
        fviz_cluster(kmeansPredict1, d, geom = c("point"), ellipse.type = "euclid")
        
    })
    
    
    output$fvizPredict = renderPlot({
        plot_reactive()
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
