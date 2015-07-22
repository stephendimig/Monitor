#############################################################################
#
# File: app.R
#
# Description:        
# This file contains both the UI and Server side code (see [1]) for an R 
# Shiny App that serves as an on-denmand report generator for performance 
# data gathering system that uses Ganglia for data collection and active 
# monitoring and R for customizable reports. The PSBR Report Generator is
# used to report on various aspects of system performance such as CPU and 
# memory usage, internal queue depths, messages rates, and database size and
# composition. This app uses some actual data that was collected from a 
# Ganglia RRD database and converted to csv files which
# is required due to the  shinyapps.io deployment. In a real world system 
# it would read read the data form a live RRD database (still converting it to a 
# csv).
#
# Author: Steve Dimig 
#
# References: 
# 1. "How to start with Shiny", Garret Grolemnd, 
#    http://shiny.rstudio.com/tutorial
# 2. Ganglia Monitoring System, Wikipedia, 
#    https://en.wikipedia.org/wiki/Ganglia_%28software%29
#
#############################################################################

library(shiny)
library(ggplot2)

#############################################################################
# 
# Function: readData
# Description:
# This method reads the data  from a CSV file based on the source, node,
# begin, and end timestamp. The data is returned as a dataframe.
#
# Params:
# source [in] - The source parameter is used by Ganglia to indicate what
# group of servers the collection data originated from.
# node [in] - The node parameter is the actual server that originated the 
# data.
# startDateTime [in] - A POSIXlt value for the starting date/time of interest.
# endDateTime [in] - A POSIXlt value for the ending date/time of interest.
#
#
#############################################################################
readData <- function(source=NULL, node=NULL, startDateTime=NULL, endDateTime=NULL)
{
    df <- data.frame()
    
    # This is a workaround for the shiyapps.io deployment since it cannot
    # read from real RRD data, it reads previously captured data. 
    if("10.240.61.140" == node)
    {
        df <- read.table("psbr_data_140.txt", sep=";", header=TRUE, stringsAsFactors=FALSE)
    }
    else if("10.240.61.141" == node)
    {
        df <- read.table("psbr_data_141.txt", sep=";", header=TRUE, stringsAsFactors=FALSE)
    }
    else if("10.240.61.146" == node)
    {
        df <- read.table("psbr_data_146.txt", sep=";", header=TRUE, stringsAsFactors=FALSE)
    }
    else if("10.240.61.147" == node)
    {
        df <- read.table("psbr_data_147.txt", sep=";", header=TRUE, stringsAsFactors=FALSE)
    }
    
    df$Date <- as.POSIXlt(df$Date, format="%Y-%m-%d %H:%M:%S")
    
    df$NumPsbrBindingRecs <- as.numeric(df$NumPsbrBindingRecs)
    df$NumPsbrSessionRecs  <- as.numeric(df$NumPsbrSessionRecs)
    df$AllocInMemorySize <- as.numeric(df$AllocInMemorySize)
    df$UsedInMemorySize <- as.numeric(df$UsedInMemorySize)
    df$BINDINGTASKQ0 <- as.numeric(df$BINDINGTASKQ0)
    df$BINDINGTASKQ1 <- as.numeric(df$BINDINGTASKQ1)
    df$BINDINGTASKQ2 <- as.numeric(df$BINDINGTASKQ2)
    df$BINDINGTASKQ3 <- as.numeric(df$BINDINGTASKQ3)
    df$SESSIONTASKQ0 <- as.numeric(df$SESSIONTASKQ0)
    df$SESSIONTASKQ0 <- as.numeric(df$SESSIONTASKQ1)
    df$SESSIONTASKQ0 <- as.numeric(df$SESSIONTASKQ2)
    df$SESSIONTASKQ0 <- as.numeric(df$SESSIONTASKQ3)
    df$ImsiAnchorKeyRecs <- as.numeric(df$ImsiAnchorKeyRecs)
    df$ImsiApnAnchorKeyRecs <- as.numeric(df$ImsiApnAnchorKeyRecs)
    df$MsisdnAlternateKeyRecs <- as.numeric(df$ImsiApnAnchorKeyRecs)
    df$MsisdnApnAlternateKeyRecs <- as.numeric(df$MsisdnApnAlternateKeyRecs)
    df$Ipv4AlternateKeyRecs <- as.numeric(df$Ipv4AlternateKeyRecs)
    df$Ipv4AlternateKeyV2Recs <- as.numeric(df$Ipv4AlternateKeyV2Recs)
    df$Ipv6AlternateKeyRecs <- as.numeric(df$Ipv6AlternateKeyRecs)
    df$Ipv6AlternateKeyV2Recs <- as.numeric(df$Ipv6AlternateKeyV2Recs)
    df$SessionRecs <- as.numeric(df$SessionRecs)
    df$OcSessionRecs <- as.numeric(df$OcSessionRecs)
    df$OcClientHostRecs <- as.numeric(df$OcClientHostRecs)
    df$IngressStackEventRate <- as.numeric(df$IngressStackEventRate)
    df$EgressStackEventRate <- as.numeric(df$EgressStackEventRate)
    df$BindingTaskCpu0 <- as.numeric(df$BindingTaskCpu0)
    df$BindingTaskCpu1 <- as.numeric(df$BindingTaskCpu1)
    df$BindingTaskCpu2 <- as.numeric(df$BindingTaskCpu2)
    df$BindingTaskCpu3 <- as.numeric(df$BindingTaskCpu3)
    df$RecordAuditTaskCpu <- as.numeric(df$RecordAuditTaskCpu)
    df$AuditStackEventTaskCpu <- as.numeric(df$AuditStackEventTaskCpu)
    df$CATxUserBundlingRatio <- as.numeric(df$CATxUserBundlingRatio)
    df$CATxProvBundlingRatio <- as.numeric(df$CATxProvBundlingRatio)
    df$CARxBundlingRatio <- as.numeric(df$CARxBundlingRatio)
    df$SessionTaskCpu0 <- as.numeric(df$SessionTaskCpu0)
    df$SessionTaskCpu1 <- as.numeric(df$SessionTaskCpu1)
    df$SessionTaskCpu2 <- as.numeric(df$SessionTaskCpu2)
    df$SessionTaskCpu3 <- as.numeric(df$SessionTaskCpu3)
    df$CATcpThread <- as.numeric(df$CATcpThread)
    df$CARIngressTask <- as.numeric(df$CARIngressTask)
    df$CADirectXferThr <- as.numeric(df$CADirectXferThr)
    df$CAREgressTask <- as.numeric(df$CAREgressTask)
    df$ComAgentTimer <- as.numeric(df$ComAgentTimer)
    df$ComAgtReTx <- as.numeric(df$ComAgtReTx)
    df$AlarmMajor <- as.numeric(df$AlarmMajor)
    df$AlarmMinor <- as.numeric(df$AlarmMinor)
    df$AlarmCrit <- as.numeric(df$AlarmCrit)
    df$SbrSisTaskCpu0 <- as.numeric(df$SbrSisTaskCpu0)
    df$SbrSisTaskCpu1 <- as.numeric(df$SbrSisTaskCpu1)
    df$SbrSisSendRarTaskCpu <- as.numeric(df$SbrSisSendRarTaskCpu)
    df$SbrSisRspHandleTaskCpu <- as.numeric(df$SbrSisRspHandleTaskCpu)
    df$SbrInvokeSisSenTaskCpu <- as.numeric(df$SbrInvokeSisSenTaskCpu)
    df$SbrInvokeSisRspTaskCpu <- as.numeric(df$SbrInvokeSisRspTaskCpu)
    df <- df[df$Date >= startDateTime & df$Date <= endDateTime, ]
    df
}

#############################################################################
# 
# UI layout for the PSBR Report Generator
#
#############################################################################
ui <- fluidPage(
    fluidRow(
        column(12, tags$h2("PSBR Report Generator", align = "center"))
    ),
    
    fluidRow(
        column(12, tags$h5("PSBR Report Generator is an on-denmand report generator for a performance data gathering system that uses Ganglia for data collection and active monitoring and R for customizable reports. The PSBR Report Generator is used to report on various aspects of system performance such as CPU and memory usage, internal queue depths, messages rates, and database size and composition.", align = "center"))
    ),
    
    fluidRow(
        column(6, tags$h6("The source parameter is used by Ganglia to indicate what group of servers the collection data originated from.", align = "left")),
        column(6, tags$h6("The node parameter is the actual server that originated the data. There are four nodes in the data set with slightly different characteristics.", align = "left"))
    ),
    
    fluidRow(
        column(6, selectInput(inputId = "source",
                            label = "Source",
                            c("sterlingdiscovery"))),
        column(6, selectInput(inputId = "node",
                              label = "Node",
                              c("10.240.61.140", "10.240.61.141", "10.240.61.146", "10.240.61.147")))
    ),
    
    fluidRow(
        column(6, tags$h3("Start Date/Time")),
        column(6, tags$h3("End Date/Time"))
    ),
    
    fluidRow(
        column(6, tags$h6("The starting date/time of interest. The date is a calendar input with sliders for HH:MM:SS. Use 06/01/2015 00:00:00 to see all data.", align = "left")),
        column(6, tags$h6("The starting date/time of interest. The date is a calendar input with sliders for HH:MM:SS. Use 07/22/2015 00:00:00 to see all data.", align = "left"))
    ),
    
    fluidRow(
            column(6, dateInput(inputId = "startDate",
                                label = "Start date")),
            column(6, dateInput(inputId = "endDate",
                                label = "End date"))
    ),
    
    fluidRow(
            column(6, sliderInput("startHours", 
                                  label = "Hours", 
                                  min = 0, 
                                  max = 23, 
                                  value = 0)),
            column(6, sliderInput("endHours", 
                                  label = "Hours", 
                                  min = 0, 
                                  max = 23, 
                                  value = 0))
    ),
    
    fluidRow(
        column(6, sliderInput("startMinutes", 
                              label = "Minutes", 
                              min = 0, 
                              max = 59, 
                              value = 0)),
        column(6, sliderInput("endMinutes", 
                              label = "Minutes", 
                              min = 0, 
                              max = 59, 
                              value = 0))
    ),
    
    fluidRow(
        column(6, sliderInput("startSeconds", 
                              label = "Seconds", 
                              min = 0, 
                              max = 59, 
                              value = 0)),
        column(6, sliderInput("endSeconds", 
                              label = "Seconds", 
                              min = 0, 
                              max = 59, 
                              value = 0))
    ),
    
    fluidRow(
            column(6, verbatimTextOutput("startDateTime")),
            column(6, verbatimTextOutput("endDateTime"))
    ),
    
    fluidRow(
        column(6, tags$h6("Use the View button to look at the data prior to generating a report.", align = "left")),
        column(6, tags$h6("Use the Download button to generate a dynamic report and save it to you local computer as a Word doc.", align = "left"))
    ),
    
    fluidRow(
        column(6, actionButton("goButton", "View")),
        column(6, downloadButton('downloadReport'))
    ),
    
    fluidRow(
            column(6,plotOutput("ramPlot", height = "600px")),
            column(6,plotOutput("cpuPlot", height = "600px"))
    ), 
    
    fluidRow(
        column(6, plotOutput("bindQPlot", height = "600px")),
        column(6, plotOutput("sessionQPlot", height = "600px"))
    ),
    
    fluidRow(
        column(6, plotOutput("idbUsagePlot", height = "600px")),
        column(6, plotOutput("recordsPlot", height = "600px"))
    ), 
    
    fluidRow(
        column(6, plotOutput("dbCompPlot", height = "600px")),
        column(6, plotOutput("caStackEventPlot", height = "600px"))
    ),
    
    fluidRow(
        column(6, plotOutput("auditStackEventPlot", height = "600px")),
        column(6, plotOutput("bindTaskCpuPlot", height = "600px"))
    ),
    
    fluidRow(
        column(6, plotOutput("sessTaskCpuPlot", height = "600px")),
        column(6, plotOutput("caTaskCpuPlot", height = "600px"))
    ),
    
    fluidRow(
        column(6, plotOutput("sisTaskCpuPlot", height = "600px"))
    )
    
)

#############################################################################
# 
# Server code for the PSBR Report Generator
#
#############################################################################
server <- function(input, output) {
    
    # Reactive method to read the data when a change is made to the input
    # parameters.
    df <- eventReactive(input$goButton, {
        startDate <- as.POSIXlt(startDateTime(), format="%Y-%m-%d %H:%M:%S")
        endDate <- as.POSIXlt(endDateTime(), format="%Y-%m-%d %H:%M:%S")
        df <- readData(input$source, input$node, startDate, endDate)
  
        return (df)
    })
    
    # Reactive method to reflect the Start Date/Time from user input.
    output$startDateTime <- renderPrint({ 
        startDateTime()
    })
    
    # Reactive method to reflect the End Date/Time from user input.
    output$endDateTime <- renderPrint({ 
        endDateTime()
    })
    
    # Simple helper function to format the Start Date/Time.
    startDateTime <- function()
    {
         return (sprintf("%s %02d:%02d:%02d", input$startDate, input$startHours, input$startMinutes, input$startSeconds))
    }
    
    # Simple helper function to format the End Date/Time.
    endDateTime <- function()
    {
        return (sprintf("%s %02d:%02d:%02d", input$endDate, input$endHours, input$endMinutes, input$endSeconds))
    }
    
    # Memory Usage
    # A brief summary of system memory usage for PSBR. This diagram shows the measured RAM usage as given by 
    # the psbr.MemPerTotal sysmetric. It also shows the minor, major, and critical threshold levels for reference. 
    # A trend line is also included which is a best fit line showing any trend in RAM usage 
    # over time. The slope of the trend line should be at, or very close to, 0 for a duration run at a constant 
    # traffic rate.
    output$ramPlot <- renderPlot({
        if(0 != nrow(df()))
        {
            ggplot(df(), aes(Date, x=Date, y=Ram)) +
                scale_colour_manual("Memory", breaks=c("Ram", "Minor", "Major", "Critical", "Trend"), values=c('blue', 'orange', 'green', 'red', 'brown')) +
                geom_line(aes(y = Ram, colour="Ram")) +
                geom_hline(aes(yintercept=70, colour="Minor")) +
                geom_hline(aes(yintercept=80, colour="Major")) +
                geom_hline(aes(yintercept=90, colour="Critical")) +
                geom_smooth(aes(colour="Trend"), method="lm", formula=y~x, se=FALSE) +
                ylab("Memory Usage") +
                xlab("Date/Time") +
                ggtitle("PSBR Memory Usage")
        }
    })
    
    # CPU Usage
    # A brief summary of CPU resource usage for PSBR. This diagram shows 
    # the measured CPU usage as given by the psbr.Cpu sysmetric. It also 
    # shows the minor, major, and critical threshold levels for reference. 
    # A trend line is also included which is a best fit line showing any 
    # trend in CPU usage over time. The slope of the trend line should be 
    # at, or very close to, 0 for a duration run at a constant traffic rate.
    output$cpuPlot <- renderPlot({
        if(0 != nrow(df()))
        {
            ggplot(df(), aes(Date, x=Date, y=Cpu)) +
                scale_colour_manual("CPU", breaks=c("Cpu", "Minor", "Major", "Critical", "Trend"), values=c('blue', 'orange', 'green', 'red', 'brown')) +
                geom_line(aes(y = Cpu, colour="Cpu")) +
                geom_hline(aes(yintercept=60, colour="Minor")) +
                geom_hline(aes(yintercept=66, colour="Major")) +
                geom_hline(aes(yintercept=72, colour="Critical")) +
                geom_smooth(aes(colour="Trend"), method="lm", formula=y~x, se=FALSE) +
                ylab("CPU Usage") +
                xlab("Date/Time") +
                ggtitle("PSBR CPU Usage")
        }
    })
    
    # Binding Queue Depths
    # A summary of Binding Queue depths for the PSBR Binding Tasks. This 
    # diagram shows the queue utilization for each of the Binding tasks on 
    # a PSBR system.
    output$bindQPlot <- renderPlot({
        if(0 != nrow(df()))
        {
            ggplot(df(), aes(Date)) + 
                scale_colour_manual("Resource", breaks=c("BINDINGTASKQ0", "BINDINGTASKQ1", "BINDINGTASKQ2", "BINDINGTASKQ3"), values=c('red', 'orange', 'yellow', 'blue')) +
                geom_line(aes(y = BINDINGTASKQ0, colour="BINDINGTASKQ0")) + 
                geom_line(aes(y = BINDINGTASKQ1, colour="BINDINGTASKQ1")) +
                geom_line(aes(y = BINDINGTASKQ2, colour="BINDINGTASKQ2")) +
                geom_line(aes(y = BINDINGTASKQ3, colour="BINDINGTASKQ3")) +
                ylab("Utilization") +
                xlab("Date/Time") +
                ggtitle("PSBR Binding Queue Depths")
        }
    })
    
    # Session Queue Depths
    # A summary of Session Queue depths for the PSBR Session Tasks. This 
    # diagram shows the queue utilization for each of the Session tasks on 
    # a PSBR system.
    output$sessionQPlot <- renderPlot({
        if(0 != nrow(df()))
        {
            ggplot(df(), aes(Date)) + 
                scale_colour_manual("Resource", breaks=c("SESSIONTASKQ0", "SESSIONTASKQ1", "SESSIONTASKQ2", "SESSIONTASKQ3"), values=c('red', 'orange', 'yellow', 'blue')) +
                geom_line(aes(y = SESSIONTASKQ0, colour="SESSIONTASKQ0")) + 
                geom_line(aes(y = SESSIONTASKQ1, colour="SESSIONTASKQ1")) +
                geom_line(aes(y = SESSIONTASKQ2, colour="SESSIONTASKQ2")) +
                geom_line(aes(y = SESSIONTASKQ3, colour="SESSIONTASKQ3")) +
                ylab("Utilization") +
                xlab("Date/Time") +
                ggtitle("PSBR Session Queue Depths")
        }
    })
    
    # IDB Memory Usage
    # A summary of IDB Memory usage. This diagram shows the amount of memory 
    # allocated by IDB versus the amount of memory actually used.
    output$idbUsagePlot <- renderPlot({
        if(0 != nrow(df()))
        {
            types <- c("Allocated", "Used")
            values <- c(mean(df()$AllocInMemorySize), mean(df()$UsedInMemorySize))
            mydf <- data.frame(types, values)
            names(mydf) <- c("Type", "Value")
            
            mydf <- mydf[mydf$Value !=0, ]
            
            ggplot(mydf, aes(x=factor(Type), y=Value, fill=Type)) + geom_bar(stat = "identity") +
                ylab("Memory Usage") +
                xlab("Date/Time") +
                ggtitle("PSBR IDB Memory Usage")
        }
    })
    
    # Session/Binding records
    # A summary of the number of Session/Binding records in the PSBR 
    # database. This diagram shows the number of binding or session records 
    # in the PSBR database.
    output$recordsPlot <- renderPlot({
        if(0 != nrow(df()))
        {
            types <- c("Session", "Binding")
            values <- c(mean(df()$NumPsbrSessionRecs), mean(df()$NumPsbrBindingRecs))
            mydf <- data.frame(types, values)
            names(mydf) <- c("Type", "Value")
            
            mydf <- mydf[mydf$Value !=0, ]
            
            ggplot(mydf, aes(x=factor(Type), y=Value, fill=Type)) + geom_bar(stat = "identity") +
                ylab("Number Records") +
                xlab("Date/Time") +
                ggtitle("PSBR Session/Binding record size")
        }
    })
    
    # Database Composition (Bar Plot)
    # A summary of the types of records in the PSBR database by table. 
    # This diagram shows the composition of the PSBR database by the number
    # of records in each table.
    output$dbCompPlot <- renderPlot({
        if(0 != nrow(df()))
        {
            types <- c("ImsiAnchor", "ImsiApnAnchor", "MsisdnAlt", "MsisdnApnAlt", "Ipv4Alt", "Ipv4AltV2", "Ipv6Alt", "Ipv6AltV2", "Session", "OcSession", "OcClientHost")
            values <- c(mean(df()$ImsiAnchorKeyRecs), mean(df()$ImsiApnAnchorKeyRecs), mean(df()$MsisdnAlternateKeyRecs), mean(df()$MsisdnApnAlternateKeyRecs), mean(df()$Ipv4AlternateKeyRecs), mean(df()$Ipv4AlternateKeyV2Recs), mean(df()$Ipv6AlternateKeyRecs), mean(df()$Ipv6AlternateKeyV2Recs), mean(df()$SessionRecs), mean(df()$OcSessionRecs), mean(df()$OcClientHostRecs))
            mydf <- data.frame(types, values)
            names(mydf) <- c("Type", "Value")
            mydf <- mydf[mydf$Value !=0, ]
            ggplot(mydf, aes(x=factor(Type), y=Value, fill=Type)) + 
                geom_bar(stat = "identity") +
                ylab("Number of Records") +
                xlab("Table") +
                ggtitle("PSBR Database Composition")
        }
    })
    
    # ComAgent Stack Event Rates
    # A summary of ComAgent stack event rates over time. This diagram shows 
    # the ingress and egress rate of ComAgent Stack Events. It also shows a 
    # line that represents the mean of the ingress rate and a line that 
    # represents the mean of the egress rate.
    output$caStackEventPlot <- renderPlot({
        if(0 != nrow(df()))
        {
            ingressMean = mean(df()$IngressStackEventRate, na.rm=TRUE)
            egressMean = mean(df()$EgressStackEventRate, na.rm=TRUE)
        
            ggplot(df(), aes(Date)) + 
                scale_colour_manual("Rate", breaks=c("Ingress", "Egress",  "IngressMean", "EgressMean"), values=c("red", "blue", "orange", "purple")) +
                geom_line(aes(y = IngressStackEventRate, colour="Ingress")) + 
                geom_line(aes(y = EgressStackEventRate, colour="Egress")) +
                #geom_hline(aes(yintercept=ingressMean, colour="IngressMean")) +
                #geom_hline(aes(yintercept=egressMean, colour="EgressMean")) +
                ylab("Rate") +
                xlab("Date/Time") +
                ggtitle("PSBR ComAgent Stack Event Rates")
        }
    })
    
    # Audit Stack Event Rate
    # A summary of Audit stack event rate over time.
    output$auditStackEventPlot <- renderPlot({
        if(0 != nrow(df()))
        {
            auditMean <- mean(df()$TxSbrAuditSEReqSent, na.rm=TRUE)
            ggplot(df(), aes(Date)) + 
                scale_colour_manual("Rate", breaks=c("Audit", "Mean"), values=c("red", "blue")) +
                geom_line(aes(y = TxSbrAuditSEReqSent, colour="Audit")) +
                #geom_hline(aes(yintercept=auditMean, colour="Mean")) +
                ylab("Rate") +
                xlab("Date/Time") +
                ggtitle("PSBR Audit Stack Event Rate")
        }
    })
    
    # Binding SBR Per Task CPU Usage
    # A summary of Binding SBR Per Task CPU Usage. This diagram shows the 
    # CPU usage of some important tasks for a Binding SBR.
    output$bindTaskCpuPlot <- renderPlot({
        if(0 != nrow(df()))
        {
            ggplot(df(), aes(Date)) + 
                scale_colour_manual("Utilization", breaks=c("BindingTask0", "BindingTask1", "BindingTask2", "BindingTask3", "RecordAuditTask", "AuditStackEventTask") , values=c("red", "blue", "green", "orange", "purple", "black")) +
                geom_line(aes(y = BindingTaskCpu0, colour="BindingTask0")) + 
                geom_line(aes(y = BindingTaskCpu1, colour="BindingTask1")) + 
                geom_line(aes(y = BindingTaskCpu2, colour="BindingTask2")) + 
                geom_line(aes(y = BindingTaskCpu3, colour="BindingTask3")) + 
                geom_line(aes(y = RecordAuditTaskCpu, colour="RecordAuditTask")) +
                geom_line(aes(y = AuditStackEventTaskCpu, colour="AuditStackEventTask")) +
                ylab("Utilization") +
                xlab("Date/Time") +
                ggtitle("PSBR Binding SBR CPU Usage")
        }
    })
    
    # Session SBR Per Task CPU Usage
    # A summary of Session SBR Per Task CPU Usage. This diagram shows 
    # the CPU usage of some important tasks for a Session SBR.
    output$sessTaskCpuPlot <- renderPlot({
        if(0 != nrow(df()))
        {
            ggplot(df(), aes(Date)) + 
                scale_colour_manual("Utilization", breaks=c("SessionTask0", "SessionTask1", "SessionTask2", "SessionTask3", "RecordAuditTask", "AuditStackEventTask") , values=c("red", "blue", "green", "orange", "purple", "black")) +
                geom_line(aes(y = SessionTaskCpu0, colour="SessionTask0")) + 
                geom_line(aes(y = SessionTaskCpu1, colour="SessionTask1")) + 
                geom_line(aes(y = SessionTaskCpu2, colour="SessionTask2")) + 
                geom_line(aes(y = SessionTaskCpu3, colour="SessionTask3")) + 
                geom_line(aes(y = RecordAuditTaskCpu, colour="RecordAuditTask")) +
                geom_line(aes(y = AuditStackEventTaskCpu, colour="AuditStackEventTask")) +
                ylab("Utilization") +
                xlab("Date/Time") +
                ggtitle("PSBR Session SBR CPU Usage")
        }
    })
    
    # PSBR ComAgent Task CPU Usage
    # A summary of PSBR ComAgent Per Task CPU Usage. This diagram shows the 
    # CPU usage of some important ComAgent tasks on a PSBR system.
    output$caTaskCpuPlot <- renderPlot({
        if(0 != nrow(df()))
        {
            ggplot(df(), aes(Date)) + 
                scale_colour_manual("Utilization", breaks=c("CATcpThread", "CARIngressTask", "CADirectXferThr", "CAREgressTask", "ComAgentTimer", "ComAgtReTx") , values=c("red", "blue", "green", "orange", "purple", "black")) +
                geom_line(aes(y = CATcpThread, colour="CATcpThread")) + 
                geom_line(aes(y = CARIngressTask, colour="CARIngressTask")) + 
                geom_line(aes(y = CADirectXferThr, colour="CADirectXferThr")) + 
                geom_line(aes(y = CAREgressTask, colour="CAREgressTask")) + 
                geom_line(aes(y = ComAgentTimer, colour="ComAgentTimer")) +
                geom_line(aes(y = ComAgtReTx, colour="ComAgtReTx")) +
                ylab("Utilization") +
                xlab("Date/Time") +
                ggtitle("PSBR ComAgent Task CPU Usage")
        }
    })
    
    # PSBR SIS Task CPU Usage
    # A summary of PSBR SIS Task CPU Usage. This diagram shows the CPU usage 
    # of some important SIS tasks on a PSBR system.
    output$sisTaskCpuPlot <- renderPlot({
        if(0 != nrow(df()))
        {
            ggplot(df(), aes(Date)) + 
                scale_colour_manual("Utilization", breaks=c("SbrSisTask0", "SbrSisTask1", "SbrSisSendRarTask", "SbrSisRspHandleTask", "SbrInvokeSisSenTask", "SbrInvokeSisRspTask") , values=c("red", "blue", "green", "orange", "purple", "black")) +
                geom_line(aes(y = SbrSisTaskCpu0, colour="SbrSisTask0")) + 
                geom_line(aes(y = SbrSisTaskCpu1, colour="SbrSisTask1")) + 
                geom_line(aes(y = SbrSisSendRarTaskCpu, colour="SbrSisSendRarTask")) + 
                geom_line(aes(y = SbrSisRspHandleTaskCpu, colour="SbrSisRspHandleTask")) + 
                geom_line(aes(y = SbrInvokeSisSenTaskCpu, colour="SbrInvokeSisSenTask")) +
                geom_line(aes(y = SbrInvokeSisRspTaskCpu, colour="SbrInvokeSisRspTask")) +
                ylab("Utilization") +
                xlab("Date/Time") +
                ggtitle("PSBR SIS Task CPU Usage")
        }
    })
    
    # This is the handler for the "Download" button that will allow the user
    # to generate an on demand report and download it to their local computer.
    output$downloadReport <- downloadHandler(
        filename = function() {
            str <- paste('psbr', sep = '.', 'docx')
            str
        },
        content = function(file) {
            src <- normalizePath('psbr.Rmd')
            data <- NULL
            
            # Same type of temporary mapping as readData().
            if("10.240.61.140" == input$node)
            {
                data <- normalizePath('psbr_data_140.txt')
            }
            else if("10.240.61.141" == input$node)
            {
                data <- normalizePath('psbr_data_141.txt')
            }
            else if("10.240.61.146" == input$node)
            {
                data <- normalizePath('psbr_data_146.txt')
            }
            else if("10.240.61.147" == input$node)
            {
                data <- normalizePath('psbr_data_147.txt')
            }
            
            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, 'psbr.Rmd', overwrite=TRUE)
            file.copy(data, 'psbr_data.txt', overwrite=TRUE)
            
            library(rmarkdown)
            out <- render('psbr.Rmd', word_document())
            file.rename(out, file)
        }
    )

}

shinyApp(ui = ui, server = server)
