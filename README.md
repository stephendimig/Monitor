# Performance Monitoring with Ganglia and R
The PSBR Report Generator is an on-denmand report generator for a performance 
data gathering system that uses Ganglia for data collection and active  monitoring and R for customizable reports. The PSBR Report Generator is
used to report on various aspects of system performance such as CPU and 
memory usage, internal queue depths, messages rates, and database size and
composition. This app uses some actual data that was collected from a 
Ganglia RRD database and converted to csv files which
is required due to the  shinyapps.io deployment. In a real world system 
it would read read the data form a live RRD database (still converting it to a 
csv).

* app.R - This file contains both the UI and Server side code (see http://shiny.rstudio.com/tutorial) for an R Shiny App that serves as an on-denmand report generator for a performance monitoring system combining Ganglia and R.
* psbr.Rmd - An R-Markdown file used as a template for generating dynamic reports.
* psbr_data_140.txt - Data collected from 10.240.61.140 node.
* psbr_data_141.txt - Data collected from 10.240.61.141 node.
* psbr_data_146.txt - Data collected from 10.240.61.146 node.
* psbr_data_147.txt - Data collected from 10.240.61.147 node.

