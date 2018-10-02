#### PLOTLY TO MAKE INTERACTIVE VIDEO
library(plotly)
library(tidyverse)
library(lubridate)
library(zoo)

#Import Data
df <- read.csv("TidyTuesday/Data/TT_27/Births.csv")

#Group data into monthly averages
df1 <- df %>%
  group_by(year, month) %>%
  summarize(births = mean(births))

#Create new row for accumulation function
df1$id <- 1:nrow(df1)

#Acculumate by Function
accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

#Remove excess attributes from data processing
df1<- as.data.frame(df1)

#Create new data set with accumulation variable (used for frames in plotly)
df_plotly <- df1 %>%
  accumulate_by(~id)

#Reformat dates into a usable format
df_plotly$Date <- as.yearmon(paste(df_plotly$year, df_plotly$month), "%Y %m")
df_plotly$Date <- as.Date.yearmon(df_plotly$Date)

#Create Interactive Plot
p <- plot_ly(data = df_plotly,
             x = ~Date,
             y = ~births) %>%
  add_trace(x = ~Date, y = ~births, frame = ~frame, type = 'scatter', mode = 'lines+markers', line = list(shape = "spline")) %>%
  layout(xaxis = list(title = "Date",
                      range = c(min(df_plotly$Date),max(df_plotly$Date)),
                      zeroline = F),
         yaxis = list(title = "Monthly Births",
                      range = c(min(df_plotly$births)-2000,max(df_plotly$births)+1000),
                      zeroline = F)) %>%
  animation_opts(frame = 100,
                 transition = 0,
                 redraw = FALSE) %>%
  animation_slider(hide = T) %>%
  animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom")

#Set plotly API information
Sys.setenv("plotly_username"="YOUR PLOTLY USERNAME")
Sys.setenv("plotly_api_key"="YOUR PLOTLY API KEY")

#Send plot to https://plot.ly/~YOUR PLOTLY USERNAME/
api_create(p)
