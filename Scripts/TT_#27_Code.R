#### PLOTLY TO MAKE INTERACTIVE VIDEO-------------------------------------------
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


#### Faceted Bar Chart ---------------------------------------------------------
library(tidyverse)

#Import Data
df <- read.csv("C:/R_Packages/TidyTuesday/Data/TT_27/Births.csv")

#Group data into monthly averages
df <- df %>%
  group_by(year, month, day_of_week) %>%
  summarize(births = mean(births))

df$month <- month.abb[df$month]
df$day_of_week <- as.character(df$day_of_week)
df$day_of_week[df$day_of_week == "1"] <- "Monday"
df$day_of_week[df$day_of_week == "2"] <- "Tuesday"
df$day_of_week[df$day_of_week == "3"] <- "Wednesday"
df$day_of_week[df$day_of_week == "4"] <- "Thursday"
df$day_of_week[df$day_of_week == "5"] <- "Friday"
df$day_of_week[df$day_of_week == "6"] <- "Saturday"
df$day_of_week[df$day_of_week == "7"] <- "Sunday"

df$day_of_week <- factor(df$day_of_week, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday", "Saturday","Sunday"))
df$month <- factor(df$month, levels=c("Jan","Feb","Mar","Apr","Jun", "Jul","Aug",
                                      "Sep", "Oct", "Nov", "Dec"))

ggplot(na.omit(df)) + geom_col(aes(x = day_of_week, y = births)) +
  geom_hline(yintercept = c(50000, 100000, 150000, 200000), color ="white") +
  facet_wrap(~month) + theme_classic() +
  labs(title = "United States Births 2000-2014",x = "Day of\nthe Week", y = "Births") +
  #scale_fill_manual("Hub Type",values=c("#0868ac", "#2ca25f", "#fd8d3c","lightgrey")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        legend.title=element_text(size=16), 
        legend.text=element_text(size=12))
