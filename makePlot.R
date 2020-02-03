library(data.table)
library(tidyverse)
library(lubridate)
library(ggplot2)

corona <- fread("./coronaData.tsv", data.table = F)
corona$date <- dmy(corona$date)
corona <- rbind(
    data.table(date = min(corona$date),
               diseased_total = 0,
               diseased_outside_china = 0,
               died_total = 0,
               died_outside_china = 0),
    corona,
    data.table(date = max(corona$date),
               diseased_total = 0,
               diseased_outside_china = 0,
               died_total = 0,
               died_outside_china = 0
    )
)
corona_gathered <- gather(corona, "type", "cases", -date)

g <- ggplot(corona_gathered, 
            mapping = aes(x = date, 
                          y = cases, 
                          color = type, 
                          fill = type))

png("corona.png", width = 600, height = 600)
g + 
    geom_polygon(alpha = 0.1, size = 1) + 
    scale_x_date(name = "",
                 breaks = scales::pretty_breaks(n=8)(c(
                     min(corona_gathered$date), 
                     max(corona_gathered$date)))) + 
    scale_y_log10(name = "") +
    scale_fill_discrete(name = "",
                         breaks = c("diseased_total",
                                    "diseased_outside_china",
                                    "died_total",
                                    "died_outside_china"),
                         labels = c("Diseased - Total",
                                    "Diseased - non-China",
                                    "Deaths - Total",
                                    "Deaths - non-China")) +
    scale_color_discrete(name = "",
                         breaks = c("diseased_total",
                                    "diseased_outside_china",
                                    "died_total",
                                    "died_outside_china"),
                         labels = c("Diseased - Total",
                                    "Diseased - non-China",
                                    "Deaths - Total",
                                    "Deaths - non-China")) +
    theme(text = element_text(size=14),
          legend.position = "bottom",
          legend.text = element_text(size = 10))
dev.off()
