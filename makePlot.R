library(data.table)
library(tidyverse)
library(lubridate)
library(ggplot2)

corona <- fread("./coronaData.tsv", data.table = F)
corona$date <- dmy(corona$date)
corona <- rbind(
                data.table(date = min(corona$date),
                           diseased = 0,
                           died = 0),
                corona,
                data.table(date = max(corona$date),
                diseased = 0,
                died = 0
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
    geom_polygon(alpha = 0.1) + 
    scale_x_date() + 
    scale_y_log10() +
    theme(text = element_text(size=14),
          axis.text.x = element_text(angle=45, hjust=1)) 
dev.off()
