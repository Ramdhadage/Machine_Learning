##---------------------------------------------------
##---------------------------------------------------
##----- ADEC 7910: Software Tools for Data Analysis
##----- R Homework 3 Template
##---------------------------------------------------
##---------------------------------------------------

##--------------------------------------------------------------------
##----- General
##--------------------------------------------------------------------

# Clear the workspace
rm(list = ls()) # Clear environment
gc()            # Clear unused memory
cat("\f")       # Clear the console

# Prepare needed libraries
packages <- c("ggplot2"
              , "lemon"
              , "gridExtra" # For Q1
              , "ggrepel"   # For labels in Q2.b
              , "scales"
              , "lubridate" # for dates
)
for (i in 1:length(packages)) {
  if (!packages[i] %in% rownames(installed.packages())) {
    install.packages(packages[i], dependencies = TRUE)
  }
  library(packages[i], character.only = TRUE)
}
rm(packages)

# Set working directory and path to data, if need be
setwd("R Assignment/ggplot example")

# Load data

sales <- read.csv("r.hw3.sales.csv"
                  , check.names = FALSE
                  , stringsAsFactors = FALSE
                  , na.strings = ""
)

items <- read.csv("r.hw3.items.csv"
                  , check.names = FALSE
                  , stringsAsFactors = FALSE
                  , na.strings = ""
)

# Merge data together
sales <- merge(sales, items, by = intersect(names(sales), names(items)))

# Reorder variables
var.order <- c("date"
               , "category"
               , "subcategory"
               , "item.name"
               , "volume"
               , "price"
               , "sale.bottles"
               , "sale.volume"
               , "sale.dollars"
)
sales <- sales[var.order]

##--------------------------------------------------------------------
##----- Q1
##--------------------------------------------------------------------  
# Change column 'date' to type "date"
sales$date <- mdy(sales$date)
# sales$date <- as.Date(sales$date, "%Y-%m-%d")

# Aggregate 'sale.dollars' by date
sales.agg <- aggregate(sale.dollars ~ date
                       , sales
                       , sum
)

# Create a dataset with all the dates from 2015:
sales.dates <- data.frame(date = seq(from = as.Date("2015-01-01")
                                     , to = as.Date("2015-12-31")
                                     , by = "day"
)
)
# Merge two together:
sales.daily <- merge(sales.dates, sales.agg
                     , by = "date"
                     , all.x = TRUE
)
rm(sales.agg)
rm(sales.dates)

# Create calendar dimensions: days, weeks, months and years
# Days:
sales.daily$day <- as.numeric(format(sales.daily$date, "%d"))

sales.daily$weekday <- factor(format(sales.daily$date, "%a") 
                              , levels = rev(c("Mon" 
                                               , "Tue"
                                               , "Wed"
                                               , "Thu"
                                               , "Fri"
                                               , "Sat"
                                               , "Sun"
                              )
                              )
                              , ordered = TRUE
)

# Calculate week of the year

sales.daily$week <- as.numeric(format(sales.daily$date, "%W")) + 1 
# Calculate week of year number for 1st day of every month
tmp <- as.numeric(format(as.Date(cut(sales.daily$date, "month")), "%W"))

sales.daily$week <- sales.daily$week - tmp

rm(tmp)

# Months:

sales.daily$month <-  months(as.Date(sales.daily$date),abbreviate = TRUE)
sales.daily$month <- factor(sales.daily$month, levels = month.abb)

#Day of the week

sales.daily$weekday <- weekdays(as.Date(sales.daily$date),abbreviate = TRUE)
sales.daily$weekday<- factor(sales.daily$weekday, 
                             levels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"),ordered=TRUE)
sales.daily$sale.dollars.category <- cut(sales.daily$sale.dollars,6)
###### Q1 chart ######
q1  <- ggplot(sales.daily, aes(x = weekday, 
                               y = -week,fill=
                                 sale.dollars.category)) +
  
  scale_x_discrete(position = "top")+
  geom_tile(colour = "white") + 
  geom_text(aes(label = day), size = 3) +
  theme(aspect.ratio = 1/4,
        legend.position = "bottom",
        # legend.key.width = unit(1, "lines"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = "#A04000",face = "bold", size = 15),
        strip.placement = "outside",
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        plot.title = element_text(colour = "#A04000", hjust = 0.5, size = 21, face = "bold",
                                  margin = margin(0,0,0,0, unit = "cm"))
  ) +
  scale_fill_manual(values=c("#C66E33","#EDAB80","#EDEB80","#88C142","#5898D2","#11100E"))+
  # scale_color_gradientn(colours = rainbow(1))+
  facet_wrap(~month, nrow = 4, ncol = 3, scales = "free") +
  labs(title = "Daily Total Sales, 2015")+
  guides(fill = guide_legend(nrow = 1))

q1


# Export Q1 chart
png(file = "q1.png", width = 1920, height = 1920, res = 180)
q1
dev.off()

##--------------------------------------------------------------------
##----- Q2
##--------------------------------------------------------------------  
#calculate price.per.l
price.per.l <- (sales$price/sales$volume)*1000
sales2 <- cbind(sales, price.per.l)
sales2 <- sales2[sample(nrow(sales2), 104857), ]
#calculate price.per.l.w
# Creating dataframe contains total volume sold and average weighted price


# splite the sale2 by subcategory
# sales3split <- split(sales2,factor(sales2$subcategory))
# for(j in 1:length(unique(sales2$subcategory)))
# {
sales3 <-as.data.frame(matrix(nrow = length(unique(sales2$subcategory)),ncol = 4))
for(j in 1:length(unique(sales2$subcategory)))
{
  sales3[j,1] <- unique(sales2$subcategory)[j]
  
  sales3[j,2] <- unique(sales2$category[sales2$subcategory == unique(sales2$subcategory)[j]])[1]
  
  sales3[j,3] <- sum(sales2$price.per.l[sales2$subcategory == unique(sales2$subcategory)[j]]*
                       sales2$sale.volume[sales2$subcategory == unique(sales2$subcategory)[j]])/sum(
                       sales2$sale.volume[sales2$subcategory == unique(sales2$subcategory)[j]]
                       )

  sales3[j,4] <- sum(
    sales2$sale.volume[sales2$subcategory == unique(sales2$subcategory)[j]]
  )
  
  

}


colnames(sales3) <- c("subcategory","category","price.per.l.w","total.sale.volume")

##### Q2a chart ######
q2a <- ggplot(sales2, aes(x = price.per.l
                          , y = category
                          # ,color=category
                          ,fill=category)
              
) +
  scale_x_continuous(limits=c(0,50),breaks=seq(0, 50, 2),expand = c(0, 0))+
  # xlim(0, 50)+
  geom_boxplot()+
  # scale_x_discrete(breaks=seq(0,50,2))+
  labs(title = "Liquor categories, price per liter"
       , subtitle = "Excluding top 5% values"
       , x = "Price per liter, $"
       , y = "Category"
  ) +
  
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5
                                  , face = "bold"
                                  , size = 20
                                  , color = "#912600"
  )
  , plot.subtitle = element_text(hjust = 0.5
                                 , face = "bold"
                                 , size = 14
                                 , color = "#912600"
  )
  , axis.title.x = element_text(face = "bold"
                                , color = "#912600"
                                , size = 14
  )
  , axis.text.x  = element_text(face = "bold"
                                , vjust = 0.5
                                , size = 12
  )
  , axis.title.y = element_text(face = "bold"
                                , color = "#912600"
                                , size = 14
  )
  , axis.text.y  = element_text(face = "bold"
                                , vjust = 0.5
                                , size = 12
  ),
  legend.title = element_blank()
  )
q2a
# Export q2a chart
png(file = "q2a.png", width = 1920, height = 1920, res = 180)
q2a
dev.off()
##### Q2b chart #####

# sales3<-sales2[sample(nrow(sales2), 50), ]#################Run this


q2b <-   ggplot(sales3, aes(x = price.per.l.w, 
                            y = total.sale.volume,label = category)) + 
  
  scale_x_continuous(limits=c(0,50),breaks=seq(0, 50, 2),expand = c(0, 0))+
  scale_y_continuous(limits=c(0,2500),breaks=seq(0, 2500, 250),expand = c(0, 0))+
  
  geom_point(aes(col = category),shape = 21,
             fill = "white", size = 3, stroke = 3) +
  geom_text_repel(aes(price.per.l.w, total.sale.volume, label = 
                        paste0(subcategory,", ", round(price.per.l.w,1), " $/I")),
                  box.padding = 0.5)+
  labs(subtitle="price vs quantity", 
       y="Liters sold, thousands", 
       x="Average weighted price per liter, $", 
       title="Liquor subcategories") +
  
  
  theme(plot.title = element_text(hjust = 0.5
                                  , face = "bold"
                                  , size = 20
                                  , color = "#912600"
  )
  , plot.subtitle = element_text(hjust = 0.5
                                 , face = "bold"
                                 , size = 14
                                 , color = "#912600"
  )
  , axis.title.x = element_text(face = "bold"
                                , color = "#912600"
                                , size = 14
  )
  , axis.text.x  = element_text(face = "bold"
                                , vjust = 0.5
                                , size = 12
  )
  , axis.title.y = element_text(face = "bold"
                                , color = "#912600"
                                , size = 14
  )
  , axis.text.y  = element_text(face = "bold"
                                , vjust = 0.5
                                , size = 12
  ),
  legend.title = element_blank())


q2b
# Export Q2b chart
png(file = "q2b.png", width = 2880, height = 1920, res = 180)
q2b
dev.off()
rm(price.per.l)
rm(price.per.l.w)
rm(sales2)


##--------------------------------------------------------------------
##----- Q3
##--------------------------------------------------------------------
#aggregate
sales.monthly <- aggregate(sale.dollars ~ format(date, "%Y") 
                           + format(date, "%b")
                           + category
                           , sales
                           , sum
)

colnames(sales.monthly)[c(1,2)] <- c("year", "month")


# Make a new date variable
sales.monthly$date <- paste(sales.monthly$year
                            , sales.monthly$month
                            , "01" 
                            , sep = "-"
)
sales.monthly$date <- as.Date(sales.monthly$date, format = "%Y-%b-%d")
sales.monthly$month <-  months(as.Date(sales.monthly$date),abbreviate = TRUE)
sales.monthly$month <- factor(sales.monthly$month, levels = month.abb)
# Q3a chart
###### Q3a chart--- ####
q3a <- ggplot(sales.monthly, aes(x = month, y= (sale.dollars*100)/sum(sale.dollars), group=category, colour=category))+
  geom_line(aes(size=category)) +
  geom_point() +
  scale_size_manual(values=c(1,1,1,1,1,1,1,1,1,1))+
  labs(title = "Share of total sales per mounth"
       , x = "Month"
       , y = ""
  ) +
  
  theme_bw()+
  geom_point(aes(fill=category),shape = 21,
             col = "black", size = 3, stroke = 1)+
  theme(plot.title = element_text(hjust = 0.5
                                  , face = "bold"
                                  , size = 20
                                  , color = "#912600"
  )
  , plot.subtitle = element_text(hjust = 0.5
                                 , face = "bold"
                                 , size = 14
                                 , color = "#912600"
  )
  , axis.title.x = element_text(face = "bold"
                                , color = "#912600"
                                , size = 14
  )
  , axis.text.x  = element_text(face = "bold"
                                , vjust = 0.5
                                , size = 12
  )
  , axis.title.y = element_text(face = "bold"
                                , color = "#912600"
                                , size = 14
  )
  , axis.text.y  = element_text(face = "bold"
                                , vjust = 0.5
                                , size = 12
  )
  )

# First we need to aggregate sales by day
##########q3b#####
##########
sales.agg <- aggregate(sale.dollars ~ format(date,"%A-%B-%C")
                       + category
                       , sales
                       , sum
)
sales.daily <- aggregate(sale.dollars ~ format(date, "%A") +
                           category
                         , sales
                         , sum
)

colnames(sales.daily)[1] <- c("Weekday")

# sales.daily$weekday <- weekdays(as.Date(sales.daily$date),abbreviate = TRUE)
# sales.daily$weekday<- factor(sales.daily$weekday, 
                             # levels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"),ordered=TRUE)


##### Q3b chart #####

q3b <- ggplot(sales.daily, aes(x=Weekday, y=(sale.dollars*100)/sum(sale.dollars), group=category, colour=category)) + 
  geom_line(aes(size=category)) +
  geom_point(aes(fill=category),shape = 21,
             col = "black", size = 3, stroke = 1)+
  scale_size_manual(values=c(1,1,1,1,1,1,1,1,1,1,1))+
  labs(title = "Share of total sales per weekday"
       , x = "weekday"
       , y = ""
  ) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5
                                  , face = "bold"
                                  , size = 20
                                  , color = "#912600"
  )
  , plot.subtitle = element_text(hjust = 0.5
                                 , face = "bold"
                                 , size = 14
                                 , color = "#912600"
  )
  , axis.title.x = element_text(face = "bold"
                                , color = "#912600"
                                , size = 14
  )
  , axis.text.x  = element_text(face = "bold"
                                , vjust = 0.5
                                , size = 12
  )
  , axis.title.y = element_text(face = "bold"
                                , color = "#912600"
                                , size = 14
  )
  , axis.text.y  = element_text(face = "bold"
                                , vjust = 0.5
                                , size = 12
  ),
  legend.position = "none"
  )
q3a
# Export Q3a chart
png(file = "q3a.png", width = 2880, height = 1920, res = 180)
q3a
dev.off()

q3b
# Export Q3b chart
png(file = "q3b.png", width = 2880, height = 1920, res = 180)
q3b
dev.off()

grid.arrange(q3a, q3b
             , nrow = 1, ncol = 2
             , widths = c(2, 1)
)
# Export Q3c chart
png(file = "q3c.png", width = 2880, height = 1920, res = 180)
grid.arrange(q3a, q3b
             , nrow = 1, ncol = 2
             , widths = c(2, 1)
)
dev.off()

rm(sales.daily)
rm(sales.monthly)

##--------------------------------------------------------------------
##----- Q4
##--------------------------------------------------------------------
rank.agg1 <- aggregate(sale.dollars ~ category 
                       , sales
                       , sum
)
rank.agg2 <- aggregate(sale.bottles ~ category 
                       , sales
                       , sum
)

rank.agg3 <- aggregate(sale.volume ~ category 
                       , sales
                       , sum
)

rank <- merge(rank.agg1, rank.agg2, by = "category")
rank <- merge(rank,rank.agg3, by = "category")



# label <- rank$category

# Q4 chart

colnames(rank) <- c("category", "sale.dollars", "sale.bottles","sale.volume")
left_label <- paste(rank$category, round(rank$sale.dollars),sep=", ")
right_label <- paste(rank$category, round(rank$sale.volume),sep=", ")
rank$class <- c("#999999","#FF0000","#00FF00","#0000FF","#FFFF00",
                "#FF00FF","#800000", "808000","#00FFFF", "#000080")

rank_1column <- c(rank(-rank[,2]), rank(-rank[,3]),rank(-rank[,4]))
xlable <-rep(paste0("Sales",",",c("$","liters","bottles")), each=10
)

rank_df <- as.data.frame(cbind(rep(rank$category, 3),
                               xlable,
                               rank_1column,rep(rank$class,3)),stringsAsFactors = TRUE)

colnames(rank_df) <- c("category","x_axis_values","combine_rank","class")

q4 <- ggplot(data = rank_df, aes(x = x_axis_values, y = category, group = combine_rank)) +
  geom_line(aes(color = class, alpha = 1), size = 2) +
  geom_point(aes(color = class, alpha = 1), size = 6)+
  geom_text(data = rank_df[rank_df$x_axis_values=="Sales,$",], 
            aes(label = category) , 
            hjust = 1.35, 
            fontface = "bold", 
            size = 4) +
  geom_text(data = rank_df[rank_df$x_axis_values=="Sales,liters",], 
            aes(label = category) , 
            hjust = -.35, 
            fontface = "bold", 
            size = 4) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(panel.border     = element_blank()) +
  # Remove just about everything from the y axis
  theme(axis.title.y     = element_blank()) +
  theme(axis.text.y      = element_blank()) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  # Remove a few things from the x axis and increase font size
  theme(axis.title.x     = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.text.x.top      = element_text(size=12)) +
  # Remove x & y tick marks
  theme(axis.ticks       = element_blank()) +
  # Format title & subtitle
  theme(plot.title       = element_text(size=14, face = "bold", hjust = 0.5)) +
  theme(plot.subtitle    = element_text(hjust = 0.5))+
geom_text_repel(aes(x_axis_values, category, label = combine_rank),colour = 'white',point.padding = NA)
q4
png(file = "q4.png", width = 2880, height = 1920, res = 180)
q4
dev.off()


