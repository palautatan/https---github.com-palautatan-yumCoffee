# 2016-12-04
# GOAL: plot coffee preferences to increase my understanding of what i like

## LIBRARIES
library(stringr)
library(ggplot2)
library(ggrepel)

## LOADING DATA (TASTE PROFILE)
my_coffee = read.csv("/Users/edie/Box Sync/GitThings/https---github.com-palautatan-yumCoffee.git/datasets/coffee_list.csv", header=TRUE)
attach(my_coffee)

## EDITING RATING
Rating = unlist(Personal.Rating)
Rating = gsub("\\/5", "", Rating)
Rating = as.numeric(Rating)


coffee_name = str_split(unlist(Coffee),": ")
coffee_name = sapply(coffee_name, function(coffee_part) {
  brand = coffee_part[1]
  name = coffee_part[2]
  c(brand, name)
}
)
detach(my_coffee)

## CREATE NEW DATA FRAME
updated_coffee = cbind(my_coffee, Rating, coffee_name[1,], coffee_name[2,])
names(updated_coffee)[9:10] = c("Brand", "Name")
attach(updated_coffee)
Roast = factor(Roast, levels=c("Special", "Light", "Medium-Light", "Medium", "French", "Medium-Dark", "Dark"))

## PLOT PREFERENCES
coffee_plot = ggplot()
coffee_plot = coffee_plot + geom_point(aes(x=Roast, y=Rating)) + geom_label_repel(aes(x=Roast, y=Rating, label=Brand)) + ylim(0,5)
coffee_plot = coffee_plot + ggtitle("Edie's Roasts")
coffee_plot


coffee_plot_2 = ggplot()
coffee_plot_2 = coffee_plot_2 + geom_point(aes(x=Store, y=Rating, colour=factor(Roast)))
coffee_plot_2 = coffee_plot_2 + ggtitle("Edie's Stores")
coffee_plot_2

## ANALYZE DATA

# What is this taster's favorite store? (Most frequented)
table_store = rbind(levels(Store), table(Store))
fave_store = table_store[1,which(table_store[2,]==max(table_store[2,]))]
cat("This taster frequently goes to:", fave_store)
fave_store


# What is this taster's average store?
avg_score = mean(Rating)
avg_score


# What is this taster's favorite roast?
df_roast = sapply(levels(Roast), function(this_level) {
  these_rows = as.numeric(which(Roast==this_level))
  stat = mean(Rating[these_rows])
  c(this_level, stat)
}
)
fave_roast = df_roast[1,which(df_roast[2,]==max(df_roast[2,]))]
fave_roast


# How about top 3 coffees?
# This takes the first 3 sorted regardless of ties
ratings_coffees = rbind(Rating, Name)
ratings_coffees = ratings_coffees[,order(Rating)]
indices = length(ratings_coffees[1,])-c(0:2)
top_three = levels(Name)[ratings_coffees[2, indices]]
top_three
