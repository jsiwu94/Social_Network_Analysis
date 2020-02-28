
library(igraph)
library(dplyr)
library(sqldf)
library("psych")

setwd("~/Desktop/MSBA/Winter 2020/Social Med Analytics/Week 8/")

## Reading the data
product <- read.csv("products.csv")
purch <- read.csv("copurchase.csv")


## 1. Deleting Product that are not books
product1 <- product[(product$group == "Book") & (product$salesrank < 150000) 
                    & (product$salesrank != -1),]

purch1 <- purch[(purch$Source %in% product1$id) & (purch$Target %in% 
                                      product1$id),]
dim(purch1)
dim(product1)

net1 <- graph.data.frame(purch1, directed=T)

## 2. Create a variable named in-degree
in_degree <- degree(net1, mode="in")
head(in_degree)

# 3. Create a variable named out-degree
out_degree <- degree(net1, mode="out")
all_degree <- degree(net1, mode="all")
all_degree[all_degree == max(all_degree)]

## 4. Choosing product id =  33 & 4429 
sub <- subcomponent(net1, "33", mode = "all")

# 5. Visualize the subcomponent
graph <- induced_subgraph(net1, sub)
V(graph)$label <- V(graph)$name
V(graph)$degree <- degree(graph)

set.seed(222)
plot(graph,
     vertex.color=rainbow(33),
     vertex.size=V(graph)$degree*0.08,
     edge.arrow.size=0.01,
     vertex.label.cex=0.2,
     layout=layout.kamada.kawai)


# 6. Computing Statistics
diameter(graph, directed=T, weights=NA)
edge_density(graph, loops=F)
mean_distance(graph, directed=T)
degree_centrality <- centr_degree(graph, mode="all")

closeness <- closeness(graph, mode="all", weights=NA) 
head(sort(closeness, decreasing = TRUE))

between <- betweenness(graph, directed=T, weights=NA)
head(sort(between, decreasing = TRUE))

hub_score <- hub.score(graph)$vector
head(sort(hub_score), descreasing=TRUE)

authority_score <- authority.score(graph)$vector
head(sort(authority_score), descreasing=TRUE)

hist(all_degree, breaks=1:vcount(graph)-1
     , main="Histogram of node degree")


# 7. Create neighbors variable
names(purch1)[1] <- "id"
sid <- as_ids(sub)
sub_prod <- product1[product1$id %in% sid,]
neighbors_mean_variables <- as.data.frame(purch1 %>%
                              group_by(Target) %>%
                              inner_join(sub_prod, by="id") %>%
                              summarize(nghb_mn_rating = mean(rating),
                                    nghb_mn_salesrank = mean(salesrank),
                                    nghb_mn_review_cnt=mean(review_cnt)))
                  

# 8. Adding the data together then do Log Transform and Poisson 
in_df <- data.frame(id = names(in_degree), in_degree)
out_df <- data.frame(id = names(out_degree), out_degree)
closeness <- data.frame(id = names(closeness), closeness)
betweenness <- data.frame(id = names(between), between)
authority <- data.frame(id = names(authority_score), authority_score)
hub <- data.frame(id = names(hub_score), hub_score)

in_df$id<-as.numeric(as.character(in_df$id))
out_df$id<-as.numeric(as.character(out_df$id))
closeness$id<-as.numeric(as.character(closeness$id))
betweenness$id<-as.numeric(as.character(betweenness$id))
authority$id<-as.numeric(as.character(authority$id))
hub$id<-as.numeric(as.character(hub$id))

names(neighbors_mean_variables)[1] <-"id"

data <- sub_prod %>% inner_join(neighbors_mean_variables, by = "id") 
data <- data  %>% inner_join(in_df, by = "id") 
data <- data  %>% inner_join(out_df, by = "id") 
data <- data %>% inner_join(closeness, by="id") 
data <- data %>% inner_join(betweenness, by="id") 
data <- data %>% inner_join(authority, by="id") 
data <- data %>% inner_join(hub, by="id")

dim(data)

describe(data[4:16])
pairs.panels(data[3:16], gap=0) # pch is shape of dots

#log transform
#data$salesrank <- ifelse(data$salesrank==0,0,log(data$salesrank, base = exp(1)))
data$review_cnt <- ifelse(data$review_cnt==0,0,log(data$review_cnt, base = exp(1)))
data$downloads <- ifelse(data$downloads==0,0,log(data$downloads, base = exp(1)))
data$nghb_mn_salesrank <- ifelse(data$nghb_mn_salesrank==0,0,log(data$nghb_mn_salesrank, base = exp(1)))
data$nghb_mn_review_cnt <- ifelse(data$nghb_mn_review_cnt==0,0,log(data$nghb_mn_review_cnt, base = exp(1)))

dat <- data[4:16]
p1 <- glm(salesrank ~., data = data[4:16], family = "poisson")
summary(p1)

