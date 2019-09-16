# detect anomalies in healthcare claims data                   ----

# load libraries                                               ----
source("https://tinyurl.com/libraryQ")
library("igraph")
library("dplyr")
library("magrittr")
library("tibble")
library("tidyr")

# read the data                                                ----
# from   : Medicare Physician and Other Supplier Data CY 2012
# source : http://tinyurl.com/j4bxtj9

required  <- c(1, 14, 17, 22)
cols      <- 1:28
cols      <- ifelse(cols %in% required, "character", "NULL")

input <- read.csv("data_pagerank.csv"
                  , skip             = 1
                  , colClasses       = cols
                  , strip.white      = TRUE
                  , na.strings       = "NA"
                  , nrows            = 10000
                  )

names(input) <- c("doctor", "speciality", "procedure", "counts")
input        <- mutate(input, counts = as.integer(gsub(",", "", counts)))

# function to get doctors for a particular speciality
get_doctors <- function(sp){
  input$doctor[input$speciality == sp]
}

# all doctor-speciality combinations                           ----
doc_spe <-  input %>% 
            group_by(doctor, speciality) %>% 
            ungroup() %>% 
            select(doctor, speciality) %>% 
            unique

# create adjacency matrix                                      ----
adj_df <- input %>% 
          group_by(doctor, procedure) %>% 
          summarise(counts = sum(counts)) %>% 
          ungroup %>% 
          spread(key = procedure, value = counts, fill = 0)

adj_mat <- adj_df[,-1] %>% as.matrix %>% dist %>% as.matrix
adj_mat <- ifelse(adj_mat > 0, 1/adj_mat, 1)
diag(adj_mat) <- 0
rownames(adj_mat) <- adj_df[["doctor"]]

# create graph object                                          ----
gr <- graph_from_adjacency_matrix(adj_mat
                                  , mode = "undirected"
                                  , weighted  = TRUE)
# plot(gr)

# function to get personalized weights for a speciality
get_weights <- function(sp, weight = 25){
  ifelse(adj_df[["doctor"]] %in% get_doctors(sp)
         , weight
         , 1)
}

# run personalized pagerank algorithm on the graph object      ----
set.seed(1)
pr <- page_rank(gr, personalized = get_weights("Gastroenterology"))

# function to get top 'k' scores
get_scores <- function(k){
  data.frame(pr$vector) %>%
    transmute(doctor = adj_df[["doctor"]], score = pr.vector) %>%
    left_join(doc_spe, by = "doctor") %>%
    arrange(desc(score)) %>% 
    head(k)
}

# observe top 'k' scores for "Gastroenterology"                ----

print(get_scores(30))
# observe that 13, 14, 15 th rows are the anomalies
rm(list = ls())



# My try                                                       ----
input %>% group_by(doctor, speciality) %>% summarise(n()) %>% View

