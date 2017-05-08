edges = read.csv('2-edges.csv')
users = read.csv('2-users.csv')

# how many facebook users are there in the dataset?
nrow(users)

# in the dataset, what is the average number of friends per user?
nrow(edges) * 2 / nrow(users)

# out of all the students who listed a school, what was the most common locale?
table(users$locale, users$school)
# => locale B

# is it possible that either school A or B is an all-girls or all-boys school?
table(users$gender, users$school)
# => no

install.packages('igraph')
library(igraph)

# graph.data.frame() function expects the first 2 columns of parameter d to specify the edges in the graph
# our edges are undirected - if A is a facebook friend of B, then B is a facebook friend of A
# therefore, we set the directed parameter to FALSE
# the vertices parameter expects a data frame where the first column is a vertex id and the remaining columns are properties of vertices in our graph
g = graph.data.frame(edges, FALSE, users)

# plot the graph
plot(g, vertex.size = 5, vertex.label = NA)

# how many users are friends with 10 or more other facebook users in this network?
table(degree(g) >= 10)

# change the 'size' attribute of the vertices of the graph to be an increasing function of their degrees
V(g)$size = degree(g) / 2 + 2
plot(g, vertex.label = NA)

# what is the largest size we assigned to any node in the graph?
max(V(g)$size)

# what is the smallest size we assigned to any node in the graph?
min(V(g)$size)

# color the vertices based on the gender of the user
V(g)$color = 'black'
V(g)$color[V(g)$gender == 'A'] = 'red'
V(g)$color[V(g)$gender == 'B'] = 'grey'

# what is the gender of the users with the highest degree in the graph?
plot(g, vertex.label = NA)

# color the vertices based on the school of the user
V(g)$color = 'black'
V(g)$color[V(g)$school == 'A'] = 'red'
V(g)$color[V(g)$school == 'AB'] = 'grey'
plot(g, vertex.label = NA)

# color the vertices based on the locale of the user
V(g)$color = 'black'
V(g)$color[V(g)$locale == 'A'] = 'red'
V(g)$color[V(g)$locale == 'B'] = 'grey'
plot(g, vertex.label = NA)

# plot the graph in 3-D
install.packages('rgl')
library(rgl)
rglplot(g, vertex.label = NA)

# change the edge width
plot(g, edge.width = 2, vertex.label = NA)
