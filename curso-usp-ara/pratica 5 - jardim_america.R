dados <- read.csv(choose.files(), header = T, sep=";")
View(dados)
nrow(dados)

# it'll be important to know the structure due to the discretization process
str(dados)
# two problems here: # of suites (among other vars) is int and "value" is char while we need to work with
# discrete vars, since we'll create a discrete Bayesian Net


# ---------------------------- #
# ----  PRE PROCESSING  ------ #
# ---------------------------- #

# transforming suites, bathrooms and parkings into factor
dados[,2:4] <- lapply(dados[,2:4], factor)
str(dados)

dados$area <- as.numeric(dados$area)
dados$value <- as.numeric(dados$value) # NAs introduced by coercion
str(dados)

# checking how many NAs were introduced
apply(is.na(dados), 2, sum) # 2 means summing by columns

# removing NAs
dados <- na.omit(dados)
nrow(dados)

# ---------------------------- #
# ---------  EDA  ------------ #
# ---------------------------- #
hist(dados$value)
summary(dados$value)


# Discretizing
require("bnlearn")
dados_d=dados
dados_d[,c(1,5)] <- bnlearn::discretize(dados[,c(1,5)], 
                               method='hartemink', breaks=3) # these are the default params
# hartemink does the discretization considering the mutual information between vars
# we could test another discretization methods

dados_d
str(dados_d)


# checking how are the categories now
boxplot(dados$area~dados_d$area)
boxplot(dados$value~dados_d$value)
table(dados_d$area)
table(dados_d$value)

# strong linear correlation between area and value, so let's check the correlation in continuous and discrete cases:
plot(dados$area, dados$value)
table(dados_d$area, dados_d$value)

# checking the levels obtained from the discretization, so we can print lines on the distribution plot
levels(dados_d$area)
levels(dados_d$value)
# ploting lines, this is how we've partitionated our continuous space
abline(v = c(123, 180), h = c(1400000, 1850000), col='red')
# when I discretize, I may be changing the relationship a bit between the vars.
# the discrete version of the BN may be different, it may have more edges
# the more classes I use in the discretization, the closer to the initial correlation. But this also brings more complexity



# creating the first model (hill climb):
mod_hc_k2 <- hc(dados_d, score="k2")
# param "start" of hc(): you can provide a graph so it can start from it
# this is how I can use the specialist knowledge. I create a graph with model2network()
# and this will act as a seed for the algorithm 
# "perturb": used to avoid local max/min
# "max.iter": maximum number of interactions. Default is infinit which depends on the machine been used
# if you have a huge dataset, it may be interesting limiting this


# exploring the structure with qgraph
require("qgraph")
qgraph(mod_hc_k2)

require("Rgraphviz")
graphviz.plot(mod_hc_k2)

# bde produces a different graph
mod_hc_bd <- hc(dados_d, score="bde")
graphviz.plot(mod_hc_bd)

# bic producess a similar one compared to bde
mod_hc_bic <- hc(dados_d, score="bic")
graphviz.plot(mod_hc_bic)


# method PC:
mod_pc <- pc.stable(dados_d)
graphviz.plot(mod_pc)

# --- BOOSTRAP 
# we can see that each method can generate different strucutures, so bootstrap is very interesting (??)
mod_bt <- boot.strength(dados_d, R=100, algorithm = 'pc.stable')
# "R" is the number of iterations ('replicates')

qgraph(mod_bt, asize=5,
       legend.cex=0.5,
       edge.color="black",
       color="tomato", edge.labels=T
       )

# BLACK LIST:
# -----------
# some relationships in this net may not make sense. The blacklist comes in!
# here, the specialist will tell which variable will NOT influence which variable:
BL <- matrix(c(
  'suites', 'area',
  'bathrooms', 'area'), byrow=T, ncol=2)


# creating a new bootstrap model, considering the blacklist
mod_bt2 <- boot.strength(dados_d, R=100, 
                         algorithm = 'pc.stable',
                         algorithm.args = list(blacklist=BL)
                         )

qgraph(mod_bt2, asize=5,
       legend.cex=0.5,
       edge.color="black",
       color="tomato", edge.labels=T
)


# this object carries the values of the model connections:
mod_bt2
# stregth: is the connection strength (regardless of direction)
# directions: % of times that this direction was present (influenced by the blacklist)



# AVERAGED NETWORK 
# -------------------
# now let's experiment with the idea of "averaged model" of the relations. Based on the threshold that we'll define
mod_BT <- averaged.network(mod_bt2) # this will generate a new model 
# it's will consider only relations with >0.5 of strength in the likely direction (default threshold [need to check if it's really 0.5]))

qgraph(mod_BT, asize=5,
       legend.cex=0.5,
       edge.color="black",
       color="tomato", edge.labels=T
)

graphviz.plot(mod_BT)
strength <- arc.strength(mod_BT, dados_d, 'bde') # I can specify the algorithm
strength.plot(mod_BT, strength)


# TABU
# --------
# now, in order to feel more confident about the model, let's check with another method: Tabu
mod_bt2_tabu <- boot.strength(dados_d, R=100, 
                         algorithm = 'tabu',
                         algorithm.args = list(blacklist=BL,
                                               score='bde'))
 
qgraph(mod_bt2_tabu, asize=5,
       legend.cex=0.5,
       edge.color="black",
       color="tomato", edge.labels=T
)
mod_bt2_tabu <- averaged.network(mod_bt2_tabu)
graphviz.plot(mod_bt2_tabu)
# reminder: tabu does not perform d-separation tests


# returns the Markov blanket of a node:
mb(mod_BT, 'value')
mb(mod_BT, 'parkings')




# PREDICTION
# -------------
# performs cross validation
bn.cv(mod_BT, data=as.data.frame(dados_d), k=5, fit='bayes')
# "fit='bayes'" changes the method from mle (maximum likelihood parameter estimation) to Bayesian parameter estimation
# it eliminates errors related to probability=0 for a node, which is a problem for mle

# "expected loss" is a measure of quality of the network.
# we can use different measures to choose a model
score(mod_BT, as.data.frame(dados_d), 'bde')

# FITTING:
# so far, we only have the structure of the network, so we can't use "predict" function
# so, let's fit to find the parameters:
mod_BT_fit <- bn.fit(mod_BT, dados_d)
# the correct way is by separating train and test data, this is just for demonstration:
value.p <- predict(mod_BT_fit, node='value', data=as.data.frame(dados_d))
table(value.p, dados_d$value)



# QUERYING
# ---------
# querying based on evidences, using cpquery (conditional probability queries)
?cpquery()
# what's the prob of parkings being level 2 given that area is the largest one (180,455]?
cpquery(mod_BT_fit, (parkings=="2"), (area=="(180,455]"))
# what's the prob of area being the largest one (180,455] given that parkings is level 2?
cpquery(mod_BT_fit, (area=="(180,455]"),(parkings=="2"))
# notice that here 'area' influences 'parkings'

# checking the conditional distributions with "cpdist"
park3_area_dist <- cpdist(mod_BT_fit, "area", (parkings=='3')) # the area, given that parkings is level 3
tab <- table(park3_area_dist) # counting the frequency for each "area" level, given parkings=3
tab
tab/sum(tab) # calculating the probability

park3_suits2_area_dist <- cpdist(mod_BT_fit, "area", (parkings=='3') & (suites=="2")) # the area, given that parkings is level 3 and suites level 2
tab <- table(park3_suits2_area_dist) # counting the frequency for each "area" level, given parkings=3
tab
tab/sum(tab) # calculating the probability

str(dados_d)
levels(dados_d$area)









