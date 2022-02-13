library(bnlearn)

# creating the DAG with one node for each variable in the survey. No archs at this point
dag <- empty.graph(nodes = c("A", "S", "E", "O", "R", "T"))
# "empty graph" = it has an empty arc set
dag

# Age and Sex have a direct influence on Education:
dag <- set.arc(dag, from = "A", to = "E")
dag <- set.arc(dag, from = "S", to = "E")

# Education strongly affects both Occupation and Residence
dag <- set.arc(dag, from = "E", to = "O")
dag <- set.arc(dag, from = "E", to = "R")

# the preferred means of transport are directly influenced by both Occupation and Residence
dag <- set.arc(dag, from = "O", to = "T")
dag <- set.arc(dag, from = "R", to = "T")

dag

# This representation of the graph structure is designed to recall 
# a product of conditional probabilities:
modelstring(dag)

# MANIPULATING ARCHS
# with bnlearn we can manipulate the arcs
# checking the nodes
nodes(dag)
arcs(dag)

# this last function can also be used to set the arcs or to create the
# networkd in an easier way, like this:
dag2 <- empty.graph(nodes = c("A", "S", "E", "O", "R", "T"))
arc.set <- matrix(c("A", "E",
                      "S", "E",
                      "E", "O",
                      "E", "R",
                      "O", "T",
                      "R", "T"),
                    byrow = TRUE, ncol = 2,
                    dimnames = list(NULL, c("from", "to")))
arcs(dag2) <- arc.set
dag2

# comparing two dags
all.equal(dag, dag2)
# all.equal is a utility from "base" package to compare two R objects
?all.equal

# if we try to insert a cycle it will prevent that, by causing an error
set.arc(dag, from = "T", to = "E")


# creating the LEVELS of the variables:
A.lv <- c("young", "adult", "old")
S.lv <- c("M", "F")
E.lv <- c("high", "uni")
O.lv <- c("emp", "self")
R.lv <- c("small", "big")
T.lv <- c("car", "train", "other")


# PROBABILITIES
# creating the probabilities tables
# Age and Sex are modelled by simple, unidimensional probability tables (they have no parent)
A.prob <- array(c(0.30, 0.50, 0.20), dim = 3,
                dimnames = list(A = A.lv))
A.prob

S.prob <- array(c(0.60, 0.40), dim = 2,
                 dimnames = list(S = S.lv))
S.prob


# Ocupation and Residence, which depend on Education, are modelled by 
# two-dimensional conditional probability tables.
O.prob <- array(c(0.96, 0.04, 0.92, 0.08), dim = c(2, 2),
                dimnames = list(O = O.lv, E = E.lv))
O.prob

R.prob <- array(c(0.25, 0.75, 0.20, 0.80), dim = c(2, 2),
                dimnames = list(R = R.lv, E = E.lv))
R.prob


# Education and Travel are modelled as three-dimensional tables, 
# since they have two parents each
# TODO: enteder melhor como essa table de 3 dim é criada com dimensoes que nao tem o mesmo tamanho
E.prob <- array(c(0.75, 0.25, 0.72, 0.28, 0.88, 0.12, 0.64,
                   0.36, 0.70, 0.30, 0.90, 0.10), dim = c(2, 3, 2),
                 dimnames = list(E = E.lv, A = A.lv, S = S.lv))
E.prob

T.prob <- array(c(0.48, 0.42, 0.10, 0.56, 0.36, 0.08, 0.58,
                    0.24, 0.18, 0.70, 0.21, 0.09), dim = c(3, 2, 2),
                   dimnames = list(T = T.lv, O = O.lv, R = R.lv))
T.prob


# recreating the DAG with the string for didactic purposes
# the string is the same of the factorized global distribution
dag3 <- model2network("[A][S][E|A:S][O|E][R|E][T|O:R]")

# equal to the first DAG
all.equal(dag, dag3)


# combining the structure of the DAG with the local distributions (which are in a list)
cpt <- list(A = A.prob, S = S.prob, E = E.prob, O = O.prob,
             R = R.prob, T = T.prob)
cpt

# fit the parameters of a BN: 
bn <- custom.fit(dag, cpt)
bn

# this function computes the number of parameters
nparams(bn)

# bn.fit objects are used to describe a BN in bnlearn. 
# They include information about both the DAG and local distributions
class(bn)

# they can be used as if they were of class bn:
arcs(bn)
nodes(bn)

# the conditional probability tables can be printed from the bn.fit object
bn$R

# extracting the probability table to use later (coef is from "stats")
R.cpt <- coef(bn$R)

# just typing the bn.fit object causes all the prob tables to be printed
bn


# ESTIMATING THE LOCAL DISTRIBUTIONS FROM DATA
# while custom.fit() constructs the BN using a set of custom parameters specified by the user, 
# bn.fit() estimates the parameters from data. Both return a bn.fit object
# "method" attribute determines which estimator to use. mle="maximum likelihood estimator"
setwd("C:/dev/bayesian-nets/book-scutari")

# reading the data
survey <- read.table("survey.txt", header = TRUE)
head(survey)
str(survey)

# converting columns to factor
survey$A <- as.factor(survey$A)
survey$R <- as.factor(survey$R)
survey$E <- as.factor(survey$E)
survey$O <- as.factor(survey$O)
survey$S <- as.factor(survey$S)
survey$T <- as.factor(survey$T)
str(survey)

# mle => maximum likelihood estimator
bn.mle <- bn.fit(dag, data = survey, method = "mle")
str(bn.mle)
bn.mle$O

# we can also estimate the probs with R's prob.table, which is also based on frequency
# so we get the same results as the bn.fit with mle
prop.table(table(survey[, c("O", "E")]), margin = 2)
# table() will put E in the columns, which is the parent
# margin=2 means the marginal sums is by columns. So, for each cell, it divides
# the cell value by the column sum

# estimating conditional probs in a Bayesian setting
bn.bayes <- bn.fit(dag, data = survey, method = "bayes", iss = 10)
# iss = imaginary sample size
bn.bayes$O

# as we increase the iss, the estimates are closer to 0.5 in a flat/uniform distribution
bn.bayes <- bn.fit(dag, data = survey, method = "bayes", iss = 20)
bn.bayes$O


#######################
# ESTIMATING THE DAG
#######################

# mutual information (mi) or "G2" test:
ci.test("T", "E", c("O", "R"), test = "mi", data = survey)

# Pearson's X2 test:
ci.test("T", "E", c("O", "R"), test = "x2", data = survey)
# both tests above return large p-values, indicating that the dependence relationship
# encoded by E x T is not significant given the current DAG structure

# we can also test if an arch can be removed
ci.test("T", "O", "R", test = "x2", data = survey)

# we can test all archs with arc.strength
arc.strength(dag, data = survey, criterion = "x2")
# if criterion is a conditional independence test, the strength is a p-value 
# (so the lower the value, the stronger the relationship)
# arc.strenght removes each arc from the graph and quantifies the change with some probabilistic criterion
# it can quantify with either a conditional independence test or with a network score
# if it's an independence teste, the test is for the "to" node to be independent from 
# the "from" node conditional on the remaining parents of "to"
# in the above example, all arcs with the exception of O -> T have p-values smaller than 0.05 and are well supported by the data.
?arc.strength

# scoring the DAG with both BIC and BDe
score(dag, data = survey, type = "bic")
score(dag, data = survey, type = "bde", iss = 10)
# BDe needs iss parameter (imaginary sample size) since it uses a prior prob
# For small values of iss or large observed samples, log BDe and BIC scores yield similar values.
score(dag, data = survey, type = "bde", iss = 1)

# using these scores, we can remove or add an arc and then check whether this change 
# improved or decreased the score of the DAG
dag4 <- set.arc(dag, from = "E", to = "T")
nparams(dag4, survey)
score(dag4, data = survey, type = "bic")

# generate a DAG at random and compare it with the previous DAG
rnd <- random.graph(nodes = c("A", "S", "E", "O", "R", "T"))
modelstring(rnd)
score(rnd, data = survey, type = "bic")


# estimating the DAG structure with hc() (hill-climbing). Default score is BIC
learned <- hc(survey)
modelstring(learned)
score(learned, data = survey, type = "bic")

# testing with "bde" score
learned2 <- hc(survey, score = "bde")
modelstring(learned)

# any arc removal will decrease the BIC score
# arc.strength returns the change in the score caused by each arc removal,
# since we provided a networkd score as the criterion
arc.strength(learned, data = survey, criterion = "bic")


# about "dag", many arc removals can improve the score, which suggests that
# not all the dependencies it encondes can be learned correctly from "survey" data
arc.strength(dag, data = survey, criterion = "bic")

###############################
# USING THE BAYESIAN NETWORK
##############################
# checking if S and R are d-separated
dsep(dag, x = "S", y = "R")

# S is associated with R because E is influenced by S and R is influenced by E
# path function shows there is a path from S to R
path.exists(dag, from = "S", to = "R")

# if we condition on E, that path is blocked and S and R become independent
dsep(dag, x = "S", y = "R", z = "E")


# USING THE CONDITION PROBABILITY TABLES
library(gRain)
#BiocManager::install("RBGL")
library(RBGL)

# building the junction tree:
junction <- compile(as.grain(bn))

# querying the distribution of node T
querygrain(junction, nodes = "T")$T

# we want to see the impact of being a woman on travel preferences
# setting the evidence of being a woman
jsex <- setEvidence(junction, nodes = "S", states = "F")
# querying the distributino of node T again, but now with the new evidence
querygrain(jsex, nodes = "T")$T

# querying to see how living in small city affects car and train use
jres <- setEvidence(junction, nodes = "R", states = "small")
querygrain(jres, nodes = "T")$T

# assessing conditional independence based on conditional probability
jedu <- setEvidence(junction, nodes = "E", states = "high")
SxT.cpt <- querygrain(jedu, nodes = c("S", "T"), type = "joint")
SxT.cpt
# querying the marginal distribution
querygrain(jedu, nodes = c("S", "T"), type = "marginal")
# querying the CONDITIONAL distribution.
# returns the distribution of the first node in "nodes" conditional on the other nodes in "nodes":
querygrain(jedu, nodes = c("S", "T"), type = "conditional")
# probabilites are identical. It suggests that S is independent from T conditional on E
# that is: considering people with high education, the travel doesn't depend on the sex
# this is also implied by graphical separation
dsep(bn, x = "S", y = "T", z = "E")


# cpquery returns the probability of a specific event given some evidence
cpquery(bn, event = (S == "M") & (T == "car"), evidence = (E == "high"))
# returned a slightly diff value from querygrain:
querygrain(jedu, nodes = c("S", "T"), type = "joint")
# we can improve the approximation using argument n to increase the number of observations
cpquery(bn, event = (S == "M") & (T == "car"), evidence = (E == "high"), n = 10^6)

# likelihood weighting is a better method for sampling observations of the BN. 
# All of them match the evidence, then they are weighted when the prob is computed
cpquery(bn, event = (S == "M") & (T == "car"), evidence = list(E = "high"), method = "lw")


# a more complex query:the probability of a man travelling by car given that 
# his Age is young and his Education is uni or that he is an adult, regardless of his Education.
cpquery(bn, event = (S == "M") & (T == "car"), evidence = ((A == "young") & (E == "uni")) | (A == "adult"))

# cpdist returns a data frame containing the random observations for the variables in nodes that match evidence.
SxT <- cpdist(bn, nodes = c("S", "T"), evidence = (E == "high"))
head(SxT)


##########################
# PLOTTING DAGs
##########################
# Bnlearn uses the functionality implemented in the Rgraphviz package to plot graph structures
graphviz.plot(dag)
# we can provide the layout we prefer:
graphviz.plot(dag, layout="fdp")
graphviz.plot(dag, layout="circo")
graphviz.plot(dag, layout="dot")
# dot is the default layout


# highlighting specific arcs and nodes
hlight <- list(nodes = nodes(dag), arcs = arcs(dag), col = "grey", textCol = "grey")
# we first change all arcs and nodes to gray
pp <- graphviz.plot(dag, highlight = hlight) # returns object of class graph
pp
# we can manipulate this graph object with "edgeRenderInfo" function
edgeRenderInfo(pp) <- list(col = c("S~E" = "black", "E~R" = "black"),
                           lwd = c("S~E" = 3, "E~R" = 3))
# and similarly we can change nodes with nodeRenderInfo
nodeRenderInfo(pp) <- list(col = c("S" = "black", "E" = "black", "R" = "black"),
                          textCol = c("S" = "black", "E" = "black", "R" = "black"),
                          fill = c("E" = "grey", "R" = "yellow"))
edgeRenderInfo(pp) <- list(lty = c("S~E" = "solid", "E~R" = "dotted"))
?edgeRenderInfo

# and now we can render the modified graph with renderGraph
library(Rgraphviz)
renderGraph(pp) # this function is from Rgraphviz


## PLOTTING CONDITIONAL PROBABILITY DISTRIBUTIONS
# (bn.fit.barchart) -> 
# (bn.fit.dotplot) -> dot plots  
# they can plot from bn.fit objects
# Both functions are based on the lattice package:
bn.fit.barchart(bn.mle$T, main = "Travel",
                xlab = "Pr(T | R,O)", ylab = "")
bn.fit.dotplot(bn.mle$T, main = "Travel",
                xlab = "Pr(T | R,O)", ylab = "")


# PLOTTING COMPLEX DISTRIBUTION CHARTS
# we can use the lattice functions directly to
# produce complex plots that are beyond the capabilities of bnlearn.
# let's compare the marginal distribution of Travel with the results of 
# two conditional probability queries
# FIRST, create a dataframe containing the three prob distributions
Evidence <- factor(c(rep("Unconditional",3), rep("Female", 3),
                     rep("Small City",3)),
                    levels = c("Unconditional", "Female", "Small City"))
Travel <- factor(rep(c("car", "train", "other"), 3),
                 levels = c("other", "train", "car"))
distr <- data.frame(Evidence = Evidence, Travel = Travel,
                    Prob = c(0.5618, 0.2808, 0.15730, 0.5620, 0.2806,
                             0.1573, 0.4838, 0.4170, 0.0990))
distr

# SECOND, with the probs organized in the df, the barchart can be created as follows
library(lattice)
barchart(Travel ~ Prob | Evidence, 
         data = distr,
         layout = c(3, 1), xlab = "probability",
         scales = list(alternating = 1, tck = c(1, 0)),
         strip = strip.custom(factor.levels =
                                  c(expression(Pr(T)),
                                      expression(Pr({T} * " | " * {S == F})),
                                      expression(Pr({T} * " | " * {R == small})))),
          panel = function(...) {
            panel.barchart(...)
            panel.grid(h = 0, v = -1)
           }
         )


barchart(Travel ~ Prob | Evidence, data = distr)
