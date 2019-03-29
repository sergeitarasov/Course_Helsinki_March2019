library("corHMM")
library("phytools")
library("plyr")

#########
# Simulating data
#########

# simulate tree
tree<-pbtree(n=200, scale=100)
plot(tree)

# simulate character history
# make Q matrix
Q <- matrix(
  c(
    -0.03, 0.01, 0.01, 0.01,
    0.03,   -0.09,   0.03,   0.03,
    0.01, 0.01, -0.03, 0.01,
    0.03,   0.03,   0.03,   -0.09
    ), 4,4, byrow = T
)

# simulate character history on tree
hist <- sim.history(tree, Q, nsim=1)
plot(hist)


#########
# Inference using ML and corHMM package (Traditional model)
#########

# make 2-rate Q matrix for inference

Qinf <- matrix(
  c(
    0, 1, 1, 1,
    2,   0,   2,   2,
    1, 1, 0, 1,
    2,   2,   2,   0
  ), 4,4, byrow = T
)
diag(Qinf) <- NA

# Inference using 2 rates
taxa <- cbind(hist$tip.label, hist$states)
rate2 <- rayDISC(hist, taxa, rate.mat=Qinf, node.states="marginal", 
        model="ARD", root.p="maddfitz")
# plot
plotRECON(tree, rate2$states,title="2-rate Matrix")

# make 1-rate Q matrix for inference
Qinf1 <- matrix(
  c(
    0, 1, 1, 1,
    1,   0,   1,   1,
    1, 1, 0, 1,
    1,   1,   1,   0
  ), 4,4, byrow = T
)
diag(Qinf1) <- NA

# Inference using 1 rate
taxa <- cbind(hist$tip.label, hist$states)
rate1 <- rayDISC(hist, taxa, rate.mat=Qinf1, node.states="marginal", 
                 model="ARD", root.p="maddfitz")
# plot
plotRECON(tree, rate1$states,title="1-rate Matrix")

# Compare AICs
rate1
rate2




#########
# Inference using ML and corHMM package (HMM)
#########
char.recode<-mapvalues(hist$states, from = c(1, 2, 3,4), 
                       to = c('1&2', '1&2', '3&4','3&4') )

# Inference using 2 rate HMM
taxa.hmm <- cbind(hist$tip.label, char.recode)
rate2.hmm <- rayDISC(hist, taxa.hmm, rate.mat=Qinf, node.states="marginal", 
                 model="ARD", root.p="maddfitz")

# Inference using 1 rate HMM
rate1.hmm <- rayDISC(hist, taxa.hmm, rate.mat=Qinf1, node.states="marginal", 
                     model="ARD", root.p="maddfitz")


