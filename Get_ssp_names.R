library("taxize", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")

#children("Parachorius", db = 'col')
ont<-children("Onthophagus", db = 'col')
ont$Onthophagus$childtaxa_id
str(ont)
ont<-children("Parachorius", db = 'col')
get_colid(sciname='Onthophagus', rank='genus')
ont<-as.colid("Parachorius")
classification("Onthophagus", db = "col")
classification("Parachorius", db = "col")
Onthophagus       genus a06929d4d099877ecd44dd3d6a55846f

ont<-children("a06929d4d099877ecd44dd3d6a55846f", db = 'col')
ssp <- ont$a06929d4d099877ecd44dd3d6a55846f$childtaxa_name
ssp <- strsplit(ssp, ' ')
lapply(ssp, length) %>% unique()
ssp <- lapply(ssp, function(x) x[length(x)] ) %>%unlist
length(ssp)
l.ssp <- nchar(ssp)
hist(l.ssp, freq=F, col='grey', ylab='Probability', xlab='N of letters', main = 'Number of letters in Onthophagus species names (2172 names)', cex.main=2, cex.lab=1.5)



#########
# Binomial
k <- c(0,1,2,3)
n <- 3
p <- 0.5
Pr <- dbinom(k, n,p)
plot(k, Pr, col='grey', type = 'h',lwd = 15, cex.lab=1.5, ylab='Probability', xlab='k (number of heads)')
