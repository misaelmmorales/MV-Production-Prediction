#Project
#install.packages("ggplot2"); install.packages("plotly")
library(ggplot2)

DF0 <- read.table("mv_unconv.csv", as.is = TRUE)
rock.data <- read.csv(text = DF0[[1]])
#eliminate wellindex column
rock.data <- rock.data[,-1]
head(rock.data); tail(rock.data)
attach(rock.data)
dim(rock.data)
pairs(rock.data)

#mean values
data.frame(mean(Por),mean(LogPerm),mean(Production),
           mean(AI),mean(Brittle),mean(TOC),mean(VR))

#plotting porosity and permeability
plot(Por,LogPerm,main="Porosity vs. LogPermeability \n for Unconventional Wells",
     xlab="Porosity",ylab="Log-Permeability")
ggplot(data=rock.data,aes(x=Por,y=LogPerm)) + geom_point(aes(color=Production)) + 
  ggtitle("Porosity vs. LogPermeability \n for Unconventional Wells")

#correlations
cov(rock.data)
cor(rock.data)
