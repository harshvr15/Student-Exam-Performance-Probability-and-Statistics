#setting the working directory
setwd("~/Desktop/P&S")

#libraries installing
install.packages("readr")
install.packages("pastecs") #For creating descriptive statistic summaries
install.packages("ggplot2") #For creating histograms with more detail than plot
install.packages("semTools") #For skewness and kurtosis
install.packages("coin")
install.packages("rstatix")
install.packages("effectsize")
install.packages("userfriendlyscience")
install.packages("sjstats")

#libraries to be loaded
library(readr)
library(pastecs) 
library(ggplot2) 
library(semTools) 
library(coin)
library(rstatix)
library(effectsize)
library(sjstats)
library(userfriendlyscience)

#loading the dataset into df
df = read_csv("sperformance-dataset.csv")

#structure of the df
dim(df)
str(df)

#checking for NA in  the file
apply(is.na(df),2,sum)

#searching for 0 throughout the dataset
colSums(df == 0)

#Replacing 0 for marks by mean
df$mG2[df$mG2==0]= round(mean(df$mG2))
df$mG3[df$mG3==0]= round(mean(df$mG3))

df$pG1[df$pG1==0]= round(mean(df$pG1))
df$pG3[df$pG3==0]= round(mean(df$pG3))



gg <- ggplot(df, aes(x=df$mG1))

#Change the label of the x axis
gg <- gg + labs(x="Marks in Maths1")

#manage binwidth and colours
gg <- gg + geom_histogram(binwidth=2, colour="black", aes(y=..density.., fill=..count..))
gg <- gg + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")

#adding a normal curve
#use stat_function to compute a normalised score for each value of mG1
#pass the mean and standard deviation
#use the na.rm parameter to say how missing values are handled
gg <- gg + stat_function(fun=dnorm, color="red",args=list(mean=mean(df$mG1, na.rm=TRUE), sd=sd(df$mG1, na.rm=TRUE)))

#to display the graph request the contents of the variable be shown
gg

qqnorm(df$mG1)
qqline(df$mG1, col=2) #show a line on theplot


pastecs::stat.desc(df$mG1, basic=F)


tpskew<-semTools::skew(df$mG1)

tpkurt<-semTools::kurtosis(df$mG1)

tpskew[1]/tpskew[2]

tpkurt[1]/tpkurt[2]


zmG1<- abs(scale(df$mG1))

FSA::perc(as.numeric(zmG1), 1.96, "gt")
FSA::perc(as.numeric(zmG1), 3.29, "gt")


gs <- ggplot(df, aes(x=df$mG2))
gs <- gs + labs(x="Perceived Stress")
gs <- gs + geom_histogram(binwidth=2, colour="black", aes(y=..density.., fill=..count..))
gs <- gs + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
gs <- gs + stat_function(fun=dnorm, color="red",args=list(mean=mean(df$mG2, na.rm=TRUE), sd=sd(df$mG2, na.rm=TRUE)))
gs

qqnorm(df$mG2)
qqline(df$mG2, col=2) #show a line on theplot

pastecs::stat.desc(df$mG2, basic=F)

tpskew<-semTools::skew(df$mG2)
tpkurt<-semTools::kurtosis(df$mG2)

tpskew[1]/tpskew[2]
tpkurt[1]/tpkurt[2]

zmG2<- abs(scale(df$mG2))

FSA::perc(as.numeric(zmG2), 1.96, "gt")
FSA::perc(as.numeric(zmG2), 3.29, "gt")

scatter <- ggplot(df, aes(df$mG1, df$mG2))

#Add a regression line
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) + labs(x = "mG1", y = "mG2") 


#CORRELATION TEST - PEARSON as data is normally distributed 
stats::cor.test(df$mG1, df$mG2, method='pearson')

# CHI-SQUARE

library(gmodels)

gmodels::CrossTable(df$Mjob, df$paid.m, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")

#INDEPENDENT T TEST
mg1 = subset(df, select = c("mG1"))

by(df$mG1,df$activities.m,median)

by(df$mG1,df$activities.m,IQR)

psych::describeBy(df$mG1, df$activities.m, mat=TRUE)

car::leveneTest(mG1 ~ activities.m, data=df)

stats::t.test(mG1~activities.m,var.equal=TRUE,data=df)

#No statistically significant difference was found

res <- stats::t.test(mG1~activities.m,var.equal=TRUE,data=df)
#Calculate Cohen's d artithmetically
effcd=round((2*res$statistic)/sqrt(res$parameter),2)

#Using function from effectsize package
effectsize::t_to_d(t = res$statistic, res$parameter)

#Eta squared calculation
effes=round((res$statistic*res$statistic)/((res$statistic*res$statistic)+(res$parameter)),3)
effes


#1. H0: There are no differences between math first period grade  for respondents in guardian group. HA: There are differences between math first period grade  for respondents in guardian group.

#We will allocate the histogram to a variable to allow use to manipulate it
gg <- ggplot(df, aes(x=Mjob))
gg <- gg+ggtitle("Figure 1 - Histogram for Total Self-Esteem")

#Change the label of the x axis
gg <- gg + labs(x="Total Self-Esteem")

#manage binwidth and colours
gg <- gg + geom_histogram(binwidth=2, colour="black", aes(y=..density.., fill=..count..))
gg <- gg + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")

#adding a normal curve
#use stat_function to compute a normalised score for each value of tslfest
#pass the mean and standard deviation
#use the na.rm parameter to say how missing values are handled
gg <- gg + stat_function(fun=dnorm, color="red",args=list(mean=mean(df$Mjob, na.rm=TRUE), sd=sd(df$Mjob, na.rm=TRUE)))

#to display the graph request the contents of the variable be shown
gg

#Create a qqplot
qqnorm(df$absences.m, main="Figure 2 - QQ Plot for Total Self-Esteem")
qqline(df$absences.m, col=2) #show a line on the plot

#stat.desc is a function from pastecs - make sure you include the basic switch=F to ensure you don't get scienfitic notation
pastecs::stat.desc(df$absences.m, basic=F)

#We can make our decision based on the value of the standardised score for skew and kurtosis
#We divide the skew statistic by the standard error to get the standardised score
#This will indicate if we have a problem
tpskew<-semTools::skew(df$absences.m)
## Warning in semTools::skew(survey$tslfest): Missing observations are removed from
## a vector.
tpkurt<-semTools::kurtosis(df$absences.m)
## Warning in semTools::kurtosis(survey$tslfest): Missing observations are removed
## from a vector.
tpskew[1]/tpskew[2]
tpkurt[1]/tpkurt[2]

zabs<- abs(scale(df$absences.m))

FSA::perc(as.numeric(zabs), 1.96, "gt")
FSA::perc(as.numeric(zabs), 3.29, "gt")

#Differences more than 2 groups - Parametric Tests
#ANOVA

psych::describeBy(df$mG1, df$guardian.m, mat=TRUE)

stats::bartlett.test(mG1~ guardian.m, data=df)

#p value is > 0.05 so the result is not statistically significant so we can assume homogeneity


#Conduct ANOVA using the userfriendlyscience test oneway
#In this case we can use Tukey as the post-hoc test option since variances in the groups are equal
#If variances were not equal we would use Games-Howell
userfriendlyscience::oneway(as.factor(df$guardian.m),y=df$mG1,posthoc='Tukey')

res1<-userfriendlyscience::oneway(as.factor(df$guardian.m),y=df$mG1,posthoc='Tukey')

#use the aov function - same as one way but makes it easier to access values for reporting
res2<-stats::aov(mG1~ guardian.m, data = df)
res2
#Get the F statistic into a variable to make reporting easier
fstat<-summary(res2)[[1]][["F value"]][[1]]
fstat
#Get the p value into a variable to make reporting easier
aovpvalue<-summary(res2)[[1]][["Pr(>F)"]][[1]]
aovpvalue
#Calculate effect
aoveta<-sjstats::eta_sq(res2)[2]
aoveta
#In the report we are using the res2 variable to retrieve the degrees of freedom
#and the eta_sq function from the sjstats package to calculate the effect
#No statistically significant difference was found so we do not need to examine and report the post-hoc results also.



####### LINEAR REGRESSION ################

library(stats)
library(ggplot2)
library(foreign) #To work with SPSS data
library(lm.beta) #Will allow us to isolate the beta co-efficients
library(stargazer)#For formatting outputs/tables

#mG1

#We will allocate the histogram to a variable to allow use to manipulate it
gg <- ggplot(df, aes(x=mG1))
gg <- gg+ggtitle("Histogram for mG1")

#Change the label of the x axis
gg <- gg + labs(x="marks for maths1")

#manage binwidth and colours
gg <- gg + geom_histogram(binwidth=0.1, colour="black", aes(y=..density.., fill=..count..))
gg <- gg + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
#adding a normal curve
#use stat_function to compute a normalised score for each value of mG1
#pass the mean and standard deviation
#use the na.rm parameter to say how missing values are handled
gg <- gg + stat_function(fun=dnorm, color="red",args=list(mean=mean(df$mG1, na.rm=TRUE), sd=sd(df$mG1, na.rm=TRUE)))

#to display the graph request the contents of the variable be shown
gg

#Create a qqplot
qqnorm(df$mG1, main="QQ Plot for mG1")
qqline(df$mG1, col=2) #show a line on the plot

mean(df$mG1)
sd(df$mG1)
length(df$mG1)

tpskew<-semTools::skew(df$mG1)
tpkurt<-semTools::kurtosis(df$mG1)

tpskew[1]/tpskew[2]
tpkurt[1]/tpkurt[2]

zmg1<- abs(scale(df$mG1))

FSA::perc(as.numeric(zmg1), 1.96, "gt")
FSA::perc(as.numeric(zmg1), 3.29, "gt")

#mg2

#We will allocate the histogram to a variable to allow use to manipulate it
gg <- ggplot(df, aes(x=mG2))
gg <- gg+ggtitle("Histogram for mG2")

#Change the label of the x axis
gg <- gg + labs(x="marks for maths2")

#manage binwidth and colours
gg <- gg + geom_histogram(binwidth=0.1, colour="black", aes(y=..density.., fill=..count..))
gg <- gg + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
#adding a normal curve
#use stat_function to compute a normalised score for each value of mG2
#pass the mean and standard deviation
#use the na.rm parameter to say how missing values are handled
gg <- gg + stat_function(fun=dnorm, color="red",args=list(mean=mean(df$mG2, na.rm=TRUE), sd=sd(df$mG2, na.rm=TRUE)))

#to display the graph request the contents of the variable be shown
gg

#Create a qqplot
qqnorm(df$mG2, main="QQ Plot for mG2")
qqline(df$mG2, col=2) #show a line on the plot

mean(df$mG2)
sd(df$mG2)
length(df$mG2)

tpskew<-semTools::skew(df$mG2)
tpkurt<-semTools::kurtosis(df$mG2)

tpskew[1]/tpskew[2]
tpkurt[1]/tpkurt[2]

zmg2<- abs(scale(df$mG2))
FSA::perc(as.numeric(zmg2), 1.96, "gt")
FSA::perc(as.numeric(zmg2), 3.29, "gt")

#Explore relationship between mG1 and mG2
#Simple scatterplot of mG1 and mG2
#aes(x,y)
scatter <- ggplot2::ggplot(regression, aes(mG1, mG2))
#Add a regression line
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) + labs(x = "Students score maths1", y = "Students score maths2") 


#Pearson Correlation
stats::cor.test(df$mG1, df$mG2, method='pearson')

#Simple Linear Regression -mG2 predicted by mG1
model1<-lm(df$mG2~df$mG1)
anova(model1)

summary(model1)

lm.beta::lm.beta(model1)

stargazer(model1, type="text") #Tidy output of all the required stats


#Multiple Linear Regression 1
#Multiple Linear Regression - mG2 predicted by mG1 including dummy variable for paid.m to investigate a differential effect by paid.m

model2<-lm(df$mG2~df$mG1+df$paid.m)
anova(model2)

summary(model2)

stargazer(model2, type="text") #Tidy output of all the required stats

lm.beta(model2)


stargazer(model1, model2, type="text") #Quick model comparison

#Multiple Linear Regression 2

model3<-lm(df$mG2~df$mG1+df$paid.m+df$internet)
anova(model3)

summary(model3)

stargazer(model3, type="text") #Tidy output of all the required stats

lm.beta(model3)


stargazer(model2, model3, type="text") #Quick model comparison


#####################LOGISTIC REGRESSION#########################

library(Epi)#ROC Curve
library(DescTools)#Pseudo Rsquare statistics
library(stargazer)
library(foreign)#read SPSS file.
library(arm)#for invlogit calculating predicted probabilities
library(lmtest)#Simple calculation of Chi-square for model
library(car)#Needed to test for colinearity of predictors
library(generalhoslem)#Needed to test assumption of linearity
library("regclass")#For confusion matrix

df$Fjob = as.factor(df$Fjob)
df$Mjob = as.factor(df$Mjob)
df$famsup.m = as.factor(df$famsup.m)

df$paid.m = as.factor(df$paid.m)
df$famsup.m = as.factor(df$famsup.m)
df$schoolsup.m = as.factor(df$schoolsup.m)


logmodel1 <- glm(paid.m ~ famsup.m+Fjob, data = df, na.action = na.exclude, family = binomial(link=logit))

#Full summary of the model
summary(logmodel1)

lmtest::lrtest(logmodel1)
modelChi <- logmodel1$null.deviance - logmodel1$deviance
modelChi
pseudo.R2 <- modelChi / logmodel1$null.deviance
pseudo.R2
chidf <- logmodel1$df.null - logmodel1$df.residual
chidf
Epi::ROC(form=df$paid.m ~ df$Fjob+df$famsup.m, plot="ROC")

################

logmodel2 <- glm(paid.m ~ famsup.m+Fjob+Mjob, data = df, na.action = na.exclude, family = binomial(link=logit))

#Full summary of the model
summary(logmodel2)

lmtest::lrtest(logmodel1)

DescTools::PseudoR2(logmodel2, which="CoxSnell")
DescTools::PseudoR2(logmodel2, which="Nagelkerke")

Epi::ROC(form=df$paid.m ~ df$Fjob+df$Mjob+df$famsup.m, plot="ROC")

stargazer(logmodel2, type="text")

exp(coefficients(logmodel2))

## odds ratios 
cbind(Estimate=round(coef(logmodel2),4),
      OR=round(exp(coef(logmodel2)),4))

regclass::confusion_matrix(logmodel2)

#Check the assumption of linearity of independent variables and log odds using a Hosmer-Lemeshow test, if this is not statistically significant we are ok

generalhoslem::logitgof(df$paid.m, fitted(logmodel2))

#Collinearity
vifmodel<-car::vif(logmodel2)#You can ignore the warning messages, GVIF^(1/(2*Df)) is the value of interest
vifmodel

1/vifmodel


################## DIMENSION REDUCTION ####################

needed_packages <- c("psych",  "REdaS", "Hmisc", "corrplot", "ggcorrplot", "factoextra",  "nFactors")                      
# Extract not installed packages
not_installed <- needed_packages[!(needed_packages %in% installed.packages()[ , "Package"])]    
# Install not installed packages
if(length(not_installed)) install.packages(not_installed, repos = "http://cran.us.r-project.org") 
library(psych)

library(REdaS)
library(Hmisc)
library(corrplot)
library(ggcorrplot)
library(factoextra)#Used for principal component analysis to get a different view of eigenvalues
library(nFactors)

df1 = df%>%
  select(c(15:17,24:33,35:37,44:53))

#create a correlation matrix (these are just some methods)
dfMatrix<-cor(df1)
round(dfMatrix, 2)

Hmisc::rcorr(as.matrix(df1))


#Using ggcorrplot. Note these are examples you need to choose a style for yourself, you do not need to create multiple correlation matrices
p.mat <- ggcorrplot::cor_pmat(df1)
ggcorrplot::ggcorrplot(dfMatrix, title = "Correlation matrix for df1")


#Showing Xs for non-significant correlations
ggcorrplot::ggcorrplot(dfMatrix, title = "Correlation matrix for df1", p.mat = p.mat, sig.level = .05)

#Showing lower diagonal
ggcorrplot::ggcorrplot(dfMatrix, title = "Correlation matrix for df1", p.mat = p.mat, sig.level = .05, type="lower")

#Overlay plot with a white grid to space things out.
#t1.cex is the text size, pch is controlling what is shown for non-significant correlations
ggcorrplot(dfMatrix, sig.level=0.05, lab_size = 4.5, p.mat = NULL,
           insig = c("pch", "blank"), pch = 1, pch.col = "black", pch.cex =1,
           tl.cex = 10) +
  theme(axis.text.x = element_text(margin=ggplot2::margin(-2,0,0,0)),
        axis.text.y = element_text(margin=ggplot2::margin(0,-2,0,0)),
        panel.grid.minor = element_line(size=10)) + 
  geom_tile(fill="white") +
  geom_tile(height=0.8, width=0.8)

#Showing the co-coefficients (this will be messy given the number of variables)
ggcorrplot::ggcorrplot(dfMatrix, lab=TRUE, title = "Correlation matrix for df1",  type="lower")

#Visualization of correlations using circles
#corrplot parameters method = c("circle", "square", "ellipse", "number", "shade",
#"color", "pie")
#type = c("full", "lower", "upper"),
corrplot::corrplot(dfMatrix, method="circle")

corrplot::corrplot(dfMatrix, method="circle", type="upper")

#Visualization using numbers
corrplot::corrplot(dfMatrix, method="number")

#Visualization of significance levels at 0.05
res1 <- corrplot::cor.mtest(dfMatrix, conf.level = .95)
corrplot::corrplot(dfMatrix, p.mat = res1$p, type="lower", sig.level = .05)

#Showing p-value for non-significant results
corrplot(dfMatrix, p.mat = res1$p, type="lower",insig = "p-value")

psych::cortest.bartlett(df1)

psych::cortest.bartlett(dfMatrix, n=nrow(df1))

#KMO (execute one of these):
REdaS::KMOS(df1)

psych::KMO(df1)

#Determinant (execute one of these):
det(dfMatrix)

det(cor(df1))

#pcModel<-principal(dataframe/R-matrix, nfactors = number of factors, rotate = "method of rotation", scores = TRUE)

#On raw data using principal components analysis
#For PCA we know how many factors if is possible to find
#principal will work out our loadings of each variable onto each component, the proportion each component explained and the cumulative proportion of variance explained 
pc1 <-  principal(df1, nfactors = 23, rotate = "none")
pc1 <-  principal(df1, nfactors = length(df1), rotate = "none")
pc1#output all details of the PCA

#Create the scree plot
plot(pc1$values, type = "b") 

pc1$Vaccounted 

pc1$values

pcf=princomp(df1)
factoextra::get_eigenvalue(pcf)

factoextra::fviz_eig(pcf, addlabels = TRUE, ylim = c(0, 50))#Visualize the Eigenvalues

factoextra::fviz_pca_var(pcf, col.var = "black")

factoextra::fviz_pca_var(pcf, col.var = "cos2",
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                         repel = TRUE # Avoid text overlapping
)

#Print the loadings above the level of 0.3
psych::print.psych(pc1, cut = 0.3, sort = TRUE)

#create a diagram showing the components and how the manifest variables load
fa.diagram(pc1) 

#Show the loadings of variables on to components
fa.sort(pc1$loading)

#Output the communalities of variables across components (will be one for PCA since all the variance is used)
pc1$communality 

#Visualize contribution of variables to each component
var <- factoextra::get_pca_var(pcf)
corrplot::corrplot(var$contrib, is.corr=FALSE) 

# Contributions of variables to PC1
factoextra::fviz_contrib(pcf, choice = "var", axes = 1, top = 10)

# Contributions of variables to PC2
factoextra::fviz_contrib(pcf, choice = "var", axes = 2, top = 10)

#Apply rotation to try to refine the component structure
pc2 <-  principal(df1, nfactors = 4, rotate = "varimax")#Extracting 4 factors
#output the components
psych::print.psych(pc2, cut = 0.3, sort = TRUE)

#output the communalities
pc2$communality

#Factor Analysis - the default here is principal axis factoring fm=pa
#If we know our data going in is normally distributed we use maximum likelihood
facsol <- psych::fa(dfMatrix, nfactors=4, obs=NA, n.iter=1, rotate="varimax", fm="pa")

#Create your scree plot
plot(facsol$values, type = "b") #scree plot

#Print the Variance accounted for by each factor/component
facsol$Vaccounted

#Output the Eigenvalues
facsol$values 

#Print the components with loadings
psych::print.psych(facsol,cut=0.3, sort=TRUE)

#Print sorted list of loadings
fa.sort(facsol$loading)

#create a diagram showing the factors and how the manifest variables load
fa.diagram(facsol)

#Apply rotation to try to refine the component structure
facsolrot <-  principal(dfMatrix, rotate = "varimax")
#output the components
psych::print.psych(facsolrot, cut = 0.3, sort = TRUE)

#output the communalities
facsolrot$communality

#If you know that variables are grouped, test each group as a separate scale
A<-df1[,c(11:13,3,16,24:26)]
B <- df1[, c(7,8,20,21,2,15)]
C <- df1[, c(4,5,6,17,18,19)]
D <- df1[, c(9,22)]

#Output our Cronbach Alpha values
psych::alpha(A)

#If some items are to be reversed keyed, then either recode or get alpha to reverse code as needed by setting check.keys=TRUE (be careful with this - make sure you know it makes sense)
psych::alpha(B, check.keys=TRUE)

psych::alpha(C)
psych::alpha(D)

############## END OF SCRIPT #####################