install.packages("Boruta")
install.packages("plotly")
install.packages("reshape")
install.packages("robustbase")
library(robustbase)
library(reshape)
library(plotly)
library(Boruta)

#example interaction
# a$intHisp_unmp <- a$hisp * a$unemply
varnames <- c("age", "age_sq", "educ", "educ_sq", "married", "nodegree", "black", "hisp", "re74", "re75", "re74_sq", "re75_sq", "u74", "u75", "u74_hisp", "u74_black") 

varnames <- names(a1)[c(2:9,11:17)]

library(foreign)
lalonde<-read.dta("https://github.com/democratizing-data-science/feature_selection/raw/main/lalonde.dta")

# Implement the algorithm 
# Unless a seed is set, results may vary slightly but these variation do not affect the
# overall performance or conclusions
# For the sake of transparency no seed was set but in case authors preferred to do so
# uncomment the following line
# set.seed(47)
boruta_output <- Boruta(treatment ~ ., data=lalonde[,c("treatment", varnames)], doTrace=2, 
						maxRuns=1000)
boruta_output <- Boruta(BS_stem_attmnt ~ ., data=a1[!is.na(a1$hs_science_reqmnt)&!is.na(a1$gpa_all_courses)&!is.na(a1$hs_math_reqmnt),c("BS_stem_attmnt", varnames)], doTrace=2, 
						maxRuns=1000)
						

# Get significant variables including tentatives
boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif)  
# The previous code reflected that all features were significant

# Variable Importance Scores in case your other data is not, you can try a tentative method see guidance paper
roughFixMod <- TentativeRoughFix(boruta_output)
imps <- attStats(roughFixMod)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
head(imps2[order(-imps2$meanImp), ])  # descending sort

# #Let’s plot it to see the importance of these variable while modifying variable names following reviewers' recommendation
# colnames(boruta_output$ImpHistory) <- c("treatment", "age", "age sq", "educ", "educ sq", "married", "nodegree", "black", "hisp", "re74", "re75", "re74_sq", "re75_sq", "u74", "u75", "u74_hisp", "u74_black" "shadowMax", "shadowMean", "shadowMin") 

# Plot variable importance
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  

# The following lines replicate the interactive HTML output
df<-as.data.frame(t(boruta_output)[2])
head(df)

mns <- colMedians(as.matrix(df), na.rm=TRUE)
df <- df[,order(mns)]
names(df)

df$ID<-rownames(df)
df <- melt(as.data.frame(df), id='ID')
df$type <- ifelse(df$variable %in% print(boruta_signif), "Confirmed", "Rejected")
df$type<-ifelse((df$variable=="shadowMax"|df$variable=="shadowMean"|df$variable=="shadowMin"), "Shadow", df$type)


plot_ly(df, x = ~variable, y = ~value, color = ~type, type = "box", 
		colors= c("#E7298A", "#FF0000", "#E6AB02"),line = list(color = 'rgb(74,74,74, 
				  max=255,255/2)')) %>% 
         layout(xaxis = list(title='Candidate features compared against their shadows'), 
         yaxis = list(title='Variable Importance'),
		 title = "<b>Feature selection, Boruta procedure (random forest classification)</b><br>Outcome: Salary in 1978")
