library(shiny)
library(e1071)

shinyServer(function(input, output) {
	mttrain<-mtcars[-c(2,12,18,25),]
	mttest<-mtcars[c(2,12,18,25),]
	withnames<-cbind(mtcars,rownames(mtcars))
	output$full<-renderTable(withnames)
	output$howto<-renderPrint("In the left pane, select the prediction model type (support vector machine or linear model) you want to use and the predictors you want to include. If you select svm and no predictors, an error will appear. Just select a predictor again and the plot will reappear. For information about what the predictors are, see the Help tab. The plot allows you to get a visual indication how accurate the prediction model is. The most interesting part is how good the predictions are for values that were not used to build the model.")
	output$help<-renderPrint("Motor Trend Car Road Tests // The data was extracted from the 1974 Motor Trend US magazine, and comprises fuel consumption and 10 aspects of automobile design and performance for 32 automobiles (1973–74 models). It is a data frame with 32 observations on 11 (numeric) variables. // [, 1] mpg Miles/(US) gallon // [, 2] cyl  Number of cylinders // [, 3] disp Displacement (cu.in.) // [, 4] hp Gross horsepower // [, 5] drat Rear axle ratio // [, 6] wt Weight (1000 lbs) // [, 7] qsec 1/4 mile time // [, 8] vs Engine (0 = V-shaped, 1 = straight) // [, 9] am Transmission (0 = automatic, 1 = manual) // [,10] gear Number of forward gears // [,11] carb Number of carburetors // Source: Henderson and Velleman (1981), Building multiple regression models interactively. Biometrics, 37, 391–411.")
	output$credits<-renderPrint("Agelink, June 2018")
	mdl<- reactive({
		input$choosemodel
		})
	mtrain<-reactive({
		goodcols<-which(colnames(mtcars) %in% input$predictors)
		mttrain[,c(goodcols,6,7)]
		})
	mtest<-reactive({
		goodcols<-which(colnames(mtcars) %in% input$predictors)
		mttest[,c(goodcols,6,7)]
	})
	output$shownplot <- renderPlot({
		if (mdl()=="lm") {
			wtmodel<-lm(wt~.-qsec,data=mtrain())
			qsmodel<-lm(qsec~.-wt,data=mtrain())
			}
		if (mdl()=="svm") {
			wtmodel<-svm(wt~.-qsec,data=mtrain())
			qsmodel<-svm(qsec~.-wt,data=mtrain())
			}
		wtpreds<-predict(wtmodel)
		qspreds<-predict(qsmodel)
		wtnew<-predict(wtmodel,newdata=mtest())
		qsnew<-predict(qsmodel,newdata=mtest())
		plot(mttrain$wt,mttrain$qsec,col="green",pch=15,
		     xlim=c(1,6),
		     ylim=c(13,23),
		     xlab="Weight in 1000s of lbs",
		     ylab="Seconds to go from standstill to 1/4 mile driven")
		points(mttest$wt,mttest$qsec,col="brown",pch=15)
		points(wtpreds,qspreds,col="blue",pch=2)
		for (i in 1:nrow(mttrain)) lines(x=c(mttrain[i,6],wtpreds[i]),
						 y=c(mttrain[i,7],qspreds[i]),
						 col="red",lwd=2)
		points(wtnew,qsnew,col="purple",pch=17)
		for (i in 1:nrow(mttest)) lines(x=c(mttest[i,6],wtnew[i]),
						y=c(mttest[i,7],qsnew[i]),
						col="red",lwd=2)
		legend("bottomright",
		       legend=c("real car used for model",
		       	 	"real car unseen by model",
		       	 	"prediction for car",
		       	 	"prediction for unseen car"),
		       col=c("green","brown","blue","purple"),
		       pch=c(15,15,2,17))
		legend("topright",
		       legend="difference prediction-actual",
		       col="red",
		       lty=1,lwd=2)
	})
})