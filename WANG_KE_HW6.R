# Question 2
# Create a simple scatter plot of Weight ("Carat") and Price using Color (the "Color" column in the diamonds dataframe) as a facet.
ggplot(diamonds, aes(carat, price, color=factor(color)))  + # Create the plot of dataframe diamonds, with axes Carat and Price, and Color as a facet.
  geom_point() + # Create a scatter plot
  ggtitle("Diamonds - Weight to Price by Color") + # Change the title
  xlab("Weight") + # Change the x-axis label of the plot
  theme(plot.title=element_text(color="blue",size=25), # Change the title of plot to blue color and font size 25.
        legend.title=element_text(face="bold")) # Change the title of the legend to bold letters.

# Question 3
# Create a linear scatter plot of Weight ("Carat") and Price using Color (the "Color" column in the diamonds dataframe) as a facet.
ggplot(diamonds, aes(log(carat), log(price), color=factor(color))) + # Create the plot of dataframe diamonds, with axes the natrual log of Carat and the natrual log of Price, and Color as a facet.
  geom_point() + # Create a scatter plot
  ggtitle("Diamonds - Weight to Price (Linear)") + # Change the title
  xlab("Weight") + # Change the x-axis label of the plot
  ylab("Price") + # Change the y-axis label of the plot
  theme(plot.title=element_text(color="blue",size=25), # Change the title of plot to blue color and font size 25.
        legend.title=element_text(face="bold")) # Change the title of the legend to bold letters.


# Question 4
# Create a linear model, then create a scatter plot using the transformed weight on the x-axis, the residuals on the y-axis, and Color as a facet.
mod <- lm(log(price) ~ log(carat), data = diamonds) # Create a linear regression model of the natrual log of Carat and the natrual log of Price in dataset diamonds. 
diamonds$res <- mod$residuals # Add the column of residuals in the linear regression model to a new column named res in dataset diamonds.
ggplot(diamonds, aes(log(carat), res, color=factor(color))) + # Create the plot of dataframe diamonds, with axes the natrual log of Carat and the residual of the linear regression model, and Color as a facet.
  geom_point() + # Create a scatter plot
  ggtitle("Diamonds - Weight to Price by Color") + # Change the title
  xlab("Weight") + # Change the x-axis label of the plot
  ylab("Price Residuals") + # Change the y-axis label of the plot
  theme(plot.title=element_text(color="blue",size=18), # Change the title of plot to blue color and font size 18.
        legend.position="top") # Change the position of the legend to top.

# Question 5
# Create the following overlay of three plots. The histogram on the bottom left is a density histogram of the price and the histogram on the upper right is a density histogram of carat.
mod <- lm(log(price) ~ log(carat), data = diamonds) # Create a linear regression model of the natrual log of Carat and the natrual log of Price in dataset diamonds. 
diamonds$res <- mod$residuals # Add the column of residuals in the linear regression model to a new column named res in dataset diamonds.
base <- ggplot(diamonds, aes(log(carat), res, color=factor(color))) + # Create the plot of dataframe diamonds, with axes the natrual log of Carat and the residual of the linear regression model, and Color as a facet. Assign it to base.
  geom_point() + # Create a scatter plot
  ggtitle("Diamonds - Weight to Price by Color") + # Change the title
  xlab("Weight") + # Change the x-axis label of the plot
  ylab("Price Residuals") + # Change the y-axis label of the plot
  theme(plot.title=element_text(color="blue",size=25), # Change the title of plot to blue color and font size 25.
        legend.position="top", # Change the position of the legend to top.
        legend.title=element_text(face="bold")) + # Change the title of the legend to bold letters.
  guides(col = guide_legend(nrow = 1)) # Make the legend in 1 line.
base # Create the plot "base".
bl <- ggplot(diamonds, aes(x=price,..density.., fill=color)) + # Create the density histogram of the price
  geom_histogram(binwidth = 110) + # Create the histogram with bin width 110
  theme(axis.title.x=element_blank(), # Remove the x-axis title
        axis.title.y=element_blank(), # Remove the y-axis title
        legend.position="none") # Remove the legend
tr <- ggplot(diamonds, aes(x=carat,..density.., colour=color)) + # Create the density histogram of carat
  geom_histogram(binwidth = .036) + # Create the histogram with bin width 0.036
  theme(axis.title.x=element_blank(), # Remove the x-axis title
        axis.title.y=element_blank(), # Remove the y-axis title
        legend.position="none") # Remove the legend
pos1 <- viewport(width=0.43,height=0.22,x=0.28,y=0.19) # Set the position and size of the histogram we want to overlay onto the "base" plot.
print(bl,vp=pos1) # Create the overlay of the bottom left density histogram
pos2 <- viewport(width=0.43,height=0.22,x=0.8,y=0.76) # Set the position and size of the histogram we want to overlay onto the "base" plot.
print(tr,vp=pos2) # Create the overlay of the top right density histogram
