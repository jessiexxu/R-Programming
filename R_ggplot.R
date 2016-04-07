# Scatter plots ordered and colored by correlation
rev_plot = ggplot(data = data_plot, aes(data_plot$`Revenue (Billions)`,reorder(data_plot$`Company Name`,data_plot$`Revenue (Billions)`))) + 
  geom_point(size = 3, colour = "#0072B2") +  xlab("Revenue ($ Billions)") + ylab("Company Name")

# Bivariate scatter plot
plot = ggplot(data = data_2, aes(data_2$`EPS Change 10 years`, data_2$`Total ROI 10 years annualized`)) + 
  geom_point(size = 3, colour = "#0072B2") +  xlab("EPS Change 10 Years") + ylab("Total ROI 10 Years")
plot
