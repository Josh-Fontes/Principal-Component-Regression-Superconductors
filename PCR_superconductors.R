
setwd('C:\\Users\\fnts0\\Documents\\Josh\\SJSU\\math 253\\Project')

library('ggplot2')
library('plotly')


# load in the data
data_super = read.table('train.csv', header = TRUE, sep = ',')
data_elements = read.table('unique_m.csv', header = TRUE, sep = ',')



# Oxygen
Oxy = data_elements['O'] > 0 
Oxy = as.numeric(Oxy)
Oxy = factor(Oxy, labels = c('Without','With'))



##########    PCA        ##############################




# design matrix
X = data_super[,1:81]



# PCA
sc_pcr = prcomp(X, scale = T)
PCscores = sc_pcr$x




# combing PC scores with factorized variable 'Oxygen'
pca_data = data.frame(PCscores, Oxygen = Oxy)

# PC score plot of the first two principal components, grouped by Oxygen
ggplot(pca_data, aes(x = PC1, y = PC2, color = Oxygen)) + geom_point() + labs(title = "Principal Components by Oxygen")




# 3D PC score plot of the first three principal components, grouped by Oxygen
fig <- plot_ly(x = PCscores[,1], y = PCscores[,2],
               z = PCscores[,3], color = Oxy, 
               colors = c('#BF382A', '#0C4B8E'),
               marker = list(symbol = 'circle', 
                             opacity = 0.5))

fig <- fig %>% add_markers()

fig <- fig %>% layout(title = 'Principal Components for Oxygen',
                      scene = list(xaxis = list(title = 'PC 1'),
                                   yaxis = list(title = 'PC 2'), 
                                   zaxis = list(title = 'PC 3')))

fig



