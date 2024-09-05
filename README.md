# DFA
#Recognizing the species according to its egg size
#DFA#DFA

 library(MASS)

# Load the data

data <- read.csv("C:/Users/name/EggSize/EggSizeData.csv")

 
data<-data %>% filter(!is.na(LENGTH), !is.na(WIDTH) )
 

# Convert necessary columns to numeric

data$LENGTH <- as.numeric(data$LENGTH)

data$WIDTH <- as.numeric(data$WIDTH)

data$MASS <- as.numeric(data$MASS)

data$YEAR<-as.factor(data$YEAR)

data$Species<-as.factor(data$Species)

print(levels(as.factor(data$Species)))

data$loc<-as.factor(data$loc)

print(levels(data$loc))

 
data$EGGNUM<-as.factor(data$EGGNUM)

print(levels(as.factor(data$EGGNUM)))

  

# Filter out unrealistic values (assume realistic ranges: Length 50-100 mm, Width 30-70 mm)

data <- data %>%

  filter(LENGTH >= 50 & LENGTH <= 100, WIDTH >= 30 & WIDTH <= 70)

 
# Combine all species as CANADA except for Cackling

data$Species <- factor(ifelse(data$Species %in% c("B.h.hutchinsii", "B.hutchinsii", "B.hutchinsii.minima"),

                              as.character(data$Species), "Canada Goose"))

data$Species <- factor(ifelse(data$Species %in% c("Canada Goose"),

                              as.character(data$Species), "Cackling Goose"))

 

# Print the levels of the Species column after combining

print(levels(data$Species))

 
# Split the data into training and test sets

set.seed(123) # for reproducibility

train_indices <- sample(1:nrow(data), size = 0.8 * nrow(data))

train_data <- data[train_indices, ]

test_data <- data[-train_indices, ]

 
# Perform Linear Discriminant Analysis (LDA)

lda_model <- lda(Species ~ LENGTH + WIDTH, data = train_data)

 

# Print the model summary

print(lda_model)

 
# Get the coefficients and intercept from the fitted model

coefficients <- lda_model$scaling

intercept <- lda_model$prior %*% lda_model$means %*% coefficients

 
# Print the coefficients

print("Coefficients of linear discriminants:")

print(coefficients)

 
# Print the intercept

print("Intercept:")

print(intercept)
 

# Display the means of each feature for each class

print("Class means:")

print(lda_model$means)

 

# Predict the species for the test set

predictions <- predict(lda_model, newdata = test_data)

 

# View the predicted classes

predictions$class

 

# Create a confusion matrix to evaluate the classification accuracy

confusion_matrix <- table(Predicted = predictions$class, Actual = test_data$Species)

confusion_matrix

 

# Calculate the classification accuracy

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

accuracy

 

#################

 

# Example data for classification

example_data <- data.frame(LENGTH = 75, WIDTH = 50)

 
# Calculate the discriminant score for the example data

discriminant_score <- as.matrix(example_data) %*% coefficients - intercept

print("Discriminant Score:")

print(discriminant_score)

 
# Classification rule

if (discriminant_score > 0) {

  print("Class: Canada Goose")

} else {

  print("Class: Cackling Goose")

}

 
###########################################################

#Create a data frame with the LDA predictions

test_data$Predicted <- predictions$class

 

# Plot the actual species

ggplot(test_data, aes(x = LENGTH, y = WIDTH, color = Species)) +

  geom_point() +

  ggtitle("Actual Species") +

  theme_minimal()

 

# Plot the predicted species

ggplot(test_data, aes(x = LENGTH, y = WIDTH, color = Predicted)) +

  geom_point() +

  ggtitle("Predicted Species") +

  theme_minimal()

 

# Corrected function to plot LDA decision boundaries

plot_lda_decision_boundary <- function(model, data, x, y) {

  grid <- expand.grid(

    setNames(list(seq(min(data[[x]], na.rm = TRUE) - 1, max(data[[x]], na.rm = TRUE) + 1, length.out = 100),

                  seq(min(data[[y]], na.rm = TRUE) - 1, max(data[[y]], na.rm = TRUE) + 1, length.out = 100)),

             c(x, y))

  )

 

  grid$Species <- predict(model, newdata = grid)$class

  ggplot(data, aes(x = !!sym(x), y = !!sym(y), color = Species)) +

    geom_point(alpha = 0.5) +

    geom_point(data = grid, aes(x = !!sym(x), y = !!sym(y), color = Species), alpha = 0.1, shape = 15, size = 0.1) +

    ggtitle("LDA Decision Boundaries") +

    theme_minimal()

}

 

# Plot the LDA decision boundaries

plot_lda_decision_boundary(lda_model, test_data, "LENGTH", "WIDTH")+theme(

  axis.text.x = element_text(angle = 45, hjust = 1, size = 14),

  axis.text.y = element_text(size = 16),

  axis.title.x = element_text(size = 16),

  axis.title.y = element_text(size = 16),

  plot.title = element_text(size = 20, face = "bold"),

  legend.text = element_text(size = 16),

  legend.title = element_text(size = 18)

)

######################################################################

###Making map

 # Load necessary libraries

library(leaflet)

library(dplyr)

library(htmlwidgets)

library(webshot)

 
# Install PhantomJS if not already installed

if (!webshot::is_phantomjs_installed()) {

  webshot::install_phantomjs()

}

 
# Check and print current working directory

print(paste("Current working directory:", getwd()))


# Set the working directory to a specific path (optional)

setwd("C:/Users/SB/EggSize")

print(paste("New working directory:", getwd()))

 
# Load the dataset

data <- read.csv("C:/SB/EggSize/EggSizeData.csv")


# Filter out NA values and convert necessary columns to numeric

data <- data %>%

  filter(!is.na(LENGTH), !is.na(WIDTH)) %>%

  mutate(

    LENGTH = as.numeric(LENGTH),

    WIDTH = as.numeric(WIDTH),

    MASS = as.numeric(MASS),

    YEAR = as.factor(YEAR),

    Species = as.factor(Species),

    loc = as.factor(loc),

    EGGNUM = as.factor(EGGNUM),

    lat = as.numeric(as.character(lat)),

    long = as.numeric(as.character(long))

  )


# Print levels to check for consistency

print(levels(data$Species))

print(levels(data$loc))

print(levels(data$EGGNUM))

 

# Filter out unrealistic values (assume realistic ranges: Length 50-100 mm, Width 30-70 mm)

data <- data %>%

  filter(LENGTH >= 50 & LENGTH <= 100, WIDTH >= 30 & WIDTH <= 70)

 

# Combine all species as CANADA except for Cackling

data$Species <- factor(ifelse(data$Species %in% c("B.h.hutchinsii", "B.hutchinsii", "B.hutchinsii.minima"),

                              as.character(data$Species), "Canada Goose"))

data$Species <- factor(ifelse(data$Species %in% c("Canada Goose"),

                              as.character(data$Species), "Cackling Goose"))

 

# Define custom color palette based on location

location_colors <- c(

  " Mackenzie Delta " = "#1f77b4", "Alaska" = "#D55E00", "East Bay " = "#7f7f7f",

  "McConnell River MBS, NU" = "#FF80FF", "North Dakota " = "#9467bd", "UNGAVA" = "#8c564b",

  "Adventure" = "#000000", "Burntpoint" = "#9edae5", "HUDSON" = "#e7298a",

  "Missouri" = "#CBDAF1", "research camp" = "#09622A", "Winnipeg" = "#bcbd22",

  "Akimiski" = "#FFFF00", "CAMP" = "#FF0033", "Karrak Lake" = "#00FFFF",

  "Newfoundland" = "#F4792D", "Simpson" = "#00FF00"

)

 
# Create a color palette function

pal <- colorFactor(palette = location_colors, domain = data$loc)

 
# Count the number of individuals at each location

location_counts <- data %>%

  group_by(lat, long, loc, Species) %>%

  summarise(count = n())

 

# Create the map

map <- leaflet(data = location_counts) %>%

  addTiles() %>%

  addCircleMarkers(

    ~long, ~lat, color = ~pal(loc), radius = ~sqrt(count) * 0.2,  # Adjusted the scaling factor here

    popup = ~paste("Location:", loc, "<br>",

                   "Count:", count, "<br>",

                   "Lat:", lat, "<br>",

                   "Long:", long)

  ) %>%

  addLabelOnlyMarkers(

    ~long, ~lat,

   # label = ~Species,

    #labelOptions = labelOptions(

    #  noHide = TRUE,

     # direction = 'auto',

     # textOnly = TRUE

     #style = list(

     #  "color" = "red",

      #  "font-weight" = "bold",

     #  "font-size" = "10px"))  # You can adjust the size as needed

     

) %>%

  addLegend("bottomright", pal = pal, values = ~loc, title = "Location", opacity = 1)

 

 

##$$$$

map <- leaflet(data = data) %>%

  addTiles() %>%

  addCircleMarkers(

    ~long, ~lat, color = ~pal(loc), radius = 5, popup = ~paste("Species:", Species, "<br>",

                                                               "Location:", loc, "<br>",

                                                               "Lat:", lat, "<br>",

                                                               "Long:", long) ) %>%

  addLegend("bottomright", pal = pal, values = ~loc, title = "Location", opacity = 1)

####$$$

 

 

# Save the map as an HTML file

html_file1 <- "my_map.html"

saveWidget(map, html_file1, selfcontained = TRUE)

 

# Check if HTML file was created

if (file.exists(html_file1)) {

  print(paste("HTML file saved as:", html_file1))

} else {

  stop("HTML file was not created.")

}

 

# Convert the HTML file to a JPEG image

jpeg_file <- "my_map1.jpeg"

webshot(html_file1, file = jpeg_file, vwidth = 1200, vheight = 800, cliprect = "viewport")

 

# Check if JPEG file was created

if (file.exists(jpeg_file)) {

  print(paste("JPEG file saved as:", jpeg_file))

} else {

  stop("JPEG file was not created.")

}

 

#######################################################
