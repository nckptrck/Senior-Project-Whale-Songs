# 1. Load the package
install.packages("remotes")
library(remotes)
remotes::install_github("kbodwin/SLOWhale")
library(SLOWhale)

# 2. Transform audio file app
run_transform_app()

# 3. Make predictions app
run_prediction_app()

# 4. Check the predictions txt in raven