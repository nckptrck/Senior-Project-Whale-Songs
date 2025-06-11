# Automated Gray Whale Vocalization Detection Using Machine Learning
### California Polytechnic State Univeristy, 2024-2025 Data Science Capstone Project

#### Authors

🐋 Alexander Jung, Nick Patrick, Kyan Wong, Raymond Yang 🐋 

### Project Overview
This project develops machine learning models to automatically detect and classify gray whale vocalizations from passive acoustic monitoring data collected off California's Central Coast. Using hydrophone recordings from Avila Bay, we apply signal processing techniques and machine learning to identify whale calls in audio data.

### Dataset
- **Source**: SoundTrap600 Recorder deployed 3-4 miles offshore of Avila Bay
- **Collection Period**: February 2024
- **Data Volume**: 707 thirty-minute WAV files
- **Annotated Files**: 38 files with manual annotations for training/validation
- **Frequency Range**: Filtered to 0-600 Hz (optimal for gray whale calls)

### Technical Approach

#### Audio Transformations
We implemented three different feature extraction methods:

1. **Fast Fourier Transform (FFT)**
  - 24 features representing frequency bands (0-600 Hz, 25 Hz intervals)
  - Time resolution: 0.1 second intervals

2. **Short-Time Fourier Transform (STFT)**
  - 51 features with 5.6 Hz frequency resolution
  - Frequency range: 0-300 Hz
  - Hann windowing for continuity

3. **Mel-Frequency Cepstral Coefficients (MFCC)**
  - 13 coefficients optimized for acoustic pattern recognition
  - 2-second windows with 1-second overlap
  - Mimics human auditory perception

#### Machine Learning Models
- **Random Forest**: Optimized for precision to minimize false positives
- **K-Nearest Neighbors (KNN)**: Tuned for pattern recognition
- **XGBoost**: Gradient boosting for complex pattern detection
- **Ensemble Methods**: Combining predictions for improved accuracy

### Performance Results
Our best-performing model (MFCC + Random Forest) achieved:
- **Precision**: 97% (wide predictions)
- **Recall**: 95%+ whale call detection
- **F1-Score**: Balanced performance across validation set

### Quick Start for Apps
Example App Script.R

```r
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
```

### Acknowledgments
- Cal Poly Professors
  - Maddie Schroth
  - Dr. Kelly Bodwin
  - Dr. Alex Dekhtyar
- Cal Poly Marine Science Students
- FROST Summer Research Team