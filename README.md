Forecasting Flight Delays
  
  Forecasting flight delays is a valuable tool in managing the impact of delays on airline operations and passenger experiences. 
  By considering key factors, leveraging data analysis, weather forecasting, and predictive models, and collaborating among industry stakeholders,
  more accurate and timely delay forecasts can help airlines, airports, and passengers make informed decisions and mitigate the effects of flight delays.

Data Source
- XieCheng database of Chinese domestic flight
- 1,048,574 different internal flights in China and the possible cause for delays
- https://github.com/udacity/CN-Flight-Delay-Forecast/blob/master/FlightDelayForecast.ipynb

Data Cleaning Process
- Drop missing values
- Remove outliers
- Remove duplicates, etc.
- Extraction and classification of the text data

Feature Importance (Selection of Features)
- Geographic Weighted Regression
- By incorporating location-specific factors, enhance the accuracy of our predictions and gain insights into spatial variations.

Prediction and Evaluation of Model Accuracy
- Logistic Regression
- Random Forest
- XGBoost
- Support Vector Machine

Limitations
- Defining delays solely based on hours can be subjective, impacting result accuracy
- Additionally, text data cleaning may have resulted in information loss
- Utilizing LOCV instead of k-folds for resampling may improve cross-validation and data balance
- Lastly, our project did not extensively consider interaction effects between variables

Conclusion

our project emphasized the application of machine learning in predicting flight delays. Through the utilization of diverse models and AUC evaluation, we gained valuable insights into their effectiveness. Our findings highlight the potential of machine learning algorithms to enhance accuracy and provide valuable information for airlines, passengers, and stakeholders. The incorporation of machine learning in flight delay prediction holds great promise for enhancing travel experiences and operational efficiency in the aviation industry.



