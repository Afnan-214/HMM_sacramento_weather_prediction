# Stochastic process: Sacramento weather prediction
*A team project to predict the weather in Sacramento,California based on the number of peoples in the parks using hidden markov chain **(From scratch)***
## Project Description
This hidden markov chain model is to detect the weather based on the mobility percentage in parks in Sacramento, California during Spring (Starting from March to May) 2020. <br>
The data was collected manaully using the Community Mobility Reports by Google During COVID-19 which provide movement trends over time by geography.And NOAA climate data to get the average daily temperature and Precipitation by month. <br>
By combining these datasets and categories it the hidden states will be defined as **“Rainy, Clear and Warm”** and the observations as **“Low,Medium and High mobility”.**
## Project Phases
1. Data Preparation
   * Data collection: Add the climate data features “Avg_Temp and Precipitation” to the filtered mobility dataset of sacramento.
   * Data transformation:
      - Using the Avg_Temp and Precipitation to create the hidden states “Rainy, Clear and Warm” Based on Approximate value visualisation.
      - convert the mobility percentage to 3 categorical observations. “Low,Medium and High mobility ”
2. Initial HMM: Initialize the Transition Matrix (Hidden-States), Emission Matrix (Observable-Events), Initial Distribution in the data
3. Evaluation: Forward and Backward Algorithm
4. Decoding: Viterbi Algorithm
5. Learning: Baum-welch Algorithm
   
   > *Which was my part to implement, first I analyse the algorithm mathematically then implement it using matrices in R, Here's the Beum-welch explaination*<br>
   [Baum-welch Algorithm](Beum-welch.pdf)
