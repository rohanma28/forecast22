# Rohan Athreya's 2022 United States Senate Election Forecast

The code in polladj.R was used to create the tools that adjusted the 2022 Senate election polls with pollster bias and precision.

raw_polls.csv includes polls from FiveThirtyEight's publically available polling database of polls released 3 weeks prior to a major election, including how biased the poll was relative to the general election result, of which only the subset of polls that includes senatorial and gubernatorial elections are used. 

correlation.csv is a file that includes the state level swings from the last 5 intervals between presidential elections as of 2022, and it is used to find the correlation in state level behavior to inform polling averages for underpolled states.

Using the bias from the raw polls and the correlation between states, I generated a more fine tuned measure of state-by-state bias for each pollster, found in bias.csv. I also found the bias for general ballot (nationwide Congressional ballot) pollsters, found in genbalbias.csv.

If the biases between the polls of each pollster have a wide spread, as measured by its standard deviation, polls from that pollster will be weighted less in the polling average. Weights are in the weights.csv for state level pollsters and the gbweights.csv file for generic ballot pollsters.

The code in model.R was used to generate the model using the polls and other fundamental factors.

polls.csv includes all 2022 US Senate and 2022 generic ballot polls that were uploaded to the FiveThirtyEight polling database.

Polls were given more weight if they were more recent, if they had a larger sample size, and the population surveyed (i.e. surveys of likely voters were given more  weight than surveys of all adults) in addition to the weights generated from past pollster precision.

I then applied the pollster bias and the pollster weights to both the state level and generic ballot polls to generate fine tuned polling averages. I exported the generic ballot average to genbal.csv

Using a Johnson distribution and the standard deviation of the state-by-state polls, I created 100,000 simulations for the outcome of the Senate elections. I then adjusted this using fundamental data, namely the tendency for senate races to mirror the partisan lean of the state relative to the national environment (using partisanlean.csv, the generic ballot data, and the generic ballot polling standard deviation), the tendency for incumbents to overperform (overperf.csv), and state-by-state correlation as generated previously.

The state-by-state probabilities were recorded in probs.csv, the whole senate probability was recorded in wholesenprob.csv, and the data to construct a histogram of the whole senate probability was recorded in hist.csv.

The code in tracker.R used genbal.csv to construct a daily tracker of the generic ballot polls, which can be found in tracker.csv, as well as the data to construct a line graph of the Democratic and Republican generic ballot averages over time.

app.R was used to construct the web app to display the data, including, the histogram, generic ballot line graph, and an interactive map that uses a shapefile of the Senate seats being contested such that when a state is being clicked on, the polling average, probabilities, and rating are displayed.