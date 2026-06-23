\# Port Commercial Fishing Activity Indicators 



\## Product Overview:



\*\* The Port Commercial Fishing Activity (PCFA) Indicators are a suites of indicators used to understand relative participation rates of commercial fishing ports over time. They are composed of commercial fishing landings and permit data from 2007 through 2024 summarized to indicate port-level activity in commercial fishing.\*



\* Unit: port-level information

\* Summary Group(s): port, year

\* Frequency: annual

\* Time Series: 2000-2024







\### Point of Contact



Robert Murphy (robert.murphy@noaa.gov)





\### Data Outputs/Outlets



N/A





\## List of Metrics



\*Port Overall Activity score (*port\_overall\_score*)

\*Port Transaction Activity score (*port\_transaction\_score*)

\*Port Volume Activity score (*port\_volume\_score*)

\*Port Permit Activity score(*port\_permit\_score*)





\## Metric Descriptions



| PORT\_NAME | character | port associated with permit or landing on record | N/A |



| STATE\_ABB | character | state associated with permit or landing on record | N/A |



| place\_id | character | a combined text string with port and state associated with the permit or landing on record | N/A |



| year | character | year (2000-2024) associated with the record | N/A |



| port\_overall\_score | numeric | The Port Overall Activity score is a relative metric that is a reflection of the overall activity of a port in the commercial fishing industry including the number of dealers buying fish, the number of vessels selling fish, total pounds and value of fish landed, and the number of dealer and commercial permits registered in that location. Each of these variables is normalized using a min-max scaling approach (between 0 and 1) and an overall mean is calculated for each port and year combination. | NA



| port\_transaction\_score| numeric | The Port Transaction Activity score is a relative metric that reflects the magnitude of fish sale transactions in each port from the perspective of dealers and commercial fishermen. | NA



| port\_volume\_score| numeric | The Port Volume Activity score is a relative metric that reflects the magnitude of fish sale transactions in each port from the perspective of dealers and commercial fishermen. | NA



| port\_permit\_score| numeric | The Port Permit Activity score is a relative metric reflects the number of dealer permits and commercial fishing permits that are registered in each port. | NA







\## Additional Methods/Decision Rules



\### The PCFA indicator provides a relative metric of the activity of a specific port in commercial fishing and utilizes the following data; number of dealers buying fish, the number of vessels selling fish, total pounds and value of fish landed, and the number of dealer and commercial permits registered in that location. Each column of data is normalized (across all years) using a min-max scaling approach (range = 0 – 1) and then we take the mean of each column to create a composite average score for each port by year. Note, the value of landed fish data is first adjusted using a GDP deflator to account for inflation over time (i.e., all values are adjusted to 2024 dollars). The PCFA analysis is conducted for each region (MA and NE) separately such that scores are comparable within-region only





\## Data Sources and Code



\### Data Sources

\*\*CAMS, 



\### Code

Metric Creation Code File Location: https://github.com/NEFSC/READ\_SSB\_Port\_Activity\_Indicators





# NOAA Requirements

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.”





# License file

See here for the [license file](License.txt)

