# Recruitment Methods Efficacy

Code to analyse the efficacy of the different SERL recruitment methods in enhancing response rates. These were tested in a 'pilot study' that took the form of a randomised control trial of a range of methods. The results were used to refine the final SERL recruitment approach.

# Outputs to date

## Article 1

A paper analysing the effects of different recruitment methods on recruitment outcomes using the SERL pilot study data - https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=8666

 * Pre-print: https://osf.io/f82b7/
 * Currently under review at ERSS

### Pre-print abstract

"Obtaining high-resolution energy consumption data from a large, representative sample of homes is critical for research, but low response rates, sample bias and high recruitment costs form substantial barriers. The wide-spread installation of smart meters offers a novel route to access such data, but in countries like Great Britain (GB) consent is required from each household; a real barrier to large-scale sampling. In this paper we show how certain study design choices can impact the response rate and sample bias in energy studies requesting access to half-hourly smart meter data and (optional) survey completion. We used a randomised control trial (RCT) with a 3 x 2 x 2 factorial design to test incentives, message content/structure and a 'push-to-web' approach in a large-scale pilot for the future 10,000+ GB sample. Up to 4 mailings were sent to 18,000 addresses, recruiting 1711 participants (9.5% response rate) from England and Wales. Our results and recommendations can be used to help future energy studies to achieve greater response rates and improved representation. UK-based researchers can apply to use our longitudinal smart meter and contextual datasets."

### Code

pilot_analysis_for_paper_v06.Rmd in the R folder contains the final version of the code used to create plots and statistical analyses used in the paper. 

This code loads a data table 'results' consisting of 18,000 rows (one per address contacted in the pilot study) and 11 variables:
- consent_source ("online", "postal", "online+postal", "NA"); how a participant signed up 
- cell (1:12); the test cell assigned to the address
- quintile (1:5) the Index of Multiple Deprivation (IMD) quintile
- region (7 of the regions in England and Wales)
- survey_status ("Completed", "Partially completed", "NA")
- survey_response_type ("online", "postal", "online + postal", "NA"); how survey response was provided
- consent_start_date(e.g. "2019-09-01"); when a participant signed up
- consent (TRUE or FALSE); whether the household gave consent to take part
- p2w (TRUE or FALSE); whether the address received the push-to-web treatment
- version (1 or 2); content version received by the household
- incentive ("None", "Thermometer", "Voucher")

# Data
Unfortunately the data cannot be made publicly available since some sub-groups of addresses contain fewer than 10 households which is against our statistical disclosure policy.
