# -*- coding: utf-8 -*-

"""
The purpose of this Python script is to get profile metadata of the U.K MPs through the Twitter (X) API. 
It requires the Twitter handles of each MP as an input, along with unique Twitter API access credentials. 
It returns the requested profile metadata of each Twitter user handle, stored as a dataframe. 
"""

#### ---- IMPORT PACKAGES ---- ####

import tweepy as tw
import pandas as pd
from datetime import datetime
import json 

#### ---- GLOBAL INPUTS ---- ####

# Specify the user fields you want to gather from each user's profile

USER_FIELDS = ["created_at","description","id","location","name","pinned_tweet_id","profile_image_url",
               "protected","public_metrics","url","username","verified","withheld"]

# Specify the profile metrics you want to unnest from the 'public_metrics' user field

PROFILE_METRICS = ["followers_count","following_count","tweet_count","listed_count"]


#### ---- INPUT TWITTER API ACCESS CREDENTIALS ---- ####

# These credentials are confidential and unique to each user. Apply for a Twitter (X) developer account
# to access the Twitter API. 

client = tw.Client(consumer_key="",
                   consumer_secret="",
                   access_token="",
                   access_token_secret="",
                   bearer_token="")

#### ---- USER PROFILE METADATA SCRAPER ---- ####

""""
Summary: 

This function accesses the Twitter API using the stored client credentials, iterating through a list of 
given Twitter handles to gather the specified user fields and returns them in a dataframe. These dataframes
are added to a single list of all user metadata dataframes and concatenated together at the end of the function. 

Input: 

Function requires two inputs -

client = Twitter API access credentials
username_list = List of Twitter usernames

Returns:
  
profile_metadata_final = Dataframe with complete profile metadata for each Twitter username
errors = Dataframe with any usernames that failed to return profile data
"""

# Function:

def get_profile_metadata(client,username_list):
  
    # Create empty list to store profile data 
    profile_metadata = []
    
    # Create empty dictionary to unnest public metrics
    public_metrics = {"username": []}
    
    # Create empty list to catch any errors
    errors = []
    
    # Populate empty public metric dictionary with specified profile metrics 
    for metric in PROFILE_METRICS:
        public_metrics[metric] = []
        
    # Iterate through list of Twitter usernames to harvest profile data     
    for count,username in enumerate(username_list):    
        user_data = client.get_user(username=username,user_fields=USER_FIELDS)
        if user_data.data != None:
            profile_metadata.append(user_data.data)
            public_metrics["username"].append(username)
            for metric in PROFILE_METRICS:
                public_metrics[metric].append(user_data.data.public_metrics.get(metric))
        else:
            errors.append(username)
        print(f"{count+1}/{len(username_list)} profiles complete!")
    
    # Convert list of profile metadata to single dataframe     
    profile_metadata_df = pd.DataFrame(profile_metadata)
    
    # Convert list of unnested public metrics to dataframe
    public_metrics_df = pd.DataFrame(public_metrics)
    
    # Merge public metrics dataframe to profile metadata 
    profile_metadata_merge = pd.merge(profile_metadata_df,public_metrics_df,how='left',on="username")
    
    # Drop the orginal public metrics column
    profile_metadata_final = profile_metadata_merge.drop(columns=["public_metrics"])    
    
    return profile_metadata_final,errors

#### ---- RUN FUNCTION ---- ####

# Specify date
date = datetime.today().strftime('%d-%m-%y')

# Load MP attributes data
mp_attributes = pd.read_csv("mp_political_attributes_22-08-22.csv")

# Select MP Twitter usernames 
mp_usernames = mp_attributes[mp_attributes["screen_name"].notnull()]

# Harvest MP profile metadata
profile_metadata,errors = get_profile_metadata(client,mp_usernames)

# Save MP profile metadata to CSV 
profile_metadata.to_csv(f"mp_profile_metadata_{date}.csv", index = False)


#### ---- END ---- ####  
    
     
    

    

    
    
    
    













































