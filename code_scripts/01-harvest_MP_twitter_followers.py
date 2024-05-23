# -*- coding: utf-8 -*-
"""
The purpose of this Python script is to harvest the complete follower networks of the U.K MPs through the Twitter (X) API.
It requires the Twitter handles of each MP as an input, along with unique Twitter API access credentials.
It returns two-column adjacency lists of MPs and their Twitter followers as dataframes, along with the profile metadata of each
follower profile. It uses pagination tokens to iterate through each MP's list of followers. 

"""

#### ---- IMPORT PACKAGES ---- ####

import pandas as pd
from datetime import datetime
import json 
import time
import tweepy as tw

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

#### ---- TWITTER FOLLOWER NETWORKS SCRAPER ---- ####

""""
Summary: 

This function accesses the Twitter API using the stored client credentials, harvesting 1,000 followers of the 
specified MP, storing each follower in an adjacency list alongside the name of the given user.
It also stores the profile metadata of each follower in a dataframe. 

Input: 

Function requires three inputs -

client = Twitter API access credentials
username = Username of Twitter user
next_token = Pagination tokens are stored seperately which can be used to track where the scraper has gotten to in the
follower list. This allows for iteration through a user's entire follower list. Starts at 'None'. 

Returns:
  
follower_adjacency_list = Dataframe with two columns: the Twitter username of the MP and the usernames of each follower
profile_metadata_final = Dataframe which contains profile metadata for each individual follower 
next_batch_df = Dataframe with the Twitter username of the MP and the next pagination token
errors_df = Dataframe with any usernames that failed to return follower data
"""

# Function:

def harvest_followers(client,username,next_token):
  
    # Create empty dictionary to store next pagination token
    next_batch = {"user_id": [], "next_token": []}
    
    # Create empty list to store follower profile usernames
    follower_list = []
    
    # Create empty list to stoe follower profile metadata
    profile_metadata = []
    
    # Create empty dictionary to unnest public metrics 
    public_metrics = {"id": []}
    
    # Create empty list to catch any errors
    errors = []
    
    # Populate empty public metric dictionary with specified profile metrics 
    for metric in PROFILE_METRICS:
        public_metrics[metric] = []
    
    # Harvest Twitter user followers (1,000 max)    
    follower_profiles = client.get_users_followers(username,max_results=1000,user_fields=USER_FIELDS,
                                                   pagination_token=next_token)
                                                   
    # If follower data is returned, append followers and their metadata to accompanying lists                                               
    if follower_profiles.data != None:
        for metadata in follower_profiles.data:
            follower_list.append(metadata.id)
            profile_metadata.append(metadata)
            public_metrics["id"].append(metadata.id)
            for metric in PROFILE_METRICS:
                public_metrics[metric].append(metadata.public_metrics.get(metric))
        if "next_token" in follower_profiles.meta:
            next_batch["user_id"].append(username)
            next_batch["next_token"].append(follower_profiles.meta["next_token"])
            
    # If no follower data is returned, append error data to errors log        
    elif follower_profiles.errors != None:
        errors.append(follower_profiles.errors[0])
    else: 
        errors.append({"value": username, "detail": "No error field returned."})
        
    # Convert follower list to dataframe and add a second column for MP Twitter username (Adj List)
    follower_adjacency_list = pd.DataFrame(data=follower_list, columns=["source_id"])
    follower_adjacency_list["target_id"] = username
    follower_adjacency_list = follower_adjacency_list[["source_id","target_id"]].astype(str)
    
    # Convert follower profile_metadata to dataframe and unnest public metrics 
    profile_metadata_df = pd.DataFrame(profile_metadata)
    public_metrics_df = pd.DataFrame(public_metrics)
    if len(errors) == 0:
        profile_metadata_merge = pd.merge(profile_metadata_df,public_metrics_df,how='left',on="id")
        profile_metadata_final = profile_metadata_merge.drop(columns=["public_metrics"])
        profile_metadata_final["id"] = profile_metadata_final["id"].astype(str) 
    else:
        profile_metadata_final = profile_metadata_df
    
    # Convert next batch dictionary to dataframe    
    next_batch_df = pd.DataFrame(next_batch).astype(str)
    
    # Convert error log to dataframe
    errors_df = pd.DataFrame(errors)
    
    return follower_adjacency_list,profile_metadata_final,next_batch_df,errors_df
        
                
#### ---- FOLLOWER NETWORK SCRAPER ITERATOR ---- ####

""""
Summary: 

This function acts as an iterator for the follower network scraper, allowing for the follower network scraper function
to be iterated through the list of MPs, getting 1,000 of their followers each. It uses the harvest_followers as the main function, 
iterating through the entire list of MPs, returning a pagination token for each one along with their followers to use in the next 
iteration. Follower scraping was done horizontally as opposed to vertically, meaning that rather than getting each MPs complete list
of followers one at a time, each iteration gets 1,000 of each MPs followers concurrently until each MP is complete. 

Input: 

Function requires two inputs -

client = Twitter API access credentials
user_id_df = Two column dataframe with the list of MP usernames and their latest pagination token. This should change with every 
iteration.

Returns:
  
follower_adjacency_list = Dataframe with two columns: the Twitter username of each MP and the usernames of each follower
profile_metadata_joined = Dataframe which contains profile metadata for each individual follower 
next_batch_list = Dataframe with the Twitter usernames of the MPs and the next pagination tokens (use in the next iteration)
errors_df = Dataframe with any usernames that failed to return follower data
"""

def construct_follower_adj_list(user_id_df,client):
  
    # Create empty list to store followers
    follower_df_list = []
    
    # Create empty list to story follower profile metadata
    profile_metadata_df_list = []
    
    # Create empty list to store next batch (MP's username and their next pagination token)
    next_batch_df_list = []
    
    # Create empty list to log any errors
    errors_df_list = []
    
    # List of MP usernames
    user_id_list = user_id_df["screen_name"] 
    
    # List of pagination tokens
    token_list = user_id_df["next_token"]
    
    # Iterate through the list of MP usernames, gathering their next set of followers using the pagination tokens
    for count,(screen_name,next_token) in enumerate(zip(user_id_list,token_list)): 
        
        # Includes an exception handler which sleeps and tries again when there are server errors
        attempt_no = 0
        delay = 30
        while True:
            attempt_no = attempt_no + 1
            delay = delay * 2
            try:
                follower_list_df,profile_metadata_df,next_batch_df,errors_df = harvest_followers(screen_name,next_token,client)
                follower_df_list.append(follower_list_df)
                profile_metadata_df_list.append(profile_metadata_df)
                next_batch_df_list.append(next_batch_df)
                errors_df_list.append(errors_df)
                print(f"{count+1}/{len(user_id_list)} profiles complete!")
            except tw.errors.TwitterServerError:
                print("Sleeping for:",delay,"seconds")
                time.sleep(delay)
                continue
            break
          
    # Concatenate follower adjacency list        
    follower_adj_list = pd.concat(follower_df_list,ignore_index=True)
    
    # Concatenate follower profile metadata list
    profile_metadata_joined = pd.concat(profile_metadata_df_list,ignore_index=True)
    
    # Concatenate next batch list of usernames and pagination tokens
    next_batch_list = pd.concat(next_batch_df_list,ignore_index=True)
    
    # Concatenate error list 
    errors_caught = pd.concat(errors_df_list,ignore_index=True)
    
    return follower_adj_list,profile_metadata_joined,next_batch_list,errors_caught


#### ---- RUN FUNCTION ---- ####

# Specify date 
date = datetime.today().strftime('%d-%m-%y')

# Load MP attributes data
mp_attributes = pd.read_csv("C:/Users/cg1g21/OneDrive - University of Southampton/PhD Politics/academic_projects/bayesian_spatial_following_model/data/final_data_files/final_datasets/mp_political_attributes_22-08-22.csv")

# Select MP Twitter usernames 
mp_usernames = pd.DataFrame(mp_attributes[mp_attributes["screen_name"].notnull()]["screen_name"])

# Set first pagination token to None
mp_usernames["next_token"] = None

# Set start time 
start_time = time.time()

# Run follower network scraper 
print(f" ---- Beginning Follower Harvesting For {date} ----")

# First iteration (pagination token = None)
follower_adj_list,profile_metadata_joined,next_batch_list,errors_caught = construct_follower_adj_list(mp_usernames,client)

# Save first batch of datasets to CSV (Set batch number to 1)
batch_no = 1

follower_adj_list.to_csv(f'follower_adjacency_list_batch_{batch_no}.csv', index = False)
profile_metadata_joined.to_csv(f'follower_profile_metadata_batch_{batch_no}.csv', index = False)
next_batch_list.to_csv(f'user_list_tokens_batch_{batch_no}.csv', index = False)
errors_caught.to_csv(f'errors_list_batch_{batch_no}.csv', index = False)

# Calculate batch runtime        
batch_runtime = round((time.time() - start_time)/60)

# Run a while loop that will then continue the follower scraper, inputting the next set of pagination tokens from the 
# last batch to get the next set of 1,000 followers for each MP until there every MP is complete. 
while len(next_batch_list) > 0:

    start_time = time.time()
    batch_no = batch_no + 1
    mp_usernames = next_batch_list

    follower_adj_list,profile_metadata_joined,next_batch_list,errors_caught = construct_follower_adj_list(mp_usernames,client)

    follower_adj_list.to_csv(f'follower_adjacency_list_batch_{batch_no}.csv', index = False)
    profile_metadata_joined.to_csv(f'follower_profile_metadata_batch_{batch_no}.csv', index = False)
    next_batch_list.to_csv(f'user_list_tokens_batch_{batch_no}.csv', index = False)
    errors_caught.to_csv(f'errors_list_batch_{batch_no}.csv', index = False)
    
    batch_runtime = round((time.time() - start_time)/60)

print("Harvesting complete!")

job_run_time = round((time.time() - start_time)/60)

print(f"--- Job Runtime: {job_run_time} minutes ---" )
    
##### ----- END ------ #####
      
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

    
