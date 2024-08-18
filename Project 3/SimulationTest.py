#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Jul  1 10:40:12 2022

@author: keatonmackey
"""

import numpy as np

# Foraging generative parameters
alpha = 14.90873 # parameters to randomly select depletion rate
beta = 2.033008
r0_m = 10 # mean number of apples at first
r0_s = 1 # standard deviation of apples at first
apple_price = 2.5/100 # in dollars #changed from 0.5 to 2.50 for config code on QuR-OUD

# Foraging timing parameters
t = 3 # including apple show, delay to decision, and too slow times
d = 10.75 + 1.25 # travel time + delay to decision and too slow times

# shake a randomly initialized tree till apple count hits threshold
# return cumulative rewards and time
def shakeTillThreshold(thresh):
	time_at_tree = t # include first shake
	r0 = np.random.normal(r0_m, r0_s) # choose size of start reward
	tree_reward = r0
	while r0 > thresh:
		# print(r0, time_at_tree)
		time_at_tree += t
		r0 = r0 * np.random.beta(alpha, beta)
		tree_reward += r0
	time_at_tree += d # travel time to next tree as well
	return tree_reward, time_at_tree

# run with shaking threshold for the amount of time specified
# return total rewards accumulated
def runUsingThreshold(thresh, max_time=420):
	time_left = max_time
	total_rewards = 0
	while time_left > 0:
		rew, time = shakeTillThreshold(thresh)
		# print(rew, time, time_left, 'New Tree')
		total_rewards += rew
		time_left -= time
	return total_rewards

# run many times
def manyRunThreshold(thresh, max_time=420, num=1000):
	rews = np.zeros(num)
	for i in range(num):
		rews[i] = runUsingThreshold(thresh, max_time=max_time)
	return rews



thresholds_to_try = np.linspace(1,10,num=40)
mean_rews = np.zeros_like(thresholds_to_try)
for i in range(len(thresholds_to_try)):
	thresh = thresholds_to_try[i]
	rews = apple_price*manyRunThreshold(thresh, max_time=360) #changed cause block time
	mean_rews[i] = rews.mean() # store the mean reward
	print(thresh, rews.mean(), rews.std()) # print out mean and standard deviation

optimal_index = np.argmax(mean_rews)
optimal_thresh = thresholds_to_try[optimal_index] # find best threshold
optimal_reward = mean_rews[optimal_index] # and corresponding mean reward
print('Optimal threshold:', optimal_thresh, 'gives reward $', optimal_reward)

