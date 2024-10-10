#!/usr/bin/env python
import numpy as np
import pickle

from collections import Counter

class ColorTagModel:

    def __init__(self):

        with open('classifier.pkl', 'rb') as file:
            clf = pickle.load(file)

        self.classifier = clf
        self.sample_count = 100 # for sampleDetect only
        self.prediction_cutoff = 0.75 # 0.75 for sampleDetect

    def quadrantDetect(self, img):
        
        # first, flatten img and remove 255s
        img = img.flatten()
        img = np.delete(img, np.where(img == 255))
        
        # second, calculate the cutoff for a quadrant
        quarter_length = round(len(img) / 4)
        
        quad_predictions = []
        # third, iterate through each quadrant and make a prediction
        for i in range(4):
            start = i * quarter_length
            end = (i * quarter_length) + quarter_length - 1
            quad = img[start:end]

            if len(quad) > 150: quad = np.random.choice(quad, 150)
            elif len(quad) < 150: quad = np.append(quad, np.repeat(np.nan, (150 - len(quad))))
            quad = quad.reshape(-1, len(quad))
            
            quad_predictions.extend(self.classifier.predict(quad))

        # fourth, create counts of each prediction (max counts = 4)
        prediction_count_dict = Counter(quad_predictions)

        # fifth, determine the top prediction percentage and if it reaches the cutoff
        top_prediction_percent = max(prediction_count_dict.values()) / sum(prediction_count_dict.values())
        if top_prediction_percent < self.prediction_cutoff: prediction = [None]
        else: prediction = [max(prediction_count_dict, key = prediction_count_dict.get)]

        # last, return the prediction
        return prediction

    def sampleDetect(self, img):

        # Create list to store sample predictions
        sample_predictions = []

        # Loop n samples images 
        for sample_img in self.sampleArray(img, self.sample_count):
            
            sample_img = sample_img.flatten()
            if len(sample_img) > 150: sample_img = np.random.choice(sample_img, 150)
            elif len(sample_img) < 150: sample_img = np.append(sample_img, np.repeat(np.nan, (150 - len(sample_img))))
            sample_img = sample_img.reshape(-1, len(sample_img))
            
            sample_predictions.extend(self.classifier.predict(sample_img))

        # Create counts of each prediction
        prediction_count_dict = Counter(sample_predictions)

        # Assign the top prediction percentage
        top_prediction_percent = max(prediction_count_dict.values()) / sum(prediction_count_dict.values())

        # Check if the top prediction should be returned
        if top_prediction_percent < self.prediction_cutoff: return [None]
        else: return [max(prediction_count_dict, key=prediction_count_dict.get)]
    
    def detect(self, img):
       
        img_mean = np.mean(img) # more efficient to initialize here

        if np.isnan(img_mean):
            raise Exception('Tag crop average value is NaN.')

        img = img.flatten()
        if len(img) > 150: img = np.random.choice(img, 150)
        elif len(img) < 150: img = np.append(img, np.repeat(np.nan, (150 - len(img))))

        img = img.reshape(-1, len(img))
        prediction = self.classifier.predict(img)

        return prediction
    
    @staticmethod
    def sampleArray (array, n, sample_crop_size = 1):

        max = array.shape[0]
        min = 0
        crop = int(array.shape[0]/10) #sample_crop_size 
        cmax = max - crop
        cmin = min + crop
        mean = (cmax + cmin) / 2
        stdev = mean / 3
    

        sample_count = 0
        while sample_count < n:

            x_value = int(np.random.normal(mean, stdev))
            y_value = int(np.random.normal(mean, stdev))
            if (cmin <= x_value <= cmax) and (cmin <= y_value <= cmax):
                
                # Create the sample array
                sample_array = array[(y_value - crop):(y_value + crop), (x_value - crop):(x_value + crop)]
                
                # Confirm the array has values
                if np.isnan(sample_array).all(): continue

                # If so, update the count and yield the sample array
                sample_count += 1
                yield sample_array

