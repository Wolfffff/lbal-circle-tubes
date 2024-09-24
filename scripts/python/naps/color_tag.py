#!/usr/bin/env python
# coding: utf-8

# In[22]:


#!/usr/bin/env python
import pickle

import numpy as np


class ColorTagModel:
    # def detect_quadrant():

    def detect(img):
        img_mean = np.mean(img)  # more efficient to initialize here

        if np.isnan(img_mean):
            raise Exception("Tag crop average value is NaN.")

        with open("classifier.pkl", "rb") as file:
            clf = pickle.load(file)

        img = img.flatten()
        img = np.delete(img, range(-(int(len(img) / 2)), 0))
        img = np.delete(img, np.where(img == 255))
        None_list = [None] * (150 - len(img))
        img = np.append(img, None_list)
        img = img.reshape(-1, len(img))
        prediction = clf.predict(img)[0]

        return [prediction]


# make settings limit to 2 instances
# get rid of unnecessary packages and libraries
# figure out #None + 1 other color (2 instances) automatically assign white or automatically asign missing color?
# figure out #None + 2 other colors (3 instances)
# test cases?
# running in batch
# separate files for naps_track and color_track?
