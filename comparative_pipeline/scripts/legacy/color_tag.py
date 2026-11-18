#!/usr/bin/env python
# coding: utf-8

# In[22]:


#!/usr/bin/env python
import numpy as np
import pickle
import os

# Load the black/purple tag classifier once, from this script's folder
# (previously reopened 'classifier.pkl' relative to the CWD on every crop).
_CLF_PATH = os.environ.get('NAPS_CLASSIFIER', os.path.join(os.path.dirname(os.path.abspath(__file__)), 'classifier.pkl'))
_CLF = None

def _get_clf():
    global _CLF
    if _CLF is None:
        with open(_CLF_PATH, 'rb') as file:
            _CLF = pickle.load(file)
    return _CLF

class ColorTagModel:

    #def detect_quadrant():

    def detect(img):
        
        img_mean = np.mean(img) # more efficient to initialize here

        if np.isnan(img_mean):
            raise Exception('Tag crop average value is NaN.')

        clf = _get_clf()

        img = img.flatten()
        img = np.delete(img, range(-(int(len(img)/2)),0))
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