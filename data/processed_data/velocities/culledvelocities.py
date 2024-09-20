import h5py
import numpy as np
import sys

prefix = sys.argv[1]
filename = prefix + "_8_8_5_1_12_interp_filtered.h5"

import copy
import math
import scipy.stats as stats
import pandas as pd
import scipy.ndimage
from tqdm import tqdm
from scipy.signal import savgol_filter
import matplotlib.colors as colors
import logging
import skvideo.io
import matplotlib.pyplot as plt
from base64 import b64encode


def diff(node_loc, diff_func=np.gradient, **kwargs):
    """
    node_loc is a [frames, 2] arrayF

    win defines the window to smooth over

    poly defines the order of the polynomial
    to fit with

    """
    node_loc_vel = np.zeros_like(node_loc)
    for c in range(node_loc.shape[-1]):
        node_loc_vel[:, c] = diff_func(node_loc[:, c], **kwargs)

    node_vel = np.linalg.norm(node_loc_vel, axis=1)

    return node_vel


def flatten_features(x, axis=0):
    if axis != 0:
        # Move time axis to the first dim
        x = np.moveaxis(x, axis, 0)

    # Flatten to 2D.
    initial_shape = x.shape
    x = x.reshape(len(x), -1)

    return x, initial_shape


def unflatten_features(x, initial_shape, axis=0):
    # Reshape.
    x = x.reshape(initial_shape)

    if axis != 0:
        # Move time axis back
        x = np.moveaxis(x, 0, axis)

    return x


def smooth_median(x, window=5, axis=0, inplace=False):
    if axis != 0 or x.ndim > 1:
        if not inplace:
            x = x.copy()

        # Reshape to (time, D)
        x, initial_shape = flatten_features(x, axis=axis)

        # Apply function to each slice
        for i in range(x.shape[1]):
            x[:, i] = smooth_median(x[:, i], window, axis=0)

        # Restore to original shape
        x = unflatten_features(x, initial_shape, axis=axis)
        return x

    y = scipy.signal.medfilt(x.copy(), window)
    y = y.reshape(x.shape)
    mask = np.isnan(y) & (~np.isnan(x))
    y[mask] = x[mask]
    return y


def fill_missing(x, kind="nearest", axis=0, **kwargs):
    """Fill missing values in a timeseries.

    Args:
        x: Timeseries of shape (time, ...) or with time axis specified by axis.
        kind: Type of interpolation to use. Defaults to "nearest".
        axis: Time axis (default: 0).

    Returns:
        Timeseries of the same shape as the input with NaNs filled in.

    Notes:
        This uses pandas.DataFrame.interpolate and accepts the same kwargs.
    """
    if x.ndim > 2:
        # Reshape to (time, D)
        x, initial_shape = flatten_features(x, axis=axis)

        # Interpolate.
        x = fill_missing(x, kind=kind, axis=0, **kwargs)

        # Restore to original shape
        x = unflatten_features(x, initial_shape, axis=axis)

        return x
    return pd.DataFrame(x).interpolate(method=kind, axis=axis, **kwargs).to_numpy()


def instance_node_velocities(fly_node_locations, start_frame, end_frame):
    frame_count = len(range(start_frame, end_frame))
    if len(fly_node_locations.shape) == 4:
        fly_node_velocities = np.zeros(
            (frame_count, fly_node_locations.shape[1], fly_node_locations.shape[3])
        )
        for fly_idx in range(fly_node_locations.shape[3]):
            for n in range(0, fly_node_locations.shape[1]):
                fly_node_velocities[:, n, fly_idx] = diff(
                    fly_node_locations[start_frame:end_frame, n, :, fly_idx]
                )
    else:
        fly_node_velocities = np.zeros((frame_count, fly_node_locations.shape[1]))
        for n in range(0, fly_node_locations.shape[1] - 1):
            fly_node_velocities[:, n] = diff(
                fly_node_locations[start_frame:end_frame, n, :]
            )

    return fly_node_velocities


def smooth_gaussian(x, std=1, window=5, axis=0, inplace=False):
    if axis != 0 or x.ndim > 1:
        if not inplace:
            x = x.copy()

        # Reshape to (time, D)
        x, initial_shape = flatten_features(x, axis=axis)

        # Apply function to each slice
        for i in range(x.shape[1]):
            x[:, i] = smooth_gaussian(x[:, i], std, window, axis=0)

        # Restore to original shape
        x = unflatten_features(x, initial_shape, axis=axis)
        return x

    y = (
        pd.DataFrame(x.copy())
        .rolling(window, win_type="gaussian", center=True)
        .mean(std=std)
        .to_numpy()
    )
    y = y.reshape(x.shape)
    mask = np.isnan(y) & (~np.isnan(x))
    y[mask] = x[mask]
    return y


def velfilt(locs, thresh):
    filledlocs = fill_missing(locs[:, :, :, :], kind="pad")
    vels = instance_node_velocities(filledlocs, 1, locs.shape[0])
    velsbool = vels > thresh
    return velsbool


def coordfilt(velsbool, limit):
    sums = np.sum(velsbool, axis=1)
    coord1dbool = sums >= limit
    coordbool = np.stack(
        (
            coord1dbool,
            coord1dbool,
            coord1dbool,
            coord1dbool,
            coord1dbool,
            coord1dbool,
            coord1dbool,
            coord1dbool,
            coord1dbool,
        ),
        axis=1,
    )  # FIX THIS EVENTUALLY
    return coordbool


def integratedfilter(locations, velsbool, extvelsbool, coordbool):
    uncoordvelsbool = velsbool & ~coordbool
    finalbool = uncoordvelsbool + extvelsbool
    coordsaved = np.sum(velsbool) - np.sum(uncoordvelsbool)
    print("Vel Outliers Saved by Coord: ", coordsaved)
    final2dbool = np.stack((finalbool, finalbool), axis=2)
    print("Final Filter: ", np.sum(finalbool))
    locationsfilter = copy.deepcopy(locations)
    locationsfilter = locationsfilter[1:, :, :, :]
    previousnans = np.isnan(locationsfilter[:, :, 0, :])
    previousandpresent = previousnans & finalbool
    locationsfilter[final2dbool] = np.nan
    return locationsfilter


def shrink_locs(locations, node, track1, track2):
    filt1 = ~np.isnan(locations[:, node_names.index(node), 0, track1])
    filt2 = ~np.isnan(locations[:, node_names.index(node), 0, track2])
    filt = filt1 & filt2
    array1 = locations[filt, node_names.index(node), :, track1]
    array2 = locations[filt, node_names.index(node), :, track2]
    distances = np.linalg.norm(array1 - array2, axis=1)
    return distances


def removelargeedges(locations, frame_start, frame_end, zscore_threshold=5):
    for track_pos in range(locations.shape[3]):
        for edge_nodeA in range(locations.shape[1]):
            for edge_nodeB in range(locations.shape[1]):
                if edge_nodeA < edge_nodeB:
                    array1 = locations[frame_start:frame_end, edge_nodeA, :, track_pos]
                    array2 = locations[frame_start:frame_end, edge_nodeB, :, track_pos]
                    distances = np.linalg.norm(array1 - array2, axis=1)
                    zscore_array = stats.zscore(distances, nan_policy="omit")
                    locations[frame_start:frame_end, edge_nodeA, :, track_pos][
                        zscore_array > zscore_threshold
                    ] = np.nan
                    locations[frame_start:frame_end, edge_nodeB, :, track_pos][
                        zscore_array > zscore_threshold
                    ] = np.nan
    return locations


def removeSharedNodes(locations, frame_start, frame_end, shared_dist_threshold=50):
    for track_posA in range(locations.shape[3]):
        for track_posB in range(locations.shape[3]):
            for nodeA in range(locations.shape[1]):
                for nodeB in range(locations.shape[1]):
                    if track_posA < track_posB:
                        array1 = locations[frame_start:frame_end, nodeA, :, track_posA]
                        array2 = locations[frame_start:frame_end, nodeB, :, track_posB]
                        distance_array = np.linalg.norm(array1 - array2, axis=1)
                        locations[frame_start:frame_end, nodeA, :, track_posA][
                            distance_array < shared_dist_threshold
                        ] = np.nan
                        locations[frame_start:frame_end, nodeB, :, track_posB][
                            distance_array < shared_dist_threshold
                        ] = np.nan
    return locations


with h5py.File(filename, "r") as f:
    dset_names = list(f.keys())
    locations = f["tracks"][:].T
    node_names = [n.decode() for n in f["node_names"][:]]
    track_names = f["track_names"][:]
    point_scores = f["point_scores"][:]
    instance_scores = f["instance_scores"][:]
    track_occupancy = f["track_occupancy"][:]
    tracking_scores = f["tracking_scores"][:]


print("===locations data shape===")
print(locations.shape)
print()

# locations = locations[0::5, :, :, :]
print(locations.shape)
filledlocs = fill_missing(locations[:, :, :, :], kind="pad")
vels = instance_node_velocities(filledlocs, 1, filledlocs.shape[0])
print(vels.shape)
outputfile = sys.argv[2]
vels = vels[:, 1, :]
vels = np.delete(vels, 0, axis=1)
print(vels.shape)
# velmed = np.nanmedian(vels[1:,:], axis=0)
# print(velmed.shape)
track_names = np.delete(track_names, 0, 0)
print(track_names)
# velmeddf = pd.DataFrame({'track': track_names, 'velmed': velmed}, columns=['track', 'velmed'])
# pd.DataFrame(velmeddf).to_csv(outputfile)
velsdf = pd.DataFrame(vels, columns=track_names)
pd.DataFrame(velsdf).to_csv(outputfile)
