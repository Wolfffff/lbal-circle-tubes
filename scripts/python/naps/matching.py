import os
from collections import defaultdict
from typing import Callable, DefaultDict

import cv2
import numpy as np
from cost_matrix import CostMatrix
from skimage.draw import disk


class Matching:
    """Matching pipeline.

    Attributes:
        video_filename (str): Matching video filename (i.e. path).
        video_first_frame (str): Frame to start matching.
        video_last_frame (str): Frame to end matching.
        marker_detector (Callable): Function used to detect/assign markers.
        tag_crop_size (int): Crop size used for marker detection.
        half_rolling_window_size (int): Window size (upstream/downstream) used by the cost matrix.
        crop_type (str): Type of crop to perform on tag node, i.e., circle or square.
        tag_node_dict (dict): Dictionary of frames, tracks, and node coordinates.
        threads (int): The number of CPU threads to use.
        min_sleap_score (float): Minimum sleap score required for matching. Should this be removed?
    """

    def __init__(
        self,
        video_filename: str,
        video_first_frame: int,
        video_last_frame: int,
        marker_detector: Callable,
        tag_crop_size: int,
        half_rolling_window_size: int,
        crop_type: str,
        tag_node_dict: dict,
        min_sleap_score: float = 0.1,
        **kwargs,
    ):
        # Confirm the video file exists
        if not os.path.isfile(video_filename):
            raise Exception(f"{video_filename} does not exist")

        # Video arguments
        self.video_filename = video_filename
        self.video_first_frame = video_first_frame
        self.video_last_frame = video_last_frame

        # SLEAP arguments
        self.tag_node_dict = tag_node_dict
        self.min_sleap_score = min_sleap_score

        # Matching arguments
        self.half_rolling_window_size = half_rolling_window_size
        self.tag_crop_size = tag_crop_size
        self.marker_detector = marker_detector
        self.crop_type = crop_type

    def match(self) -> DefaultDict:
        """Performs matching.

        Returns:
            defaultdict(lambda: defaultdict(str)): Dictionary of matching results with the form dictionary[frame][track] = tag.
        """

        # Create a dict to store the matches for this job
        job_match_dict = {}

        # Set the current frame of the job
        current_frame = self.video_first_frame
        print(f"Processing frames {self.video_first_frame} to {self.video_last_frame}")

        # Initialize OpenCV, then set the starting frame (0-based)
        video = cv2.VideoCapture(self.video_filename)
        video.set(cv2.CAP_PROP_POS_FRAMES, current_frame)

        # Read in the frame, confirm it was successful
        frame = MatchFrame.fromCV2(*video.read())
        while frame and current_frame <= self.video_last_frame:
            # Crop and return the ArUco tags or color tags
            frame.cropMarkerWithCoordsArray(
                self.tag_node_dict[current_frame], self.tag_crop_size, self.crop_type
            )
            job_match_dict[current_frame] = frame.returnMarkerTags(self.marker_detector)

            # Advance to the next frame
            frame = MatchFrame.fromCV2(*video.read())
            current_frame += 1

        # Create the cost matrix and assign the track/tag pairs for each frame
        job_cost_matrix = CostMatrix.fromDict(
            job_match_dict,
            self.video_first_frame,
            self.video_last_frame,
            self.half_rolling_window_size,
        )

        # job_match_dict_keys = list(job_match_dict.keys())
        # print(job_match_dict[job_match_dict_keys[91]][2])

        if self.crop_type != "square" and self.crop_type != "circle":
            # raise an error
            raise Exception(f"unknown type: {self.crop_type}")

        return job_cost_matrix.assignTrackTagPairs()


class MatchFrame:
    """Class used to assign markers for a frame and associated data.

    Attributes:
        frame (np.ndarray): Value array of the frame image.
        frame_exists (bool): Boolean indicating if the frame was read correctly.
        frame_images (dict): Dictionary to store value arrays for each marker image.

    """

    def __init__(self, frame_exists: bool, frame: np.ndarray, *args, **kwargs):
        # Image arguments
        self.frame = frame
        self.frame_exists = frame_exists
        self.frame_images: dict = {}

    def __nonzero__(self):
        """Replaces nonzero for the class

        Returns:
            bool: Boolean indicating if the frame was read correctly.
        """

        return self.frame_exists

    @classmethod
    def fromCV2(cls, *args, **kwargs):
        """Class methods to create a MatchFrame from CV2.read()

        Args:
            bool: Boolean indicating if the frame was read correctly.
            np.ndarray: Value array of the frame image.

        Returns:
            MatchFrame: A MatchFrame object.
        """

        return cls(*args, **kwargs)

    def cropMarkerWithCoordsArray(self, coords_dict, crop_size: int, crop_type: str):
        """Creates cropped images for each marker in the coords_dict.

        Args:
            coords_dict (dict): Dictionary of marker coordinates to crop for each track.
            crop_size (float): Crop size.

        """

        def croppedCoords(coord: float, crop_size: float, coord_max: int):
            """Gets the cropped coordinates for the given single coordinate

            Args:
                coord (float): Coordinate to get crop range.
                crop_size (float): Crop size.
                coord_max (int): Maximum coordinate possible before leaving frame.

            Returns:
                tuple(int): The minimum and maximum coordinates for the given single coordinate

            """
            return np.maximum(int(coord) - crop_size, 0), np.minimum(
                int(coord) + crop_size, coord_max - 1
            )

        if crop_type == "square":
            # Loop the frame track coordinates
            for track, coords in coords_dict.items():
                # Skip track if NaN found in coordinates
                if np.isnan(coords).any():
                    self.frame_images[track] = None
                    continue

                # Assign the min/max coords for cropping
                y_min, y_max = croppedCoords(coords[1], crop_size, self.frame.shape[0])
                x_min, x_max = croppedCoords(coords[0], crop_size, self.frame.shape[1])

                # Assign and store the cropped track image
                self.frame_images[track] = self.frame[y_min:y_max, x_min:x_max, 0]

        elif crop_type == "circle":
            for track, coords in coords_dict.items():
                if np.isnan(coords).any():
                    self.frame_images[track] = None
                    continue

                # gray_img = cv2.cvtColor(self.frame, cv2.COLOR_BGR2GRAY)
                mask = np.ones(self.frame.shape[:2], dtype=np.uint8) * 255

                cx = coords[0]
                cy = coords[1]

                # Feed in the coords for the center of the circle
                # Creates the y_axis and x_axis of where the circle exists
                y_axis, x_axis = disk((cx, cy), crop_size, shape=self.frame.shape[:2])

                # Set the circle axes in mask equal to the corresponding color values in the frame
                mask[x_axis, y_axis] = self.frame[x_axis, y_axis, 0]

                # Overlay frame & mask to check on crop
                # img_frame = plt.imshow(self.frame)
                # img_mask = plt.imshow(mask, alpha = 0.5)

                # plt.show()

                # Assign the min/max coords for cropping a smaller frame circumscribed about the circle
                y_min, y_max = croppedCoords(coords[1], crop_size, self.frame.shape[0])
                x_min, x_max = croppedCoords(coords[0], crop_size, self.frame.shape[1])

                # Store the fully cropped image for the corresponding track
                self.frame_images[track] = mask[y_min:y_max, x_min:x_max]

        else:
            # raise an error
            raise Exception(f"unknown type: {crop_type}")

    def returnMarkerTags(self, marker_detect: Callable):
        """Detect marker tags using the specified marker_detect function

        Args:
            marker_detect (Callable): Marker detector function. Function is expected
            to return a list of marker assignments.

        Returns:
            defaultdict(list): Dictionary of matching results for a single frame.
        """

        track_tag_dict: DefaultDict = defaultdict(list)

        # Loop the frame track images
        for track, track_image in self.frame_images.items():
            # Store None if no image was found
            if track_image is None:
                track_tag_dict[track].append(None)
                continue

            # Assign the marker using the detector function
            track_tag_dict[track].extend(marker_detect(track_image))

        return track_tag_dict
