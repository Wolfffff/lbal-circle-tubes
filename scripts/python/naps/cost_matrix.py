#!/usr/bin/env python
from collections import defaultdict

import pandas as pd
from scipy.optimize import linear_sum_assignment


class CostMatrix:
    def __init__(
        self,
        unmatched_dict: dict,
        first_frame: int,
        last_frame: int,
        half_rolling_window_size: int,
        **kwargs,
    ):
        # Matrix arguments
        self.unmatched_dict = unmatched_dict

        # Matching arguments
        self.first_frame = first_frame
        self.last_frame = last_frame
        self.half_rolling_window_size = half_rolling_window_size

        # Assignment argument
        self.assignment_method = self._linearAssignment

    @classmethod
    def fromDict(cls, *args, **kwargs):
        return cls(*args, **kwargs)

    def assignTrackTagPairs(self):
        # Create the cost matrix
        cost_dict = defaultdict(lambda: defaultdict(int))
        matched_dict = defaultdict(lambda: defaultdict(str))

        all_tags_list = []
        # Loop the frames in the unmatched matrix to create cost matrix
        for frame in range(self.first_frame, self.last_frame + 1):
            # Assign the track/tag combinations for this frame
            track_tag_dict = self.unmatched_dict[frame]
            for (
                track,
                tag_list,
            ) in (
                track_tag_dict.items()
            ):  # color_tag clf will not assign multiple tags to one track
                for tag in tag_list:
                    all_tags_list.append(tag)
                    if tag is not None:
                        cost_dict[track][tag] -= 1

            # Assign the rolling window frame
            match_frame = frame - self.half_rolling_window_size

            # Check if the match frame should undergo linear assignment
            if (
                match_frame < self.first_frame + self.half_rolling_window_size
                or match_frame > self.last_frame - self.half_rolling_window_size
            ):
                continue

            # Assign the first frame of the window
            start_of_window = match_frame - self.half_rolling_window_size

            # Remove a frame, if needed
            if start_of_window > self.first_frame:
                track_tag_dict = self.unmatched_dict[start_of_window - 1]
                for track, tag_list in track_tag_dict.items():
                    for tag in tag_list:
                        if tag != -1:
                            cost_dict[track][tag] += 1
                            if cost_dict[track][tag] == 0:
                                del cost_dict[track][tag]

            # Create a dataframe of the matrix
            cost_dataframe = pd.DataFrame.from_dict(cost_dict).fillna(0)
            cost_dataframe = cost_dataframe.loc[~(cost_dataframe == 0).all(axis=1)]
            cost_dataframe = cost_dataframe.loc[:, ~(cost_dataframe == 0).all(axis=0)]

            match_tag_list = list(self.unmatched_dict[match_frame])
            cost_df_col_list = list(cost_dataframe.columns)
            set_list = list(set(cost_df_col_list).intersection(set(match_tag_list)))

            match_cost_dataframe = cost_dataframe[set_list]

            if 1878 <= match_frame <= 1892:
                print(match_cost_dataframe)

            # Store the assignments
            for track_index, tag_index in self.assignment_method(
                match_cost_dataframe.values
            ):
                matched_dict[match_frame][match_cost_dataframe.columns[track_index]] = (
                    match_cost_dataframe.index[tag_index]
                )

            # if 1882 <= frame <= 1892:
            #    print(matched_dict[frame])

        for frame in range(self.first_frame + 1, self.last_frame + 1):
            for track, tag in matched_dict[frame - 1].items():
                if (
                    track in self.unmatched_dict[frame].keys()
                    and tag not in matched_dict[frame].values()
                ):
                    matched_dict[frame][track] = matched_dict[frame - 1][track]

            # if 1882 <= frame <= 1892:
            #    print(matched_dict[frame])

        # tag_list = []
        # for frame in range(self.first_frame, self.last_frame + 1):
        #    for track in self.unmatched_dict[frame].keys():
        #        tag_list.append(self.unmatched_dict[frame][track][0])
        # unique_tag_list = set(tag_list)

        # unique_tag_list = set([self.unmatched_dict[frame][track][0] for track in self.unmatched_dict[frame].keys() for frame in range(self.first_frame, self.last_frame + 1)])
        unique_tag_list = set(all_tags_list)
        for frame in range(self.first_frame, self.last_frame + 1):
            track_tag_dict = matched_dict[frame]
            all_tracks_in_frame = list(
                track_tag_dict.keys()
            )  # color_tag clf will not assign multiple tags to one track
            all_tags_in_frame = list(track_tag_dict.values())
            if all_tags_in_frame.count(None) == 1 and len(all_tags_in_frame) == 2:
                none_index = all_tracks_in_frame[all_tags_in_frame.index(None)]
                matched_dict[frame][none_index] = list(
                    unique_tag_list.difference(all_tags_in_frame)
                )[0]

            # if 1882 <= frame <= 1892:
            #    print(matched_dict[frame])

        return matched_dict

    @staticmethod
    def _linearAssignment(value_array):
        # Solve the linear assignment problem using Jonker-Volgenant algorithm
        tag_indices, track_indices = linear_sum_assignment(value_array)

        # Store the assignments
        for track_index, tag_index in zip(track_indices, tag_indices):
            yield track_index, tag_index
