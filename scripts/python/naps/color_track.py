#!/usr/bin/env python
# coding: utf-8

# In[ ]:
import argparse
import logging
import time
from collections import defaultdict

import sleap
from color_tag import (
    ColorTagModel,
)  # this should be from naps.color_tag import ColorTagModel
from matching import Matching  # this should be from naps.matching import Matching
from naps.sleap_utils import update_labeled_frames

logger = logging.getLogger("NAPS Logger")


def build_parser():
    """Builds the argument parser for the main function.

    Returns:
        argparse.ArgumentParser: Parser for command line arguments.
    """
    parser = argparse.ArgumentParser(
        description="NAPS -- Hybrid tracking using SLEAP and ArUco tags"
    )

    parser.add_argument(
        "--slp-path",
        help="The filepath of the SLEAP (.slp or .h5) file to pull coordinates from. This should correspond with the input video file",
        type=str,
        required=True,
    )
    parser.add_argument(
        "--h5-path",
        help="The filepath of the analysis h5 file to pull coordinates from. This should correspond with the input video file and slp file",
        type=str,
        default=None,
    )

    parser.add_argument(
        "--video-path",
        help="The filepath of the video used with SLEAP",
        type=str,
        required=True,
    )

    parser.add_argument(
        "--tag-node-name",
        help="The ArUco or circle tag SLEAP node name",
        type=str,
        required=True,
    )

    parser.add_argument(
        "--start-frame",
        help="The zero-based fully-closed frame to begin NAPS assignment",
        type=int,
        required=True,
    )

    parser.add_argument(
        "--end-frame",
        help="The zero-based fully-closed frame to stop NAPS assignment",
        type=int,
        required=True,
    )

    parser.add_argument(
        "--half-rolling-window-size",
        help="Specifies the number of flanking frames (prior and subsequent) required in the rolling window for Hungarian matching a frame",
        type=int,
        default=5,
    )

    parser.add_argument(
        "--tag-crop-size",
        help="The number of pixels horizontally and vertically around the aruco or color tag SLEAP node to identify the marker",
        type=int,
        default=50,
    )

    parser.add_argument(
        "--tag-crop-type",
        help="Type of crop to perform on tag node, i.e., circle or square.",
        type=str,
        default="square",
    )

    parser.add_argument(
        "--output-path",
        help="Output path of the resulting SLEAP analysis file.",
        type=str,
        default="output.analysis.h5",
    )

    return parser


def main(argv=None):
    """Main function for the NAPS tracking script."""

    # Set the start time for the entire pipeline
    t0_total = time.time()

    # Build the arguments from the .parse_args(args)
    args = build_parser().parse_args(argv)

    # Assign the h5 path if not specified
    if args.h5_path is None:
        args.h5_path = args.slp_path

    # Create a track array from the SLEAP file(s)
    logger.info("Loading predictions...")
    t0 = time.time()
    # locations, node_names = load_tracks_from_slp(args.h5_path)
    tag_locations_dict = defaultdict(lambda: defaultdict(tuple))
    labels = sleap.Labels.load_file(args.slp_path)
    for lf in labels.labeled_frames:
        if lf.frame_idx < args.start_frame or lf.frame_idx > args.end_frame:
            continue
        for instance in lf.instances:
            tag_idx = instance.skeleton.node_names.index(args.tag_node_name)
            track_name = int(instance.track.name.split("_")[-1])
            # print(f'Frame {lf.frame_idx} track {track_name} has points {instance.numpy()[tag_idx]}')
            tag_locations_dict[lf.frame_idx][track_name] = instance.numpy()[tag_idx]

    # Create an color tag model with the default parameters
    logger.info("Create color tag model...")
    t0 = time.time()
    color_tag_model = ColorTagModel
    logger.info("Color tag model built in %s seconds.", time.time() - t0)

    # Match the track to the ArUco markers
    logger.info("Starting matching...")
    t0 = time.time()
    matching = Matching(
        args.video_path,
        args.start_frame,
        args.end_frame,
        marker_detector=color_tag_model.detect,
        half_rolling_window_size=args.half_rolling_window_size,
        tag_crop_size=args.tag_crop_size,
        crop_type=args.tag_crop_type,
        tag_node_dict=tag_locations_dict,
    )
    matching_dict = matching.match()
    logger.info("Done matching in %s seconds.", time.time() - t0)

    # Create the output
    logger.info("Reconstructing SLEAP file...")
    t0 = time.time()
    # Right now the reconstruction assumes that we each track has a single track ID assigned to it. We'll generalize so that a track can switch IDs over time.
    labels = update_labeled_frames(
        args.slp_path, matching_dict, args.start_frame, args.end_frame
    )
    labels.save(args.output_path)
    logger.info("Done reconstructing SLEAP file in %s seconds.", time.time() - t0)

    logger.info("Complete NAPS runtime: %s", time.time() - t0_total)


if __name__ == "__main__":
    main()
