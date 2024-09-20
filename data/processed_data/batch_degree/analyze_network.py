import sys
import networkx as nx
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd


def groupByBouts(
    interaction_dataframe,
    origin_col="Origin interactor",
    dest_col="Destination interactor",
    interaction_col="Interaction Name",
    frame_col="Interaction Frame",
    frame_buffer=20,
):
    # Set the bout counter to 1
    bout_counter = 1
    # Create a list to store the bouts dataframes
    bouts_list = []

    # Iterate by bouts
    for _, bout_dataframe in interaction_dataframe.groupby(
        [
            interaction_col,
            origin_col,
            dest_col,
            interaction_dataframe[frame_col].diff().abs().gt(frame_buffer).cumsum(),
        ]
    ):
        # Add the bout number to the bout dataframe
        bout_dataframe["Bout"] = bout_counter

        # Append the bout dataframe to the bouts list
        bouts_list.append(bout_dataframe)

        # Increment the bout counter
        bout_counter += 1

    # Concatenate all the bout dataframes in the list
    bouts_dataframe = pd.concat(bouts_list, ignore_index=True)

    # Sort the bouts dataframe by the interaction frame
    bouts_dataframe = bouts_dataframe.sort_values(by=frame_col)

    # Return the bouts dataframe
    return bouts_dataframe


df = pd.read_table(sys.argv[1])
df_onetype = df[df["Interaction Name"] == sys.argv[3]]
if sys.argv[4] == "True":
    df_bouts = groupByBouts(df_onetype)
    df_bouts_unique = df_bouts.drop_duplicates(subset="Bout")
    df_bouts.to_csv(sys.argv[2])
    counts = pd.DataFrame(
        {
            "count": df_bouts_unique.groupby(
                ["Origin interactor", "Destination interactor"]
            ).size()
        }
    ).reset_index()
else:
    counts = pd.DataFrame(
        {
            "count": df_onetype.groupby(
                ["Origin interactor", "Destination interactor"]
            ).size()
        }
    ).reset_index()
counts["log_count"] = np.log10(counts["count"]) + 1
G = nx.from_pandas_edgelist(
    counts,
    source="Origin interactor",
    target="Destination interactor",
    edge_attr=("count", "log_count"),
)
degree_G = nx.degree(G, weight="count")
degree_df = pd.DataFrame(degree_G, columns=["Track", "Degree"])
# degree_df.to_csv(sys.argv[2])
