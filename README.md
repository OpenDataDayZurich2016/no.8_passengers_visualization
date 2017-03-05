# no.8_passengers_visualization

This repository collects the code produced at ODD Zurich 2017 for visualizing the passenger spatial-temporal data on the station level.

It is connected to Issue #8： number of passengers traveling visualization https://github.com/OpenDataDayZurich2016/ideas/issues/8.

A brief prototype of our work is shown here: https://invis.io/ZDAPT1M82.

There are passengers data (in and out) of stations at every stop and we try to analyze the spatial temporal pattern of stations on certain days (Monday-Thursday, Friday, weekend). We try to cluster stations based on their passenger volume change pattern through the day. Using Dynamic Time Warping (DTW) Distances as features allow the analysis of time series data, (Kate, 2016). The hierarchical cluster analysis of data on day type 6 is implemented in R programming. The clusters of example stations are visualization on the map in QGIS.
