
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Flux measurements

This is a small project to process data from the EGM4 IRGA. A work in
progress, mainly personal project.

Currently there is a file for processing cleaned data. There is work on
a script to process raw-data from the EGM4, merge it with other
measurements of the same day and merge it with data from a TinyTag
temperature logger and PAR Photon Flux Sensor coupled to a EM50 logger
station.

Stil in the planing phase is to expand to the EGM5 which has a slightly
different file system

# The EGM4

The EGM4 only stores 1000 lines of data internally and therefore several
files might be generated during one days measurements.
