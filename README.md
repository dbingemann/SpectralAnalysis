# SpectralAnalysis

An app that performs spectral analysis on the fly

## General Purpose

This app is made to work along with Enlighten. When capturing a file in Enlighten, a single specturm is saved to the EnlightenSpectra folder under the User's "Documents".

This app monitors the most recent folder with a data stamp for the most recent file. This most recent file is imported and displayed. If a new file is captured in Enlighten to this (or an even more recent folder), this newest file is displayed instead.

## Chemometric Analysis

The app then performs an analysis as specified by the user. Typically, this is the application of a chemometric model, if multiple models are available, the user can chose between them, or apply all at once. The analysis results are displayed below the spectrum. Shown is a SIMCA classification analysis with multiple models and one matching model. Additional graphical feedback about the match is shown if there is exactly one matching model.

For some analysis model tuning parameters might be useful. Those can also be defined in the analysis script and are then added to the user interface. An example is the acceptance threshold in the present app. This parameter can be adjusted without having to create a new model.


![Screen Shot of the app matching a sample](https://github.com/dbingemann/SpectralAnalysis/blob/main/doc/ThresholdAdjustmentUpdate.png)

## Disclaimer

This tool is a development tool that provides a nice interface for building and testing models for chemometric development, but it has not been developed nor tested for implementation into production environments and is not CFR compliant.

