
#%%
import numpy as np
from scipy.signal import find_peaks as fp 
from scipy.signal import peak_prominences
from scipy.signal import savgol_filter
from scipy.ndimage import gaussian_filter1d, median_filter
from scipy.interpolate import make_interp_spline
import pandas as pd
# from pybaselines import Baseline
# from matplotlib import pyplot as plt
# import peakutils

# ref  https://github.com/michaelmarty/AnalChem/blob/master/Statistics3.ipynb
# https://github.com/chrisp33/Python-For-RnD/blob/30085c0f124694438b5e32c7d20759895d29c680/Bacteria_Proc_Methods.py#L70

#%%

def smooth_peaks(intensity, filter, window=3, iter = 2, **kwargs):
    """
    Smooth the intensity array using different filters
    if iter = 0, it will not smooth, return the original intensity
    """
    for i in range(iter):
        if filter == "savgol":
            intensity = savgol_filter(intensity, window, polyorder=1, **kwargs)
        if filter == "mean":
            intensity = np.convolve(intensity, np.ones(window)/window, mode='same')
        if filter == "gaussian":
            intensity = gaussian_filter1d(intensity, window)
        if filter == "median":
            intensity = median_filter(intensity, window)
    return intensity


def integ(x,y):
    rng = np.random.default_rng(42)

    centers = (30.5, 72.3, 112.1)
    x = np.arange(0, 120)
    y = (peakutils.gaussian(x, 5, centers[0], 3)
        + peakutils.gaussian(x, 7, centers[1], 10)
        + peakutils.gaussian(x, 6, centers[2], 5)
        + rng.random(x.size)*0.2)
     
    peak_indices, _ = fp(-y, prominence=1)

    fig, ax = plt.subplots()
    ax.plot(x, y, label="Data")
    ax.plot(x[peak_indices], y[peak_indices], "x", color="r", label="End points")
    # ax.title("Data with noise")


    x1, x2 = x[peak_indices]

    # Interpolate the peak
    interp = make_interp_spline(x, y, k=1)  
    # m is the slope of the line connecting the two ends of the peak
    m = (interp(x2) - interp(x1))/(x2 - x1)

    # x values for the area under the peak
    x_peak = np.linspace(x1, x2, 200)


    ax.fill_between(x_peak, interp(x_peak), m*(x_peak - x1) + interp(x1), alpha=0.5, 
                    label="Desired area")
    ax.fill_between(x_peak, m*(x_peak - x1) + interp(x1), alpha=0.5, 
                    label="Trapezoid area")

    ax.legend()

    # Integral under the peak minus the area of the trapezoid connecting the two
    # ends of the peak.
    # NOTE: EDGE CASE OF PEAK ENDS GOING BELOW THE X-AXIS IS NOT BEING CONSIDERED
    # integral = interp.integrate(x1, x2) - 0.5*(x2-x1)*(interp(x1) + interp(x2))
    integral = integral_range(x, y, x1, x2)[0] -  0.5*(x2-x1)*(interp(x1) + interp(x2))

def find_peak_opt(intensity, height= 1, **kwargs):
    # TODO add interpolation step to get more accurate peak position
    # TODO add peak window
    prominence = float(height)
    intensity = np.array(intensity)
    peaks = fp(intensity, height=height, prominence=0.8*height, **kwargs)
    return peaks

#%%
def find_peak2(intensity, height= 1):
    """ only get one peak 
    
    """
    prominence = float(height)
    intensity = np.array(intensity)
    if np.all(intensity < prominence):
        return 0
    else:
        peak_height = np.max(intensity)
        RT = np.argmax(intensity)
        RT = np.array([RT])
        prom = peak_prominences(intensity, RT)
        # 0: RT 
        # 1: start index
        # 2: end index

        return {"peak_height": peak_height,
                 'RT': RT, 'prom': prom}
#%%

#%%


def find_peak2_d(d, height = 1):
    """
    d: dictionary of dataframes
    height: a hard cutoff not a promience 
    """
    promience = float(height)

    
    holder = dict(sample_id = [], observed_rt = [], observed_peak_height = [], 
                observed_peak_start = [], observed_peak_end = [])
    for i in list(d.keys()):
        df = d[i]
        intensity = df[df.columns[1]]
        RT_list = df[df.columns[0]]

        peaks = find_peak2(intensity, height = promience)
        sample_id  = i
        holder["sample_id"].append(sample_id)
        if peaks == 0:
            holder["observed_peak_height"].append(0)
            holder["observed_peak_start"].append(0)
            holder["observed_peak_end"].append(0)
            holder["observed_rt"].append(0)
        else:
            holder["observed_peak_height"].append(peaks['peak_height'])
            holder["observed_peak_start"].append(RT_list[peaks["prom"][1][0]])
            holder["observed_peak_end"].append(RT_list[peaks["prom"][2][0]])
            holder["observed_rt"].append(RT_list[peaks["RT"]].values[0])

    final_df = pd.DataFrame(holder)
    return final_df

#%%
# pick and save and reload 
# import pickle
# import os 
# os.chdir("../../../")
# with open('intensity.pkl', 'rb') as f:
    # d = pickle.load(f)
# find_peak2_d(d, height = 1)

#%%
def baseline_correction(rt, intensity, **kwargs):
    baseline_fit = Baseline(rt)
    # return baseline_fit.modpoly(intensity, **kwargs)
    return baseline_fit.mor(intensity, **kwargs)

def baseline_correction2(intensity, degree = 2):
    from peakutils.baseline import baseline
    degree = int(degree)
    intensity = np.array(intensity)
    return baseline(intensity, degree)


# Define a Gaussian distribution function (see Statistics 1 notebook)
def Gaussian(xvalues, mean, standard_deviation):
    sdsquared = standard_deviation**2
    exponent = -(xvalues - mean)**2/(2 * sdsquared)
    preexponential = 1 / np.sqrt(2 * np.pi * sdsquared)
    return preexponential * np.exp(exponent)


# A function to calculate the trapezoid area for two points. This comes directly from the code above.
def data_point_area(x1, x2, y1, y2):
    average_y = (y1 + y2)/2
    delta_x = (x2-x1)
    area = average_y * delta_x
    return area


# A function to calculate the integral for an array
def integral(x, y):
    length = len(x) # Calculate the length of the x array
    indexes = np.arange(1,length) # Create an array of indexes. Don't start at 0 but 1.
    sum_value = 0 # Set the sum value to 0 initially
    for k in indexes:
        # Get the correct x and y values from the list based on their indexes
        x1 = x[k-1]
        y1 = y[k-1]
        x2 = x[k]
        y2 = y[k]
        # Calculate the area for those set of two points from the function above
        area = data_point_area(x1, x2, y1, y2)
        # Add the area from the two points to the total sum
        sum_value = sum_value + area
    return sum_value

# Chop an array to include only x values between minx and maxx
def datachop(x, y, minx, maxx):
    boo1 = np.logical_and(x <= maxx, x >= minx)
    return x[boo1], y[boo1]

def integral_range(x, y, minx, maxx): # This uses np.trapz
    # x provide the spacing between the y values
    x = np.array(x)
    y = np.array(y)
    minx = float(minx)
    maxx = float(maxx)

    cutx, cuty = datachop(x, y, minx, maxx)
    area = np.trapz(cuty, x = cutx)
    return area, cutx, cuty

