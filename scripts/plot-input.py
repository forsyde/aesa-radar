#!/bin/python3

import matplotlib.pyplot as plt
import os
import math
from matplotlib import colors
import numpy as np
import argparse

parser = argparse.ArgumentParser(description='Plots the AESA signal processing input data.')
parser.add_argument('inpath', nargs=1, type=str,  metavar='PATH',
                    help='Path to generated antenna data file.')
parser.add_argument('-f', '--first', nargs='?', type=int,  metavar='VAL', default=250,
                    help='First row of plotted samples. Default 250')
parser.add_argument('-l', '--last', nargs='?', type=int,  metavar='VAL', default=375,
                    help='First row of plotted samples. Default 375')

args = parser.parse_args()
     
data = np.genfromtxt(args.inpath[0], delimiter=" ")[:args.last]
reals=np.transpose([i[0::2] for i in data])
imags=np.transpose([i[1::2] for i in data])
absval=list(map(lambda a, b: list(map(lambda x, y: 20*math.log10(math.sqrt(x*x+y*y)), a, b)), reals, imags))

pdata=[reals,imags]

fig, axs = plt.subplots(1, 1, figsize=(12,4))
images = []
# for i in range(2):
#     # Generate data with a range that varies from one plot to the next.
images.append(axs.imshow(absval, cmap="RdBu", aspect='equal', interpolation="nearest"))
axs.grid(False)
axs.label_outer()
axs.set_xlim(args.first,args.last)

axs.set_ylabel("antenna")   
axs.set_xlabel("sample")

# Find the min and max of all colors for use in setting the color scale.
vmin = min(image.get_array().min() for image in images)
vmax = max(image.get_array().max() for image in images)
norm = colors.Normalize(vmin=vmin, vmax=vmax)
for im in images:
    im.set_norm(norm)

fig.colorbar(images[0], ax=axs, orientation='horizontal', fraction=.2)
filename=os.path.splitext(os.path.basename(args.inpath[0]))[0] + '_s.pdf'
plt.savefig(filename, bbox_inches='tight')
plt.show()
