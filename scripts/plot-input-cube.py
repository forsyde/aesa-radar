#!/bin/python3

import matplotlib.pyplot as plt
import math
import os
from matplotlib import colors
import numpy as np
import argparse

parser = argparse.ArgumentParser(description='Plots the AESA signal processing data.')
parser.add_argument('inpath', nargs=1, type=str,  metavar='PATH',
                    help='Path to AESA output data')
parser.add_argument('-t', '--threshold', nargs='?', type=int,  metavar='VAL', default=15,
                    help='Threshold value for detected objects. If 0 then detection values are not annotated. Default: 15')

args = parser.parse_args()

nantennas=16
npulses=256
nbins=1024

data = np.genfromtxt(args.inpath[0], delimiter=" ")
cube = np.transpose(data[:npulses*nbins])
abscube = []
for i in range(nantennas):
    reals = cube[2*i]
    imags = cube[2*i+1]
    abscube.append(np.array(list(map(lambda x, y: 20*math.log10(math.sqrt(x*x+y*y)), reals, imags))))

antennas=[]
for c in abscube:
    mat=np.transpose(np.array(np.split(c,npulses)))
    antennas.append(mat)

fig, axs = plt.subplots(1, nantennas, figsize=(6*nantennas/4,5), sharey=True)
images = []
for i in range(nantennas):
    images.append(axs[i].imshow(antennas[i], cmap='RdBu', aspect='equal', interpolation="nearest"))
    axs[i].grid(False)
    axs[i].label_outer()
    axs[i].set_title('antenna ' + str(i), size=10)
    axs[i].tick_params(which='major', labelsize=8# , labelrotation=45
    );

vmin = min(image.get_array().min() for image in images)
vmax = max(image.get_array().max() for image in images)
norm = colors.Normalize(vmin=vmin, vmax=vmax)
for im in images:
    im.set_norm(norm)

    
fig.colorbar(images[nantennas-1], ax=axs[nantennas-1], orientation='vertical',use_gridspec=True)
# fig.colorbar(images[nantennas-1], ax=axs.ravel().tolist(), shrink=0.7, fraction=.1)
   
plt.yticks(rotation=0,fontsize=10);
plt.xticks(fontsize=12);
plt.tight_layout()

filename=os.path.splitext(os.path.basename(args.inpath[0]))[0] + '_C.pdf'
plt.savefig(filename)
# plt.show()
