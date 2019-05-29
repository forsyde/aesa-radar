#!/bin/python3

import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np

inFile="gen/AESA_OUTPUT.csv"
plotName="AESA"
offs=0

with open(inFile) as fp:  
   line = fp.readline()
   rangeBins = []
   beams = []
   while line:
       line = fp.readline()
       if (len(line)>1):
           window=[float(s) for s in line.strip().split(' ')]
           rangeBins.append(window)
       else:
           if (rangeBins):
               beams.append(rangeBins)
           rangeBins=[]
   print(len(beams))


maxv= max([max([max (row) for row in beam]) for beam in beams])
   
fig, (ax1,ax2,ax3,ax4,ax5,ax6,ax7,ax8,axcb) = plt.subplots(1,9, figsize=(40,10), gridspec_kw={'width_ratios':[1,1,1,1,1,1,1,1,0.08]})
ax1.get_shared_y_axes().join(ax2,ax3,ax4,ax5,ax6,ax7,ax8)
axes = [ax1,ax2,ax3,ax4,ax5,ax6,ax7,ax8,axcb]
heatms = []
for i in list(range(7)):
   x = beams[i+offs]
   y=np.array([np.array(xi) for xi in x])
   g = sns.heatmap(y, vmin=0, vmax=maxv, square=True, ax=axes[i],cbar=False)
   heatms.append(g)
   g.set_title('beam ' + str(i))

y=np.array([np.array(xi) for xi in beams[7+offs]])
g = sns.heatmap(y, vmin=0, vmax=maxv, square=True, ax=ax8, cbar_ax=axcb)
heatms.append(g)
g.set_title('beam 8')

for i in list(range(7)):  
   heatms[i+1].set_ylabel('')
   heatms[i+1].set_xlabel('')
   heatms[i+1].set_yticks([])
   
plt.yticks(rotation=0,fontsize=10);
plt.xticks(fontsize=12);
plt.tight_layout()

plt.savefig(plotName + '.pdf')
plt.show()
