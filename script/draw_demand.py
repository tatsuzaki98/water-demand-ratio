import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as patches
import cartopy.crs as ccrs
import cartopy.feature as cfeature
import sys
from cartopy.mpl.ticker import LongitudeFormatter, LatitudeFormatter,LatitudeLocator,LongitudeLocator
import color_BR
from matplotlib.colors import ListedColormap, BoundaryNorm
import namelist
#===============set param===============
isy = 2000 ; iey = 2012

mx = namelist.mx
my = namelist.my
resl = namelist.resl
xmin = namelist.xmin ; xmax = namelist.xmax
ymin = namelist.ymin ; ymax = namelist.ymax
cname = namelist.cname ; cresl = namelist.cresl

resolution:int=2 #0:rough 1:middle 2:hi
undef=np.nan
lakes_10m = cfeature.NaturalEarthFeature('physical', 'lakes', '10m',edgecolor='black',facecolor='none')
states_10m  = cfeature.NaturalEarthFeature('cultural', 'admin_1_states_provinces_lines', '10m',edgecolor='black',facecolor='none')
xloc = np.arange(135.5,136.5,0.5) 
yloc = np.arange(34.5,36.0,0.5) 

data = open('./demand/Irrig-demand2002.bin','rb')
d = np.fromfile(data,np.float32)
d = np.where(d<=0.,np.nan,d)
d = d.reshape(my,mx)
d = np.flipud(d)

fig = plt.figure(figsize=(12,9), dpi=300)
ax = plt.axes(projection=ccrs.PlateCarree())
ax.set_extent([xmin,xmax,ymin,ymax])
plt.title('irrig-demand', fontsize=30)
ax.coastlines(resolution='10m')
#plt.imshow(d[:,:], extent=[xmin,xmax,ymin,ymax], interpolation='nearest', transform=ccrs.PlateCarree(), cmap=cmap, norm=norm)
#plt.colorbar(extend='max' ,shrink=1.0, ticks=bounds, orientation='vertical')
plt.imshow(d[:,:], extent=[xmin,xmax,ymin,ymax], interpolation='nearest', transform=ccrs.PlateCarree(), cmap=plt.cm.jet)
plt.colorbar(extend='max' ,shrink=1.0, orientation='vertical')
#----------------------------------make grid lines---------------------------------------
ax.coastlines(resolution='10m',linewidth=3)
gl = ax.gridlines(xlocs=xloc, ylocs=yloc, crs=ccrs.PlateCarree(), draw_labels=True, linewidth=1, color='gray', alpha=0.5, linestyle='--')
gl.top_labels = False
gl.bottom_labels = True
gl.right_labels = False
gl.left_labels = True
gl.xlines = True
gl.ylines = True
gl.xlabel_style = {'size':10,'color':'black'}
gl.ylabel_style = {'size':10,'color':'red'}
gl.xlocator = LongitudeLocator()
gl.ylocator = LatitudeLocator()
gl.xformatter = LongitudeFormatter()
gl.yformatter = LatitudeFormatter()
ax.add_feature(lakes_10m, linewidth=1.5)
ax.add_feature(states_10m, linewidth=2, linestyle='dashdot')
#----------------------------------------------------------------------------------------
plt.savefig('./demand/Irrig-demand2002.png')
plt.cla()
plt.clf()
plt.close()