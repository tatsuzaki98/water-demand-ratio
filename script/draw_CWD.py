import numpy as np
import matplotlib.pyplot as plt
import matplotlib.patches as patches
import matplotlib.ticker as mticker
import cartopy.crs as ccrs
import cartopy.feature as cfeature
import sys
from cartopy.mpl.ticker import LongitudeFormatter, LatitudeFormatter,LatitudeLocator,LongitudeLocator
import color_BR
from matplotlib.colors import ListedColormap, BoundaryNorm
import namelist
#===============set param===============
isy = 1981 ; iey = 2015
ctitle = '(natural)'
suffix = '_JRA'

mx = namelist.mx
my = namelist.my
resl = namelist.resl
xmin = namelist.xmin ; xmax = namelist.xmax
ymin = namelist.ymin ; ymax = namelist.ymax
cname = namelist.cname ; cresl = namelist.cresl
damx=[21,31,33,31,34,30,27,29]
damy=[13,20,23,24,23,21,18,17]

resolution:int=2 #0:rough 1:middle 2:hi
undef=np.nan
lakes_10m = cfeature.NaturalEarthFeature('physical', 'lakes', '10m',edgecolor='black',facecolor='none')
states_10m  = cfeature.NaturalEarthFeature('cultural', 'admin_1_states_provinces_lines', '10m',edgecolor='black',facecolor='none')
#=======================================
data = open('./output/CWD_Japan3min'+suffix+'_'+str(isy)+'-'+str(iey)+'.bin','rb')
d0 = np.fromfile(data,np.float32)
d = np.where(d0<0.,np.nan,d0)
d = d.reshape(my,mx)
d = np.flipud(d)

d0 = d0.reshape(my,mx)
d0 = np.flipud(d0)
d0 = np.where( d0 == 1.e0 , 2. , d0 )


def make_col(RGB255):
  color=np.array(RGB255)
  color=color/255.e0
  RGB1=color.tolist()
  return RGB1
g5=make_col([35,113,25])
g8=make_col([34,187,23])

cmap = color_BR.make_cmap2(11)
cmap = cmap.reversed()
cmap.set_over(g8)
bounds = np.arange(0,1.1,0.1)
norm = BoundaryNorm(bounds,cmap.N)  #Essentially when plot


fig = plt.figure(figsize=(12,8), dpi=300)
ax = plt.axes(projection=ccrs.PlateCarree())
ax.set_extent([123,150,30,46])
ax.coastlines(resolution='10m',linewidth=1, zorder=2)
ax2 = plt.axes([0.10,0.55,0.3,0.18], projection=ccrs.PlateCarree())
ax2.set_extent([123,133,24,30]) #[123,150,24,48]
ax2.coastlines(resolution='10m', linewidth=1)

ax.set_title('CWD'+ctitle, fontsize=30)
draw = ax.imshow(d[40:360,:], extent=[123,150,30,46], interpolation='nearest', transform=ccrs.PlateCarree(), cmap=cmap, norm=norm, zorder=1)
ax2.imshow(d[360:,:200], extent=[123,133,24,30], interpolation='nearest', transform=ccrs.PlateCarree(), cmap=cmap, norm=norm, zorder=1)
fig.colorbar(draw, ax=ax, extend='max' ,shrink=0.75, ticks=bounds, orientation='vertical')
#----------------------------------make grid lines---------------------------------------
gl = ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True, linewidth=1, color='gray', alpha=0.5, linestyle='--')
gl.top_labels = False
gl.bottom_labels = True
gl.right_labels = False
gl.left_labels = True
gl.xlines = True
gl.ylines = True
gl.xlabel_style = {'size':10,'color':'black'}
gl.ylabel_style = {'size':10,'color':'black'}
gl.xlocator = LongitudeLocator()
gl.ylocator = LatitudeLocator()
gl.xformatter = LongitudeFormatter()
gl.yformatter = LatitudeFormatter()
#ax.add_feature(lakes_10m, linewidth=0.5)
ax.add_feature(states_10m, linewidth=0.5, zorder=3)

gl2=ax2.gridlines(crs=ccrs.PlateCarree(),draw_labels=True,linewidth=0.5,color='gray',alpha=0.5,linestyle='--')
gl2.top_labels=False
gl2.bottom_labels=True
gl2.right_labels=True
gl2.left_labels=False
gl2.xlines=True
gl2.ylines=True
gl2.xlabel_style={'size':8,'color':'red','weight':'bold'}
gl2.ylabel_style={'size':8,'color':'red','weight':'bold'}
gl2.xlocator = mticker.FixedLocator([125,130])
gl2.ylocator = mticker.FixedLocator([27])
gl2.xformatter=LongitudeFormatter()
gl2.yformatter=LatitudeFormatter()
#----------------------------------------------------------------------------------------
plt.savefig('./CWD'+suffix+'.png')
#plt.savefig('./CWD.png',bbox_inches='tight' )
plt.cla()
plt.clf()
plt.close()

#sys.exit()

for year in range(isy,iey+1):
  print( year )       
  data = open('./output/CWD_Japan3min'+suffix+'_'+str(year)+'.bin','rb')
  d0 = np.fromfile(data,np.float32)
  d = np.where(d0<0.,np.nan,d0)
  d = d.reshape(my,mx)
  d = np.flipud(d)
  d0 = d0.reshape(my,mx)
  d0 = np.flipud(d0)
  d0 = np.where( d0 == 1.e0 , 2. , d0 )

  fig = plt.figure(figsize=(12,8), dpi=300)
  ax = plt.axes(projection=ccrs.PlateCarree())
  ax.set_extent([123,150,30,46])
  ax.coastlines(resolution='10m',linewidth=1, zorder=2)
  ax2 = plt.axes([0.10,0.55,0.3,0.18], projection=ccrs.PlateCarree())
  ax2.set_extent([123,133,24,30]) #[123,150,24,48]
  ax2.coastlines(resolution='10m', linewidth=1)
  
  ax.set_title('CWD'+ctitle+' '+str(year), fontsize=30)
  draw = ax.imshow(d[40:360,:], extent=[123,150,30,46], interpolation='nearest', transform=ccrs.PlateCarree(), cmap=cmap, norm=norm, zorder=1)
  ax2.imshow(d[360:,:200], extent=[123,133,24,30], interpolation='nearest', transform=ccrs.PlateCarree(), cmap=cmap, norm=norm, zorder=1)
  fig.colorbar(draw, ax=ax, extend='max' ,shrink=0.75, ticks=bounds, orientation='vertical')
  #----------------------------------make grid lines---------------------------------------
  gl = ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True, linewidth=1, color='gray', alpha=0.5, linestyle='--')
  gl.top_labels = False
  gl.bottom_labels = True
  gl.right_labels = False
  gl.left_labels = True
  gl.xlines = True
  gl.ylines = True
  gl.xlabel_style = {'size':10,'color':'black'}
  gl.ylabel_style = {'size':10,'color':'black'}
  gl.xlocator = LongitudeLocator()
  gl.ylocator = LatitudeLocator()
  gl.xformatter = LongitudeFormatter()
  gl.yformatter = LatitudeFormatter()
  #ax.add_feature(lakes_10m, linewidth=0.5)
  ax.add_feature(states_10m, linewidth=0.5, zorder=3)
  
  gl2=ax2.gridlines(crs=ccrs.PlateCarree(),draw_labels=True,linewidth=0.5,color='gray',alpha=0.5,linestyle='--')
  gl2.top_labels=False
  gl2.bottom_labels=True
  gl2.right_labels=True
  gl2.left_labels=False
  gl2.xlines=True
  gl2.ylines=True
  gl2.xlabel_style={'size':8,'color':'red','weight':'bold'}
  gl2.ylabel_style={'size':8,'color':'red','weight':'bold'}
  gl2.xlocator = mticker.FixedLocator([125,130])
  gl2.ylocator = mticker.FixedLocator([27])
  gl2.xformatter=LongitudeFormatter()
  gl2.yformatter=LatitudeFormatter()
  #----------------------------------------------------------------------------------------
  plt.savefig('./fig/CWD'+suffix+'_'+str(year)+'.png')
  plt.cla()
  plt.clf()
  plt.close()