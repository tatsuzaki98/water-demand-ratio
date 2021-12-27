module ctrl_vars
implicit none
save
!----- calculation settings -----
include '/data01/tasaka/150run/sibuc/Data/Japan3min/include/namelist.in'
integer,parameter :: ibin = 1 !ifort:1 , gfortran:4
integer,parameter :: isy = 1981 , iey = 2015
character(16) ,parameter :: csave = 'JRA' !save name
character(128),parameter :: outflw_path = '/data01/tasaka/150run/CaMa-Flood/out_daily/Jpn_3min_qsb_JRA/'
character(64) ,parameter :: sibuc_path = '/data01/tasaka/150run/sibuc/Japan3min_current/output/'
character(64) ,parameter :: land_path = '/data01/tasaka/150run/sibuc/Data/Japan3min/out_basic/'
character(16) ,parameter :: runname = 'JRA' !SiBUC Runname
character(16) ,parameter :: suffix = cname//cresl 
character(32) ,parameter :: map_path = './map/'
real,parameter :: we = 0.85 !Irrigation efficiency
real,parameter :: dd = 12.e0   !Daily Declease of paddy water by penetration 12mm/day
real,parameter :: wu = 307.e0 !Water use [L/(person*day)]
!----- calculation options -----
logical,parameter :: b1gpath = .true. !1grid cell path
logical,parameter :: bjump = .true. !fill CaMa-Jump
logical,parameter :: burbanwl = .false. !uraban water line
!----- calculation vars -----
real wd(mx,my) , wr(mx,my) !water demand and water resources  [m3/day]
real wd_DI(mx,my) !Domestic + Industrial water demand
real WD_A(mx,my)  !Agricultural water demand, 12mm/day
real wl(mx,my) !water lack, +:lack ; -:surplus
!----- comomon vars -----
real mask(mx,my)
real area(mx,my)
real paddy(mx,my)
integer pref(mx,my) !Prefecture
integer jx(mx,my) , jy(mx,my) !CaMa Next Grids
integer rank(mx,my,3) !1:rivre number 2:from estuary 3:branch number
integer ig(mx,my) , jg(mx,my) !get grid
integer iuwl(mx,my) !Urban Water Line Mask, 1:Osaka

end module ctrl_vars