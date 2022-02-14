module ctrl_vars
  implicit none
  save

  !----- sibuc settings -----
  integer, parameter :: MX = 87
  integer, parameter :: MY = 95
  integer, parameter :: ISY = 2010
  integer, parameter :: IEY = 2020
  character(8), parameter :: CNAME = 'th-gsmap'
  character(6), parameter :: CRESL = '0.1deg'


  !----- depot path ---
  character(128), parameter :: DEPOT_PATH = '/Volumes/tz98-hdd-bu/Depot/raw'
  character(128), parameter :: OUTFLW_PATH = trim(DEPOT_PATH)//'/outflw_th_biased_dam_paddy0.1deg/'
  character(128), parameter :: SIBUC_PATH = trim(DEPOT_PATH)//'/sibuc_th-fixed_paddy/'
  character(128), parameter :: LAND_PATH = trim(DEPOT_PATH)//'/landsurface/'
  character(128), parameter :: MAP_PATH =  trim(DEPOT_PATH)//'/cwd_th-Bgsmap-Wdam-0.1deg/map/'


  !----- calc settings -----
  character(16) ,parameter :: runname = 'proc' !SiBUC Runname
  character(16) ,parameter :: suffix = cname//cresl 
  character(16) ,parameter :: csave = 'th-gsmap' !save name
  integer,parameter :: ibin = 4 !ifort:1 , gfortran:4
  real,parameter :: we = 0.6 !Irrigation efficiency
  real,parameter :: dd = 12.e0   !Daily Declease of paddy water by penetration 12mm/day
  real,parameter :: wu = 307.e0 !Water use [L/(person*day)]

  logical,parameter :: b1gpath = .true. !1grid cell path
  logical,parameter :: bjump = .true. !fill CaMa-Jump
  logical,parameter :: burbanwl = .true. !uraban water line


  !----- calculation vars -----
  real wd(mx,my) , wr(mx,my) !water demand and water resources  [m3/day]
  real wd_DI(mx,my) !Domestic + Industrial water demand
  real WD_A(mx,my)  !Agricultural water demand, 12mm/day
  real wl(mx,my) !water lack, +:lack ; -:surplus


  !----- comomon vars -----
  real mask(mx,my)
  real area(mx,my)
  real catchment2(mx,my) !catchment(original)
  real paddy(mx,my)
  integer pref(mx,my) !Prefecture
  integer jx(mx,my) , jy(mx,my) !CaMa Next Grids
  integer rank(mx,my,3) !1:rivre number 2:from estuary 3:branch number
  integer ig(mx,my) , jg(mx,my) !get grid
  integer iuwl(mx,my) !Urban Water Line Mask, 1:Osaka
end module ctrl_vars
