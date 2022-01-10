module ctrl_vars
  implicit none
  save

  !----- calculation settings -----
  include '/Users/tatsuzaki98/Lab/thailand-sibuc-211128/include/define.cwd.in'

  integer,parameter :: ibin = 4 !ifort:1 , gfortran:4
  character(16) ,parameter :: csave = 'th-gsmap' !save name
  character(128),parameter :: outflw_path = '/Users/tatsuzaki98/Lab/camaflood/out/th-gsmap_0.1deg/'
  character(128) ,parameter :: sibuc_path = '/Users/tatsuzaki98/Lab/thailand-sibuc-211128/output/'
  character(128) ,parameter :: land_path = '/Users/tatsuzaki98/Lab/thailand-sibuc-211128/out_basic/'
  character(16) ,parameter :: runname = 'proc' !SiBUC Runname
  character(16) ,parameter :: suffix = cname//cresl 
  character(128) ,parameter :: map_path = '/Users/tatsuzaki98/Lab/cwd/map/'
  real,parameter :: we = 0.6 !Irrigation efficiency
  real,parameter :: dd = 12.e0   !Daily Declease of paddy water by penetration 12mm/day
  real,parameter :: wu = 307.e0 !Water use [L/(person*day)]

  !----- calculation options -----
  logical,parameter :: b1gpath = .true. !1grid cell path
  logical,parameter :: bjump = .true. !fill CaMa-Jump
  logical,parameter :: burbanwl = .false. !uraban water line

  !----- calculation vars -----
  real wd(mx,my) , wr(mx,my) !water demand and water resources  [m3/day]
  real wd_DI(mx,my) !Domestic + Industrial water demand
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
