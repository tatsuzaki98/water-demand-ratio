program calc_demand
implicit none
include '/lake5/tasaka/sibuc/Data/Yodo2/include/namelist.in'
real,parameter :: we = 0.85 !Irrigation Efficiency
integer,parameter :: ibin = 1 !ifort:1 , gfortran:4
integer,parameter :: isy = 2000 , iey = 2012
character(64) ,parameter :: sibuc_path = '/lake5/tasaka/sibuc/Yodo2/output/'
character(64) ,parameter :: land_path = '/lake5/tasaka/sibuc/Data/Yodo2/out_basic/'
character(16) ,parameter :: runname = 'obs' !SiBUC Runname
character(16) ,parameter :: suffix = cname//cresl 
real win(mx,my) , wout(mx,my)
real w(mx,my) , wp(47)
real ocean(mx,my) , paddy(mx,my) , pp(47) !paddy_prefec
real area(mx,my)
real crpstg(mx,my,36)
integer pref(mx,my)
character(16) cpref(47) !name
integer i , j , k , t
integer irec
integer iy , id , days
character(4) cyear
interface
  subroutine get_monday(iy,day,doy,days)
    integer,intent(in) :: iy
    integer,intent(out),optional :: day(12)
    integer,intent(out),optional :: doy(12)
    integer,intent(out),optional :: days
  end subroutine
end interface
!======================================================================
open(11,file='/lake5/tasaka/data_prefecture.csv', action='read')
do i = 1 , 47
  read(11,*) j , cpref(i)
end do
close(11)

open(12,file=trim(land_path)//'Prefecture_'//trim(suffix)//'.bin',&
&form='unformatted', access='direct',recl=mx*my*ibin, action='read', status='old')
read(12,rec=1) ((pref(i,j),i=1,mx),j=1,my)
close(12)

open(13,file=trim(land_path)//'ClassFrac_'//trim(suffix)//'_ALOS.bin',&
&form='unformatted', access='direct',recl=mx*my*ibin, action='read', status='old')
read(13,rec=2)  ((ocean(i,j),i=1,mx),j=1,my)
read(13,rec=15) ((paddy(i,j),i=1,mx),j=1,my)
close(13)

open(14,file=trim(land_path)//'cropstage_'//trim(suffix)//'_MAFF.bin',&
&form='unformatted', access='direct',recl=mx*my*ibin, action='read', status='old')
do t = 1 , 36
  read(14,rec=t) ((crpstg(i,j,t),i=1,mx),j=1,my)
end do
close(14)

open(15,file=trim(land_path)//'Area_'//trim(suffix)//'.bin',&
&form='unformatted', access='direct',recl=mx*my*ibin, action='read', status='old')
read(15,rec=1) ((area(i,j),i=1,mx),j=1,my)
close(15)

pp(:) = 0.e0
do k = 1 , 47
  do j = 1 , my
  do i = 1 , mx
    if( pref(i,j) == k )then
      pp(k) = pp(k) + area(i,j) * paddy(i,j) * 1.e6 !m2
    end if
  end do
  end do
end do      

do iy = isy , iey
  write(cyear,'(i4)') iy
  call get_monday(iy=iy,days=days)
  open(21,file=trim(sibuc_path)//'SiBUC_'//trim(runname)//'_'//cyear//'_'//trim(suffix)//'.bin',&
& form='unformatted', access='direct', recl=mx*my*ibin, status='old', action='read')
  
  w(:,:) = 0.e0
  do id = 1 , days
    call get_term(iy,id,t)
    irec = (id-1) * 23 + 22
    read(21,rec=irec) ((win(i,j),i=1,mx),j=1,my) 
    irec = (id-1) * 23 + 23
    read(21,rec=irec) ((wout(i,j),i=1,mx),j=1,my)
    do j = 1 , my
    do i = 1 , mx
      if( paddy(i,j) <= 0.e0 ) cycle
      if( crpstg(i,j,t) == 1 .or. crpstg(i,j,t) == 3 .or. crpstg(i,j,t) == 4 .or. crpstg(i,j,t) == 5 )then
        w(i,j) = w(i,j) + ( win(i,j) * (1.e0 - ocean(i,j)) / paddy(i,j) + 12.e0 ) / we !mm
      else
        w(i,j) = w(i,j) + win(i,j) * (1.e0 - ocean(i,j)) / paddy(i,j) / we
      end if
    end do
    end do
  end do
  close(21)
  open(31,file='./demand/Irrig-demand'//cyear//'.bin',&
& form='unformatted', access='direct', recl=mx*my*ibin, status='replace')
  write(31,rec=1) ((w(i,j),i=1,mx),j=1,my)
  close(31)

  wp(:) = 0.e0
  do j = 1 , my
  do i = 1 , mx
    wp( pref(i,j) ) = wp( pref(i,j) ) + w(i,j) * area(i,j) * paddy(i,j) * 1.e3 ! mm * 1.e-3 * km2 * 1.e6 => * 1.e3 m3
  end do
  end do  
  
  open(32,file='./demand/Irrig-demand_prefec'//cyear//'.txt')
  do k = 1 , 47
    if( pp(k) > 0.e0 )then  
      write(32,*) cpref(k) , wp(k) * 1.e-8 , wp(k) / pp(k) * 1.e3
    end if   
  end do
  close(32)

end do  

end program calc_demand    
!======================================================================
subroutine get_monday(iy,day,doy,days)
implicit none
integer,intent(in) ::  iy
integer,intent(out),optional :: day(12)
integer,intent(out),optional :: doy(12)
integer,intent(out),optional :: days
integer leap

leap = 0
if(mod(iy,4) == 0) leap = 1
if(mod(iy,100) == 0 .and. mod(iy,400) /= 0) leap = 0
if(leap.eq.0)then
  if( present(day) )  day(1:12)=(/31,28,31,30,31,30,31,31,30,31,30,31/)
  if( present(doy) )  doy(1:12)=(/0,31,59,90,120,151,181,212,243,273,304,334/)
  if( present(days) ) days=365     
else
  if( present(day) )  day(1:12)=(/31,29,31,30,31,30,31,31,30,31,30,31/)
  if( present(doy) )  doy(1:12)=(/0,31,60,91,121,152,182,213,244,274,305,335/)
  if( present(days) ) days = 366
end if
end subroutine get_monday
!======================================================================
subroutine get_term(iy,day,t)
integer,intent(in) ::  iy , day
integer,intent(out) :: t
integer leap
integer doy(13)
integer im , id

leap=0
if( mod(iy,4) == 0 ) leap=1
if( mod(iy,100) == 0 .and. mod(iy,400) /= 0 ) leap=0
if( leap == 0 )then
  doy(1:13)=(/0,31,59,90,120,151,181,212,243,273,304,334,365/)
else
  doy(1:13)=(/0,31,60,91,121,152,182,213,244,274,305,335,366/)
end if

do im = 1 , 12
  if( day <= doy(im+1) )then
    id = day - doy(im)
    select case(id)
      case(:10)   ; t = (im-1) * 3 + 1
      case(11:20) ; t = (im-1) * 3 + 2
      case(21:)   ; t = (im-1) * 3 + 3
      case default
        write(6,*) 'ERROR get_term, id'
        stop
    end select
    exit
  end if
end do
if( im == 13 )then
  write(6,*) 'ERROR get_term, im'
  stop
end if  


return
end subroutine get_term